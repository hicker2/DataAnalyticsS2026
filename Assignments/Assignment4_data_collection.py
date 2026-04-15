#!/usr/bin/env python3
"""
GitHub GraphQL data fetcher.
Reads usernames from usernames.txt and produces:
  - users.csv          — one row per user
  - pull_requests.csv  — one row per PR (100 most recent)
  - users_prs.csv      — joined table (user x PR)
Uses the `gh` CLI (must be authenticated: gh auth login).
One API call per user.
"""

import csv
import json
import subprocess
import sys

# ── helpers ───────────────────────────────────────────────────────────────────

def gh_graphql(query: str, variables: dict) -> dict:
    payload = json.dumps({"query": query, "variables": variables})
    result = subprocess.run(
        ["gh", "api", "graphql", "--input", "-"],
        input=payload,
        capture_output=True,
        text=True,
        encoding="utf-8",
    )
    if result.returncode != 0:
        raise RuntimeError(f"gh api failed: {result.stderr.strip()}")
    data = json.loads(result.stdout)
    if "errors" in data:
        raise RuntimeError(f"GraphQL errors: {data['errors']}")
    return data


def load_usernames(path: str = "usernames.txt") -> list[str]:
    with open(path) as f:
        return [line.strip() for line in f if line.strip()]


# ── Single combined query: user stats + 100 most recent PRs ──────────────────

COMBINED_QUERY = """
query($login: String!) {
  user(login: $login) {
    login
    name
    bio
    company
    location
    createdAt
    updatedAt
    followers { totalCount }
    following  { totalCount }
    repositories(privacy: PUBLIC) { totalCount }
    pullRequests(first: 30, orderBy: {field: CREATED_AT, direction: DESC}) {
      nodes {
        number
        title
        state
        isDraft
        createdAt
        updatedAt
        mergedAt
        closedAt
        url
        additions
        deletions
        baseRepository { nameWithOwner }
        mergedBy { login }
      }
    }
  }
}
"""

# ── fetch ─────────────────────────────────────────────────────────────────────

def fetch_user_and_prs(login: str) -> tuple[dict | None, list[dict]]:
    data = gh_graphql(COMBINED_QUERY, {"login": login})
    user = data.get("data", {}).get("user")
    if user is None:
        print(f"[WARN] User '{login}' not found - skipping.", file=sys.stderr)
        return None, []
    
    user_row = {
        "login":         user["login"],
        "name":          user.get("name") or "",
        "bio":           (user.get("bio") or "").replace("\n", " "),
        "company":       user.get("company") or "",
        "location":      user.get("location") or "",
        "created_at":    user.get("createdAt", ""),
        "updated_at":    user.get("updatedAt", ""),
        "followers":     user["followers"]["totalCount"],
        "following":     user["following"]["totalCount"],
        "public_repos":  user["repositories"]["totalCount"],
    }

    pr_rows = []
    for node in user["pullRequests"]["nodes"]:
        pr_rows.append({
            "author_login":    login,
            "repo":            (node.get("baseRepository") or {}).get("nameWithOwner", ""),
            "number":          node["number"],
            "title":           node["title"].replace("\n", " "),
            "state":           node["state"],
            "is_draft":        node["isDraft"],
            "base_branch":     node.get("baseRefName", ""),
            "head_branch":     node.get("headRefName", ""),
            "created_at":      node.get("createdAt", ""),
            "updated_at":      node.get("updatedAt", ""),
            "merged_at":       node.get("mergedAt") or "",
            "closed_at":       node.get("closedAt") or "",
            "url":             node.get("url", ""),
            "additions":       node.get("additions", 0),
            "deletions":       node.get("deletions", 0),
            "merged_by":       (node.get("mergedBy") or {}).get("login", ""),
        })

    return user_row, pr_rows


# ── CSV ───────────────────────────────────────────────────────────────────────

USER_FIELDS = [
    "login", "name", "bio", "company", "location",
    "created_at", "updated_at", "followers", "following", "public_repos",
]

PR_FIELDS = [
    "author_login", "repo", "number", "title", "state", "is_draft",
    "base_branch", "head_branch", "created_at", "updated_at",
    "merged_at", "closed_at", "url", "additions", "deletions",
    "merged_by",
]

JOINED_FIELDS = USER_FIELDS + [f"pr_{f}" for f in PR_FIELDS if f != "author_login"]


def write_csv(path: str, fieldnames: list[str], rows: list[dict]) -> None:
    with open(path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)
    print(f"  v {len(rows):,} row(s) -> {path}")


def fetch_with_retry(login: str, retries: int = 2):
    for attempt in range(retries):
        try:
            return fetch_user_and_prs(login)
        except Exception as e:
            if attempt < retries - 1:
                print(f"[WARN] Retry {attempt+1} for {login}...", file=sys.stderr)
            else:
                raise e

# ── main ──────────────────────────────────────────────────────────────────────

def main() -> None:
    usernames = load_usernames()
    if not usernames:
        print("[ERROR] usernames.txt is empty or missing.", file=sys.stderr)
        sys.exit(1)
    print(f"Processing {len(usernames)} user(s): {', '.join(usernames)}\n")

    all_users: list[dict] = []
    all_prs:   list[dict] = []

    for login in usernames:
        print(f"-> {login}")
        if 'bot' in login:
            print(f"  [SKIP] Bot account detected - skipping.", file=sys.stderr)
            continue
        try:
            user_row, pr_rows = fetch_with_retry(login)
        except Exception as e:
            print(f"[WARN] Skipping {login}: {e}", file=sys.stderr)
            continue
        all_users.append(user_row)
        all_prs.extend(pr_rows)
        print(f"  fetched user stats + {len(pr_rows)} PR(s)")

    write_csv("users.csv", USER_FIELDS, all_users)
    write_csv("pull_requests.csv", PR_FIELDS, all_prs)

    user_map = {u["login"]: u for u in all_users}
    joined: list[dict] = []
    for pr in all_prs:
        u = user_map.get(pr["author_login"], {})
        row = {f: u.get(f, "") for f in USER_FIELDS}
        for f in PR_FIELDS:
            if f != "author_login":
                row[f"pr_{f}"] = pr.get(f, "")
        joined.append(row)
    write_csv("users_prs.csv", JOINED_FIELDS, joined)

    print("\nDone!")


if __name__ == "__main__":
    main()