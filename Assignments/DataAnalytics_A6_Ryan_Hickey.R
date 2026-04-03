library(readr)
library(EnvStats)
library(nortest)
library(class)
library(ggplot2)

# Below is the extra information detailing how the numbers of the dataset map
# to real outputs
"
Class Labels

Student ID
1- Student Age (1: 18-21, 2: 22-25, 3: above 26)
2- Sex (1: female, 2: male)
3- Graduated high-school type: (1: private, 2: state, 3: other)
4- Scholarship type: (1: None, 2: 25%, 3: 50%, 4: 75%, 5: Full)
5- Additional work: (1: Yes, 2: No)
6- Regular artistic or sports activity: (1: Yes, 2: No)
7- Do you have a partner: (1: Yes, 2: No)
8- Total salary if available (1: USD 135-200, 2: USD 201-270, 3: USD 271-340, 4: USD 341-410, 5: above 410)
9- Transportation to the university: (1: Bus, 2: Private car/taxi, 3: bicycle, 4: Other)
10- Accommodation type in Cyprus: (1: rental, 2: dormitory, 3: with family, 4: Other)
11- Mothersâ€™ education: (1: primary school, 2: secondary school, 3: high school, 4: university, 5: MSc., 6: Ph.D.)
12- Fathersâ€™ education: (1: primary school, 2: secondary school, 3: high school, 4: university, 5: MSc., 6: Ph.D.)
13- Number of sisters/brothers (if available): (1: 1, 2:, 2, 3: 3, 4: 4, 5: 5 or above)
14- Parental status: (1: married, 2: divorced, 3: died - one of them or both)
15- Mothersâ€™ occupation: (1: retired, 2: housewife, 3: government officer, 4: private sector employee, 5: self-employment, 6: other)
16- Fathersâ€™ occupation: (1: retired, 2: government officer, 3: private sector employee, 4: self-employment, 5: other)
17- Weekly study hours: (1: None, 2: <5 hours, 3: 6-10 hours, 4: 11-20 hours, 5: more than 20 hours)
18- Reading frequency (non-scientific books/journals): (1: None, 2: Sometimes, 3: Often)
19- Reading frequency (scientific books/journals): (1: None, 2: Sometimes, 3: Often)
20- Attendance to the seminars/conferences related to the department: (1: Yes, 2: No)
21- Impact of your projects/activities on your success: (1: positive, 2: negative, 3: neutral)
22- Attendance to classes (1: always, 2: sometimes, 3: never)
23- Preparation to midterm exams 1: (1: alone, 2: with friends, 3: not applicable)
24- Preparation to midterm exams 2: (1: closest date to the exam, 2: regularly during the semester, 3: never)
25- Taking notes in classes: (1: never, 2: sometimes, 3: always)
26- Listening in classes: (1: never, 2: sometimes, 3: always)
27- Discussion improves my interest and success in the course: (1: never, 2: sometimes, 3: always)
28- Flip-classroom: (1: not useful, 2: useful, 3: not applicable)
29- Cumulative grade point average in the last semester (/4.00): (1: <2.00, 2: 2.00-2.49, 3: 2.50-2.99, 4: 3.00-3.49, 5: above 3.49)
30- Expected Cumulative grade point average in the graduation (/4.00): (1: <2.00, 2: 2.00-2.49, 3: 2.50-2.99, 4: 3.00-3.49, 5: above 3.49)
31- Course ID
32- OUTPUT Grade (0: Fail, 1: DD, 2: DC, 3: CC, 4: CB, 5: BB, 6: BA, 7: AA)
"

label_mappings <- list(
  Age                              = c("1" = "18-21",      "2" = "22-25",               "3" = "26+"),
  Sex                              = c("1" = "Female",     "2" = "Male"),
  high_school_type                 = c("1" = "Private",    "2" = "State",               "3" = "Other"),
  Scholarship_type                 = c("1" = "None",       "2" = "25%",                 "3" = "50%",            "4" = "75%",         "5" = "Full"),
  Additional_work                  = c("1" = "Yes",        "2" = "No"),
  Regular_artistic_or_sports_activity = c("1" = "Yes",    "2" = "No"),
  Has_partner                      = c("1" = "Yes",        "2" = "No"),
  Total_salary                     = c("1" = "$135-200",   "2" = "$201-270",            "3" = "$271-340",       "4" = "$341-410",    "5" = "$410+"),
  Transportation_to_university     = c("1" = "Bus",        "2" = "Car/Taxi",            "3" = "Bicycle",        "4" = "Other"),
  Campus_Accommodation_type        = c("1" = "Rental",     "2" = "Dormitory",           "3" = "With Family",    "4" = "Other"),
  Mother_education                 = c("1" = "Primary",    "2" = "Secondary",           "3" = "High School",    "4" = "University",  "5" = "MSc",  "6" = "PhD"),
  Father_education                 = c("1" = "Primary",    "2" = "Secondary",           "3" = "High School",    "4" = "University",  "5" = "MSc",  "6" = "PhD"),
  Sibling_count                    = c("1" = "1",          "2" = "2",                   "3" = "3",              "4" = "4",           "5" = "5+"),
  Parental_status                  = c("1" = "Married",    "2" = "Divorced",            "3" = "Deceased"),
  Mother_occupation                = c("1" = "Retired",    "2" = "Housewife",           "3" = "Gov. Officer",   "4" = "Private",     "5" = "Self-Employed", "6" = "Other"),
  Father_occupation                = c("1" = "Retired",    "2" = "Gov. Officer",        "3" = "Private",        "4" = "Self-Employed", "5" = "Other"),
  Weekly_study_hours               = c("1" = "None",       "2" = "<5 hrs",              "3" = "6-10 hrs",       "4" = "11-20 hrs",   "5" = "20+ hrs"),
  Non_academic_Reading_frequency   = c("1" = "None",       "2" = "Sometimes",           "3" = "Often"),
  Academic_Reading_frequency       = c("1" = "None",       "2" = "Sometimes",           "3" = "Often"),
  Seminars_attendance              = c("1" = "Yes",        "2" = "No"),
  Projects_on_success              = c("1" = "Positive",   "2" = "Negative",            "3" = "Neutral"),
  Class_attendance                 = c("1" = "Always",     "2" = "Sometimes",           "3" = "Never"),
  Who_you_study_with               = c("1" = "Alone",      "2" = "With Friends",        "3" = "N/A"),
  When_you_study                   = c("1" = "Closest to Exam", "2" = "Regularly",      "3" = "Never"),
  Taking_notes                     = c("1" = "Never",      "2" = "Sometimes",           "3" = "Always"),
  Listening_during_class           = c("1" = "Never",      "2" = "Sometimes",           "3" = "Always"),
  Discussion_helps_learning        = c("1" = "Never",      "2" = "Sometimes",           "3" = "Always"),
  Flip_classroom                   = c("1" = "Not Useful", "2" = "Useful",              "3" = "N/A"),
  GPA_last_sem                     = c("1" = "<2.00",      "2" = "2.00-2.49",           "3" = "2.50-2.99",      "4" = "3.00-3.49",   "5" = "3.49+"),
  Expected_GPA                     = c("1" = "<2.00",      "2" = "2.00-2.49",           "3" = "2.50-2.99",      "4" = "3.00-3.49",   "5" = "3.49+"),
  Grade                            = c("0" = "Fail",       "1" = "DD",                  "2" = "DC",             "3" = "CC",          "4" = "CB",   "5" = "BB", "6" = "BA", "7" = "AA")
)

# set working directory
setwd("~\\GitHub\\DataAnalyticsS2026\\Assignments")

# read data
data <- read_csv("DATA (1).csv")

col_names <- c("Student_ID", "Age", "Sex", "high_school_type", "Scholarship_type",
               "Additional_work", "Regular_artistic_or_sports_activity", "Has_partner",
               "Total_salary", "Transportation_to_university", "Campus_Accommodation_type",
               "Mother_education", "Father_education", "Sibling_count", "Parental_status",
               "Mother_occupation", "Father_occupation", "Weekly_study_hours",
               "Non_academic_Reading_frequency", "Academic_Reading_frequency",
               "Seminars_attendance", "Projects_on_success", "Class_attendance",
               "Who_you_study_with", "When_you_study", "Taking_notes",
               "Listening_during_class", "Discussion_helps_learning", "Flip_classroom",
               "GPA_last_sem", "Expected_GPA", "Course_ID", "Grade")

colnames(data) <- col_names

###################################################
# EDA 
##################################################

plot_var <- function(name) {
    ggplot(data, aes(x = .data[[name]])) +
      geom_histogram(bins = 8, fill = "skyblue", color = "black") +
      labs(title = paste("Histogram of", name),
           x =name,
           y = "Count") +
      theme_minimal()
}

analyze_distribution <- function(name) {
  var <- data[[name]]
  
  summary(var)
  test <- shapiro.test(var)
  # print(test)
  
  if (test$p.value < 0.05) {
    cat(sprintf("Conclusion %s: Not normally distributed\n", name))
  } else {
    cat("Conclusion %s: Approximately normal\n", name)
  }
}

for (col in colnames(data)) {
  if (col == "Student_ID") {
    next
  }
  analyze_distribution(col)
}

plot_var("Grade")
plot_var("Total_salary")
