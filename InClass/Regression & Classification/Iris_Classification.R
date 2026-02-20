################################################
##### Classification with the Iris dataset #####
###### Decision Tree, kNN, Random Forest #######
################################################

# Install the following libararies/packages in RStudio 
library(ggplot2)

library(rpart)
library(rpart.plot)

library(class)
library(randomForest)

# we will be using the iris dataset
iris.data <- iris
dim(iris.data)

## scatter plot of 2 variables
ggplot(iris.data, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point()

## scatter plot of 2 variables
ggplot(iris.data, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()


# creating a sample from the iris dataset 
s.train <- sample(150,100) 
s.train

# creat training and testing sets 
iris.train <-iris.data[s.train,]
iris.test <-iris.data[-s.train,] 



dim(iris.test)
dim(iris.train) 

## scatter plot of 2 variables for training set
ggplot(iris.train, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

## scatter plot of 2 variables for test set
ggplot(iris.test, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()


# Decision tree model 
tree.model <- rpart(Species~., iris.train, method = "class")

tree.model

#plotting the decision tree model using rpart.plot() function 
rpart.plot(tree.model)

tree.model.predicted <-  predict(tree.model, iris.test, type = "class")

tree.model.predicted

## confusion matrix/contingency table
table(tree.model.predicted, iris.test$Species, dnn=list('predicted','actual'))


## kNN Model
knn.predicted <- knn(iris.train[,1:4], iris.test[,1:4], iris.train[,5], k=3 )


## confusion matrix/contingency table
table(knn.predicted, iris.test[,5], dnn=list('predicted','actual'))


## Random Forest Model
rf.model <- randomForest(Species~., data=iris.train)

rf.model

## predict class labels
rf.predicted <- predict(rf.model, iris.test)

## confusion matrix/contingency table
table(rf.predicted,iris.test$Species, dnn=list('predicted','actual'))




########## OPTIONAL ##########
## This section is just for ##
## plotting trees from      ##
## training Random Forest   ##
##############################

#### Plot Trees in Random Forest ####
library(caret)
library(dplyr)
library(ggraph)
library(igraph)

## Train the model using Train function in the Caret package
rf.model <- train(Species ~ . ,data=iris.train, method = "rf", prox = TRUE)


rf.model

## Smallest tree(s)
which(rf.model$finalModel$forest$ndbigtree == min(rf.model$finalModel$forest$ndbigtree))

## Largest tree(s)
which(rf.model$finalModel$forest$ndbigtree == max(rf.model$finalModel$forest$ndbigtree))

## set tree number to plot
tree.num <- 116

## extract tree from model
tree <- randomForest::getTree(rf.model$finalModel, 
                              k = tree.num, 
                              labelVar = TRUE) %>%
  tibble::rownames_to_column() %>%
  # make leaf split points to NA, so the 0s won't get plotted
  mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))

# prepare data frame for graph
graph_frame <- data.frame(from = rep(tree$rowname, 2),
                          to = c(tree$`left daughter`, tree$`right daughter`))

# convert to graph and delete the last node that we don't want to plot
graph <- graph_from_data_frame(graph_frame) %>%
  delete_vertices("0")

# set node labels
V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
V(graph)$leaf_label <- as.character(tree$prediction)
V(graph)$split <- as.character(round(tree$`split point`, digits = 2))

# create plot
plot <- ggraph(graph, 'dendrogram') + 
  theme_bw() +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
  geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
  geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                  repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 18))

# plot plot!
print(plot)


##### THE END #####


