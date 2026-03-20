###############################
### Support Vector Machines ###
###         Review          ###
###############################


library(caret)
library(e1071)
library(GGally)


make.grid = function(X, n = 75) {
  grange = apply(X, 2, range)
  X1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  X2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(Petal.Length = X1, Petal.Width = X2)
}

plotBoundary <- function(X, Y, modelObject) {
  Y <- as.numeric(Y)
  Y[Y==2] <- -1
  
  xgrid = make.grid(X)
  ygrid = predict(modelObject, xgrid)
  
  plot(xgrid, col = c("red","blue","darkgreen")[as.numeric(ygrid)], pch = 20, cex = .2)
  points(X, col = Y + 3, pch = 19)
  points(X[modelObject$index,], pch = 5, cex = 2)
}


## take copy
dataset <- iris

# dataset$Species <- as.character(dataset$Species)
# # dataset <- dataset[-which(dataset$Species=="versicolor"),]
# dataset <- dataset[-which(dataset$Species=="setosa"),]
# dataset$Species <- as.factor(dataset$Species)

# ## split train/test
N <- nrow(dataset)
train.indexes <- sample(N,0.7*N)

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

## feature plots
# ggpairs(dataset, ggplot2::aes(colour = dataset$Species))


ggplot(train, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

ggplot(test, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()



#### train SVM model - linear kernel ####
svm.mod0 <- svm(Species ~ Petal.Length + Petal.Width, data = train, kernel = 'linear')

svm.mod0

X <- train[,c(3,4)]
Y <- train[,5]



### better decision boundary plot
plotBoundary(X,Y,svm.mod0)
##################


### predict for test data
test.pred <- predict(svm.mod0, test)

cm = as.matrix(table(Actual = test$Species, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diagv = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diagv)/n
accuracy

recall = diagv / rowsums 
precision = diagv / colsums
f1 = 2 * precision * recall / (precision + recall) 

svm.mod0.res <- data.frame(model="linear", precision, recall, f1)
svm.mod0.res

results <- svm.mod0.res

##################


#### train SVM model - radial kernel ####
svm.mod1 <- svm(Species ~ Petal.Length + Petal.Width, data = train, kernel = 'radial')

svm.mod1

X <- train[,c(3,4)]
Y <- train[,5]



### better decision boundary plot
plotBoundary(X,Y,svm.mod1)
##################


### predict for test data
test.pred <- predict(svm.mod1, test)

cm = as.matrix(table(Actual = test$Species, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diagv = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diagv)/n
accuracy

recall = diagv / rowsums 
precision = diagv / colsums
f1 = 2 * precision * recall / (precision + recall) 

svm.mod1.res <- data.frame(model = "radial", precision, recall, f1)
svm.mod1.res


results <- rbind(results,svm.mod1.res)

##################


#### train SVM model - ploynomial kernel ####

gamma.range <- seq(.1,10,.1)
gamma.range

tuned.svm <- tune.svm(Species~Petal.Length+Petal.Width, data = train, kernel = 'polynomial', gamma = gamma.range)
tuned.svm

gamma <- tuned.svm$best.parameters$gamma

svm.mod2 <- svm(Species ~ Petal.Length + Petal.Width, data = train, kernel = 'polynomial', gamma = gamma)


X <- train[,c(3,4)]
Y <- train[,5]



### better decision boundary plot
plotBoundary(X,Y,svm.mod2)
##################


### predict for test data
test.pred <- predict(svm.mod2, test)

cm = as.matrix(table(Actual = test$Species, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diagv = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diagv)/n
accuracy

recall = diagv / rowsums 
precision = diagv / colsums
f1 = 2 * precision * recall / (precision + recall) 

svm.mod2.res <- data.frame(model = "polynomial", precision, recall, f1)
svm.mod2.res


results <- rbind(results,svm.mod2.res)
##################

results
