##############################################
############## Useful Functions ##############
##############################################

library(caret)

## quantitative variable entered as string including a non-numeric character 
x <- "80%"

## remove perdiod
x <- gsub("%","",x)

## convert veriable to numeric
x <- as.numeric(x)


## create example dataframe
df <- data.frame(species=c("cat","cat","dog","cat","dog","dog","cat","cat","dog","cat","dog"), cuteness=c("80%","30%","10%","70%","50.5%","25%","90%","65%","45%","20%","85%"), friendliness=c(2,1.5,6,5,8,6,3.5,0.5,3,7,4))

## convert character variable to factor
## factors in R represent categorical variables
## they let R know that a variable has a set of possible values which is necessary to use the variable as a class label in classification
## check out the summary before and after conversion
summary(df$species)

df$species <- as.factor(df$species)

summary(df$species)

## to correctly convert a character variable to numeric you must remove any letter/punctuation characters and only keep numbers
## otherwise you will get NAs
as.numeric(df$cuteness)

## remove the % sign
df$cuteness <- gsub("%","",df$cuteness)
df$cuteness

df$cuteness <- as.numeric(df$cuteness)
df$cuteness

## try classification

## evaluate models using leave-one-out cross-validation
control <- trainControl(method="LOOCV")
metric <- "Accuracy"

## train model using k = 3,5 
knn.mod <- train(species~.,data = df, method="knn", metric=metric, trControl=control, tuneGrid = data.frame(k = c(3,5)))

knn.mod

## train model using k = 5 (higher accuracy)
knn.mod <- train(species~.,data = df, method="knn", metric=metric, trControl=control, tuneGrid = data.frame(k = 5))

knn.pred <- predict(knn.mod,df)

cm = as.matrix(table(Actual = df$species, Predicted = knn.pred))
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy = sum(diag)/n
accuracy

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

cm
data.frame(recall, precision, f1)


################# THE END #################

