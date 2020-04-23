library(corrplot)
library(caret)
pima <- read.csv("diabetes.csv", col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))
head(pima) # # visualize the header of Pima data
str(pima)
sapply(pima, function(x) sum(is.na(x)))
pairs(pima, panel = panel.smooth)
corrplot(cor(pima[, -9]), type = "lower", method = "number")
# Preparing the DataSet
set.seed(1000)
n <- nrow(pima)
train <- sample(n, trunc(0.70*n))
pima_training <- pima[train, ]
pima_testing <- pima[-train, ]

# Training The Model
glm_fm1 <- glm(Diabetes ~., data = pima_training, family = binomial)
summary(glm_fm1)
glm_fm2 <- update(glm_fm1, ~. - Triceps_Skin - Serum_Insulin - Age )
summary(glm_fm2)
par(mfrow = c(2,2))
plot(glm_fm2)
# Testing the Model
glm_probs <- predict(glm_fm2, newdata = pima_testing, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)
test_tab1 <-table(Predicted = glm_pred, Actual = pima_testing$Diabetes)
test_tab1
accura1 <- round(sum(diag(test_tab1))/sum(test_tab1),2)
accura1
#confusionMatrix(glm_pred, pima_testing$Diabetes )
#acc_glm_fit <- confusionMatrix(glm_pred, pima_testing$Diabetes )$overall['Accuracy']
# Preparing the DataSet:
pima <- read.csv("diabetes.csv", col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))
pima$Diabetes <- as.factor(pima$Diabetes)

library(caret)
library(tree)
library(e1071)
set.seed(1000)
intrain <- createDataPartition(y = pima$Diabetes, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]

# Training The Model
treemod <- tree(Diabetes ~ ., data = train)

summary(treemod)
treemod # get a detailed text output.
plot(treemod)
text(treemod, pretty = 0)
# Testing the Model
tree_pred <- predict(treemod, newdata = test, type = "class" )
confusionMatrix(tree_pred, test$Diabetes)
acc_treemod <- confusionMatrix(tree_pred, test$Diabetes)$overall['Accuracy']
# Training The Model
set.seed(123)
library(randomForest)
rf_pima <- randomForest(Diabetes ~., data = pima_training, mtry = 8, ntree=50, importance = TRUE)
# Testing the Model
rf_probs <- predict(rf_pima, newdata = pima_testing)
rf_pred <- ifelse(rf_probs > 0.5, 1, 0)
test_tab3 <-table(Predicted = rf_pred, Actual = pima_testing$Diabetes)
test_tab3
accura3 <- round(sum(diag(test_tab3))/sum(test_tab3),2)
accura3
#confusionMatrix(rf_pred, pima_testing$Diabetes )
#acc_rf_pima <- confusionMatrix(rf_pred, pima_testing$Diabetes)$overall['Accuracy']
importance(rf_pima)
par(mfrow = c(1, 2))
varImpPlot(rf_pima, type = 2, main = "Variable Importance",col = 'black')
plot(rf_pima, main = "Error vs no. of trees grown")
#Load the DataSet
pima <- read.csv("diabetes.csv", col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))
pima$Diabetes <- as.factor(pima$Diabetes)

library(e1071)

#Preparing the DataSet:
set.seed(1000)
intrain <- createDataPartition(y = pima$Diabetes, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]
tuned <- tune.svm(Diabetes ~., data = train, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned) # to show the results
svm_model  <- svm(Diabetes ~., data = train, kernel = "radial", gamma = 0.01, cost = 10) 
summary(svm_model)
svm_pred <- predict(svm_model, newdata = test)
confusionMatrix(svm_pred, test$Diabetes)
acc_svm_model <- confusionMatrix(svm_pred, test$Diabetes)$overall['Accuracy']
accuracy <- data.frame(Model=c("Logistic Regression","Decision Tree","Random Forest", "Support Vector Machine (SVM)"), Accuracy=c(accura1, acc_treemod, accura3, acc_svm_model ))
ggplot(accuracy,aes(x=Model,y=Accuracy)) + geom_bar(stat='identity') + theme_bw() + ggtitle('Comparison of Model Accuracy')
