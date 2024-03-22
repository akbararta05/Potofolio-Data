data<-read.csv(file.choose(),header=T,sep=",",dec=".")
attach(data)
head(data)

library(dplyr)
library(partykit)
library(caret)
library(UBL)
library(ggplot2)
library(e1071)
library(gridExtra)
library(pROC)

str(data)

#Mengubah menjadi faktor
data <- data %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(Microcalcification = factor(Microcalcification, levels = c("'-1'","'1'"), labels = c("Non-Micro","Micro")))
glimpse(data)

str(data)

#Cek Missing Value
colSums(is.na(data))

#Cek proporsi data
prop.table(table(data$Microcalcification)) #tidak balance

#Data balance
smote <- SmoteClassif(form = Microcalcification ~ ., 
                      dat = data, 
                      C.perc = "balance",  
                      dist = "HVDM")
table(smote$Microcalcification)
prop.table(table(smote$Microcalcification))

#Data hampir balance
almost_balanced <- list("Non-Micro" = 1, "Micro" = 35)

smote1 <- SmoteClassif(form = Microcalcification ~ ., 
                       dat = data, 
                       C.perc = almost_balanced,  
                       dist = "HVDM")
table(smote1$Microcalcification)
prop.table(table(smote1$Microcalcification))

##Data sebelum balance
data %>% 
  mutate(Microcalcification = as.factor(Microcalcification)) %>% 
  ggplot(mapping = aes(x = Grey.Level , y = Shape.Descriptor, color = Microcalcification)) +
  geom_point() +
  theme_classic()

##Data setelah hampir balance
smote1 %>% 
  mutate(Microcalcification = as.factor(Microcalcification)) %>% 
  ggplot(mapping = aes(x = Grey.Level , y = Shape.Descriptor, color = Microcalcification)) +
  geom_point() +
  theme_classic()

##Data setelah balance
smote %>% 
  mutate(Microcalcification = as.factor(Microcalcification)) %>% 
  ggplot(mapping = aes(x = Grey.Level , y = Shape.Descriptor, color = Microcalcification)) +
  geom_point() +
  theme_classic()

#Split Train Test Dataset
set.seed(250)
intrain <- sample(nrow(smote1),nrow(smote1)*0.8)
data_train <- smote1[intrain, ]
data_test <- smote1[-intrain, ]

##Cek proporsi ketika data sudah dibagi
prop.table(table(smote1$Microcalcification))
prop.table(table(data_train$Microcalcification))
prop.table(table(data_test$Microcalcification))


####----- Decision Tree-----####
##Algoritma C5.0
library(C50)
library(printr)

model_c5.0 <- C5.0(Microcalcification ~., data = data_train)
model_c5.0

plot(model_c5.0, type = "simple", gp = gpar(fontsize = 3),inner_panel = node_inner,
     ip_args = list(abbreviate = FALSE, id = FALSE))

#Model Evaluation
##Data Train
pred_data_train1 <- predict(model_c5.0, data_train)
cm_datatrain1 <- confusionMatrix(pred_data_train1, data_train$Microcalcification, positive = "Non-Micro")
cm_datatrain1
eval_datatrain_tree1 <- tibble(Accuracy = cm_datatrain1$overall[1],
                               Recall = cm_datatrain1$byClass[1],
                               Specificity = cm_datatrain1$byClass[2],
                               Precision = cm_datatrain1$byClass[3])
eval_datatrain_tree1
##Data Test
pred_data_test1 <- predict(model_c5.0, data_test)
cm_datatest1 <- confusionMatrix(pred_data_test1, data_test$Microcalcification, positive = "Non-Micro")
cm_datatest1
eval_datatest_tree1 <- tibble(Accuracy = cm_datatest1$overall[1],
                              Recall = cm_datatest1$byClass[1],
                              Specificity = cm_datatest1$byClass[2],
                              Precision = cm_datatest1$byClass[3])
eval_datatest_tree1

##Algoritma Random Forest
library(randomForest)

model_RF <- randomForest(Microcalcification ~., data = data_train)
model_RF

plot(model_RF)

#Model Evaluation
##Data Train
pred_data_train2 <- predict(model_RF, data_train)
cm_datatrain2 <- confusionMatrix(pred_data_train2, data_train$Microcalcification, positive = "Non-Micro")
cm_datatrain2
eval_datatrain_tree2 <- tibble(Accuracy = cm_datatrain2$overall[1],
                               Recall = cm_datatrain2$byClass[1],
                               Specificity = cm_datatrain2$byClass[2],
                               Precision = cm_datatrain2$byClass[3])
eval_datatrain_tree2
##Data Test
pred_data_test2 <- predict(model_RF, data_test)
cm_datatest2 <- confusionMatrix(pred_data_test2, data_test$Microcalcification, positive = "Non-Micro")
cm_datatest2
eval_datatest_tree2 <- tibble(Accuracy = cm_datatest2$overall[1],
                              Recall = cm_datatest2$byClass[1],
                              Specificity = cm_datatest2$byClass[2],
                              Precision = cm_datatest2$byClass[3])
eval_datatest_tree2

####-----SVM -----####
##Membuat fungsi
perform <- function(pred,data1){
  tabel <- caret::confusionMatrix(pred, data1$Microcalcification, positive = "Non-Micro")
  result <- tibble(Accuracy = tabel$overall[1],
                   Recall = tabel$byClass[1],
                   Specificity = tabel$byClass[2],
                   Precision = tabel$byClass[3])
  return(result)
}

#Model SVM dengan kernel Linear
model.svm1 <- svm(Microcalcification~., data=data_train,kernel="linear");model.svm1
pred.svm1 <- predict(model.svm1, data_test)
perform(pred.svm1, data_test)

#Model SVM dengan kernel Radial
model.svm2 <- svm(Microcalcification~., data=data_train,kernel="radial");model.svm2
pred.svm2 <- predict(model.svm2, data_test)
perform(pred.svm2, data_test)

#Model SVM dengan kernel Polynomial
model.svm3 <- svm(Microcalcification~., data=data_train,kernel="polynomial");model.svm3
pred.svm3 <- predict(model.svm3, data_test)
perform(pred.svm3, data_test)

#Model SVM dengan kernel Sigmoid
model.svm4 <- svm(Microcalcification~., data=data_train,kernel="sigmoid");model.svm4
pred.svm4 <- predict(model.svm4, data_test)
perform(pred.svm4, data_test)

#Best Model dengan AUC terbesar
auc_DT5.0 <- roc(data_test$Microcalcification,factor(pred_data_test1,ordered = TRUE))
auc_RF <- roc(data_test$Microcalcification,factor(pred_data_test2,ordered = TRUE))
auc_svmL <- roc(data_test$Microcalcification,factor(pred.svm1,ordered = TRUE))
auc_svmR <- roc(data_test$Microcalcification,factor(pred.svm2,ordered = TRUE))
auc_svmP <- roc(data_test$Microcalcification,factor(pred.svm3,ordered = TRUE))
auc_svmS <- roc(data_test$Microcalcification,factor(pred.svm4,ordered = TRUE))

#Tabel ringkasan
nama_metode<-c("Decision Tree C5.0", "Decision Tree Random Forest", "SVM Linear", "SVM Radial", "SVM Polunomial", "SVM Sigmoid")
accuracy <- c(cm_datatest1$overall[1], cm_datatest2$overall[1], perform(pred.svm1, data_test)$Accuracy, perform(pred.svm2, data_test)$Accuracy, perform(pred.svm3, data_test)$Accuracy,perform(pred.svm4, data_test)$Accuracy)
recall <- c(cm_datatest1$byClass[1], cm_datatest2$byClass[1], perform(pred.svm1, data_test)$Recall, perform(pred.svm2, data_test)$Recall, perform(pred.svm3, data_test)$Recall, perform(pred.svm4, data_test)$Recall)
specitifity <- c(cm_datatest1$byClass[2], cm_datatest2$byClass[2], perform(pred.svm1, data_test)$Specificity, perform(pred.svm2, data_test)$Specificity, perform(pred.svm3, data_test)$Specificity, perform(pred.svm4, data_test)$Specificity)
precision <- c(cm_datatest1$byClass[3], cm_datatest2$byClass[3], perform(pred.svm1, data_test)$Precision, perform(pred.svm2, data_test)$Precision, perform(pred.svm3, data_test)$Precision, perform(pred.svm4, data_test)$Precision)
auc <- c(auc_DT5.0$auc, auc_RF$auc, auc_svmL$auc, auc_svmR$auc, auc_svmP$auc, auc_svmS$auc)
data.frame(nama_metode, accuracy, recall, specitifity,precision, auc)
