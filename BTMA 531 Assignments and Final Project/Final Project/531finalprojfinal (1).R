getwd()
setwd("C:/Users/Rupin/Desktop")
data <- read.csv("train.csv") #loading the data
library(MASS) #loading required libraries 
library(neuralnet)
library(e1071)
library(nnet)
library(ROSE)
library(ROCR)
library(pROC)
library(caret)
library(ggplot2)
library(fastDummies)
library(tidyverse)
library(broom)
library(class)



set.seed(123) #setting seed 



#dummy coding-------------------------------------------------------------------------------------------------------------------------------------------------

dum.data <- fastDummies::dummy_cols(data, select_columns = c("Application.Type","Grade","Sub.Grade","Employment.Duration","Verification.Status","Payment.Plan","Initial.List.Status")) #Dummy coding categorical variables
colnames(dum.data[,-c(-1,-6,-8,-9,-10,-12,-13,-14,-23,-29,-31,-37,-38,-45,-80,-83,-86,-87)])
dum.data <- dum.data[,c(-1,-6,-8,-9,-10,-12,-13,-14,-23,-29,-31,-37,-38,-45,-80,-83,-86,-87)]  #removing unnecessary columns and one column for each dummy code to avoid multicollinearity 
colnames(dum.data) #final column names
colnames(dum.data)[68] <- "Verification.Status_SourceVerified" #fixing issues with naming
Index <- sample(2, nrow(dum.data),replace = T, prob = c(0.7,0.3))
train <- dum.data[Index==1,] #USE THESE FOR TRAIN AND TEST SETS. Please store them as another var first before using so orignal data split stays intact
colnames(train)[68] <- "Verification.Status_SourceVerified" #fixing issues
test <- dum.data[Index==2,]
colnames(test)[68] <- "Verification.Status_SourceVerified"
imbalance.loanstatus <- data.frame(table(train$Loan.Status)) #plotting imbalance in predicted class Loan Status
colnames(imbalance.loanstatus) <- c("Classes","Frequency")
imbalance.loanstatus$Classes <- c("Non Defaulter","Defaulter")
ggplot(imbalance.loanstatus, aes(x = Classes, y = Frequency, fill = Classes)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loan Status Imbalance",
       x = "Loan Status",
       y = "Count",
       fill = "Status") +
  scale_fill_manual(values = c("steelblue", "maroon")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#PCA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pca = prcomp(dum.data[,-24], scale = TRUE, center = TRUE) #PCA
pcaVar = pca$sdev^2
pve = pcaVar/sum(pcaVar)
plot(pve, xlab = "PC", ylab = "PVE", type = "b", ylim = c(0,0.03), main = "Explained Variance by Principal Components")
train_data_pca <- cbind(as.data.frame(predict(pca, train[, -24])),Loan.Status = train[,24])[,c(1:12,70)] #applying pca done on entire data to get train and test sets
test_data_pca <- cbind(as.data.frame(predict(pca, test[, -24])),Loan.Status = test[,24])[,c(1:12,70)]

#Basic glm------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

logistic.basic <- glm(train$Loan.Status~.,data=train,family="binomial") #starting w basic logistic model to compare w others later on
pred.basic <- predict(logistic.basic,test,type = "response")
roc_obj <- roc(test$Loan.Status, pred.basic) #finding optimal threshold (wont be .5 due to the variation in weights)
coords <- coords(roc_obj, "best", ret = "threshold")
opt_threshold <- coords[1]
Optimal.threshold <- opt_threshold$threshold
pred.basic <- ifelse(pred.basic >Optimal.threshold, "Defaulter","nonDefaulter") #classification based on optimal threshold
pred.basic <- as.factor(pred.basic) #setting as factor for later
test.Loan.Status <- ifelse(test$Loan.Status==1,"Defaulter","nonDefaulter") #changing testset to be factor
test.Loan.Status <- as.factor(test.Loan.Status)
CM.BASIC <- confusionMatrix(pred.basic,test.Loan.Status) #cf matrix with perf measures
CM.BASIC

#OVERSAMPLING - NPCA GLM- reproducing the biased class (defaulters) so there are more cases for model to learn from
over <- ovun.sample(Loan.Status~. ,data = train, method = "over", N = 85690) #N is just 43136  * 2. we have an equal amount of 0 and 1 cases 
train.cv.over.npca <- over$data 

imbalance.loanstatus <- data.frame(table(train.cv.over.npca$Loan.Status))
colnames(imbalance.loanstatus) <- c("Classes","Frequency")
imbalance.loanstatus$Classes <- c("Non Defaulter","Defaulter")
ggplot(imbalance.loanstatus, aes(x = Classes, y = Frequency, fill = Classes)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loan Status Imbalance",
       x = "Loan Status",
       y = "Count",
       fill = "Status") +
  scale_fill_manual(values = c("steelblue", "maroon")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


test.cv.over.npca <- test #unaffected test class
class.weights.over<- ifelse(train.cv.over.npca$Loan.Status == 1, 10, 1) #setting weights to put more emphasis on avoiding false negative
control <- trainControl(method = "cv", number = 5, savePredictions = T) #cross validation w 5 folds of testing
cv.model.over <- train(Loan.Status ~ ., data = train.cv.over.npca, trControl = control, weights = class.weights.over,method = "glm", family = "binomial") #doing a logistic model on our training
pred.over <- predict(cv.model.over, newdata = test.cv.over.npca) #predict on test
min(pred.over) #our model naturally wants to class more as 1 so the probs are higher here
roc_obj <- roc(test.cv.over.npca$Loan.Status, pred.over) #finding optimal threshold (wont be .5 due to the variation in weights)
coords <- coords(roc_obj, "best", ret = "threshold")
opt_threshold <- coords[1]
Optimal.threshold <- opt_threshold$threshold
pred.over <- ifelse(pred.over > Optimal.threshold,"Defaulter","nonDefaulter")
table(pred.over,test.cv.over.npca$Loan.Status)
pred.over <- as.factor(pred.over)
test.cv.over.npca$Loan.Status <- ifelse(test.cv.over.npca$Loan.Status==1,"Defaulter","nonDefaulter")
test.cv.over.npca$Loan.Status <- as.factor(test.cv.over.npca$Loan.Status)
CV.OVER.CM.npca <- confusionMatrix(pred.over,test.cv.over.npca$Loan.Status)
CV.OVER.CM.npca

#OVERSAMPLING PCA-GLM reproducing the biased class (defaulters) so there are more cases for model to learn from
table(train_data_pca$Loan.Status)
over <- ovun.sample(Loan.Status~. ,data = train_data_pca, method = "over", N = 85690) #N is just 43136  * 2. we have an equal amount of 0 and 1 cases 
train.cv.over <- over$data 
test.cv.over <- test_data_pca #unaffected test class
class.weights.over<- ifelse(train.cv.over$Loan.Status == 1, 10, 1) #setting weights to put more emphasis on avoiding false negative
control <- trainControl(method = "cv", number = 5, savePredictions = T) #cross validation w 5 folds of testing
cv.model.over <- train(Loan.Status ~ ., data = train.cv.over, trControl = control, weights = class.weights.over,method = "glm", family = "binomial") #doing a logistic model on our training
pred.over <- predict(cv.model.over, newdata = test.cv.over) #predict on test
min(pred.over) #our model naturally wants to class more as 1 so the probs are higher here
roc_obj <- roc(test.cv.over$Loan.Status, pred.over) #finding optimal threshold (wont be .5 due to the variation in weights)
coords <- coords(roc_obj, "best", ret = "threshold")
opt_threshold <- coords[1]
Optimal.threshold <- opt_threshold$threshold #Used the ROC curve to find the optimal threshold for our model
pred.over <- ifelse(pred.over > Optimal.threshold,"Defaulter","nonDefaulter") #setting up classification as factor
table(pred.over,test.cv.over$Loan.Status)
pred.over <- as.factor(pred.over)
test.cv.over$Loan.Status <- ifelse(test.cv.over$Loan.Status==1,"Defaulter","nonDefaulter")
test.cv.over$Loan.Status <- as.factor(test.cv.over$Loan.Status)
CV.OVER.CM <- confusionMatrix(pred.over,test.cv.over$Loan.Status)
CV.OVER.CM #confusion matrix and performance measures
# UNDER SAMPLING GLM- nonpca removing records of 1 so we have more 0 records, opposite of before, same process.
under <- ovun.sample(Loan.Status~. ,data = train, method = "under", N = 8698)
train.cv.under.npca <- under$data

table(train.cv.under.npca$Loan.Status)

imbalance.loanstatus <- data.frame(table(train.cv.under.npca$Loan.Status))
colnames(imbalance.loanstatus) <- c("Classes","Frequency")
imbalance.loanstatus$Classes <- c("Non Defaulter","Defaulter")
ggplot(imbalance.loanstatus, aes(x = Classes, y = Frequency, fill = Classes)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loan Status Imbalance",
       x = "Loan Status",
       y = "Count",
       fill = "Status") +
  scale_fill_manual(values = c("steelblue", "maroon")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


test.under.npca <- test 
class.weights.under <- ifelse(train.cv.under.npca$Loan.Status == 1, 10, 1)
control <- trainControl(method = "cv", number = 5, savePredictions = T)
cv.model.under <- train(Loan.Status ~ ., data = train.cv.under.npca, trControl = control, weights = class.weights.under,method = "glm", family = "binomial") #doing a logistic model on our training
pred.under.npca <- predict(cv.model.under, newdata = test.under.npca) 
min(pred.under.npca)
roc_obj <- roc(test.under.npca$Loan.Status, pred.under.npca) 
coords <- coords(roc_obj, "best", ret = "threshold")
opt_threshold <- coords[1]
Optimal.threshold <- opt_threshold$threshold
pred.under.npca <- ifelse(pred.under.npca > Optimal.threshold,"Defaulter","nonDefaulter")
table(pred.under.npca,test.under.npca$Loan.Status)
pred.under.npca <- as.factor(pred.under.npca)
test.under.npca$Loan.Status <- ifelse(test.under.npca$Loan.Status==1,"Defaulter","nonDefaulter")
test.under.npca$Loan.Status <- as.factor(test.under.npca$Loan.Status)
CV.UNDER.CM.npca <- confusionMatrix(pred.under.npca,test.under.npca$Loan.Status)
CV.UNDER.CM.npca
# UNDER SAMPLING PCA-GLM removing records of 1 so we have more 0 records, opposite of before, same process.
under <- ovun.sample(Loan.Status~. ,data = train_data_pca, method = "under", N = 8698)
train.cv.under <- under$data

test.under <- test_data_pca 
class.weights.under <- ifelse(train.cv.under$Loan.Status == 1, 10, 1)
control <- trainControl(method = "cv", number = 5, savePredictions = T)
cv.model.under <- train(Loan.Status ~ ., data = train.cv.under, trControl = control, weights = class.weights.under,method = "glm", family = "binomial") #doing a logistic model on our training
pred.under <- predict(cv.model.under, newdata = test.under) 
min(pred.under)
roc_obj <- roc(test.under$Loan.Status, pred.under) 
coords <- coords(roc_obj, "best", ret = "threshold")
opt_threshold <- coords[1]
Optimal.threshold <- opt_threshold$threshold
pred.under <- ifelse(pred.under > Optimal.threshold,"Defaulter","nonDefaulter")
table(pred.under,test.under$Loan.Status)
pred.under <- as.factor(pred.under)
test.under$Loan.Status <- ifelse(test.under$Loan.Status==1,"Defaulter","nonDefaulter")
test.under$Loan.Status <- as.factor(test.under$Loan.Status)
CV.UNDER.CM <- confusionMatrix(pred.under,test.under$Loan.Status)
CV.UNDER.CM
#BOTH sampling glm non pca------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
both.nonpca <- ovun.sample(Loan.Status~. ,data = train, method = "both") #using both over and undersampling to increase our unrepresented class (defaulters (1)
both.nonpca.d <- both.nonpca$data
imbalance.loanstatus <- data.frame(table(both.nonpca.d$Loan.Status))
colnames(imbalance.loanstatus) <- c("Classes","Frequency")
imbalance.loanstatus$Classes <- c("Non Defaulter","Defaulter")
ggplot(imbalance.loanstatus, aes(x = Classes, y = Frequency, fill = Classes)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loan Status Imbalance",
       x = "Loan Status",
       y = "Count",
       fill = "Status") +
  scale_fill_manual(values = c("steelblue", "maroon")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

table(both.nonpca.d$Loan.Status) #imbalance fixed
train.cv.npca.both <- both.nonpca.d 
test.cv.npca.both <- test #unaffected test class
class.weights <- ifelse(train.cv.npca.both$Loan.Status == 1, 10, 1)
control <- trainControl(method = "cv", number = 5, savePredictions = T)
cv.model.both.npca <- train(Loan.Status ~ ., data = train.cv.npca.both, trControl = control, weights = class.weights,method = "glm", family = "binomial") #doing a logistic model on our training
pred.both.npca <- predict(cv.model.both.npca, newdata = test.cv.npca.both) #predict on test
range(pred.both.npca)
roc_obj <- roc(test.cv.npca.both$Loan.Status, pred.both.npca) #finding optimal threshold (wont be .5 due to the variation in weights)
coords <- coords(roc_obj, "best", ret = "threshold")
opt_threshold <- coords[1]
opt_threshold
Optimal.threshold <- opt_threshold$threshold
pred.both.npca <- ifelse(pred.both.npca > Optimal.threshold,"Defaulter","nonDefaulter")
table(pred.both.npca,test.cv.npca.both$Loan.Status)
pred.both.npca <- as.factor(pred.both.npca)
test.cv.npca.both$Loan.Status <- ifelse(test.cv.npca.both$Loan.Status==1,"Defaulter","nonDefaulter")
test.cv.npca.both$Loan.Status <- as.factor(test.cv.npca.both$Loan.Status)
NonPCA.CV.BOTH.CM <- confusionMatrix(pred.both.npca,test.cv.npca.both$Loan.Status)
NonPCA.CV.BOTH.CM
#both sampling logistic- pca ------------------------------------------------------------------------------------------------------------------------
both <- ovun.sample(Loan.Status~. ,data = train_data_pca, method = "both")
both.d <- both$data
table(both.d$Loan.Status) #imbalance fixed
train.cv.both <- both.d 
test.cv.both <- test_data_pca 
class.weights <- ifelse(train.cv.both$Loan.Status == 1, 10, 1)
control <- trainControl(method = "cv", number = 5, savePredictions = T)
cv.model.both <- train(Loan.Status ~ ., data = train.cv.both, trControl = control, weights = class.weights,method = "glm", family = "binomial")
pred.both <- predict(cv.model.both, newdata = test.cv.both)
test.cv.both$Loan.Status <- ifelse(test.cv.both$Loan.Status==1,"Defaulter","nonDefaulter")
test.cv.both$Loan.Status <- as.factor(test.cv.both$Loan.Status)
max(pred.both)
roc_obj <-  roc(test.cv.both$Loan.Status, pred.both) #finding optimal threshold (wont be .5 due to the variation in weights)
coords <- coords(roc_obj, "best", ret = "threshold")
opt_threshold <- coords[1]
opt_threshold$threshold
range(pred.both)
pred.both <- ifelse(pred.both > opt_threshold$threshold,"Defaulter","nonDefaulter")
pred.both <- as.factor(pred.both)
confusionMatrix(pred.both,test.cv.both$Loan.Status)
#KNN-weights-pca-sampled
k.grid1 <- expand.grid(k = 2:10) #set of ks for testing
knn.test.class <- test_data_pca #pca test class
knn.test.class$Loan.Status <- as.factor(ifelse(knn.test.class$Loan.Status==1,"Defaulter","nonDefaulter"))
weights <- ifelse(knn.train.class$Loan.Status == "Defaulter", 10, 1)
knn.train.class <- both.d #sampled train PCA
knn.train.class$Loan.Status <- as.factor(ifelse(train_data_pca$Loan.Status==1,"Defaulter","nonDefaulter")) #first model with weights and pca and sampling
knn.model.w.pca <- train(Loan.Status ~ ., data = knn.train.class, method = "knn", tuneGrid = k.grid1,weights = weights)
probs.pca.n <- predict(knn.model.w.pca,newdata = test_data_pca)
knn.w.cf <- confusionMatrix(probs.pca.n,knn.test.class$Loan.Status)
knn.w.cf
#KNN- no weights
k.grid2 <- expand.grid(k = 2:10)
knn.test.class <- test_data_pca
knn.test.class$Loan.Status <- as.factor(ifelse(knn.test.class$Loan.Status==1,"Defaulter","nonDefaulter"))
knn.train.class <- train_data_pca
knn.train.class$Loan.Status <- as.factor(ifelse(knn.train.class$Loan.Status==1,"Defaulter","nonDefaulter"))
knn.model.nw.pca <- train(Loan.Status ~ ., data = knn.train.class, method = "knn", tuneGrid = k.grid2)
probs.nw.pca <- predict(KNN.pca,newdata = test_data_pca)
Knn.nw.cf <- confusionMatrix(probs.nw.pca,knn.test.class$Loan.Status)
Knn.nw.cf

#K Means Clustering
train.cv.both.plotsize <- as.data.frame(-train.cv.both[,1:3])
kmpca <- kmeans(train.cv.both.plotsize, 5, nstart = 20)
plot(train.cv.both.plotsize, col = (kmpca$cluster), main = "K-means PCA clustering, K=5", pch =19, cex = 1)
#
#Base Neural Network
#
detach("package:dplyr", unload = TRUE)
Loan.Status <- dum.data[,24]
dum.data.b <- cbind(dum.data[,-24],
                    Loan.Status =class.ind(as.factor(Loan.Status)))
Loan.Status <- train[,24]
train.b <- cbind(train[,-24],
                 Loan.Status = class.ind(as.factor(Loan.Status)))
Loan.Status <- test[,24]
test.b <- cbind(test[,-24],
                Loan.Status = class.ind(as.factor(Loan.Status)))
set.seed(1)
maxs <- apply(train.b[,1:23 ], 2, max)
mins <- apply(train.b[,1:23 ], 2, min)
scaled <- data.frame(scale(dum.data.b[,1:23], center = mins, scale = maxs -
                             mins) )
scaled <- cbind(scaled, dum.data.b[,24:71])

train.b <- scaled[Index==1, ]
test.b <- scaled[Index==2, ]
set.seed(2)
NN <-  neuralnet(Loan.Status.0+Loan.Status.1 ~., train.b,
                 linear.output = F)
#plot(NN)
predict_NN <- compute(NN, test.b[,1:69])
predict_NNclass <- apply(predict_NN$net.result,1, which.max)
teststatus <- max.col(test.b[,70:71])
mean(predict_NNclass == teststatus)
mtrx <- table(predict_NNclass,teststatus)
sensitivity(mtrx)
specificity(mtrx)

#PCA for Neural Network
pca = prcomp(dum.data[,-24], scale = TRUE, center = TRUE) #PCA
pca.vals <- cbind(pca$x,Loan.Status = dum.data$Loan.Status) #binding back predicted class
pca.vals <- as.data.frame(pca.vals)[,c(1:12,70)]
train_data_pca <- cbind(as.data.frame(predict(pca, train[, -24])),Loan.Status = train[,24])[,c(1:12,70)]
Loan.Status <- train_data_pca[,13]
train_data_pca <- cbind(as.data.frame(train_data_pca[,-13]),
                        Loan.Status = class.ind(as.factor(Loan.Status)))
test_data_pca <- cbind(as.data.frame(predict(pca, test[, -24])),Loan.Status = test[,24])[,c(1:12,70)]
Loan.Status <- test_data_pca[,13]
test_data_pca <- cbind(as.data.frame(test_data_pca[,-13]),
                       Loan.Status = class.ind(as.factor(Loan.Status)))
#
#Neural Network Both sampling PCA
#
train_data_pca <- cbind(as.data.frame(predict(pca, train[, -24])),Loan.Status = train[,24])[,c(1:12,70)]
both <- ovun.sample(Loan.Status~. ,data = train_data_pca, method = "both")
both.d <- both$data
table(both.d$Loan.Status) 

Loan.Status <- both.d[,13]
train.cv.both<- cbind(as.data.frame(both.d[,-13]),
                      Loan.Status = class.ind(as.factor(Loan.Status)))
test.cv.both <- test_data_pca

#NN Both pca
set.seed(2)
NN <-  neuralnet(Loan.Status.0+Loan.Status.1 ~., train.cv.both,
                 hidden = c(1,1),
                 algorithm = "rprop+",
                 linear.output = F)
#plot(NN)
predict_NN <- compute(NN, test.cv.both[,1:12])
predict_NNclass <- apply(predict_NN$net.result,1, which.max)
teststatus <- max.col(test.cv.both[,13:14])
mean(predict_NNclass == teststatus)
mtrx <- table(predict_NNclass,teststatus)
sensitivity(mtrx)
specificity(mtrx)

#Summary Table
library(vtable)
library(dplyr)
#EDA Graphs
loan <- read_csv("train.csv")
loan <- loan%>% select(-c(ID,`Batch Enrolled`,
                          `Loan Title`,  `Payment Plan`,))
st(loan)
summary(loan)
#
#loan status by sub grade
subgrade.loan <- loan[,c(
  "Sub Grade","Loan Status")]
subgrade.loan$`Loan Status` <- as.factor(subgrade.loan$`Loan Status`)
plot.bar <- subgrade.loan %>% count(`Loan Status`,`Sub Grade`) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))
ggplot(plot.bar,
       aes(x = `Sub Grade`,
           y = pct,
           fill = `Loan Status`)) +
  geom_bar(stat = "identity",
           position = position_dodge())+
  scale_fill_brewer(palette =  "Paired")+
  theme_minimal()+
  labs(x = "Sub Grade",
       y = "Loans",
       title = "Loan Status by Sub Grade")

grade.loan <- loan[,c(
  "Grade","Loan Status")]
grade.loan$`Loan Status` <- as.factor(subgrade.loan$`Loan Status`)
plot.bar <- grade.loan %>% count(`Loan Status`,`Grade`) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))
ggplot(plot.bar,
       aes(x = `Grade`,
           y = pct,
           fill = `Loan Status`)) +
  geom_bar(stat = "identity",
           position = position_dodge())+
  scale_fill_brewer(palette =  "Paired")+
  theme_minimal()+
  labs(x = "Grade",
       y = "Loans",
       title = "Loan Status by Grade")


















