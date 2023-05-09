#1
#a
getwd()
setwd("C:/Users/Rupin/Downloads")
#install.packages("data.table")
ether <- read.csv("transaction_dataset.csv")
class(ether$FLAG)
ether$FLAG <- as.factor(ether$FLAG)

set.seed(1)

# sample 2,000 observations from the dataset
ether_train_index <- sample(nrow(ether), 2000)
ether_train <- ether[ether_train_index, ]

# create a logistic regression model
Logistic_FLAG = glm(formula=FLAG ~ .-Index, data=ether_train, family = binomial)
#b
ether_test <- ether[-ether_train_index, ]

Pred_Test <- predict(Logistic_FLAG, ether_test, type="response")

Pred_Stor <- rep(0,7841)

Pred_Stor[Pred_Test>0.5]=1 #probabilities with a 0.5 threshold

#c
mean(ether_test$FLAG==Pred_Stor)#confusion matrix
CM.FLAG <- table(Pred_Test,ether_test$FLAG)
CM.FLAG
FP.Rate <- CM.FLAG[3]/sum(CM.FLAG[,2])
FP.Rate
FN.Rate <- CM.FLAG[2]/sum(CM.FLAG[,1])
FN.Rate

# d) Based on the prediction accuracy and the confusion matrix, we can say that the performance of the classifier is not adequate. To improve its performance when predicting fraudulent
# transactions, we may need to try a different model that can create a model that has a higher accuracy, sensitivity(TP) and specificity(TN).
#Our model has a very low FN rate (higher specificity) meaning that it can correctly identify a true negative ~98% of the time

#2a
library(MASS)
MFCC_Data <- read.csv("accent-mfcc-data-1.csv")
set.seed(1)
mfcc_train_lda_index <- sample(nrow(MFCC_Data), 250) #selecting a subset of 250 random row numbers
mfcc_train_lda <- MFCC_Data[mfcc_train_lda_index, ] #getting values by calling those numbers 
lda_mfcc <- lda(language~., data = mfcc_train_lda) #lda
lda_mfcc
#b
mfcc_test_lda <- MFCC_Data[-mfcc_train_lda_index, ]
lda_mfcc_pred <- predict(lda_mfcc,mfcc_test_lda)
#c
table(lda_mfcc_pred$class,mfcc_test_lda$language)
lda_pred_accuracy <- mean(lda_mfcc_pred$class == mfcc_test_lda$language)
#d
mfcc_train_qda <- MFCC_Data[mfcc_train_lda_index, ]
qda_mfcc <- qda(language~., data = mfcc_train_qda)
#e
mfcc_test_qda <- MFCC_Data[-mfcc_train_lda_index, ]
qda_mfcc_pred <- predict(qda_mfcc,mfcc_test_qda)
qda_pred_accuracy <- mean(qda_mfcc_pred$class == mfcc_test_qda$language)
qda_pred_accuracy
#f
# Although the comparison between the QDA and LDA models suggests that the QDA model may be a better fit for the data due to its higher accuracy of 0.6962025 compared to
# the LDA model's accuracy of 0.6708861, it is important to note that accuracy is not the only performance metric to consider when evaluating the effectiveness of a model.
# Other metrics such as precision, recall, F1 score, and AUC-ROC should also be taken into account. Additionally, it is crucial to assess the model's ability to generalize
# to new data and to check for overfitting, which occurs when a model performs well on the training data but poorly on new, unseen data. Further exploration and analysis of the
# features of the data may also be beneficial in gaining insights into the underlying patterns and relationships. It is also possible that the higher accuracy of the QDA model
# suggests that the data assumes a Gaussian distribution and that each category has its unique covariance matrix. The findings further imply that a linear decision boundary, 
# such as the quadratic one provided by QDA, may be more appropriate.
#g
library(class)

train_knn_MFCC.in <- MFCC_Data[mfcc_train_lda_index, ] #creating training 
train_knn_MFCC.in <- subset(train_knn_MFCC.in, select = -language) #removing caetgorical var
test_knn_MFCC.in <- MFCC_Data[-mfcc_train_lda_index, ]
test_knn_MFCC.in <- subset(test_knn_MFCC.in, select = -language) #same with test set
train_knn_MFCC.out <- MFCC_Data[mfcc_train_lda_index, 1]
test_knn_MFCC.out <- MFCC_Data[-mfcc_train_lda_index, 1]

train_knn_MFCC.inSC <- scale(train_knn_MFCC.in)

means = attr(train_knn_MFCC.inSC,"scaled:center") #normalizing the data
sds = attr(train_knn_MFCC.inSC,"scaled:scale")

test_knn_MFCC.inSC = scale(test_knn_MFCC.in,center = means, scale = sds) #normalizing test set based on training normalization parameters



KNN_5 <- knn(train_knn_MFCC.inSC,test_knn_MFCC.inSC,train_knn_MFCC.out ,k=5) #k=5
KNN_10 <- knn(train_knn_MFCC.inSC,test_knn_MFCC.inSC,train_knn_MFCC.out ,k=10) #k=10

KNN_5_accuracy <- mean(KNN_5 == test_knn_MFCC.out) 
KNN_10_accuracy <- mean(KNN_10 == test_knn_MFCC.out)
KNN_5_accuracy
KNN_10_accuracy


# The accuracy for KNN with k=5 larger KNN with k=10 
# This means that KNN with k=5 performs better than KNN with k=10. This could be because 
# with a smaller value of k, the model is more sensitive to the training data and can capture 
# more of the local structure of the data. However, a smaller k may also lead to overfitting and poor generalization to new data.
# Therefore, it is important to tune the value of k and evaluate the performance of the model on a test set.

#3
#a
library(data.table)
CarEvals <- fread("CarEvals.csv")
library(tree)
CarEvals <- data.frame(lapply(CarEvals, as.factor)) #making all vars categorical
summary(CarEvals)
nrow(CarEvals)
class_tree <- tree(Class ~ ., data = CarEvals) #tree classification
summary(class_tree)   
par(mfrow=c(1,1)) #ploy
plot(class_tree)
text(class_tree,pretty =0,cex =.5)

#b
set.seed(1)
CarEvals_train.index <- sample(nrow(CarEvals), 1000)
CarEvals_train <- CarEvals[CarEvals_train.index,]
class_tree_b <- tree(Class ~ ., data = CarEvals_train)
CarEvals_test <- CarEvals[-CarEvals_train.index,]
b_pred <- predict(class_tree_b, CarEvals_test, type = "class") #predicting ont est set
#c
accuracy_class <- mean(b_pred == CarEvals_test$Class) #accuracy of model compared with test 
accuracy_class
CM_class <- table(b_pred, CarEvals_test$Class)
CM_class
#d
CrossVal_class <- cv.tree(class_tree_b ,FUN = prune.misclass) #cross validation
names(CrossVal_class)
#draw the plots
par(mfrow=c(1,2))
plot(CrossVal_class$size ,CrossVal_class$dev ,type="b") #plotting and using elbow technique to determine best k (13)
plot(CrossVal_class$size ,CrossVal_class$k ,type="b")

Prune_class = prune.misclass(class_tree_b , best = 13)
par(mfrow=c(1,1))
plot(Prune_class )
text(Prune_class ,pretty = 0)

CV_pred <- predict(Prune_class, CarEvals_test, type = "class") 

accuracy_class_cv <- mean(CV_pred == CarEvals_test$Class) 
accuracy_class_cv

#Q4
#a
AQdata <- read.csv("AirQualityUCI.csv")
AQdataFULL <- AQdata[complete.cases(AQdata), ]
nrow(AQdataFULL)
#b
# If we have missing data in a time series, there are several methods we can use to impute the missing values, including:
#   
# Mean imputation: This method involves replacing missing values with the mean value of the non-missing values. 
#This is a simple and fast method, but it may lead to biased estimates if the missing data is not missing completely at random, or if there are outliers in the data.
# 
# Linear interpolation: This method involves estimating missing values by linearly interpolating between adjacent values in the time series.
#This assumes that the time series varies linearly between the observed values, and can work well if the time series is relatively smooth.
# In the case of hourly air quality data, we may want to consider using interpolation methods to estimate missing values, 
#since the time series is likely to be relatively smooth over short time periods (i.e., within an hour). However, the choice 
#of method will depend on the specific characteristics of the data and the research question. It's important to note that imputing missing values can introduce additional uncertainty into the data, 
#so it's important to assess the impact of missing data on any conclusions drawn from the analysis.


#c
# Sequence plot
plot(AQdata$CO, main = "CO Concentration (Sequence Plot)", ylab = "CO Concentration")
# Our scatterplot shows no sign of correlation as the points are spread randomly
# Lag plot
lag.plot(AQdataFULL$CO, 1, main = "CO Concentration (LAG PLOT)")
# By utilizing the Lag Plot, we can examine the association between the present CO concentration value and its preceding value.
# Our analysis reveals a triangular pattern, indicating the presence of positive autocorrelation. Positive autocorrelation implies that consecutive 
# values of the variable are linked in a positive manner, implying that if the variable's value is elevated during one time period, it is more probable to be elevated during the next period and the other way around
# Histogram
hist(AQdata$CO, xlab = "CO Concentration", main = "Histogram of CO Concentration")
# By examining the Histogram, we can evaluate the normality of the CO concentration distribution. In this instance, 
# a distribution with a right-skew is noticeable. Additionally, the plot suggests that there may be outliers present in the higher values.
#d
qqnorm(AQdataFULL$CO)
qqline(AQdataFULL$CO)
# Based on this plot, we observe that the distribution of CO data is not exactly normal,
# as the points deviate from the Q-Q line at the tails of the distribution. There is evidence 
# of heavier tails than expected under the normal distribution.

