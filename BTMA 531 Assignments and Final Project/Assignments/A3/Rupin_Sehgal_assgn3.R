library(MASS)
library(neuralnet)
library(e1071)
#Q1
#A
# It is not needed to split the data for unsupervised learning methods as there is no resulting class (no prediction class)
#B
getwd()
setwd("C:/Users/Rupin/Downloads") #working directory
data <- read.csv("ad.csv") #load data
data <- data[,2:5]  #excluding the index column, not a part of the data
set.seed(1) # Set seed for reproducibility
scaled_data <- scale(data) # Scale the data, using the scale function (mu = 0, sd = 1)


k3 <- kmeans(scaled_data, centers = 3, nstart = 10) # K-means clustering with K=3

k4 <- kmeans(scaled_data, centers = 4, nstart = 10) # K-means clustering with K=4

#C

cluster_assignments <- k3$cluster

plot(data, col = cluster_assignments, 
     main = "K-means Clustering, K=3", pch = 19, cex = 1) #scatter plot 
#when examining scatter plots for TV, it is evident that red and green tend to be higher than the black cluster
#when looking at radio, it is evident that green tends to be highest across the clusters andf read and black have a similar smaller range
#when looking at newspaper, it is evident that all three clusters are on the smaller side and fall within the same range
#when looking at sales it si evident that green is higher than the other two cluters followed by the red and then finally the black cluster
#D
wss <- k4$withinss #the within cluster error
tss <- sum(k4$withinss)+sum(k4$betweenss) #total

ratios <- wss / tss
plot(ratios, type = "b", xlab = "Cluster", ylab = "Ratio of WSS to TSS") #plot to find the best K

which.min(ratios) #most homogeneous cluster

#E

hc <- hclust(dist(scaled_data), method = "complete") #creating a hierarchical cluster w complete linkage
plot(hc) #dendogram
clusters <- cutree(hc, k = 4)
plot(hc, main = "Complete Linkage", xlab = "", ylab = "",)

#F
clusters <- cutree(hc, h = 3) #dissimilarity level of h=3

length(unique(clusters)) # Number of clusters

#Q2
#A
data_boston <- Boston
set.seed(1)

Index.B <- sample(nrow(data_boston),size = 400, replace = F) #no. of rows for sampling training and test

maxs = apply(Boston[Index.B,], 2 , max)
mins = apply(Boston[Index.B,], 2 , min)
scaled.B = data.frame(scale(Boston, center = mins, scale = maxs - mins)) #min max scaling

TrainNN.B <- as.data.frame(scaled.B[Index.B, ]) #data split
TestNN.B <- as.data.frame(scaled.B[-Index.B, ])

NN = neuralnet(TrainNN.B$medv ~ ., TrainNN.B, hidden = c(3), linear.output = T ,
               lifesign = "minimal") #Neural net with one hidden layer and 3 nodes
plot(NN)

#B

predict_testNN = compute(NN, TestNN.B[,1:13]) #predicting on test set, excluding the predicted class


mse <- mean((TestNN.B$medv - predict_testNN$net.result)^2)

mse 



#Q3
#A
data.OS <- read.csv("online_shoppers_intention2.csv") #loading data
set.seed(1)
Index.OS <- sample(nrow(data.OS),size = 2000, replace = F) 
maxs.OS = apply(data.OS[Index.OS,1:10], 2 , max)#scaling min max
mins.OS = apply(data.OS[Index.OS,1:10], 2 , min)
scaled.OS = data.frame(scale(data.OS[,1:10], center = mins.OS, scale = maxs.OS - mins.OS))
VistorType = class.ind(as.factor(data.OS[,11]))
Weekend=class.ind(as.factor(data.OS[,12]))
colnames(Weekend) <- c("Weekend_0", "Weekend_1")
Revenue=class.ind(as.factor(data.OS[,13]))
colnames(Revenue) <- c("Revenue_0", "Revenue_1")

scaled.OS <- cbind(scaled.OS,VistorType,Weekend,Revenue)


TrainNN.OS <- as.data.frame(scaled.OS[Index.OS, ]) #data split
TestNN.OS <- as.data.frame(scaled.OS[-Index.OS, ])
colnames(data.OS)
colnames(TestNN.OS[,1:13])

NN.OS <- neuralnet(TrainNN.OS$Revenue_0 + TrainNN.OS$Revenue_1 ~ ., TrainNN.OS, hidden = c(12,11,10), linear.output = F ,
               lifesign = "minimal") #deep learning neural net with 3 layers and 12,11,10 nodes
plot(NN.OS)
#B
predict_testNN.OS = compute(NN.OS, TestNN.OS[,1:15]) #predicting while excluding the output var
View(predict_testNN.OS$net.result)

predict_testShoppersClass <- apply(predict_testNN.OS$net.result, 1, which.max) #turning continous values into 0 and 1s
predict_testShoppersClass <- predict_testShoppersClass-1 #turned 0s into 1s and 1s into 2s, reverting by -1
mse <- mean(((TestNN.OS[,12]) - predict_testShoppersClass)^2)
mse

testActivity=TestNN.OS[,12]


mean(predict_testShoppersClass==testActivity) #check this out
table(predict_testShoppersClass,testActivity)


#Q4
#A

data.ACC <- read.csv("accent-mfcc-data-1.csv")
data.ACC <- as.data.frame(data.ACC)
dummy_vars_ACC <- model.matrix(~ data.ACC$language)
Index.ACC <- sample(nrow(data.ACC),size = 280, replace = F)


TrainNN.ACC <- as.data.frame(data.ACC[Index.ACC, ])
TestNN.ACC <- as.data.frame(data.ACC[-Index.ACC, ])

TrainNN.ACC$language <- as.factor(TrainNN.ACC$language)

m <- svm(TrainNN.ACC$language ~ ., data = TrainNN.ACC, kernel = "radial", cost = 4, scale = F) #svm model

#B
predict_testLang <- predict(m, TestNN.ACC) #predicting on test set

mean(predict_testLang == TestNN.ACC$language)

table(predict_testLang, TestNN.ACC$language)






