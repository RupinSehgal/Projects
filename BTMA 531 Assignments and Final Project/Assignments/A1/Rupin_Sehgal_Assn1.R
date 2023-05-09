#Q1a
object1 <- seq(10,100,4)
object1
paste0("The 14th object is ",object1[14]) #62 is the answer
#Q1b
ls() #listing all objects
rm(object1) #removing object 1
#Q1c
install.packages('tidyr') #installing and activating tidyr
library('tidyr')
#Q1d
matrix1 <- matrix(c(runif(15,1,25), rnorm(15,5,2)), nrow = 15, ncol = 2) #creating the matrix
matrix1
colMeans(matrix1) #using colmeans to get the mean of all columns
#Q1e
plot(x = matrix1[,1], y = matrix1[,2]) #using [] to delineate x and y
#Q2a/b
install.packages('ISLR') #loading ISLR and Wage
library("ISLR")
data("Wage")
ncol(Wage) #number of vars
num_qualitative <- sum(sapply(Wage, is.factor)) #using .isfactor to find all non quant vars
num_quantitative <- ncol(Wage) - num_qualitative
print(paste0("The number of quantitative variables are ",num_quantitative, " The number of qualitative variables are ", num_qualitative))
install.packages('dplyr') 
library(dplyr)
Wage <- Wage %>% select_if(function(x) length(unique(x)) > 1) #using dplyr to only have factors with 1 level to ensure regression works
model <- lm(wage ~ ., data = Wage) #conducting linear regression and looking at coefs/tvalues to find all important variables
summary(model)
coefs <- coef(model) #taking the coefs of all variables
t_value <- coefs / summary(model)$coef[, "Std. Error"] #calculating t values, which represent how confident we are in the coef as being a predictor (higher the better)
coef_table <- data.frame(coefs, t_value)
coef_table$t_value <- abs(coef_table[,2])
coef_table <- coef_table[order(coef_table$t_value,decreasing = TRUE),] #setting up both coefs and t values in a descending order and taking the most important (top 5 coef/tval) and least important (bottom 5)

most_important_vars <- coef_table[1:5,]
least_important_vars <- coef_table[(nrow(coef_table)-4):nrow(coef_table)-1,]

print("most important variables:")
print(most_important_vars)
print("least important variables:")
print(least_important_vars)

#Q3
data("Carseats") #importing data
ncol(Carseats)
Carseats_Filtered_Bad <- Carseats %>% filter(Carseats$ShelveLoc == "Bad") #filtering bad and good w dplyr
Carseats_Filtered_Good <- Carseats %>% filter(Carseats$ShelveLoc == "Good")
mean(Carseats_Filtered_Bad$Advertising) #means of bad shelveloc and good shelveloc
mean(Carseats_Filtered_Good$Advertising)
#Q4a/b
library(ggplot2) #using ggplot for all graphs from here, just more use to it
ggplot(data = Carseats, aes(x=Advertising,y=Sales)) + geom_point() + ggtitle("Sales vs Advertising") + theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Advertising", y = "Sales") + geom_smooth(method = "lm")
#^^^^ creating a point plot (geom_point) and setting up titles and axis names. added linaer estimate via regression line
#Q4c
cor(Carseats$Advertising, Carseats$Sales) #Correlation is a statistical measure that describes the association between two variables. A positive correlation means that as one variable increases, the other variable also tends to increase, while a negative correlation means that as one variable increases, the other variable tends to decrease. The strength of the correlation is measured by a correlation coefficient, which ranges from -1 (perfect negative correlation) to +1 (perfect positive correlation). Zero correlation means that there is no relationship between the two variables.
#Q4d
#No, one cannot state that Advertising causes Sales to increase or decrease solely based on the above analysis. The correlation coefficient can provide information about the relationship between two variables, but it cannot show causality. A correlation coefficient of 1 does not imply causality.
#Q5a
Freq_Table_ShelveLoc <- table(Carseats$ShelveLoc)
Freq_Table_ShelveLoc_df <- as.data.frame(Freq_Table_ShelveLoc) #creating data frames for later use
print(Freq_Table_ShelveLoc) 
Freq_Table_Urban <- table(Carseats$Urban)
Freq_Table_Urban_df <- as.data.frame(Freq_Table_Urban)
print(Freq_Table_Urban)
#Q5b
Plot1 <- ggplot(data = Freq_Table_ShelveLoc_df,aes(x= Var1, y = Freq)) + geom_bar(stat = "identity") +
  xlab("Shelve Location") + ylab("Frequency") + ggtitle("Frequency of Shelve Location") + theme(plot.title = element_text(hjust = 0.5))
#^^^creating bar graph for shelveloc
Plot2 <- ggplot(data = Freq_Table_Urban_df,aes(x= Var1, y = Freq)) + geom_bar(stat = "identity") +
  xlab("Urban") + ylab("Frequency") + ggtitle("Urban: Yes or No") + theme(plot.title = element_text(hjust = 0.5))
#^^^creating urban bar graph
install.packages("gridExtra") #using gridextra package for side by side graphs
library(gridExtra)
grid.arrange(Plot1, Plot2, ncol = 2) #printing side by side plots
#Q5c
ggplot(Carseats, aes(x=Sales))+geom_histogram(binwidth=2, color="black") #ggplot to make histogram

#Q6a
getwd() #swetting the working directory with the csv
setwd("C:/Users/Rupin/Downloads")
AdData <- read.csv("ad.csv")
AdData <- fread("ad.csv")
AdData
#6b
reg_adData <-  lm(sales ~ radio, data = AdData) #linear regression model
reg_adData
radio_new_predictor <- data.frame(radio = 25) #new value for prediction
predict(reg_adData, radio25)
#This means that the expected value of adData$radio is intercept + ceof$radio * 20
#From the coefficients, we can conclude that radio advertising does have an impact on sales, since the coefficient of adData$sales is significantly different from zero (as indicated by the dot next to the coefficient).
#Q6c
multireg_adData <- lm(Sales ~ ShelveLoc + Advertising + Price, data = Carseats) #multiple regression
summary(multireg_adData)
#q6d
residuals_multireg_adData <- residuals(multireg_adData)
qqnorm(residuals_multireg_adData)
qqline(residuals_multireg_adData)
#A normality plot of residuals for a multiple linear regression is used to check the assumption of normality of errors. The plot displays a histogram of residuals and a normal probability plot. If the residuals are normally distributed, the points on the normal probability plot should follow a straight line. In this case they are normal and we have normally distributed residuals
#Q7
data("Carseats")
ls(Carseats)
carseats_sales <- Carseats$Sales #setting up as seperate objects for predicting new vars. could have done data frame as well i believe
Price <- Carseats$Price
ShelveLoc <- Carseats$ShelveLoc
Advertising <- Carseats$Advertising
multireg_carseats <- lm(carseats_sales ~ ShelveLoc + Advertising + Price) #multiple regression
#q7b
New_data_reg_carseats <- data.frame(ShelveLoc = "Good", Advertising = 10, Price = 100) #data frame with new observation
predict(multireg_carseats, New_data_reg_carseats, type = "response")

#Q8
ls(Carseats)
# a) sales is the most important variable as it can be used as the prediction variable to find potential sales. also sales = revenue = profits
# 
# b) The data captures the sales and advertising data, demographic information of the consumers, and the prices of car seats. 
#   Aspects missing in this data might include consumer behavior patterns, reasons for buying car seats, competition data, gender of consumers, etc.
# 
# c) Some questions that might be helpful for the business are:
#   
#   What is the impact of advertising, price, and shelf location on sales?
#   What is the consumer demographic most likely to buy car seats and why?
#   What is the consumer behavior pattern with regards to buying car seats?
#   How does the company's performance compare to its competitors?


