# install.packages("forecast")
# install.packages('TTR')
# install.packages("arules")
# install.packages("SnowballC")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("syuzhet")
library(forecast)
library(TTR)
library(ggplot2)
library(lmtest)
library(dplyr)
library(syuzhet)
library(wordcloud)
library(tm)
library(SnowballC)
library(arules)
#Q1
#A
getwd() #data import
setwd("C:/Users/Rupin/Downloads")
data <- read.csv("PRSA_DataSample.csv")
temp_ts <- ts(data$TEMP, frequency = 12) #time-series object with only the TEMP variable
temp_ts_interp <- na.interp(temp_ts) #missing values using na.interp() function
#B
plot(runMean(temp_ts_interp, n = 2),col = "red") #plotting the moving averafe with actual
lines(temp_ts_interp,col = "black")
#C
acf(temp_ts_interp) #yes there is autocorrelation in the data both pos and neg
acf(diff(temp_ts_interp)) #differencing the values helps, but not enough
pacf(temp_ts_interp) #pacf helps capture both direct and indirect cor so this is much better
pacf(diff(temp_ts_interp))#removes almost all autocorrelation
#D
fitq1 <- auto.arima(temp_ts_interp) #fitting the auto arima model
summary(fitq1)
#The parameters:
# Series: temp_ts_interp 
# ARIMA(0,1,1)(1,1,0)[12] 
#The data has been differenced once = (0,1,1). a seasonal model has been used to account for any seasonality in the data, as indicated by the (1,1,0)[12].

predq1 <- forecast(fitq1, h=24)
plot(predq1)    
#E
plot(decompose(temp_ts_interp))
fit2q1 = HoltWinters(temp_ts_interp)
summary(fit2q1)
predq1.2 = forecast(fit2q1, h = 24)
plot(predq1.2)
#F
wspm <- na.interp(data$WSPM)
regmodel3 <- auto.arima(wspm, xreg = data$TEMP)

coeftest(regmodel3) #No, because p value is > 0.05 for xreg

#Q2

data("Groceries")
#A
rules <- apriori(Groceries,parameter = c(support = 0.02, confidence = 0.4))
#B
detach(package:tm, unload=TRUE) #for some reason it would not work with tm package also loaded https://stackoverflow.com/questions/18934098/r-error-with-inspect-function
inspect(rules[13])
# rule [13]: {root vegetables, other vegetables} => {whole milk}.
# 
# The measures for this rule are:
#   
# Support: This indicates the proportion of transactions in the dataset that contain both the LHS and RHS of the rule. In this case, the support for the rule is 0.023, which means that the combination of root vegetables, other vegetables, and whole milk appears in about 2.3% of all transactions.
# 
# Confidence: This measures how often the RHS of the rule (whole milk) appears in transactions that contain the LHS (root vegetables and other vegetables). The confidence for this rule is 0.489, which means that out of all transactions containing both root vegetables and other vegetables, 48.9% of them also contain whole milk.
# 
# Lift: This measures the extent to which the occurrence of the RHS (whole milk) is dependent on the occurrence of the LHS (root vegetables and other vegetables) above and beyond what would be expected by chance. A lift value greater than 1 indicates that the RHS is more likely to occur when the LHS is present. In this case, the lift for the rule is 1.915, which means that the occurrence of whole milk is almost twice as likely when both root vegetables and other vegetables are present compared to what would be expected if the occurrence of whole milk were independent of the occurrence of root vegetables and other vegetables.
# 
# In summary, the rule [13] has a relatively low support but high confidence and lift, suggesting that when both root vegetables and other vegetables are purchased together, customers are more likely to also purchase whole milk.
# 
library(tm)
#Q3
#A
dataq3 <- data.frame(read.csv("TextData.csv",stringsAsFactors = F)) #string as false so they are as character
docs <- VCorpus(DataframeSource(dataq3))
docs <- docs %>% 
  tm_map(content_transformer(tolower)) %>% #removing all things with dplyr package and alsos steming
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)


inspect(docs[[1]]) #check
#B

tdm <- DocumentTermMatrix(docs)
inspect(tdm) #matrix of words
Freqterms <- findFreqTerms(tdm)
Freqterms
assoc_terms <- findAssocs(tdm, c("industri", "happier"), corlimit = 0.5) #used industri and happier
assoc_terms
#C
m <- as.matrix(tdm)
term_freq <- sort(colSums(m), decreasing = T ) #different than the notes, not rowsum. it would reutrn a cloud of numbers, so i used colsums
set.seed(1) #to help reproduce results
wordcloud(names(term_freq), term_freq, min.freq = 20, random.order = F)

#D
tdm2 <- removeSparseTerms(tdm, sparse = 0.95) #cleaning further
m2 <- as.matrix(tdm2)
distm <- dist(scale(m2))
fit <- hclust(distm) #clustering w dendogram
plot(fit)
rect.hclust(fit, k = 5) #chosing a k of 5

#E
groups <- cutree(fit, k = 5)
m3 <- t(m2)
dim(m3)
set.seed(1)
kmeansr <- kmeans(m3,5)
kmeansr
kmeansr$cluster

#F
d = get_nrc_sentiment(dataq3$text)
t_sum = colSums(d)
names(t_sum)
barplot(sort(t_sum,decreasing = T), las = 2) #plotting in a descending fashion
