#Delete Environment
rm(list =ls())

#Packages for data scraping
library(tidyverse)
library(dplyr)
library(rvest)
library(ggplot2)
#Scraping ---------------------------------------------------------------
#scraping Player data
url <- "https://www.hockey-reference.com/leagues/NHL_2023_skaters.html"
x <- url %>% read_html()
player.df <- x %>%
  html_element(css = '#stats') %>%
  html_table()

#Scraping teams data
url <- "https://www.hockey-reference.com/teams/#active_franchises"
x <- url %>% read_html()
team.df <- x %>%
  html_element(css = '#active_franchises') %>%
  html_table()

#Cleaning ----------------------------------------------------------------
teamCleaned.df <- team.df
playerCleaned.df <- player.df

#Cleaning teams data
#Removing all NA from dataframe
teamCleaned.df[is.na(teamCleaned.df)] <- 0

#Find all teams that didn't play to 2023
for(i in 1:NROW(team.df)){
  if(team.df$To[i] != "2023"){
    teamCleaned.df$To[i] <- NA
  }
}
teamCleaned.df <- na.omit(teamCleaned.df) #remove NA assigned to non 2023 years

#Find all duplicate teams
for(i in 2:NROW(teamCleaned.df)){
  if(teamCleaned.df$Franchise[i] == teamCleaned.df$Franchise[i-1]){
    maxYear <- max(teamCleaned.df$From[i],teamCleaned.df$From[i-1])
    if(teamCleaned.df$From[i] == maxYear){
      teamCleaned.df$From[i] <- NA
    }else(teamCleaned.df$From[i-1])
  } 
}
#Remove all NA from data
teamCleaned.df <- na.omit(teamCleaned.df) #remove NA assigned to non 2023 years

#Cleaning player data
colnames(playerCleaned.df) <- playerCleaned.df[1,] #get column names from first row
playerCleaned.df <- playerCleaned.df[-1,] #remove first row (only had column headers)
playerCleaned.df <- playerCleaned.df[!duplicated(playerCleaned.df$Rk), ] #remove all duplicated players based on duplicate rankings
playerCleaned.df <- playerCleaned.df[!grepl("Rk", playerCleaned.df$Rk),] #remove all duplicate column headers from within the data (remove all rows that start with "Rk")

#Removing all empty cells from data frame
playerCleaned.df <- replace(playerCleaned.df, playerCleaned.df=='', NA)

#Changing columns types
#Return the datatype of each column
print(sapply(playerCleaned.df, class)) #Lets you see what data type each column is

#Convert all numeric data to actually numeric
playerCleaned.df$Rk = as.numeric(as.character(playerCleaned.df$Rk))
playerCleaned.df$Age = as.numeric(as.character(playerCleaned.df$Age))
playerCleaned.df$GP = as.numeric(as.character(playerCleaned.df$GP))
playerCleaned.df$G = as.numeric(as.character(playerCleaned.df$G))
playerCleaned.df$A = as.numeric(as.character(playerCleaned.df$A))
playerCleaned.df$PTS = as.numeric(as.character(playerCleaned.df$PTS))
playerCleaned.df$PIM = as.numeric(as.character(playerCleaned.df$PIM))
playerCleaned.df$PS = as.numeric(as.character(playerCleaned.df$PS))
playerCleaned.df$EV = as.numeric(as.character(playerCleaned.df$EV))#Not working?
playerCleaned.df$PP = as.numeric(as.character(playerCleaned.df$PP))#Not working?
playerCleaned.df$SH = as.numeric(as.character(playerCleaned.df$SH))#Not working?
playerCleaned.df$GW = as.numeric(as.character(playerCleaned.df$GW))
playerCleaned.df$S = as.numeric(as.character(playerCleaned.df$S))
playerCleaned.df$`S%` = as.numeric(as.character(playerCleaned.df$`S%`))
playerCleaned.df$TOI = as.numeric(as.character(playerCleaned.df$TOI))
playerCleaned.df$BLK = as.numeric(as.character(playerCleaned.df$BLK))
playerCleaned.df$HIT = as.numeric(as.character(playerCleaned.df$HIT))
playerCleaned.df$FOW = as.numeric(as.character(playerCleaned.df$FOW))
playerCleaned.df$FOL = as.numeric(as.character(playerCleaned.df$FOL))
playerCleaned.df$`FO%` = as.numeric(as.character(playerCleaned.df$`FO%`))
playerCleaned.df$`+/-` = as.numeric(as.character(playerCleaned.df$`+/-`))

#replacing all NA with 0
playerCleaned.df[is.na(playerCleaned.df)] <- 0
playerCleanedAgeAbove0.df <- subset(playerCleaned.df, playerCleaned.df$Age > 0) # removing all players with age of 0


#Regressions ----------------------------------------------------------------------
### Q1 ###
#does experience that comes with years of play for veterans help them score more on average or does young talent score more on average
#is there a significant difference in the performance of teams that have a large number of veteran players versus teams that have a large number of young players?
playerCleanedtemp.df <- playerCleaned.df[,-c(17:19)] %>%
  mutate(AgeGroup = if_else(Age <= 25, "Young", "Veteran"))
performance_summary <- playerCleanedtemp.df %>%
  group_by(AgeGroup, Tm) %>%
  summarize(
    mean_PTS = mean(PTS),
    mean_W = sum(if_else(PTS > 0, 1, 0)))
model <- lm(mean_PTS + mean_W ~ AgeGroup, data = performance_summary)
summary(model) #tm ref is ari, age group ref is vet(intercept). output basically shows significant on mean goals depending on age group, and significance increase for mean goals for younger players (youth/talent > wisdom that comes with age)
#both additions, vet and young, bring positive impact on points of course, but younger players slightly outperform older players, although the change is not by much so experience can ALMOST keep up with young talent and reflexes
#Now lets how teams perform based on their roster having younger or older players.
ggplot(performance_summary, aes(x = Tm, y = mean_PTS, fill = AgeGroup)) +
  geom_col(position = "dodge", width = 0.7) +
  labs(title = "Mean Points Scored by Veteran vs Young Players by Team",x = "Team", y = "Mean Points & Wins", fill = "Age Group") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#its natural that older players have an higher average pts scored since they have played longer, but it is interesting to note that teams that have younger talent in recent years perform nearly as well as some of the highest scoring teams, where as teams that are highly skewed towards older players generally have higher variations in performance
#this is especially true for teams that lack a younger player presence

playerTimeCleaned.df <- playerCleanedAgeAbove0.df[,-c(17:19)] #removing duplicated columns
ATOI_datetime <- as.POSIXct(playerTimeCleaned.df$ATOI, format = "%M:%S") #setting up time on ice
secs.1 <- as.numeric(format(ATOI_datetime, "%M"))*60
playerTimeCleaned.df$secs <- secs.1 + as.numeric(format(ATOI_datetime, "%S"))
colnames(playerTimeCleaned.df)
playerTimeCleaned.df <- playerTimeCleaned.df[,-c(19:20)] %>% #adding pts per second of play time
  mutate(pts_per_secs = PTS/secs)


summary(playerTimeCleaned.df)

which.min(playerTimeCleaned.df$Age)

getwd()
setwd("C:/Users/wills/Downloads") #new data
newdata <- read.csv("train_1.csv")
newdata
colnames(playerTimeCleaned.df)
newdata <- data.frame(newdata)
colnames(newdata) <- c("Salary","Player")
colnames(newdata) 
playerTimeCleaned.df <- merge(newdata,playerTimeCleaned.df,by = "Player")

#Here we can see how salaries are distributed by age through the NHL. Players from 28 to 35 seem to be making the most. Does this suggest those are the best players as supply and demand would dictate their value (salary) to correspond to their skill right?
ggplot(playerTimeCleaned.df, aes(x = Age)) + #bar plot of salary by age of players
  geom_col(aes(y = Salary), alpha = 0.5, width = 0.5, fill = "blue") +  theme_classic() + #its evident 29-35 year old players make the most, at least from their NHL salaries. Interesting when looking at the last graph where players evidently become over paid after 28
  labs(title = "Salary by Age") + #to maximize team $ teams should have a few veteran players while focusing heavy attention on acquiring younger talent as they tend to be cheaper and perform better for longer
  theme(plot.title = element_text(hjust = 0.5))
#now we must find out how to maximize a teams potential by comparing performance, time on ice and salaries to figure out if its more worth keeping older or younger players
#this graph shows a player's play time, points per second of play time, and salary by age. A clear pattern of increasing salary can be seen from the chart, where as the points per second and play time following an non linear pattern. 
#Its evident that players are on a very steep decline in terms of performance after 24 years of age with both their time on ice and points per sec decreasing rapidly. This somewhat stabilizes at 28. Its interesting to also that after 28 a players efficiency as decreases as their pts per decreasing to a lower point than their play time
#Interestingly as performance decreases a players pay drastically continues to increase! Could suggest that teams could benefit from employing players under the age of 28.
ggplot(playerTimeCleaned.df, aes(x = Age)) +
  geom_smooth(aes(y = scale(secs), color = "Play Time"), method = "loess", linetype = "solid",se = F) +
  geom_smooth(aes(y = scale(pts_per_secs), color = "Points per Second"), method = "loess", linetype = "solid",se = F) +
  geom_smooth(aes(y = scale(Salary),color = "Salary"), method = "loess",linetype = "solid",se = F) +
  scale_color_manual(values = c("red", "blue","green")) +
  theme_classic() +
  labs(title = "Player Statistics", x = "Age", y = "Normalized Values", color = "Legend")+
  theme(plot.title = element_text(hjust = 0.5))
#What impacts a player's salary, with Age does a player make more or less money?
#multiple highlights with P<0.05. look at summary and explain all details in report.
salarypred <- lm(playerTimeCleaned.df$Salary ~.,data = playerTimeCleaned.df[,c(2,4,6:9,11:15,17:26)])
summary(salarypred)

### Q2 ###

# What game statistics have a significant impact on a players points ?

#Q2 part 1
Q2corFit.1 <- lm(PTS ~ G + A + GP + PIM + GW + S + BLK + HIT + Age, data = playerCleaned.df)
summary(Q2corFit.1)
#GP, PIM, GW, S, BLK, HIT, Age all seem to not be statically significant
#G, A, GP, and HIT both have a p value less than 0.05



#plot correlated statistics 
plot(playerCleaned.df$G, playerCleaned.df$PTS) #goals
plot(playerCleaned.df$A, playerCleaned.df$PTS) #assists



#plot non-correlated statistics
plot(playerCleaned.df$GP, playerCleaned.df$PTS) 
plot(playerCleaned.df$PIM, playerCleaned.df$PTS) 
plot(playerCleaned.df$GW, playerCleaned.df$PTS) 
plot(playerCleaned.df$S, playerCleaned.df$PTS) 
plot(playerCleaned.df$BLK, playerCleaned.df$PTS) 
plot(playerCleaned.df$HIT, playerCleaned.df$PTS) 
plot(playerCleaned.df$Age, playerCleaned.df$PTS) 





#Q2 follow up Question
#How does the average goals scored per player change across all age groups?

# finding avg goals scored per age
Average_Age <- list()

for (Age in unique(playerCleanedAgeAbove0.df$Age)){
  age_data <- playerCleanedAgeAbove0.df[playerCleanedAgeAbove0.df$Age == Age,]
  Average_Age[as.character(Age)] <- mean(age_data$G)
  
}


#double checked the math to ensure loop works above
pp<- subset(playerCleanedAgeAbove0.df, playerCleanedAgeAbove0.df$Age == 19)

pt<- mean(pp$G)

pt

which.max(Average_Age)



### Q3 ###



#Predictor Evaluation------------------------------------------------------------
#Linearity Test (see if the data is linearity correlated with Stanley cup wins)
cor.test(teamCleaned.df$`St Cup`, teamCleaned.df$`PTS%`)  #PTS% below .70
cor.test(teamCleaned.df$`St Cup`, teamCleaned.df$Yrs) #Yrs
cor.test(teamCleaned.df$`St Cup`, teamCleaned.df$GP) #GP
cor.test(teamCleaned.df$`St Cup`, teamCleaned.df$W) #W
cor.test(teamCleaned.df$`St Cup`, teamCleaned.df$L) #L Below .70
cor.test(teamCleaned.df$`St Cup`, teamCleaned.df$T) #T
cor.test(teamCleaned.df$`St Cup`, teamCleaned.df$`Yrs Plyf`) #Yrs Plyf
cor.test(teamCleaned.df$`St Cup`, teamCleaned.df$Div) #Div
cor.test(teamCleaned.df$`St Cup`, teamCleaned.df$Conf) #Conf # Below .70
cor.test(teamCleaned.df$`St Cup`, teamCleaned.df$PTS) #PTS
cor.test(teamCleaned.df$`St Cup`, teamCleaned.df$OL) #OL Below .70

#Plot non/weak correlated predictors
plot(teamCleaned.df$`PTS%`, teamCleaned.df$`St Cup`) #PTS% no-relation cone shaped
plot(teamCleaned.df$L, teamCleaned.df$`St Cup`) #L linear
plot(teamCleaned.df$Conf, teamCleaned.df$`St Cup`) #Conf discrete linear
plot(teamCleaned.df$OL, teamCleaned.df$`St Cup`) #OL non-linear
#Only OL looks to potentially related and be non-linear

#A model made from all strongly correlated predictors
corFit <- lm(`St Cup` ~ Yrs + GP + W + T + `Yrs Plyf` + Div + PTS, data = teamCleaned.df) #Franchises not included since should have no predictive value, only strongly correlated data used linear data used
corFitSummary <- summary(corFit)
#GP,W,T,Yrs Plyf,Div, and PTS all seem to not be statistically significant 
#only Yrs has a p-value less then 5%

#Checking higher powers of OL for better fit with only statistically significant predictors 
fitOLPower <- lm(teamCleaned.df$`St Cup` ~ poly(teamCleaned.df$OL, 4) + teamCleaned.df$Yrs)
fitOLPowerSummary <- summary(fitOLPower) #high powers of OL do not improve fit. The number of years played remain the only statistically significant predictor

#checking just statistically significant predictors (only yrs)
yrsFit <- lm(`St Cup` ~ Yrs, data = teamCleaned.df) 
yrsFitSummary <- summary(yrsFit)

#stepwise function 
teamCleanedStep.df <- teamCleaned.df[,c(-1,-2,-16)]

null.model <- lm(`St Cup` ~ 1, data = teamCleanedStep.df) #null model no predictors
full.model <- lm(`St Cup` ~ ., data = teamCleanedStep.df) #All predictors

step.model1 <- step(null.model, 
                    scope = list(lower = null.model, upper = full.model),
                    direction = "both")
step.model1Summary <- summary(step.model1)

#Removing highest p-value till all predictors are significant
#T removed
step.model2 <- lm(`St Cup` ~ Div + Yrs + GP +  Conf, data = teamCleaned.df)
step.model2Summary <- summary(step.model2)

#Div removed
step.model3 <- lm(`St Cup` ~ Yrs + GP +  Conf, data = teamCleaned.df)
step.model3Summary <- summary(step.model3) #all predictors now have a p-value < 5%

#plot residuals to visually assess normality
qplot(step.model3$residuals)
