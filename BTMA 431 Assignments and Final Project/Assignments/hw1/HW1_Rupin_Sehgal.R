#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
getwd()
setwd("C:/Users/Rupin/Downloads")
load("HW1_data.rData")
install.packages('ggplot2')
library('ggplot2')
install.packages("dplyr")
library("dplyr")
install.packages('lubridate')
library('lubridate')

#Q1a Highest Long Run ROI
BTC_FILTERED <- BTC.charts %>% filter(date >= "2016-01-06" & date <= "2023-01-06") #filtering all cryptos with the specified dates (inclusive) in the HW Assignment
XRP_FILTERED <- XRP.charts %>% filter(date >= "2016-01-06" & date <= "2023-01-06")
LTC_FILTERED <- LTC.charts %>% filter(date >= "2016-01-06" & date <= "2023-01-06")
DASH_FILTERED <- DASH.charts %>% filter(date >= "2016-01-06" & date <= "2023-01-06")
ETH_FILTERED <- ETH.charts %>% filter(date >= "2016-01-06" & date <= "2023-01-06")
PPC_FILTERED <- PPC.charts %>% filter(date >= "2016-01-06" & date <= "2023-01-06")
XLM_FILTERED <- XLM.charts %>% filter(date >= "2016-01-06" & date <= "2023-01-06")

BTC_LONG_ROI <- (BTC_FILTERED$close[which(BTC_FILTERED$date == "2023-01-06")] - BTC_FILTERED$close[which(BTC_FILTERED$date == "2016-01-06")]) / BTC_FILTERED$close[which(BTC_FILTERED$date == "2016-01-06")] #calculating the ROI for each crypto with the formula provided
XRP_LONG_ROI <- (XRP_FILTERED$close[which(XRP_FILTERED$date == "2023-01-06")] - XRP_FILTERED$close[which(XRP_FILTERED$date == "2016-01-06")]) / XRP_FILTERED$close[which(XRP_FILTERED$date == "2016-01-06")] 
LTC_LONG_ROI <- (LTC_FILTERED$close[which(LTC_FILTERED$date == "2023-01-06")] - LTC_FILTERED$close[which(LTC_FILTERED$date == "2016-01-06")]) / LTC_FILTERED$close[which(LTC_FILTERED$date == "2016-01-06")] 
DASH_LONG_ROI <- (DASH_FILTERED$close[which(DASH_FILTERED$date == "2023-01-06")] - DASH_FILTERED$close[which(DASH_FILTERED$date == "2016-01-06")]) / DASH_FILTERED$close[which(DASH_FILTERED$date == "2016-01-06")] 
ETH_LONG_ROI <- (ETH_FILTERED$close[which(ETH_FILTERED$date == "2023-01-06")] - ETH_FILTERED$close[which(ETH_FILTERED$date == "2016-01-06")]) / ETH_FILTERED$close[which(ETH_FILTERED$date == "2016-01-06")]
PPC_LONG_ROI <- (PPC_FILTERED$close[which(PPC_FILTERED$date == "2023-01-06")] - PPC_FILTERED$close[which(PPC_FILTERED$date == "2016-01-06")]) / PPC_FILTERED$close[which(PPC_FILTERED$date == "2016-01-06")]
XLM_LONG_ROI <- (XLM_FILTERED$close[which(XLM_FILTERED$date == "2023-01-06")] - XLM_FILTERED$close[which(XLM_FILTERED$date == "2016-01-06")]) / XLM_FILTERED$close[which(XLM_FILTERED$date == "2016-01-06")]

MAX_LONGRUN_ROI <- round(max(BTC_LONG_ROI,XRP_LONG_ROI,LTC_LONG_ROI,XLM_LONG_ROI,PPC_LONG_ROI,DASH_LONG_ROI,ETH_LONG_ROI),1) #find the highest long run ROI from the 7 cryptos and rounding to the nearest whole number

print(paste0("The maximum Long Run ROI rounded to the nearest hundred percent is ", (round(MAX_LONGRUN_ROI, 2) * 100),"%")) #printing the result as the nearest hundred percent

#Q1b Highest mean daily return
N <- nrow(BTC_FILTERED) #finding the number of rows (done for BTC, but can use for any crypto as it is the same number of rows, due to the filter above)

todays_price_BTC <- BTC_FILTERED$close[2:N] #getting today's price for each crypto as sepcified in the slides
todays_price_XRP <- XRP_FILTERED$close[2:N]
todays_price_LTC <- LTC_FILTERED$close[2:N]
todays_price_DASH <- DASH_FILTERED$close[2:N]
todays_price_ETH <- ETH_FILTERED$close[2:N]
todays_price_PPC <- PPC_FILTERED$close[2:N]
todays_price_XLM <- XLM_FILTERED$close[2:N]
yesterdays_price_BTC <- BTC_FILTERED$close[1:N-1] #getting yesterday's price for each crypto as sepcified in the slides
yesterdays_price_XRP <- XRP_FILTERED$close[1:N-1]
yesterdays_price_LTC <- LTC_FILTERED$close[1:N-1]
yesterdays_price_DASH <- DASH_FILTERED$close[1:N-1]
yesterdays_price_ETH <- ETH_FILTERED$close[1:N-1]
yesterdays_price_PPC <- PPC_FILTERED$close[1:N-1]
yesterdays_price_XLM <- XLM_FILTERED$close[1:N-1]
BTC_DAILY_RETURN <- c(NA, (todays_price_BTC - yesterdays_price_BTC) / yesterdays_price_BTC) #calculating daily return for each crypto with the formula int he slides
XRP_DAILY_RETURN <- c(NA, (todays_price_XRP - yesterdays_price_XRP) / yesterdays_price_XRP)
LTC_DAILY_RETURN <- c(NA, (todays_price_LTC - yesterdays_price_LTC) / yesterdays_price_LTC)
DASH_DAILY_RETURN <- c(NA, (todays_price_DASH - yesterdays_price_DASH) / yesterdays_price_DASH)
ETH_DAILY_RETURN <- c(NA, (todays_price_ETH - yesterdays_price_ETH) / yesterdays_price_ETH)
PPC_DAILY_RETURN <- c(NA, (todays_price_PPC - yesterdays_price_PPC) / yesterdays_price_PPC)
XLM_DAILY_RETURN <- c(NA, (todays_price_XLM - yesterdays_price_XLM) / yesterdays_price_XLM)


maximum_daily_return_mean <- max(mean(na.omit(BTC_DAILY_RETURN)), #finding the highest average daily return and exluduing all NAs to make the calculation possible
    mean(na.omit(XRP_DAILY_RETURN)), 
    mean(na.omit(LTC_DAILY_RETURN)), 
    mean(na.omit(DASH_DAILY_RETURN)), 
    mean(na.omit(ETH_DAILY_RETURN)),
    mean(na.omit(PPC_DAILY_RETURN)),
    mean(na.omit(XLM_DAILY_RETURN))) 

print(paste0("The highest mean daily return as a percent rounded to the nearest hundreth is ", round(maximum_daily_return_mean,4) * 100, "%")) #printing final result with specified rounding instructions

#1c
minimum_daily_return_sd <- min(sd(na.omit(BTC_DAILY_RETURN)), #finding the minimum standard deviation of daily returns from all 7 cryptos
                                 sd(na.omit(XRP_DAILY_RETURN)), 
                                 sd(na.omit(LTC_DAILY_RETURN)), 
                                 sd(na.omit(DASH_DAILY_RETURN)), 
                                 sd(na.omit(ETH_DAILY_RETURN)),
                                 sd(na.omit(PPC_DAILY_RETURN)),
                                 sd(na.omit(XLM_DAILY_RETURN))) 
print(paste0("The lowest standard deviation of the daily returns as a percent rounded to the nearest hundreth is ", round(minimum_daily_return_sd,4) * 100, "%")) #printing the final result with the specific format

#Q2a Worth of Each Portfolio 

Portfolio_1_Price_2016 <- c(BTC.charts$close[BTC.charts$date == "2016-01-06"]) #specifying prices on the date that these cryptos were bought
Portfolio_1_Price_2023 <- c(BTC.charts$close[BTC.charts$date == "2023-01-06"]) #specifying prices on most recent date available
Portfolio_2_Price_2016 <- c(XRP.charts$close[XRP.charts$date == "2016-01-06"], LTC.charts$close[LTC.charts$date == "2016-01-06"])
Portfolio_2_Price_2023 <- c(XRP.charts$close[XRP.charts$date == "2023-01-06"], LTC.charts$close[LTC.charts$date == "2023-01-06"])
Portfolio_3_Price_2016 <- c(ETH.charts$close[ETH.charts$date == "2016-01-06"], DASH.charts$close[DASH.charts$date == "2016-01-06"], PPC.charts$close[PPC.charts$date == "2016-01-06"], XLM.charts$close[XLM.charts$date == "2016-01-06"])
Portfolio_3_Price_2023 <- c(ETH.charts$close[ETH.charts$date == "2023-01-06"], DASH.charts$close[DASH.charts$date == "2023-01-06"], PPC.charts$close[PPC.charts$date == "2023-01-06"], XLM.charts$close[XLM.charts$date == "2023-01-06"])


Portfolio_1 <- data.frame(BTC_FILTERED) #creating portfolios with their specific cryptos (filtered for specific dates)
Portfolio_2 <- data.frame(LTC_FILTERED, XRP_FILTERED)
Portfolio_3 <- data.frame(ETH_FILTERED, DASH_FILTERED, PPC_FILTERED, XLM_FILTERED)


Portfolio_1_Current_Value <- round((5000/Portfolio_1_Price_2016) * Portfolio_1_Price_2023,-3) #calculating the current worth of each portfolio by dividing the amount invest by the price bought and multiplying that by current prices to get the current worth of each portfolio
                           
Portfolio_2_Current_Value <- round(sum((2500/Portfolio_2_Price_2016) * Portfolio_2_Price_2023),-3)

Portfolio_3_Current_Value <- round(sum((1250/Portfolio_3_Price_2016) * Portfolio_3_Price_2023),-3)

Highest_Worth_Portfolio_value <- max(Portfolio_1_Current_Value, Portfolio_2_Current_Value, Portfolio_3_Current_Value) #finding the highest value of the 3 portfolios

print(paste0("Rounded to the nearest thousand dollars the highest valued portfolio is worth $", Highest_Worth_Portfolio_value))

#Q2b
Portfolio_1$value <- (5000/Portfolio_1_Price_2016) * Portfolio_1$close #creating a new column in portfolio 1 that takes the number of shares bought in 2016 and multiples that amount by the closing price to find the portfolio's value day by day
Portfolio_2$value <- (2500/Portfolio_2_Price_2016[2] * Portfolio_2$close) + (2500/Portfolio_2_Price_2016[1] * Portfolio_2$close.1)
Portfolio_3$value <- (1250/Portfolio_3_Price_2016[1] * Portfolio_3$close) + (1250/Portfolio_3_Price_2016[2] * Portfolio_3$close.1) + (1250/Portfolio_3_Price_2016[3] * Portfolio_3$close.2) + (1250/Portfolio_3_Price_2016[4] * Portfolio_3$close.3)


PORTFOLIO_1_FILTERED_PLOT <- ggplot(data=BTC_FILTERED,aes(x=date,y=close,col="BTC")) +  #using ggplot to create a line graph for each portfolio
  geom_line() +
  scale_color_manual(name = "Data", values = c("BTC" = "blue"), labels = c("BTC")) +
  ggtitle('Portfolio 1 Stock Price') + theme(plot.title = element_text(hjust = 0.5))

print(PORTFOLIO_1_FILTERED_PLOT) #printing the plot created above

Portfolio_1_Max_Value <- round(max(Portfolio_1$value),-3) #finding the highest valuation for the portfolio by using max function and rounding to the nearest thousand
print(paste0("The maximum value achieved by Portfolio 1 rounded to the nearest thousand is $",Portfolio_1_Max_Value))

PORTFOLIO_2_FILTERED_PLOT <- ggplot(data=LTC_FILTERED,aes(x=date,y=close,col="LTC")) + 
  geom_line() +
  geom_line(data=XRP_FILTERED,aes(x=date,y=close,col="XRP"))+
  scale_color_manual(name = "Data", values = c("LTC" = "blue", "XRP" = "red"), labels = c("LTC", "XRP")) +
  ggtitle('Portfolio 2 Stock Price') + theme(plot.title = element_text(hjust = 0.5))


print(PORTFOLIO_2_FILTERED_PLOT) #refer to comments above 
Portfolio_2_Max_Value <- round(max(Portfolio_2$value),-3)
print(paste0("The maximum value achieved by Portfolio 2 rounded to the nearest thousand is $",Portfolio_2_Max_Value))

PORTFOLIO_3_FILTERED_PLOT <- ggplot(data=ETH_FILTERED,aes(x=date,y=close,col="ETH")) + 
  geom_line() +
  geom_line(data=DASH_FILTERED,aes(x=date,y=close,col="DASH"))+ 
  geom_line() +
  geom_line(data=PPC_FILTERED,aes(x=date,y=close,col="PPC")) +
  geom_line() +
  geom_line(data=XLM_FILTERED,aes(x=date,y=close,col="XLM")) +
  scale_color_manual(name = "Data", values = c("ETH" = "blue", "DASH" = "red", "PPC" = "purple", "XLM" = "orange"), labels = c("DASH", "ETH", "PPC", "XLM")) +
  ggtitle('Portfolio 2 Stock Price') + theme(plot.title = element_text(hjust = 0.5))
  

print(PORTFOLIO_3_FILTERED_PLOT) #refer to comments above 
Portfolio_3_Max_Value <- round(max(Portfolio_3$value),-3)
print(paste0("The maximum value achieved by Portfolio 3 rounded to the nearest thousand is $",Portfolio_3_Max_Value))


#Q2c

paste0("The highest value achieved by Portfolio 1 was in ", month(Portfolio_1$date[which.max(Portfolio_1$value)], label = T, abbr = F),", ", year(Portfolio_1$date[which.max(Portfolio_1$value)])) #using the dates given and which max function to find the date of the highest value achieved by a portfolio and converting that into month, year with the month/year functions

paste0("The highest value achieved by Portfolio 2 was in ", month(Portfolio_2$date[which.max(Portfolio_2$value)], label = T, abbr = F),", ", year(Portfolio_2$date[which.max(Portfolio_2$value)]))

paste0("The highest value achieved by Portfolio 3 was in ", month(Portfolio_3$date[which.max(Portfolio_3$value)], label = T, abbr = F),", ", year(Portfolio_3$date[which.max(Portfolio_3$value)]))

#Q2d

Portfolio_2_Yesterday_Price <- Portfolio_2$close[1:N-1] + Portfolio_2$close.1[1:N-1] #calculating the daily return for each portfolio. did not do the first portfolio because it consists of only BTC , for which the daily return was calculated above
Portfolio_2_Today_Price <- Portfolio_2$close[2:N] + Portfolio_2$close.1[2:N]

Portfolio_3_Yesterday_Price <- Portfolio_3$close[1:N-1] + Portfolio_3$close.1[1:N-1]+ Portfolio_3$close.2[1:N-1] + Portfolio_3$close.3[1:N-1]
Portfolio_3_Today_Price <- Portfolio_3$close[2:N] + Portfolio_3$close.1[2:N] +  Portfolio_3$close.2[2:N]+  Portfolio_3$close.3[2:N]

Portfolio_2_Daily_Return <- (Portfolio_2_Today_Price - Portfolio_2_Yesterday_Price)/Portfolio_2_Yesterday_Price

Portfolio_3_Daily_Return <- (Portfolio_3_Today_Price - Portfolio_3_Yesterday_Price)/Portfolio_3_Yesterday_Price



Daily_Returns <- data.frame(BTC_DR = na.omit(BTC_DAILY_RETURN), Portfolio_2_DR = Portfolio_2_Daily_Return, Portfolio_3_DR = Portfolio_3_Daily_Return) #adding all daily returns for each portfolio into a dataframe

colnames(Daily_Returns) <- c("BTC_Daily_Return", "Portfolio_2_Daily_Return", "Portfolio_3_Daily_Return") #changing the column names to be more user friendly

ggplot(data = Daily_Returns, aes(x = BTC_Daily_Return, y = Portfolio_2_Daily_Return)) + #using ggplot to create a "point" graph (used to show correlation) between the portfolios
  geom_point() + ggtitle("Bitcoin vs Portfolio 2 Daily Returns")  + theme(plot.title = element_text(hjust = 0.5))


ggplot(data = Daily_Returns, aes(x = BTC_Daily_Return, y = Portfolio_3_Daily_Return)) +
  geom_point() + ggtitle("Bitcoin vs Portfolio 3 Daily Returns")  + theme(plot.title = element_text(hjust = 0.5))



print(paste0("The correlation between Portfolio 2 and 1 rounded to the nearest hundreth is ", round(cor(na.omit(Portfolio_2_Daily_Return), na.omit(BTC_DAILY_RETURN)),2))) #calculating correlation coefficient between the portfolios


print(paste0("The correlation between Portfolio 3 and 1 rounded to the nearest hundreth is ", round(cor(na.omit(Portfolio_3_Daily_Return), na.omit(BTC_DAILY_RETURN)), 2)))

#Q2e

Portfolio_1$ATH_Prior_to_Today <- cummax(Portfolio_1$value) #creating a new column using cummax to find the running maximum of each portfolio by finding the maximum value up to that point in time
Portfolio_1$Drop_ATH_to_Today <-  (Portfolio_1$ATH_Prior_to_Today - Portfolio_1$value)/Portfolio_1$ATH_Prior_to_Today #finding the the worst drop from ATH

print(paste0("The worst drop from the all time high for Portfolio 1 expressed as a percent rounded to the nearest hundreth is ",round(max(Portfolio_1$Drop_ATH_to_Today)*100,2), "%"))


Portfolio_2$ATH_Prior_to_Today <- cummax(Portfolio_2$value)
Portfolio_2$Drop_ATH_to_Today <-  (Portfolio_2$ATH_Prior_to_Today - Portfolio_2$value)/Portfolio_2$ATH_Prior_to_Today

print(paste0("The worst drop from the all time high for Portfolio 2 expressed as a percent rounded to the nearest hundreth is ",round(max(Portfolio_2$Drop_ATH_to_Today)*100,2),"%"))



Portfolio_3$ATH_Prior_to_Today <- cummax(Portfolio_3$value)
Portfolio_3$Drop_ATH_to_Today <-  (Portfolio_3$ATH_Prior_to_Today - Portfolio_3$value)/Portfolio_3$ATH_Prior_to_Today

print(paste0("The worst drop from the all time high for Portfolio 3 expressed as a percent rounded to the nearest hundreth is ",round(max(Portfolio_3$Drop_ATH_to_Today)*100,2),"%"))

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

