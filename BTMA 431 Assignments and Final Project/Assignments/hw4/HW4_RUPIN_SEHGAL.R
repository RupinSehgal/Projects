library(ggplot2)
#Q1
getwd() #setting directory, attaching dataset
setwd("C:/Users/Rupin/Downloads")
load("btma.431.736.f2018.v2.rda")
attach(btma.431.736.f2018)
#Q1A
Q1alm <- lm(final.raw.score.excluding.bonus~., data = btma.431.736.f2018) #setting up the linear regression model
summary(Q1alm)
round(Q1alm$coefficients[2],2) #The coef of final project
#Q1B
btma.431.736.f2018$HW.scaled <- HW.average * 5 #scaling by the 100/20
btma.431.736.f2018$Textbook.scaled <- textbook.quiz.average * 6.666667 #same as above but ...
btma.431.736.f2018 <- btma.431.736.f2018[,c(-3,-4)] #removing orignal columns
Q1blm <- lm(final.raw.score.excluding.bonus~., data = btma.431.736.f2018) #new reg
summary(Q1blm) #sumamry to compare coefs
#Q1C
q1c.sum <- summary(Q1alm) #storing 1as reg summary
print(paste0("BANAYES p value: ",round(coef(q1c.sum)[6,4],2)))#P value for BANA
#Q1D
Q1dlm <- lm(final.raw.score.excluding.bonus~.+ post.retake.midterm:BANA, btma.431.736.f2018) #reg wit interaction term
paste0("P value for interaction term: ",round(coef(summary(Q1dlm))[7,4],2))
#Q1E
load("btma.431.736.f2018.v2.rda") #reloading orignal data
btma.431.736.f2018.log <- log(btma.431.736.f2018[,-5]) #removing BANA, since its categorical and cannot be log'd
Q1elm <- lm(final.raw.score.excluding.bonus~., data = btma.431.736.f2018.log) #new reg model
summary(Q1elm)
paste0("Estimate coef value for final project: ",round(coef(summary(Q1elm))[2,1],2))

#Q2A
store <- numeric() #empty vec for storage 
profit <- function(p){ #function that calcs. profit based on p
  quant <- (50 - (5*p))
  Profit <- (quant * p) - (quant * 1)
}

reps <- seq(1,9,0.01) #p range 1 to 9
storage <- as.data.frame(sapply(reps, profit)) #using sapple to apply the function to each point of [1,9]
storage <- cbind(reps,storage) #binding P values with calc'd profit
colnames(storage) <- c("Price","Profit")

qplot(storage$Price,storage$Profit) #plotting the change in profit as price changes.
storage[which.max(storage$Profit),c(1,2)]

#Q2B

profit.2b <- function(m,p){ #function that takes into account m and p when calculating profit
  quant <- (m - (5*p))
  Profit <- (quant * p) - (quant * 1)
  return(Profit)
}
storage.2b.1 <- as.data.frame(mapply(FUN = profit.2b, 45,reps)) #using mapply because of multiple args the function needs with 45 as m and reps as p [1,9]
storage.2b.1 <- as.data.frame(cbind(reps,storage.2b.1)) #binding price and profit
colnames(storage.2b.1) <- c("Price","Profit") #renaming
storage.2b.1[which.max(storage.2b.1$Profit),] #finding row with max profit at price

storage.2b.2 <- as.data.frame(mapply(FUN = profit.2b, 55,reps)) #same as above
storage.2b.2 <- as.data.frame(cbind(reps,storage.2b.2))
colnames(storage.2b.2) <- c("Price","Profit")
storage.2b.2[which.max(storage.2b.2$Profit),]

#Q2C

profit.2c <- function(m,p){ #same function as above
  quant <- (m - (5*p))
  Profit <- (quant * p) - (quant * 1)
  return(Profit)
}

storage.2c <- numeric() #empty vec for storage
for (m in seq(40, 60,0.01)) { #looping m from 40 to 60
  result <- optimize(profit.2c, interval = c(1, 15), m = m, maximum = TRUE) #using optimize to find best profit from p 1,15 for every m
  storage.2c[match(m, seq(40, 60, 0.01))] <- result$maximum #storing the max from the optimize as 1,2,3 depending on the iteration of the for loop seq
}
qplot(x = seq(40, 60,0.01), y = storage.2c,geom = "line") #plot is straight increasing line

#Q2D

profit.2d <- function(m,p,k){ #function that takes m,p,k to calc profit
  quant <- (m - (k*p))
  Profit <- (quant * p) - (quant * 1)
  return(Profit)
}
storage.2d <- numeric() #storage vecs
storage.2d.1 <- numeric()
for(m in c(45,55)){ #for loop for m being 45 once and 55 once
  if(m == 45){ #when m is 45
  for(k in seq(2,8,0.01)){ #same optimization and storage as 2c
    result <- optimize(profit.2d, interval = c(1, 15), m = m, k= k, maximum = TRUE)
    storage.2d[match(k, seq(2,8,0.01))] <- result$maximum
  }}
    else{ #when m is 55
      for(k in seq(2,8,0.01)){ #same optimization and storage as 2c
      result <- optimize(profit.2d, interval = c(1, 15), m = m, k= k, maximum = TRUE)
      storage.2d.1[match(k, seq(2,8,0.01))] <- result$maximum
      }
    }
  
}
storage.2d.F <- as.data.frame(cbind(seq(2,8,0.01),storage.2d,storage.2d.1)) #binding the vectors with the changing k
colnames(storage.2d.F) <- c("k","M.45","M.55") #renaming the columns
ggplot()+
  geom_line(data=storage.2d.F,aes(y=M.45,x= k,colour="M.45"),linewidth=1.25 )+ #plotting the 2 lines according to the link in the pdf of assignment 4
  geom_line(data=storage.2d.F,aes(y=M.55,x= k,colour="M.55"),linewidth=1.25 ) +
  scale_color_manual(name = "Y series", values = c("M.45" = "darkblue", "M.55" = "darkred"))

storage.2d.F[storage.2d.F$M.45 == 5,] #k and m 55 when m 45 is 5

#2E

load("salesData.rda")
salesData <- data.frame(salesData)
Optimal.Price.Func <- function(price,quantity){ #function that calcs optimal price based on time series quant and price data 
Profit <- (price * quantity) - (quantity * 1)
Data.F <- as.data.frame(cbind(price,quantity,Profit))
# Step 3: Build a quadratic regression model to predict profit from price
polyreg.model <- lm(Profit ~ price + I(price^2), data = Data.F) #polynomial reg with price to second degree
polyreg.s <- summary(polyreg.model) #storing the summary
Coef <- coef(polyreg.s)[,1] #getting the coefs
return(round(-Coef[2]/(2*Coef[3]),1)) #returning the optimal price based on the quadratic profit equations first derivative
#-54.462*P^2+376.232P-199.101 This is what is being returned by the function above. Creating the profit polynomial reg equation based on estimate coefficients 
#2*-54.462P+376.232 Taking the first derivation of the profit model based on price and setting it 0. the first derivative allows us to solve for an optimal price
#376.232/(2*54.462) = P #solving for optimal p
}
Optimal.Price.Func(salesData$price,salesData$quantity) #inputting dataset into function

