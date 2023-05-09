library(ggplot2)
library(dplyr)

#Q1
Profit_Calc <- function(demand,supply){ #function that finds profit given demand and supply
  demand_for_supply <- pmin(demand,supply) #finding the lowest possible point for revenue
  revenue <- 2.5 * demand_for_supply #
  cost <- 0.2 * supply
  profit <- revenue - cost
  print(profit)
}
#1a
Profit_Calc(120,100)
#1b
Profit_Calc(80,100)
#1C Since stocking level is the only thing that is in our control (only thing we can change the levels of manually) that becomes our lever to maximize profits (demand is out of our control)
#Q1D
nsims <- 1000000
demand.muffin <- rnorm(nsims, mean = 120, sd = 25) #generating random normal demand w given mu and sd
supply.muffin.max <- 1:500 #range for supply
muffin.storage <- vector(mode = "numeric", length = length(supply.muffin.max)) #empty vec
for (i in 1:length(supply.muffin.max)) { #loop that changes supply level (simulation) to find highest average profit from the number of sims (nsims)
  supply_level <- supply.muffin.max[i]
  demand_for_supply <- pmin(demand.muffin, supply_level)
  revenue <- 2.5 * demand_for_supply
  cost <- 0.2 * supply_level
  muffin.storage[i] <- mean(revenue - cost) #storing in empty vec
}
optimal_stock <- supply.muffin.max[which.max(muffin.storage)] #finding the supply level associated with highest profit
print(optimal_stock)
qplot(supply.muffin.max,muffin.storage, geom = 'line') #creating line graph to show profit changes with supply level

#1E
supply_level <- 155 #setting our supply level as the optimal
round(sum(demand.muffin <= 155)/nsims,2) #averaging the amount of times demand was less than our 155, meaning that we meet demand during this % of times
#1F
Demand.M <- as.data.frame(demand.muffin) #turning demand muffin into df so we can filter
Demand.M.Filtered <- Demand.M  %>% filter(demand.muffin > 155)  #conditional on running out
Demand.M.Filtered$diff <- Demand.M.Filtered$demand.muffin - 155 
round(mean(Demand.M.Filtered$diff),2)

#1G
Demand.M.2 <- as.data.frame(demand.muffin)
Demand.M.2.Filtered <- Demand.M.2  %>% filter(demand.muffin < 155)  
Demand.M.2.Filtered$diff <- Demand.M.2.Filtered$demand.muffin - 155
round(-mean(Demand.M.2.Filtered$diff))


#1H
set.seed(1)
nsims <- 1000000
mu <- 120 #mean
c <- 0.2 #cost to produce
p <- 2.5 #revenue
sds <- seq(0, 30, 5) #changes in variation
profits <- rep(0, length(sds)) #different profits at those sds
opt_stock <- rep(0, length(sds)) #optimal stock at those sds
for (i in 1:length(sds)) { #for loop to calculate highest average profit and corresponding supply level
  sd <- sds[i] #changing sd from 0 to 30 by 5
  demand <- rnorm(nsims, mean = mu, sd = sd) #random normal demand simulated
  supply <- 1:500 #change in supply 
  demand_for_supply <- pmin(demand, supply) 
  revenue <- p * demand_for_supply
  cost <- c * supply
  profit <- revenue - cost
  profits[i] <- mean(profit) #average profits at sd = i
  opt_stock[i] <-  demand_for_supply[which.max(profit)] #optimal supply level

}
sd.change.data <- data.frame(sd = sds, profit = profits, stock = opt_stock) #creating df for plots
print(sd.change.data) #profit and supply level at various sds, plotted this data
ggplot(sd.change.data, aes(x = sd)) +
  geom_line(aes(y = profit)) +
  geom_point(aes(y = profit)) +
  labs(x = "Standard deviation", y = "Expected profit")

#Q1I
supply.muffin.max <- 1:500 #range for supply
muffin.storage <- vector(mode = "numeric", length = length(supply.muffin.max)) #empty vec
nsims <- 1000000
demand.muffin <- rnorm(nsims, mean = 120, sd = 25) #generating random normal demand w given mu and sd
for (i in 1:length(supply.muffin.max)) { #same as above basically just set it starting at 100
  supply_level <- supply.muffin.max[i] + 100 #adding a 100 from 0 to simulate the gift of free 100 muffin supply
  demand_for_supply <- pmin(demand.muffin, supply_level)
  revenue <- 2.5 * demand_for_supply
  cost <- 0.2 * supply_level
  muffin.storage[i] <- mean(revenue - cost)
}
optimal_stock <- supply.muffin.max[which.max(muffin.storage)]
print(optimal_stock)
qplot(supply.muffin.max,muffin.storage, geom = 'line')

#Q2
url <- "https://s3.amazonaws.com/tripdata/202212-citibike-tripdata.csv.zip"
temp <- tempfile()
download.file(url, temp)
citibike <- read.csv(unz(temp, "202212-citibike-tripdata.csv"),
                     stringsAsFactors = FALSE) #data import
unlink(temp)
citibike.trips <- citibike %>% filter(member_casual == "member" | member_casual == "casual")
citibike.trips$time.spent <- as.numeric(difftime(citibike.trips$ended_at, citibike.trips$started_at)) #taking the time difference between end and start times 
citibike.trips <- citibike.trips %>% filter(time.spent <= (24*60*60) & time.spent >= 0) #filtering data as specified
citibike.trips$payment.owed <- ifelse(citibike.trips$member_casual == "member",ceiling((citibike.trips$time.spent)-(45*60))*(.17/60),ceiling((citibike.trips$time.spent)-(30*60))*(.26/60)) #calcualting owed amount based on member or casual
citibike.trips <- citibike.trips %>% filter(payment.owed > 0) #filtering for only positive payments owed
#q2a
round(ceiling(mean(citibike.trips$payment.owed)*100)/100,1)
#Q2B
round(ceiling(sd(citibike.trips$payment.owed)*100)/100,1)
#Q2C
citibike.member <- citibike.trips %>% filter(member_casual == 'member') #filtering for member or casual
citibike.casual <- citibike.trips %>% filter(member_casual == 'casual')
round(sum(citibike.casual$payment.owed),-3) #casual payment owed total
round(sum(citibike.member$payment.owed),-3) #member payment owed total



#Q2D

citibike.trips$time.spent.permin <- citibike.trips$time.spent/60 #changing the time to be per min

citibike.trips$q2d <- ifelse(citibike.trips$member_casual == "member",ceiling((citibike.trips$time.spent.permin-45))*(.17),ceiling((citibike.trips$time.spent.permin-30))*(.26)) #repeating same ifelse as above with slight changes to account for the per min cahnge
citibike.trips <- citibike.trips %>% filter(q2d > 0) #filtering for above 0 dollars

round(mean(citibike.trips$q2d), 1) #AVERAGE EXPECTED PAYMENT OWED
#Q2E
round(sd(citibike.trips$q2d), 1) #sd of expected payment
#Q2F/H
citibike.member.permin <- citibike.trips %>% filter(member_casual == 'member')
citibike.casual.permin <- citibike.trips %>% filter(member_casual == 'casual')

round(mean(citibike.member.permin$q2d),1) #owed by member/casual under the per min policy

round(mean(citibike.casual.permin$q2d),1)

#Q2G
round(sum(citibike.member$payment.owed),-3) + #payment totals for per min and continous (stick with current)
round(sum(citibike.casual$payment.owed),-3)


round(sum(citibike.member.permin$q2d),-3) +
  round(sum(citibike.casual.permin$q2d),-3)

#3a as the hint says, the marginal is a constant and is the same as marginal cost === $0.2

#3b The marginal revenue of stocking one more muffin is 2.5*P(D>S*) because when demand is greater than the optimal stocking level is when we will see the increase in revenue that results from the sale of another muffin .

#Q3c
mu <- 120  # mean and sd of demand
sigma <- 25

EMR <- function(S){ #function for P(D>S) and multiplying by 2.5 to find marginal rev
  2.5 * (1 - pnorm(S,120,25))
  }

optimal_stock <- optimize(function(J) abs(EMR(J) - 0.2), lower = 0, upper = 500)$minimum #using the optimze function to find the best supply level (where MR = MC). using $min (since we used abs to make all profits positive) because we are looking for a difference of 0 between marginal cost and revenue since at the optimal levels they are the same
optimal_stock <- round(optimal_stock, 0) 
optimal_stock #same as simulation



#4
simulate_raffle_draws <- function(total_tickets, my_tickets, weeks) {
  n_draws <- weeks * 7 * 24 * 60 * 60 / 3 # number of draws in the given number of weeks
  draws <- sample.int(total_tickets, size = n_draws, replace = TRUE) # simulate raffle draws
  wins <- length(which(draws %in% 1:my_tickets)) # count the number of times we won
  zero_wins <- ifelse(wins == 0, 1, 0) # check if we won zero raffle draws
  at_least_one_win <- ifelse(wins > 0, 1, 0) # check if we won at least one raffle draw
  return(c(zero_wins, at_least_one_win))
}


#4a
set.seed(1) # for reproducibility
total_tickets <- 20e9 # total number of raffle tickets
my_tickets <- 1e4 # number of raffle tickets owned by a person
weeks <- 1 

simulations <- replicate(10000, simulate_raffle_draws(total_tickets, my_tickets, weeks)) # simulate 10,000 times
zero_wins <- mean(simulations[1,]) # estimate probability of winning zero raffle draws
at_least_one_win <- mean(simulations[2,]) # estimate probability of winning at least one raffle draw
print(round(zero_wins,2)) #prob of winning 0 raffles
print(round(at_least_one_win,2)) #prob of winning atleast 1



#4B
draws <- 1 * 7 * 24 * 60 * 60 / 3  #  number of raffle draws in a week

poisson_prob_at_least_one <- function(n_tickets) {
  ppois(0, n_tickets/20e9*draws, lower.tail = FALSE) #using poisson distribution to calc the prob observing value greater than 0
}

# Loop to find the number of tickets needed for an 80% chance of winning at least one raffle draw
n_tickets <- 1
while (poisson_prob_at_least_one(n_tickets) < 0.8) {
  n_tickets <- n_tickets + 1
}

round(n_tickets,-3) 

#4C
draws.per.year <- (60/3)*60*24*365 
num_tickets <- 10000
total_tickets <- 20000000000
prob_win <- num_tickets / total_tickets
winnings_per_round <- 250
expected_winnings_per_round <- prob_win * winnings_per_round
expected_winnings_per_year <- expected_winnings_per_round * draws.per.year # multiplying the expected winnings per round by the total number of rounds in a year
round(expected_winnings_per_year, -2) # rounding to the nearest hundred dollars




#4D
total_tickets <- 20000000000
winnings_per_round <- 250
num_rounds_per_year <- 10512000

calc_expected_reward <- function(num_tickets) { #function that finds the expected reward given number of tickets owned
  prob_win <- num_tickets / total_tickets 
  expected_winnings_per_round <- prob_win * winnings_per_round
  expected_winnings_per_year <- expected_winnings_per_round * num_rounds_per_year
  return(expected_winnings_per_year)
}

num_tickets_vec <- seq(100, 10000000, by = 100) #seq of tickets
expected_reward_vec <- sapply(num_tickets_vec, calc_expected_reward )#applying seq to function

ggplot(data = data.frame(num_tickets = num_tickets_vec, expected_reward = expected_reward_vec), aes(x = num_tickets, y = expected_reward)) +  #plot
  geom_line() +
  scale_x_log10() +
  labs(x = "Number of raffle tickets", y = "Expected yearly reward amount ($)", title = "Expected yearly reward amount vs. number of raffle tickets")


expected_reward_friend1 <- calc_expected_reward(100000)
expected_reward_friend2 <- calc_expected_reward(1000000)

round(expected_reward_friend1, -3)
round(expected_reward_friend2, -3)



