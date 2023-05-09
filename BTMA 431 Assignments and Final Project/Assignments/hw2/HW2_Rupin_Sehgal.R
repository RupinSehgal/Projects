
#Q1
Midterm.Retake.Policy <- function(S,P,B,R){ #function that takes inital attempt, threshold, boost, and retake grade 
  if (S < P) { #using if statements to meet the obligations of the question
    ifelse(R >= P + B,P + B, R) 
  }
  else{
    ifelse(R >= S + B,S + B, R)
  }
}
#Q1.1
Midterm.Retake.Policy(S = 21,P = 30,B = 30,R = 50)
#Q1.2
Midterm.Retake.Policy(S = 21,P = 30,B = 30,R = 60)
#Q1.3
Midterm.Retake.Policy(S = 21,P = 30,B = 30,R = 70)
#Q1.4
Highest.Possible.R.01 <- Midterm.Retake.Policy(S = 21,P = 30,B = 30,R = 1:100)
which.max(Highest.Possible.R.01) #calculating the function from retake scores of 1:100 and finding the maximum value (since from 100 each indexed val = grade re attempted)
#Q1.5
Midterm.Retake.Policy(S = 54,P = 22,B = 30,R = 67)
#Q1.6
Midterm.Retake.Policy(S = 54,P = 22,B = 30,R = 78)
#Q1.7
Midterm.Retake.Policy(S = 54,P = 22,B = 30,R = 89)
#Q1.7
Midterm.Retake.Policy(S = 54,P = 22,B = 30,R = 89)
#Q1.8
Highest.Possible.R.02 <- Midterm.Retake.Policy(S = 54,P = 22,B = 30,R = 1:100)
which.max(Highest.Possible.R.02)
#Q1.9
Highest.Possible.R.03 <- Midterm.Retake.Policy(S = 22,P = 30,B = 30,R = 1:100)
which.max(Highest.Possible.R.03)
#Q1.10
Highest.Possible.R.04 <- Midterm.Retake.Policy(S = 86,P = 30,B = 30,R = 1:150)
which.max(Highest.Possible.R.04)
#Q1.11

MRP_BEST_P_B <- function(Initial_Grades_file_Name) {
  
  avg_pre_retake <- mean(Initial_Grades_file_Name)
  SD_Initial_Grades <- sd(Initial_Grades_file_Name)
  
  storage_sd <- Inf #any standard deviation difference calculated later in the code will be smaller than closest_sd initially, allowing for the first difference to be stored as the closest standard deviation.
  storage_P <- numeric(0) #set as empty vectors to store P and Bs
  storage_B <- numeric(0)
  
  for (P in seq(0, 100, by=2)) {  #forloops for P and B sequencing by 2 to meet the multiples of 2 obligation
    for (B in seq(0, 100, by=2)) {
      if (P + B >= 60) { #only considering P+B >= 60
        Post_Grades <- sapply(Initial_Grades_file_Name, function(S) { #using sapply to apply the function as to our inital grades input vector
          if (S < P) { #similar to part 1 calculating if initial attempt is less than the threshold 
            ifelse(P + B > S, P + B, S)
          } else {
            ifelse(S + B > S, S + B, S)
          }
        })
        Mean_Post_Grades <- mean(Post_Grades) #taking the outputs from the if else and averaging them
        if (Mean_Post_Grades > 70 && Mean_Post_Grades < 75) { #meeting the second condition with an if of the post retake grades
          SD_Post_Grades <- sd(Post_Grades)
          if (abs(SD_Post_Grades - SD_Initial_Grades) < storage_sd) {
            storage_sd <- abs(SD_Post_Grades - SD_Initial_Grades)
            storage_P <- P
            storage_B <- B
          }
        }
      }
    }
  }
  
  return(c(P = storage_P, B = storage_B)) #return p and b
}

#Q1.11-12
load("C:/Users/Rupin/Downloads/exam_scores1.rda") #please load in the rda file with grades or enter any vector of grades into the function below
MRP_BEST_P_B(exam_scores1) #please enter the file path to the file and the file name
#Q1.13-14
load("C:/Users/Rupin/Downloads/exam_scores2.rda")
MRP_BEST_P_B(exam_scores2)
#Q1.15-16
load("C:/Users/Rupin/Downloads/exam_scores3.rda")
MRP_BEST_P_B(exam_scores3)
#Q1.17-18
load("C:/Users/Rupin/Downloads/exam_scores4.rda")
MRP_BEST_P_B(exam_scores4)
#Q1.19-20
load("C:/Users/Rupin/Downloads/exam_scores5.rda")
MRP_BEST_P_B(exam_scores5)



#Q2
N_Student_Selector <- function(N,file.name){
  
 file.chosen <- read.csv(file.name, stringsAsFactors = F)
 N_Student_Selector <- file.chosen[sample(nrow(file.chosen), N, replace = F), ]
  
  return(N_Student_Selector)
}

N_Student_Selector(N = 5, file.name = "C:/Users/Rupin/Downloads/BTMA 431 L01 - (Winter 2023).csv")


#Q3
# function to evaluate students
Student_Evaluator <- function(students_listed) {

  for (i in 1:nrow(students_listed)) {
    # choose a student to evaluate
    n <- nrow(students_listed)
    evaluator_index <- i
    evaluator_name <- paste(students_listed[evaluator_index, "First.Name"], students_listed[evaluator_index, "Last.Name"], sep = " ")
    
    to_be_evaluated_indices <- c((i + 1) %% n + 1, (i + 2) %% n + 1, (i + 3) %% n + 1)     # find the indices of the three students to be evaluated
    to_be_evaluated_names <- paste(students_listed[to_be_evaluated_indices, "First.Name"], students_listed[to_be_evaluated_indices, "Last.Name"], sep = " ") #get their name
    
    evaluators_indices <- c((i - 1 + n) %% nrow(students_listed) + 1, (i - 2 + nrow(students_listed)) %% n + 1, (i - 3 + nrow(students_listed)) %% nrow(students_listed) + 1)    # find the indices of the three students evaluating this student
    evaluators_names <- paste(students_listed[evaluators_indices, "First.Name"], students_listed[evaluators_indices, "Last.Name"], sep = " ")  #finding their name
    
    # print the evaluation
    cat(evaluator_name, "will evaluate", paste(to_be_evaluated_names, collapse = ", "), "\n") #print evaluators and evaluated
  }
}


getwd()
setwd("C:/Users/Rupin/Downloads") #please set your working directory prior to commencing function
Imported_Names <- data.frame(read.csv("BTMA 431 L01 - (Winter 2023).csv"))
Student_Evaluator(Imported_Names)



#Q4
Loss_calculator <- function(number.selected){
  N <- 1000000 
  random.sample <- sample(c(7,77,777), size = N, replace = T)
  loss <- (random.sample-number.selected)^2
  round(mean(loss)/5000)*5000
}
#Q4.1
Loss_calculator(25)
#Q4.2
Loss_calculator(115)
#Q4.3
Loss_calculator(510)
#Q4.4
N <- 1000000 # Number of simulations.
choices <- 7:777 #choices
exp_loss_vec <- numeric(0) #empty vector for storage
for (i in 1:length(choices)) { #for loop for finding the exp_loss for each choice
  random.sample <- sample(c(7,77,777), size = N, replace = T)
  loss <- (random.sample - choices[i])^2
  exp_loss_vec[i] <- mean(loss)
}
lowest_cost_choice <- round(choices[which.min(exp_loss_vec)]/10)*10 #finding lowest cost choice
lowest_cost_choice
#Q4.5
Loss_calculator(lowest_cost_choice)

#4.6
Game_Rand_N <- function(numbers){ #function that takes in numbers as the input
  N <- 1000000 #number of simulations
  random.sample <- sample(numbers, size = N, replace = T) #taking a random sample of the numbers
  exp_loss_vec <- numeric(0)
  for (i in 1:length(numbers)) { #forloop that finds the expected loss
  loss <- (random.sample - numbers[i])^2
  exp_loss_vec[i] <- mean(loss)
} 
lowest_cost_choice <- round(numbers[which.min(exp_loss_vec)]/5)*5 #rounded to the nearest multiple of 5
return(lowest_cost_choice)
}
getwd()
setwd("C:/Users/Rupin/Downloads") #please set your working directory
load("C:/Users/Rupin/Downloads/HW2Q4_v1.rda") #load in wanted dataset
Game_Rand_N(HW2Q4_v1) #use function on loaded dataset or enter a vector of numbers
#q4.7
load("C:/Users/Rupin/Downloads/HW2Q4_v2.rda")
Game_Rand_N(HW2Q4_v2)
#q4.8
load("C:/Users/Rupin/Downloads/HW2Q4_v3.rda")
Game_Rand_N(HW2Q4_v3)
#q4.9
load("C:/Users/Rupin/Downloads/HW2Q4_v4.rda")
Game_Rand_N(HW2Q4_v4)





