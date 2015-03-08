Problem1 <- function()
{
  # part A
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  month <- 3
  monthYear0 <- day$cnt[(day$yr == 0) & (day$mnth == month)]
  monthYear1 <- day$cnt[(day$yr == 1) & (day$mnth == month)]
  X_bar <- mean(monthYear0)
  Y_bar <- mean(monthYear1)
  Difference <- X_bar - Y_bar
  s1 <- sd(monthYear0)
  s2 <- sd(monthYear1)
  SE <- sqrt(((s1 ^2) / length(s1)) + ((s2 ^ 2) / length(s2)))
  CI_2means <- c(Difference - 1.96 * SE, Difference + 1.96 * SE)
  # cat(c(month,X_bar,Y_bar,Difference,s1,s2,SE,CI_2means))
  
  cat("Confidence Interval for Difference between 2 population means of bike rentals in March from 2011 to 2012")
  cat("\n")
  cat(CI_2means)
  cat("\n")
  
  # part B
  tempMonthYear0 <- day$cnt[(day$yr == 0) & (day$mnth == month) & (day$temp >.3)]
  tempMonthYear1 <- day$cnt[(day$yr == 1) & (day$mnth == month) & (day$temp > .3)]
 
  n0<-length(monthYear0)
  n1<-length(monthYear1)
  
  p_hat0<-length(tempMonthYear0)/n0
  p_hat1<-length(tempMonthYear1)/n1
  
  Difference2<-p_hat0-p_hat1
  
  SE2<-sqrt((p_hat0*(1-p_hat0)/n0)+(p_hat1*(1-p_hat1)/n1))
  CI_2props<-c(Difference2-1.96*SE2,Difference2+1.96*SE2)
  
  cat("Confidence Interval for Difference between 2 population proportions of days in which \n the temperature in March was greater than .3 from 2011 to 2012")
  cat("\n")
  cat(CI_2props)
  cat("\n")
  
  
} # end Problem1
