Problem1 <- function()
{
  # import daily data
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  month <- 3
  
  ############################### PART A ###############################
  
  # obtain vectors of the cnt values for which 
  # the month is 3 in the years 2011 and 2012
  monthYear0 <- day$cnt[(day$yr == 0) & (day$mnth == month)]
  monthYear1 <- day$cnt[(day$yr == 1) & (day$mnth == month)]
  
  # calculate the sample mean cnt for 
  # March 2011 and March 2012 from the above vectors
  X_bar <- mean(monthYear0)
  Y_bar <- mean(monthYear1)
  
  # calculate the difference between the sample mean cnt values
  Difference <- X_bar - Y_bar
  
  # calculate the sample standard deviation 
  # for cnt in March 2011 and March 2012
  s1 <- sd(monthYear0)
  s2 <- sd(monthYear1)
  
  # calculate the standard error
  SE <- sqrt(((s1 ^2) / length(s1)) + ((s2 ^ 2) / length(s2)))
  
  # construct the confidence interval
  CI_2means <- c(Difference - 1.96 * SE, Difference + 1.96 * SE)
  
  # display results
  cat("Confidence Interval for Difference between 2 population means\n")
  cat("of bike rentals in March from 2011 to 2012:\n")
  cat(CI_2means)
  cat("\n\n")
  
  ############################### PART B ###############################
  
  # obtain vectors of values for which the daily temperatures in the 
  # months of March 2011 and March 2012 were above 0.3 
  tempMonthYear0 <- day$cnt[(day$yr == 0) & (day$mnth == month) & (day$temp >.3)]
  tempMonthYear1 <- day$cnt[(day$yr == 1) & (day$mnth == month) & (day$temp > .3)]
  
  # obtain total number of recordings taken 
  # in the month of March 2011 and March 2012
  n0 <- length(monthYear0)
  n1 <- length(monthYear1)
  
  # calculate the proportion of days in the months of 
  # March 2011 and March 2012 for which 
  # the temperature was reater than 0.3
  p_hat0 <- length(tempMonthYear0) / n0
  p_hat1 <- length(tempMonthYear1) / n1
  
  # calculate the difference between the proportions
  Difference2 <- p_hat0 - p_hat1
  
  # calculate the standard error
  SE2 <- sqrt((p_hat0 * (1 - p_hat0) / n0) + (p_hat1 * (1 - p_hat1) / n1))
  
  # construct the confidence interval
  CI_2props <- c(Difference2 - 1.96 *SE2, Difference2 + 1.96 * SE2)
  
  # display results
  cat("Confidence Interval for difference between 2 population proportions\n")
  cat("of days in which the temperature in March \n")
  cat("was greater than .3 from 2011 to 2012:\n")
  cat(CI_2props)
  cat("\n")
} # end Problem1
