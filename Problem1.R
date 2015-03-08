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
  cat(CI_2means)
  
  # part B
  
} # end Problem1
