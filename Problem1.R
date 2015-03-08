Problem1 <- function()
{
  # part A
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  Jan2011 <- day$cnt[(day$yr == 0) & (day$mnth == 1)]
  Jan2012 <- day$cnt[(day$yr == 1) & (day$mnth == 1)]
  X_bar <- mean(Jan2011)
  Y_bar <- mean(Jan2012)
  Difference <- X_bar - Y_bar
  s1 <- sd(Jan2011)
  s2 <- sd(Jan2012)
  SE <- sqrt(((s1 ^2) / length(s1)) + ((s2 ^ 2) / length(s2)))
  CI_2means <- c(Difference - 1.96 * SE, Difference + 1.96 * SE)
  return (c(X_bar,Y_bar,Difference,s1,s2,SE,CI_2means))
} # end Problem1
