library(ggplot2)
library(stats4)

PartA <- function()
{
    day <- read.csv(file="day.csv",header=TRUE,sep=",")
    r <- range(day$temp)
    width <- (r[2] - r[1]) / 20
    p <- ggplot(day)
    p <- p + geom_histogram(aes(temp,y =..density..),binwidth=width) 
    p <- p + xlab("Temperature") + ylab("Frequency") + ggtitle("Daily Temperature Distribution")
    p <- p + theme(title=element_text(size=12),text=element_text(size=12),axis.ticks=element_line(size=2)) 
    print(p)
    ggsave("Problem3A.pdf",width=5,height=7,plot=last_plot())
    return (p)
} # PartA

PartB <- function()
{
  # read data file
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  
  ########## METHOD OF MOMENTS ########## 
  # moment1 = E(X)
  moment1 <- mean(day$temp) 
  
  # moment2 = E(X^2)
  moment2 <- mean((day$temp) ^ 2) 

  # construct vector of mu and sigma
  moments <-c(moment1,sqrt(moment2 - (moment1 ^ 2)))
  
  ########## MAXIMUM LIKELIHOOD ########## 
  
  # define function to be passed to mle
  x <- day$temp
  n <- length(day$temp)
  ll <- function(mu,sigma)
  {
    loglik <- ((-n / 2) * log(2 * pi)) - (n * log(sigma)) - ((1 / (2 * (sigma ^ 2))) * sum((x - mu) ^ 2))
    return (-loglik)
  }
  
  # calc mle
  ests <- mle(minuslogl=ll,start=list(mu=0.5,sigma=0.5))
  
  # get estimates for parameters
  ests2 <- ests@coef
  
  return(cbind(moments,ests2))
} # PartB

PartC <- function()
{
  p <- PartA()
  
  moments <- PartB()
  
  ########## METHOD OF MOMENTS ##########
  p1 <- p + stat_function(fun=dnorm,color="red",args=list(mean=moments[1,1],sd=moments[2,1]))
  print(p1)
  ggsave("Problem3CMoments.pdf",width=5,height=7,plot=last_plot())
  
  ########## MAXIMUM LIKELIHOOD ##########
  p2 <- p + stat_function(fun=dnorm,color="blue",args=list(mean=moments[1,2],sd=moments[2,2]))
  print(p2)
  ggsave("Problem3CMLE.pdf",width=5,height=7,plot=last_plot())
  
  p3 <- p1 + stat_function(fun=dnorm,color="blue",args=list(mean=moments[1,2],sd=moments[2,2]))
  print(p3)
  ggsave("Problem3C.pdf",width=5,height=7,plot=last_plot())
} # PartC
