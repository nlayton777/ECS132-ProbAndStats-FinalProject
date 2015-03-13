PartA <- function()
{
    day <- read.csv(file="day.csv",header=TRUE,sep=",")
    summary(lm(day$cnt ~ day$season + day$yr + day$mnth + day$holiday + day$weekday + day$workingday + day$weathersit + day$temp + day$atemp + day$hum + day$windspeed))
} # PartA()

PartB <- function()
{
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  n <- ceiling(2 * nrow(day) / 3)
  training <- day[sample(n, replace=FALSE),]
  validation <- setdiff(day,training)
  
  #summary(lm(training$cnt ~ training$season + training$yr + training$mnth + training$holiday + training$weekday + training$workingday + training$weathersit + training$temp + training$atemp + training$hum + training$windspeed))
  trainingModel <- lm(training$cnt ~ training$season + training$yr + training$mnth + training$holiday + training$weekday + training$workingday + training$weathersit + training$temp + training$atemp + training$hum + training$windspeed)
  summary(predict(trainingModel,validation))
}
