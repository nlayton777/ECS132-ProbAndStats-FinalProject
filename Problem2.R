PartA <- function()
{
    day <- read.csv(file="day.csv",header=TRUE,sep=",")
    summary(lm(day$cnt ~ day$season + day$yr + day$mnth + day$holiday + day$weekday + day$workingday + day$weathersit + day$temp + day$atemp + day$hum + day$windspeed))
} # PartA()

PartB <- function()
{
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  n1 <- ceiling(2 * nrow(day) / 3)
  n2 <- ceiling(nrow(day) / 3)
  training <- day[sample(n1, replace=FALSE),]
  validation <- day[sample(n2, replace=FALSE),]
  
  trainingModel <- lm(training$cnt ~ training$season + training$yr + training$mnth + training$holiday + training$weekday + training$workingday + training$weathersit + training$temp + training$atemp + training$hum + training$windspeed)
  summary(trainingModel)
  
  
  predict(trainingModel,validation)
} # PartB()

PartC <- function()
{
  # similar to above 
} # PartC()

PartD <- function() 
{ #Still need to fix 
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  iTermWorkdayWeatherSit <- day$workingday * day$weathersit
  summary(lm(day$cnt ~ day$season + day$yr + day$mnth + day$holiday + day$weekday + day$workingday + day$weathersit + day$temp + day$atemp + day$hum + day$windspeed+iTermWorkdayWeatherSit))
} # PartD()

PartE <- function()
{
  
} # PartE()

PartF <- function()
{
  
} # PartF()