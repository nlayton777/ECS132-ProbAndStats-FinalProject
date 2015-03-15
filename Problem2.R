PartA <- function()
{
    day <- read.csv(file="day.csv",header=TRUE,sep=",")
    summary(lm(day$cnt ~ day$season + day$yr + day$mnth + day$holiday + day$weekday + day$workingday + day$weathersit + day$temp + day$atemp + day$hum + day$windspeed))
} # PartA()

PartB <- function()
{
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  
  # get 2/3 of original data set for training set
  n1 <- ceiling(2 * nrow(day) / 3)
  training <- day[sample(n1, replace=FALSE),]
  
  # get 1/3 of original data set for validation set
  n2 <- nrow(day) - n1
  validation <- day[sample(n2, replace=FALSE),]
  validation <- cbind(validation['season'],validation['yr'],validation['mnth'],validation['holiday'],validation['weekday'],validation['workingday'],validation['weathersit'],validation['temp'],validation['atemp'],validation['hum'],validation['windspeed'])
  
  # model the training set
  trainingModel <- lm(training$cnt ~ training$season + training$yr + training$mnth + training$holiday + training$weekday + training$workingday + training$weathersit + training$temp + training$atemp + training$hum + training$windspeed)
  summary(trainingModel)

  predictions <- predict(trainingModel,validation)
  return (predictions)

} # PartB()

PartC <- function()
{

  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  
  # get 2/3 of original data set for training set
  n1 <- ceiling(2 * nrow(day) / 3)
  training <- day[sample(n1, replace=FALSE),]
  trainingModel <- lm(training$cnt ~ training$season + training$yr + training$mnth + training$holiday + training$weekday + training$workingday + training$weathersit + training$temp + training$atemp + training$hum + training$windspeed)
  summary(trainingModel)

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
