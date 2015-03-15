PartA <- function()
{
    # read in the data
    day <- read.csv(file="day.csv",header=TRUE,sep=",")
    
    # fit the model with all predictors possible
    mod <- lm(day$cnt ~ day$season + day$yr + day$mnth + 
               day$holiday + day$weekday + day$workingday + 
               day$weathersit + day$temp + day$atemp + 
               day$hum + day$windspeed)
    
    # display a summary of the model
    summary(mod)
} # PartA()

PartB <- function()
{
  # read in the data
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  
  # get 2/3 of original data set for training set
  n1 <- ceiling(2 * nrow(day) / 3)
  training <- day[sample(n1, replace=FALSE),]
  
  # get 1/3 of original data set for validation set
  n2 <- nrow(day) - n1
  validation <- day[sample(n2, replace=FALSE),]
  validation <- cbind(validation['season'],
                      validation['yr'],validation['mnth'],
                      validation['holiday'],validation['weekday'],
                      validation['workingday'],validation['weathersit'],
                      validation['temp'],validation['atemp'],
                      validation['hum'],validation['windspeed'])
  
  # model the training set
  trainingModel <- lm(training$cnt ~ training$season + 
                        training$yr + training$mnth + 
                        training$holiday + training$weekday + 
                        training$workingday + training$weathersit + 
                        training$temp + training$atemp + 
                        training$hum + training$windspeed)
  
  # display a summary of the model
  summary(trainingModel)

  # predict values based on training model
  predictions <- predict(trainingModel,validation)
  return (predictions)

} # PartB()

PartC <- function()
{
  # read in the data
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  
  # get 2/3 of original data set for training set
  n1 <- ceiling(2 * nrow(day) / 3)
  training <- day[sample(n1, replace=FALSE),]
  
  # model the training set
  trainingModel <- lm(training$cnt ~ training$season + 
                        training$yr + training$mnth + 
                        training$holiday + training$weekday + 
                        training$workingday + training$weathersit + 
                        training$temp + training$atemp + 
                        training$hum + training$windspeed)
  
  # show a summary of model
  summary(trainingModel)

} # PartC()

PartD <- function()
{
  # read in the data
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  day$tempandworkingday <- day$temp * day$workingday
  #day$windspeedandworkingday <- day$windspeed * day$workingday
  day$mnthandweathersit <- day$mnth * day$weathersit
  day$tempsquared <- day$temp * day$temp
  
  # get 2/3 of original data set for training set
  n1 <- ceiling(2 * nrow(day) / 3)
  training <- day[sample(n1, replace=FALSE),]
  
  # model the training set
  trainingModel <- lm(training$cnt ~ training$season + 
                        training$yr + training$mnth + 
                        training$holiday + training$weekday + 
                        training$workingday + training$weathersit + 
                        training$temp + training$atemp + 
                        training$hum + training$windspeed +
                        training$tempandworkingday + 
                        #training$windspeedandworkingday +
                        training$mnthandweathersit +
                        training$tempsquared)
  summary(trainingModel)
} # PartD()

PartE <- function()
{
  # read in the data
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
} # PartE()

PartF <- function()
{
  # read in the data
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
} # PartF()
