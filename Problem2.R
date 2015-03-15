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
  nr <- nrow(day)
  #training and validation are indexes into our data
  training <- sample(1:nr, ceiling(2*nr/3), replace=FALSE)
  validation <- setdiff(1:nr, training)
  
  #modelling count based on 
  # "season"     "yr"    "mnth"    
  # "holiday"    "weekday"    "workingday"
  # "weathersit" "temp"       "atemp"      "hum"       
  # "windspeed"
  trainingModel <- lm(day[training, 16] ~ .,day[training, 3:13])
  print(summary(trainingModel))
  prediction <- predict(trainingModel,newdata=day[validation, 3:13])
  difference <- day[validation,16] - prediction
  
  diff <- c(mean(abs(difference)), min(abs(difference)), max(abs(difference)))
  names(diff) <- c("mean", "min", "max")
  return(diff)  

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
  
  # display summary 
  summary(trainingModel)
} # PartD()

PartE <- function()
{
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  nr <- nrow(day)
  #training and validation are indexes into our data
  training <- sample(1:nr, ceiling(2*nr/3), replace=FALSE)
  validation <- setdiff(1:nr, training)
  
  #modelling count based on 
  # "season"     "yr"    "mnth"    
    # "holiday"        "workingday"
  # "weathersit" "temp"       "atemp"      "hum"       
  # "windspeed"
  trainingModel <- lm(day[training, 16] ~ .,day[training, c(3:6, 8:13)])
  print(summary(trainingModel))
  prediction <- predict(trainingModel,newdata=day[validation, c(3:6, 8:13)])
  difference <- day[validation,16] - prediction
  
  diff <- c(mean(abs(difference)), min(abs(difference)), max(abs(difference)))
  names(diff) <- c("mean", "min", "max")
  return(diff)  
} # PartE()

PartF <- function()
{
  # read in the data
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
} # PartF()
