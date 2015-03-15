PartA <- function()
{
    day <- read.csv(file="day.csv",header=TRUE,sep=",")
    summary(lm(day$cnt ~ day$season + day$yr + day$mnth + day$holiday + day$weekday + day$workingday + day$weathersit + day$temp + day$atemp + day$hum + day$windspeed))
} # PartA()

PartB <- function()
{
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
  
} # PartC()

PartD <- function()
{
  
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
  
} # PartF()