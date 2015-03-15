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
  # "dteday"     "season"     "yr"    "mnth"    
  # "holiday"    "weekday"    "workingday"
  # "weathersit" "temp"       "atemp"      "hum"       
  # "windspeed"
  trainingModel <- lm(day[training, 16] ~ .,day[training, 4:13])
  print(summary(trainingModel))
  prediction <- predict(trainingModel,newdata=day[validation, 4:13])
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
  
} # PartE()

PartF <- function()
{
  
} # PartF()