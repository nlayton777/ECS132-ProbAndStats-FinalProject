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
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  nr <- nrow(day)
  
  #training and validation are indexes into our data
  training <- sample(1:nr, ceiling(2*nr/3), replace=FALSE)
  validation <- setdiff(1:nr, training)
  
  # modeling count based on the following attributes
  # from training data
  trainingModel <- lm(day[training, 16] ~ .,day[training, 3:13])
  prediction <- predict(trainingModel,newdata=day[validation, 3:13])
  difference <- day[validation,16] - prediction
  print(summary(trainingModel))
  
  # calculate differences between prediction and actual 
  diff <- c(mean(abs(difference)), min(abs(difference)), max(abs(difference)))
  names(diff) <- c("mean", "min", "max")
  return(diff)  
} # PartB()

PartC <- function()
{
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
  day$mnthandweathersit <- day$mnth * day$weathersit
  day$tempsquared <- day$temp * day$temp
  day$cold <- ifelse(day$temp < 0.25, 1, 0)
  day$hot <- ifelse(day$temp > 0.75, 1, 0)
  
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
                        training$mnthandweathersit +
                        training$tempsquared)
  
  # display summary 
  summary(trainingModel)
} # PartD()

PartEF <- function()
{
  
  # read in the data
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  nr <- nrow(day)
  
  # modify/append to original data set
  day$tempandworkingday <- day$temp * day$workingday
  day$tempsquared <- day$temp * day$temp
  day$spring <- ifelse(day$season == 1,1,0)
  day$summer <- ifelse(day$season == 2,1,0)
  day$fall <- ifelse(day$season == 3,1,0)
  day$raining <- ifelse(day$weathersit == 3 | day$weathersit == 4,1,0)
  
  #training and validation are indexes into our data
  training <- sample(1:nr, ceiling(2*nr/3), replace=FALSE)
  validation <- setdiff(1:nr, training)
  
  # modeling count based on training data
  attrs <- c(4,10,12:13,18:22)
  trainingModel <- lm(day[training, 16] ~ .,day[training, attrs])
  prediction <- predict(trainingModel,newdata=day[validation, attrs])
  difference <- day[validation,16] - prediction
  print(names(day))
  print(summary(trainingModel))
  
  # calculate differences between prediction and actual 
  diff <- c(mean(abs(difference)), min(abs(difference)), max(abs(difference)))
  names(diff) <- c("mean", "min", "max")
  return(diff)
} # PartEandF()

library(freqparcoord)
partG <- function(k){
  # We know all the parameters for this specific function,
  # so we won't pass them in (or require some of the if)
  
  # read in the data
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
  
  # omit attributes that we deem unnecessary
  day <- day[c(3:13, 16)]
  nr <- nrow(day)
  
  print(names(day))
  
  #Modelling on all data, printing plots of the model
  cnt <- smoothz(day, knnreg, 10)
  
  tmp <- day$temp
  season <- day$season

  # create plot of temperature
  p1 <- ggplot(data.frame(tmp,cnt))
  p1 <- p1 + geom_smooth(aes(x=tmp,y=cnt))
  print(p1)
  
  # create plot of season
  p2 <- ggplot(data.frame(season,cnt))
  p2<- p2 + geom_smooth(aes(x=season,y=cnt))
  print(p2)
  
  # training and validation are indexes into our data
  training <- sample(1:nr, ceiling(2*nr/3), replace=FALSE)
  validation <- setdiff(1:nr, training)

  #modelling on training set and testing with prediction set
  model <- smoothz(day[training,], knnreg, 10)
  prediction <- smoothzpred(day[validation,], day[training,], model)

  # calculate differences between prediction and actual 
  difference <- day[validation,12] - prediction
  diff <- c(mean(abs(difference)), min(abs(difference)), max(abs(difference)))
  names(diff) <- c("mean", "min", "max")
  return(diff)  
} # PartG

