PartA <- function()
{
    day <- read.csv(file="day.csv",header=TRUE,sep=",")
    #summary(lm(day$cnt ~ day$dteday + day$season + day$yr + day$mnth + day$holiday + day$weekday + day$workingday + day$weathersit + day$temp + day$atemp + day$hum + day$windspeed + day$casual + day$registered))
    summary(lm(day$cnt ~ day$season + day$yr + day$mnth + day$holiday + day$weekday + day$workingday + day$weathersit + day$temp + day$atemp + day$hum + day$windspeed))
} # end Problem2
