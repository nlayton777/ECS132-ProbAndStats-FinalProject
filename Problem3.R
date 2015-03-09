library(ggplot2)

PartA <- function()
{
    day <- read.csv(file="day.csv",header=TRUE,sep=",")
    p <- ggplot(day) + geom_histogram(aes(temp)) 
    p <- p + labs(title="Daily Temperate Frequency", x="Temperature",y="Frequency") 
    p <- p + theme(title=element_text(size=3),text=element_text(size=3),axis.ticks=element_line(size=1)) 
    print(p)
    ggsave("Problem3A.pdf",width=1,height=1.5,plot=last_plot())
    dev.off()
} # PartA

PartB <- function()
{
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
} # PartB

PartC <- function()
{
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
} # PartC
