library(ggplot2)

PartA <- function()
{
    day <- read.csv(file="day.csv",header=TRUE,sep=",")
    p <- ggplot(day)
    p <- p + geom_histogram(aes(temp)) 
    #p <- p + labs(title="Daily Temperature Frequency", x="Temperature",y="Frequency")
    p <- p + xlab("Temperature") + ylab("Frequency") + ggtitle("Daily Temperature Distribution")
    p <- p + theme(title=element_text(size=12),text=element_text(size=12),axis.ticks=element_line(size=2)) 
    print(p)
    ggsave("Problem3A.pdf",width=5,height=7,plot=last_plot())
    #dev.off()
} # PartA

PartB <- function()
{
  day <- read.csv(file="day.csv",header=TRUE,sep=",")
} # PartB

PartC <- function()
{
  day <- read.csv(file="day.csv",header=TRUE,sep=",")

} # PartC
