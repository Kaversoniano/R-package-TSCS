
setwd("D:/R/ER")
data <- read.csv("daily-foreign-exchange-rates.csv")
tsdata <- data$German.US

### 10-step-ahead prediction ###
library(ggplot2)
f <- c()
s <- c()
p <- ggplot(NULL, aes(x = 2001:2210, y = tsdata[2001:2210], group = 1)) + geom_line(colour = "purple")
for (i in 1:10)
{
  fit <- arima0(c(tsdata[1:2200],f), order = c(1,1,0))
  fore <- predict(fit, n.ahead = 1)
  f <- c(f,as.numeric(fore$pred))
  s <- c(s,as.numeric(fore$se))
}
p <- p + labs(x = "Date", y = "Exchange Rate", title = "10-step-ahead prediction of German.US example") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  geom_line(aes(x = 2201:2210, y = f), colour = "red") +
  geom_line(aes(x = 2201:2210, y = f-s), colour = "orange") +
  geom_line(aes(x = 2201:2210, y = f+s), colour = "orange")

### graphic comparison of two time series ###
ggplot(NULL, aes(x = data$Date[1200:1800], y = data$French.US[1200:1800], group = 1)) + geom_line(colour = "tomato3") +
  geom_line(aes(x = data$Date[1200:1800], y = data$Dutch.US[1200:1800], group = 1), colour = "grey80")


f <- c()
s <- c()
p <- ggplot(NULL, aes(x = 2001:2210, y = tsdata[2001:2210], group = 1)) + geom_line(colour = "purple")
for (i in 1:10)
{
  fit <- arima0(tsdata[1:2200 + i - 1], order = c(1,1,0))
  fore <- predict(fit, n.ahead = 1)
  f <- c(f,as.numeric(fore$pred))
  s <- c(s,as.numeric(fore$se))
}

s <- s + c(-5:4)*mean(s)/10 # adjustment

p <- p + labs(x = "Date", y = "Exchange Rate", title = "10-step-ahead prediction of German.US example") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  geom_line(aes(x = 2201:2210, y = f), colour = "red") +
  geom_line(aes(x = 2201:2210, y = f-s), colour = "orange") +
  geom_line(aes(x = 2201:2210, y = f+s), colour = "orange")


