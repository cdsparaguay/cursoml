## libs
library(stringr)
library(ggplot2)
library(dplyr)

## stock prices
stocks = read.csv(file = 'data/stocks/stockprices.csv', header = T, stringsAsFactors = F)
stocks$date = as.Date(stocks$date, format = "%Y-%m-%d")
str(stocks)

## tendencia central y dispersión sobre nasdaq
summary(stocks$nasdaq)
var(stocks$nasdaq)
sd(stocks$nasdaq)

## correlaciones entre stocks y nasdaq

## low r
cor(stocks$aaba, stocks$nasdaq)
cor(stocks$aal, stocks$nasdaq)
cor(stocks$bidu, stocks$nasdaq)
cor(stocks$intc, stocks$nasdaq)

## low positive r
cor(stocks$fox, stocks$nasdaq)
cor(stocks$fb, stocks$nasdaq)

## positive r
cor(stocks$aapl, stocks$nasdaq)
cor(stocks$adbe, stocks$nasdaq)
cor(stocks$atvi, stocks$nasdaq)
cor(stocks$msft, stocks$nasdaq)
cor(stocks$nflx, stocks$nasdaq)

## high positive r
cor(stocks$amzn, stocks$nasdaq)
cor(stocks$fox, stocks$foxa)

## correlation matrix
corMatrix = cor(stocks[2:31])
corM05 = subset(corMatrix, corMatrix[, "nasdaq"] >= 0.5 & corMatrix[, "nasdaq"] < 1)
corM05[,"nasdaq"]

## scatter plots 
plot(x = stocks$goog, y = stocks$nasdaq)
plot(x = stocks$khc, y = stocks$nasdaq)

## linear model - regresión lineal
m1 = lm(nasdaq ~ amzn, data = stocks)
m2 = lm(nasdaq ~ goog, data = stocks)
m3 = lm(nasdaq ~ amzn + goog, data = stocks)
summary(m1)
summary(m2)
summary(m3)

summary(lm(nasdaq ~ amzn + goog + aapl, data = stocks))
summary(lm(nasdaq ~ khc, data = stocks))

## Principal Component Analysis (PCA)
pcaStocks = prcomp(stocks[2:30], scale = TRUE)
screeplot(pcaStocks)
summary(pcaStocks)

## 90% cumulative proportion of variance con PC1-5
trainUnificado = head(cbind(pcaStocks$x[,1:5], stocks[31]), 188)
m = lm(nasdaq ~ PC1 + PC2 + PC3 + PC4 + PC5, data = trainUnificado)
summary(m)
matplot(cbind(trainUnificado$nasdaq,m$fitted.values),type="l",col=c("red","green"),lty=c(1,1))

## validation
validationUnificado = tail(cbind(pcaStocks$x[,1:5], stocks[31]),62)
p = predict(m, validationUnificado, se.fit = TRUE)
matplot(cbind(validationUnificado$nasdaq,p$fit),type="l",col=c("red","green"),lty=c(1,1))

