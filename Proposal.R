credit.df <- read.csv("AER_credit_card_data.csv")

library(gplots)
library(MASS)
library(dplyr)
library(ggplot2)
library(tidyverse)

summary(credit.df)

## Correlation maxtrix
# binary for card, owner, selfemp
dummies <- data.frame(model.matrix(~ 0 + card + owner + selfemp, data = credit.df))
dummies <- dummies[,-1]
credit.df <- cbind(credit.df[, -c(1,7,8)], dummies)

colfunc<-colorRampPalette(c("green","white","red"))
heatmap.2(cor(credit.df),Rowv=FALSE,dendrogram="none",lwid=c(0.1,4),lhei=c(0.1,4),col=colfunc(15),cellnote=round(cor(credit.df),2),notecol="black",key=FALSE,trace='none')

##box plots
#reports
boxplot(credit.df$reports ~ credit.df$card, data = credit.df)
#age
boxplot(credit.df$age ~ credit.df$card, data = credit.df)
#income
boxplot(credit.df$income ~ credit.df$card, data = credit.df)
#share
boxplot(credit.df$share ~ credit.df$card, data = credit.df)



# performance chart
library(PerformanceAnalytics)
credit.perf <- data.frame(credit.df$cardyes,credit.df$owneryes,credit.df$selfempyes,credit.df$age,credit.df$income,credit.df$expenditure,credit.df$dependents,credit.df$months,credit.df$active)
pairs(credit.perf, pch = 19)
round(cor(credit.perf), digits = 3)
chart.Correlation(R = credit.perf, histogram = TRUE, pch = 19)

# Linear model
library(ggpubr)
ggscatter(credit.df, x = "income", y = "age", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "blue", fill = "gray"),
          xlab = "income", ylab = "age")

ggscatter(credit.df, x = "expenditure", y = "age", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "blue", fill = "gray"),
          xlab = "expenditure", ylab = "age")

## Box plot for people having credit card (chosen variables: age, income, expenditure, dependents)
# With expenditure
card.yes <- credit.df %>% filter(cardyes == 1)
boxplot(card.yes[,c(2,3,5,6)], notch = FALSE, col = c("red", "blue", "yellow", "green"))

# Without expenditure
boxplot(card.yes[,c(2,3,6)], notch = FALSE, col = c("red","blue","green"))

# Without expenditute & age
boxplot(card.yes[,c(3,6)], notch = FALSE, col = c("blue","green"))

# Card owner with/without using credit card
bar <- ggplot(data = credit.df, aes(as.factor(owneryes), fill = as.factor(owneryes))) + geom_bar() + facet_wrap(~cardyes) + xlab("Credit card use") + ggtitle("Card owner with/without using credit card") 
bar + scale_fill_discrete(name = "Owner")

# Historgrams
## How many dependents usually occur for people having credit card
hist(card.yes$dependents, xlab = "dependents")

## In which income range mostly using credit card
hist(card.yes$income, xlab = "income")

## How many months that credit card has been used most
hist(card.yes$months, xlab = "month")

