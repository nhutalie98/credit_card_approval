library(caret)
library(e1071)
library(gplots)
library(forecast)
library(gains)
library(DiscriMiner)
library(rpart)
library(rpart.plot)
library(randomForest)


credit.df <- read.csv("AER_credit_card_data.csv")

#summary statistics
summary(credit.df)
#missing data - there are no NAs in the dataset

##box plots
#reports
boxplot(credit.df$reports ~ credit.df$card, data = credit.df)
#age
boxplot(credit.df$age ~ credit.df$card, data = credit.df)
#income
boxplot(credit.df$income ~ credit.df$card, data = credit.df)
#share
boxplot(credit.df$share ~ credit.df$card, data = credit.df)

#dummy variables for card, owner and selfemp
dummies <- data.frame(model.matrix(~ 0 + card + owner + selfemp, data = credit.df))
dummies <- dummies[,-1]
credit.df <- cbind(credit.df[, -c(1,7,8)], dummies) #drop one dummy variable for each group
t(t(names(credit.df)))

#aggregate data into groups of approved/not approved and show avg for each variable
aggdata <- aggregate(. ~ cardyes, credit.df, mean)
#calculate the difference of the approved vs. not approved mean for each variable
meandiff <- abs(aggdata[1,] - aggdata[2,]) 
sort(meandiff, decreasing=TRUE) #sort variables by highest to lowest avg difference

#highest difference is in expenditure, followed by reports and active

#heat map
colfunc<-colorRampPalette(c("green","white","red"))
heatmap.2(cor(credit.df),Rowv=NA,Colv=NA,dendrogram="none",lwid=c(0.1,4),lhei=c(0.1,4),col=colfunc(15),cellnote=round(cor(credit.df),2),notecol="black",key=FALSE,trace='none')

#scatter plot for income and expenditure shows a slight positive correlation, but not much
plot(credit.df$income, credit.df$expenditure, xlab = "Income", ylab = "Expenditure")

#partition the data for forward selection using 60% for training and the remaining 40% as the validation dataset
set.seed(1) #ensure we get the same partitions
train.rows <- sample(rownames(credit.df), nrow(credit.df)*.6) 
train.data <- credit.df[train.rows, ]
valid.rows <- setdiff(rownames(credit.df), train.rows)
valid.data <- credit.df[valid.rows, ]

#create null model then use forward selection to find a reduced model
glm <- glm(cardyes ~., data = train.data, family= "binomial")
glm.null <- glm(cardyes  ~ 1, data = train.data, family = "binomial")
glm.fwd <- step(glm.null, scope = list(glm.null, upper = glm), direction = "forward", family ="binomial")
summary(glm.fwd)
#fwd model deviance, AIC, BIC
glm.fwd$deviance
AIC(glm.fwd)
BIC(glm.fwd)
#predictions on validation data
pred.fwd.valid <- predict(glm.fwd, valid.data)
options(scipen = 999)
accuracy(pred.fwd.valid, valid.data$cardyes)

#glm.fwd confusionmatrix
confusionMatrix(as.factor(ifelse(pred.fwd.valid >= 0.5, "1", "0")),
                as.factor(valid.data$cardyes), positive = "1")
#glm.fwd lift chart
gainfwd <- gains(valid.data$cardyes, pred.fwd.valid, groups = length(pred.fwd.valid))
plot(c(0, gainfwd$cume.pct.of.total * sum(valid.data$cardyes)) ~ c(0, gainfwd$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(valid.data$cardyes)) ~ c(0, nrow(valid.data)), lty = 2)
#odds
odds <- data.frame(summary(glm.fwd)$coefficient, odds =  exp(coef(glm.fwd)))
round(odds, 4)

#reload and partition the data using 60% for training and the remaining 40% as the validation dataset
credit.df <- read.csv("AER_credit_card_data.csv")
dummies <- data.frame(model.matrix(~ 0 + card + owner + selfemp, data = credit.df))
dummies <- dummies[,-1]
credit.df <- cbind(credit.df[, -c(1,7,8)], dummies)
set.seed(1)
train.index <- sample(nrow(credit.df), nrow(credit.df) * 0.6)
valid.index <- as.numeric(setdiff(rownames(credit.df), train.index))
credit.train <- credit.df[train.index, ]
credit.valid <- credit.df[valid.index, ]

#discriminant analysis
da <- linDA(credit.df[,-10], credit.df$cardyes, validation = "learntest", learn = train.index, test = valid.index)
da$functions

#propensities
propensity <- exp(da$scores[, "1"]) / (exp(da$scores[, "0"]) + exp(da$scores[, "1"]))
da.results <- data.frame(Actual = credit.valid$cardyes, da$classification, da$scores, 
                         propensity = propensity)
options(scipen = 999)
head(da.results, 25)

#DA confusion matrix
confusionMatrix(da$classification, as.factor(credit.valid$cardyes), positive = "1")

#DA lift chart
gainda <- gains(as.numeric(credit.valid$cardyes), 
              exp(da$scores[, 2]) / (exp(da$scores[, 1]) + exp(da$scores[, 2])), 
              groups = length(credit.valid))
plot(c(0, gainda$cume.pct.of.total * sum(as.numeric(credit.valid$cardyes))) ~ c(0, gainda$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(as.numeric(credit.valid$cardyes))) ~ c(0, nrow(credit.valid)), lty = 2)


#full classification tree
credit.ct0 <- rpart(cardyes ~ ., data = credit.train, method = "class", cp = 0, minsplit = 1)
credit.ct0
prp(credit.ct0, digits = 4, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(credit.ct0$frame$var == "<leaf>", 'gray', 'white'))
#in sample prediction
pred0 <- predict(credit.ct0, type = "class")
confusionMatrix(pred0, as.factor(credit.train$cardyes), positive = "1")
#out of sample prediction
pred1 <- predict(credit.ct0, credit.valid, type = "class")
confusionMatrix(pred1, as.factor(credit.valid$cardyes), positive = "1")

#pruning and finding the best-pruned tree
#cross validation
cv.ct <- rpart(cardyes ~ ., data = credit.train, method="class",
               cp = .00001, minsplit = 3, xval = 10)
printcp(cv.ct) 
#minimum xerror is 0.067416 at cp = 0.002809    
#add xstd 0.019313 + 0.067416 = 0.086729
#choose the simplest with xerror < 0.086729
#choose cp =  0.005618 (1 splits)

#prune by lower cp
pruned.ct <- prune(cv.ct, cp = 0.005618)
prp(pruned.ct, digits = 4, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))
#out of sample prediction
pruned.pred1 <- predict(pruned.ct,credit.valid, type = "class")
confusionMatrix(pruned.pred1, as.factor(credit.valid$cardyes), positive = "1")

#view classification rules
pruned.ct

#random forest with 500 trees selecting from a random subset of 3 predictors at each decision node
rf <- randomForest(as.factor(cardyes) ~ ., data = credit.train, ntree = 500,
                   mtry = 3, nodesize = 5, importance = TRUE)
#variable importance plot
varImpPlot(rf, type=1)
#expenditure is most important, followed by share, reports, and active
#confusion matrix
rf.pred <- predict(rf, credit.valid)
confusionMatrix(rf.pred, as.factor(credit.valid$cardyes), positive = "1")

