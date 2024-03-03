# Load the required libraries
library("rmdformats")  
library("corrgram")
library("MASS")
library("ggplot2")
library("naniar")
library("e1071")
library("lattice")
library("caret")
library("car")
library("caTools")
library("knitr")


# Read the CSV file
wineds <- read.csv("C:/Users/KIIT/Downloads/winequality-red.csv", header = TRUE)

View(wineds)

# View the first few rows of the data
head(wineds)

# Display the structure of the dataset
str(wineds)

# Generate summary statistics
summary(wineds)

# Get the column names
colnames(wineds)

wineds <- wineds[!duplicated(wineds),]

dim(wineds)

sum(is.na(wineds))

vis_miss(wineds)

table(wineds$quality)

round(cor(wineds,method = "pearson"),2)

corrgram(wineds,type = "data",lower.panel = panel.conf,
         upper.panel = panel.shade, main="Corrgram for the wine quality dataset", order = T, cex.labels = 1.2)

attach(wineds)

par(mfrow=c(2,2),oma=c(1,1,0,0)+0.1, mar = c(3,3,1,1)+0.1)

barplot((table(fixed.acidity)), col = c("red","blue","green","darkorchid", "coral", "yellow"))
mtext("fixed.acidity", side = 1, outer = F, line = 2,cex = 0.8)

barplot((table(volatile.acidity)), col = c("red","blue","green","darkorchid", "coral", "yellow"))
mtext("volatile.acidity", side = 1, outer = F, line = 2,cex = 0.8)

barplot((table(citric.acid)), col = c("red","blue","green","darkorchid", "coral", "yellow"))
mtext("citric.acid", side = 1, outer = F, line = 2,cex = 0.8)

barplot((table(residual.sugar)), col = c("red","blue","green","darkorchid", "coral", "yellow"))
mtext("residual.sugar", side = 1, outer = F, line = 2,cex = 0.8)

par(mfrow=c(2,2),oma=c(1,1,0,0)+0.1, mar = c(3,3,1,1)+0.1)

barplot((table(chlorides)), col = c("red","blue","green","darkorchid", "coral", "yellow"))
mtext("chlorides", side = 1, outer = F, line = 2,cex = 0.8)

barplot((table(free.sulfur.dioxide)), col = c("red","blue","green","darkorchid", "coral", "yellow"))
mtext("free.sulfur.dioxide", side = 1, outer = F, line = 2,cex = 0.8)

barplot((table(total.sulfur.dioxide)), col = c("red","blue","green","darkorchid", "coral", "yellow"))
mtext("total.sulfur.dioxide", side = 1, outer = F, line = 2,cex = 0.8)

barplot((table(density)), col = c("red","blue","green","darkorchid", "coral", "yellow"))
mtext("density", side = 1, outer = F, line = 2,cex = 0.8)

par(mfrow=c(2,2),oma=c(1,1,0,0)+0.1, mar = c(3,3,1,1)+0.1)

barplot((table(pH)), col = c("red","blue","green","darkorchid", "coral", "yellow"))
mtext("pH", side = 1, outer = F, line = 2,cex = 0.8)

barplot((table(alcohol)), col = c("red","blue","green","darkorchid", "coral", "yellow"))
mtext("alcohol", side = 1, outer = F, line = 2,cex = 0.8)

barplot((table(quality)), col = c("red","blue","green","darkorchid", "coral", "yellow"))
mtext("quality", side = 1, outer = F, line = 2,cex = 0.8)
 
#Boxplot

par(mfrow=c(1,5),oma=c(1,1,0,0)+0.1, mar = c(3,3,1,1)+0.1)

boxplot(fixed.acidity,col = "cadetblue",pch=19)
mtext("fixed.acidity", cex = 0.8,side = 1,line = 2)

boxplot(volatile.acidity,col = "coral",pch=19)
mtext("volatile.acidity", cex = 0.8,side = 1,line = 2)

par(mfrow=c(1,5),oma=c(1,1,0,0)+0.1, mar = c(3,3,1,1)+0.1)

boxplot(citric.acid,col = "darkviolet",pch=19)
mtext("citric.acid", cex = 0.8,side = 1,line = 2)

boxplot(residual.sugar,col = "darkred",pch=19)
mtext("residual.sugar", cex = 0.8,side = 1,line = 2)

par(mfrow=c(1,5),oma=c(1,1,0,0)+0.1, mar = c(3,3,1,1)+0.1)

boxplot(chlorides,col = "darkgreen",pch=19)
mtext("chlorides", cex = 0.8,side = 1,line = 2)

boxplot(alcohol,col = "gold",pch=19)
mtext("alcohol", cex = 0.8,side = 1,line = 2)

par(mfrow=c(1,5),oma=c(1,1,0,0)+0.1, mar = c(3,3,1,1)+0.1)

boxplot(density,col = "slategrey",pch=19)
mtext("density", cex = 0.8,side = 1,line = 2)

boxplot(free.sulfur.dioxide,col = "magenta",pch=19)
mtext("free.sulfur.dioxide", cex = 0.8,side = 1,line = 2)

par(mfrow=c(1,5),oma=c(1,1,0,0)+0.1, mar = c(3,3,1,1)+0.1)

boxplot(pH,col = "navy",pch=19)
mtext("pH", cex = 0.8,side = 1,line = 2)

boxplot(sulphates,col = "maroon",pch=19)
mtext("sulphates", cex = 0.8,side = 1,line = 2)

boxplot(total.sulfur.dioxide,col = "plum",pch=19)
mtext("total.sulfur.dioxide", cex = 0.8,side = 1,line = 2)

str(wineds)

skewness(fixed.acidity)
skewness(volatile.acidity)
skewness(citric.acid)
skewness(residual.sugar)
skewness(chlorides)
skewness(free.sulfur.dioxide)
skewness(total.sulfur.dioxide)
skewness(density)
skewness(pH)
skewness(sulphates)
skewness(alcohol)
skewness(quality)

#Train - test set

set.seed(100)
trainingRowIndex <- sample(1:nrow(wineds),0.8*nrow(wineds))
winedstrain <- wineds[trainingRowIndex,]
winedstest <- wineds[-trainingRowIndex, ]

#Model Selection

linear0 <- lm(quality ~ . , winedstrain)
summary(linear0)

#Checking Multicollinearity over here , We remove density cause it exhibits multicollinarity

vif(linear0)

#we can see Multicollinearity over here , We remove density cause it exhibits multicollinarity

linear1 <- lm(quality ~ . -density , winedstrain)
summary(linear1)

vif(linear1)

linear2 <- lm(quality ~ . -density - fixed.acidity , winedstrain)
summary(linear2)

linear3 <- lm(quality ~ . -density - fixed.acidity -citric.acid , winedstrain)
summary(linear3)

vif(linear3)

par(mfrow=c(2,2), oma=c(1,1,0,0)+0.1, mar=c(3,3,1,1)+0.1)
plot(linear3)
return

#predicting- Trained set

distPred1 <- predict(linear3,winedstrain)
head(distPred1)

distPred1 <- ceiling(distPred1)
head(distPred1)

#Training Data Confusion Matrix

trn_tab <-table(predicted = distPred1, actual = winedstrain$quality)
trn_tab

#Accuracy for the linear model

sum(diag(trn_tab))/length(winedstest$quality)

#Accuracy Prediction over train set linear Model is 23%
#Testing or validating the Model

distPred <- predict(linear3, winedstest)
head(distPred)

distPred1 <- ceiling(distPred)
head(distPred1)

tst_tab <- table(predicted = distPred1 , actual = winedstest$quality)
tst_tab

#Checking the accuracy of the test Data

sum(diag(tst_tab))/length(winedstest$quality)

#Accuracy Prediction over test set Linear Model is 3.6%

#Assumptions for logistic Regression

wineds$quality2 <- as.factor(wineds$quality)

#Train- Test Set

set.seed(3000)
spl = sample.split(wineds$quality2, SplitRatio = 0.7)

winedstrain=subset(wineds, spl==TRUE)
winedstest = subset(wineds,spl == FALSE)

head(winedstrain)

require(MASS)
require(reshape2)

#Fitting Model

o_lrm <- polr(quality2 ~ . - quality, data = winedstrain, Hess = TRUE)
vif(o_lrm)
summary(o_lrm)

o_lr = step(o_lrm)

head(fitted(o_lr))

#Training Set Accuracy

p<-predict(o_lr,type = "class")
head(p)

#confusion Matrix Train Set

cm1 = as.matrix(table(Actual=winedstrain$quality2, Predicted = p))
cm1

sum(diag(cm1))/length(winedstrain$quality2)

#Training Set Accuracy is 57.20%
#Test Set Accuracy

tst_pred <- predict(o_lr, newdata = winedstest, type = "class")

#Confusion Matrix

cm2 <- table(predicted=tst_pred, actual = winedstest$quality2)
cm2

sum(diag(cm2))/length(winedstrain$quality2)

#Test Set Accuracy = 25.97%

#Binomial Logistic Regression Model

wineds$category[wineds$quality <=5 ] <- 0
wineds$category[wineds$quality > 5 ] <- 1

wineds$category <- as.factor(wineds$category)

head(wineds)

#Train Test Split 

set.seed(3000)

spl=sample.split(wineds$category, SplitRatio = 0.7)

winedstrain = subset(wineds, spl == TRUE)
winedstest = subset(wineds, spl == FALSE)

head(winedstrain)

#We will use glm() - Generalized Linear Model Command to run a logistic regression

model_glm <- glm(category ~ . - quality - quality2, data = winedstrain, family = binomial(link = "logit"))
model_gl <- step(model_glm)

#Prediction - Train Set

head(fitted(model_gl))

head(predict(model_gl))

head(predict(model_gl, type ="response"))

#Categorization Set
trn_pred <- ifelse(predict(model_gl, type = "response") > 0.5, "Good Wine", "Bad Wine")
head(trn_pred)

#Confusion Matrix - Trainig Set

trn_tab <- table(predicted = trn_pred, actual = winedstrain$category)
trn_tab

#Training Set Accuracy

sum(diag(trn_tab))/length(winedstrain$category)

#We can see that Binomial Logistic Regression Gives an Training Set Accuracy of 74.55%

#Confusion Matrix - Test Set

tst_pred <- ifelse(predict(model_gl, newdata = winedstest, type = "response") >0.5 , "Good Wine", "Bad Wine")
tst_tab <- table(predicted = tst_pred, actual = winedstest$category)
tst_tab

#Test Set Accuracy

sum(diag(tst_tab))/length(winedstest$category)

#We can see that Binomial Logistic Regression Gives an Test Set Accuracy of 75.98%