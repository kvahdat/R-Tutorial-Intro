#############################################
######      Introduction to R    ############
######     programming for DS    ############
######    Author: Kimia Vahdat   ############
#############################################

## Loading and installing packages
packages <- c("caret", "dplyr","tidyverse","ggplot2","GGally")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(dplyr)
library(caret)
library(tidyverse)
library(ggplot2)
library(GGally)

## Loading the data
setwd("C:/Users/Administrator/OneDrive - North Carolina State University/Spring 2021/R Intro Presentation files")

## Let's load three different datasets to practice loading the data
crime <- read.csv("./crime.csv")
head(crime)
## based on the data documentation the first 5 variables are not predictive
crime <- crime[,-c(1:5)]

anyNA(crime)
######################################################################
###################       Practice!                     ##############
###################  Read and check for missing values  ##############
######################################################################

car <- read.table("./auto-mpg.data",header = F)
head(car)
car_names=c("mpg","cylinders","displacement","horsepower","weight",
            "acceleration","model year","origin","carname")
colnames(car)<-car_names
# remove the last column
car= car[,-ncol(car)]
glimpse(car)
# notice anything strange?
# check for missing values.

car$horsepower=as.numeric(car$horsepower)
anyNA(car)
###################### Good Job! #####################################

## To practice dealing with missing values, let's work on the crime 
## dataset first, by finding which variables have most of their rows
## missing and which rows are missing.

# let's check the data again first
glimpse(crime)

cl_sums=colSums(is.na(crime))
cl_sums[cl_sums>0]
crime <- crime %>% dplyr::select(!names(which(cl_sums>1500)))
crime <- crime %>% filter(!is.na(ViolentCrimesPerPop))
## for the remaining missing values we replace them with the mean of that column
## only keeping those with missing values
miss_names=names(which(colSums(is.na(crime))>0))
miss_names
for(cl in miss_names){
  crime[is.na(crime[,cl]),cl]=mean(crime[,cl],na.rm = TRUE)
}
anyNA(crime)

######################################################################
###################       Practice!                     ##############
################### Replace missing values for car data ##############
######################################################################

miss_names=names(which(colSums(is.na(car))>0))

for(cl in miss_names){
  car[is.na(car[,cl]),cl]=mean(car[,cl],na.rm = TRUE)
}

anyNA(car)

######################################################################

glimpse(crime)
## no categorical variable here, but don't worry decomposing the nominal
## or categorical variables is very easy, see the next commented code:
# dummies <- dummyVars("~.",data= crime[,"Some categorical Variable"], fullRank=T)
# crime <- data.frame(cbind(
#          crime[,-which(colnames(crime)=="Some categorical Variable")],
#          predict(dummies, newdata=crime[,"Some categorical Variable"])))

## Finding the variables with no information

nzv_crime<- nearZeroVar(crime, uniqueCut = 5)
nzv_crime
## no such variable

################### Practice with Car data ##########################

nzv_car= nearZeroVar(car, uniqueCut = 5)
nzv_car

#####################################################################

cor_crime=cor(crime, method = "pearson")

library(corrplot)
corrplot(cor_crime, tl.col = "black", order="hclust", tl.pos="n", 
         method="color", type = "upper",tl.srt = 45)

## there are some very dark blue and dark reds in the plot, let's check them
# this plot is not very clear, but it gives us some idea. 
# This function returns the names of the variables with high correlation, if any.
corr_check <- function (df, thd){
  high_corr_index <- which(apply(df,2,function(x){any(abs(x)>thd &  x!=1)})) 
  if (length(high_corr_index)>0) {
    return(names(high_corr_index))
  }
  else
    return(NA)
}

crime_highCorr <- corr_check(cor_crime,0.95)
crime_highCorr

# let's plot only these
corrplot(cor(crime[,crime_highCorr]), tl.col = "black", order="hclust", tl.cex=0.7, 
         method="color", type="upper")


## It seems like a lot of crimes has very positive correlation with the population
## So it seems using only per population version of these variables would be enough
perpop=crime %>% dplyr::select(contains("perPop"))
names(perpop)
## We have all of them, so we can remove them.

to_remove_crime <- crime_highCorr[c(2:4,6,7,9:11,14:21,24,25,27,28,30:32,34:40)]
## Also checked for numbers variables that are highly correlated and there are Pct
## variables that compute their ratio with the population
crime <- crime %>% dplyr::select(!to_remove_crime)

## Let's go back and check again

crime_highCorr <- corr_check(cor(crime,method = "pearson"),0.95)
crime_highCorr

corrplot(cor(perpop), tl.col = "black")
perpop= perpop %>% mutate(sum_crimes=murdPerPop+rapesPerPop+assaultPerPop)
corrplot(cor(perpop), tl.col = "black")

crime = crime %>% dplyr::select(!names(perpop)[c(1:8)])
####################################################################################
################################# Your Turn! #######################################
####################################################################################
ggpairs(car,
        progress = F,
        upper = list(continuous = wrap("cor", size = 5)))+ 
  theme_grey(base_size = 12)
## there are some nonlinear relationships to look for!
## It looks like you can add some useful new variable, add them to the data

car = car %>% 
####################################################################################

## Since the number of variables is large for Crime, we do a backward selection
library(leaps)
library(MASS)

## To account for randomness in our selection, we use a 10 fold cross validation

# Set seed for reproducibility
set.seed(12356)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(ViolentCrimesPerPop ~., data = crime,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 5:80),
                    trControl = train.control
)
step.model$results

step.results<- step.model$results

ggplot(step.results,aes(x=nvmax,y=RMSE,ymin=RMSE-RMSESD,ymax=RMSE+RMSESD))+
  geom_errorbar()+geom_point()+ggtitle("Backward Selection")

# what is the minimum
step.model$bestTune

# Set seed for reproducibility
set.seed(12356)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(ViolentCrimesPerPop ~., data = crime,
                    method = "leapSeq",
                    tuneGrid = data.frame(nvmax = 5:80),
                    trControl = train.control
)
step.model$results

step.results<- step.model$results

## Plot
ggplot(step.results,aes(x=nvmax,y=RMSE,ymin=RMSE-RMSESD,ymax=RMSE+RMSESD))+
  geom_errorbar()+geom_point()+ggtitle("Sequential Selection")

# what is the minimum
step.model$bestTune

selected_variables <- names(coef(step.model$finalModel, 37))[-1]

crime <- crime %>% dplyr::select(c(selected_variables,"ViolentCrimesPerPop"))
###############################################################################

### Now we're ready for some modeling!! 

## Simple linear regression

lm_mod<- lm(ViolentCrimesPerPop~., crime)
summary(lm_mod)
par(mfrow=c(2,2))
plot(lm_mod)

pred_res=data.frame(cbind(crime$ViolentCrimesPerPop,predict(lm_mod)))
colnames(pred_res)=c("True","Predicted")
ggplot(pred_res, aes(x = True, y = Predicted)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

## For cute reports use this:
library(sjPlot)
tab_model(lm_mod)

## building one model on the whole dataset is not very helpful though!
## We need at least two subset of the data to evaluate the performance

## Method 1:
set.seed(1245)
train_id=sample(1:nrow(crime),floor(0.8*nrow(crime)),replace = F)

lm_mod<- lm(ViolentCrimesPerPop~., crime[train_id,])
summary(lm_mod)
predicted_lm <- predict(lm_mod, crime[-train_id,])

# RMSE
sqrt(mean((predicted_lm-crime[-train_id,"ViolentCrimesPerPop"])^2))

## Polynomial
crime_new= crime %>% dplyr::mutate(
  population.2=(population-mean(population))^2,
  agePct16t24PctFam2Par=(PctFam2Par-mean(PctFam2Par))*(agePct16t24-mean(agePct16t24)))
crime_new=as.data.frame(crime_new)

poly_mod <- lm(ViolentCrimesPerPop~., crime_new[train_id,])
summary(poly_mod)
predicted_poly <- predict(poly_mod, crime_new[-train_id,])

# RMSE
sqrt(mean((predicted_poly-crime_new[-train_id,"ViolentCrimesPerPop"])^2))

## LASSO
library(glmnet)

x=model.matrix(ViolentCrimesPerPop~., crime[train_id,])[,-1]
y=crime$ViolentCrimesPerPop[train_id]

x_test=model.matrix(ViolentCrimesPerPop~., crime[-train_id,])[,-1]
y_test=crime$ViolentCrimesPerPop[-train_id]

# create grid for lambda, fit model using all lambdas
grid=10^seq(2,-4,length=400) 
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)  

# check coefficent values for each value of lambda
plot(lasso.mod)  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.lasso=cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv.lasso)
bestlam.l=cv.lasso$lambda.1se
bestlam.l

# get coefficents for best model and compare to OLS
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam.l)
lasso.coef

predicted_lasso= predict(lasso.mod,s=bestlam.l,x_test)
# RMSE
sqrt(mean((predicted_lasso-crime[-train_id,"ViolentCrimesPerPop"])^2))

# plot
res= data.frame(cbind(predicted_lasso,predicted_lm,
                      predicted_poly,crime[-train_id,"ViolentCrimesPerPop"]))
colnames(res)[4]= "True_val"
colnames(res)[1]= "predicted_lasso"


library(reshape2)
res_melted= melt(res,id=c("True_val"))
ggplot(res_melted,aes(x=True_val,y=value, color=variable))+
  geom_point()+geom_abline(slope = 1)
##################################################################################
############################### Practice with Car dataset #######################
############################### Build two models and compare them ###############
##################################################################################

## First create a train and test set


## Then build your model on the train set

## evaluate on the test

##################################################################################

##################################################################################
###############################       Classification       #######################
##################################################################################

br = read.table("./breast-cancer.data",sep=",")
head(br)
br=br[,-1]

anyNA(br)

## Plotting for analysis
br$V10=as.factor(br$V10)
ggpairs(br, 
        mapping=ggplot2::aes(colour=V10),progress=F,
        upper = list(continuous = wrap("density", alpha = 0.5), combo = "box_no_facet"),
        lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4)))

## converting the categorical variables
dummies <- dummyVars("~.",data= br[,c("V2","V3","V4","V5","V6","V8","V9","V10")], fullRank=T)
br <- data.frame(cbind(
  br[,c("V7")],
  predict(dummies, newdata=br[,c("V2","V3","V4","V5","V6","V8","V9","V10")])))


# changing the name of the last column
colnames(br)[ncol(br)]="y"

## checking for near zero variables
nzv_br= nearZeroVar(br, uniqueCut = 5)
colnames(br)[nzv_br]

## need to combine groups
br = br %>% mutate(V260.79=V260.69+V270.79,
                   V3lt40premeno=V3lt40+V3premeno,
                   V445.54=V445.49+V450.54,
                   V45.14=V45.9+V410.14,
                   V59.17=V59.11+V512.14+V515.17)

br = br %>% dplyr::select(!colnames(br)[nzv_br])

# checking again
nzv_br= nearZeroVar(br, uniqueCut = 5)
colnames(br)[nzv_br]

br = br %>% mutate(V440.54=V445.54+V440.44)
br = br %>% dplyr::select(!c("V445.54","V440.44","V410.14","V3premeno","V260.69"))

# checking again
nzv_br= nearZeroVar(br, uniqueCut = 5)
colnames(br)[nzv_br]

## Checking the correlations
br_cor=
# plot it

## Let's remove V6no
br= br %>% dplyr::select(!"V6no")


## This data is clean and ready to use
set.seed(1256)
train_id=createDataPartition(br$y,p=0.8)[[1]]

## GLM 
glm_mod=glm(y~., br[train_id,], family = "binomial")
summary(glm_mod)
predicted_glm_test=predict(glm_mod,br[-train_id,],type = "response")
predicted_glm=predict(glm_mod,br[train_id,],type = "response")

require(pROC)
par(mfrow=c(1,1))
lr.roc=roc(br$y[train_id],predicted_glm)
lr.roc
mfrow=c(1,1)
plot(lr.roc)

# Optimize threshold -- Youden method -- maximize sens+spec

t.calc=data.frame(cbind(as.vector(lr.roc$thresholds),(lr.roc$sensitivities+lr.roc$specificities)))
colnames(t.calc)=c("thresh","d")
opt.t=t.calc$thresh[which.max(t.calc$d)]
opt.t

conf.mtrx=table(predicted_glm_test>opt.t,br$y[-train_id])
conf.mtrx
## accuracy
sum(as.numeric(predicted_glm_test>opt.t)==br$y[-train_id])/length(predicted_glm_test)

## compute sensitivity and specificity

## This is not a great model!

## LASSO logistic regression
x_br=model.matrix(y~., br[train_id,])[,-1]
y_br=br$y[train_id]

x_br_test=model.matrix(y~., br[-train_id,])[,-1]
y_br_test=br$y[-train_id]

# create grid for lambda, fit model using all lambdas
grid=10^seq(5,-5,length=500) 
lasso.mod=glmnet(x_br,y_br,alpha=1,lambda=grid,family="binomial")  

# check coefficent values for each value of lambda
plot(lasso.mod)  
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.lasso=cv.glmnet(x_br,y_br,alpha=1,lambda=grid,family="binomial")
plot(cv.lasso)
bestlam.l=cv.lasso$lambda.min
bestlam.l

# get coefficents for best model and compare to OLS
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam.l)
lasso.coef

predicted_lasso= predict(lasso.mod,s=bestlam.l,x_br, type = "response")
predicted_lasso_test= predict(lasso.mod,s=bestlam.l,x_br_test, type = "response")

par(mfrow=c(1,1))
lr.roc.lasso=roc(br$y[train_id],predicted_lasso[,1])
lr.roc.lasso
mfrow=c(1,1)
plot(lr.roc)

# Optimize threshold -- Youden method -- maximize sens+spec

t.calc.lasso=data.frame(cbind(as.vector(lr.roc.lasso$thresholds),
                              (lr.roc.lasso$sensitivities+lr.roc.lasso$specificities)))
colnames(t.calc.lasso)=c("thresh","d")
opt.t.lasso=t.calc.lasso$thresh[which.max(t.calc.lasso$d)]
opt.t.lasso

conf.mtrx.lasso=table(predicted_lasso_test>opt.t,br$y[-train_id])
conf.mtrx.lasso
## compute accuracy, sensitivity and specificity

## random forest
# tune model parameter mtry using caret
br$y=as.factor(br$y)
names(br)=make.names(colnames(br))
control=trainControl(method="cv", number=10, search="grid",
                     savePredictions="final",classProbs=T)
set.seed(123)
tunegrid=expand.grid(mtry=c(1:24))
rf_gridsearch=train(make.names(y)~.,data=br[train_id,], method="rf", metric="Accuracy", 
                    tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
rf_gridsearch$finalModel

library(randomForest)
set.seed(123)
rf.mod=randomForest(y~.,data=br[train_id,],mtry=2, ntree=2000, 
                    importance=T)


varImpPlot(rf.mod,type=1,pch=19)

# compute and display ROC curve
par(mfrow=c(1,1))
rf.roc=roc(rf_gridsearch$pred[,3],rf_gridsearch$pred[,5])
rf.roc
plot(rf.roc)

## predicting on out sample data
predicted_rf_test=predict(rf.mod,br[-train_id,])

# plot confusion matrix
conf.mtrx.rf=confusionMatrix(predicted_rf_test,br$y[-train_id])
conf.mtrx.rf$table
conf.mtrx.rf$table/length(br$y[-train_id])
par(mfrow=c(1,1))
barplot(conf.mtrx$table,col=c("light green","pink"),beside=T,xlab="actual")
legend("topright",legend=c("predicted 0","predicted 1"),
       col=c("light green","pink"),lwd=10)

## plotting one tree
library("party")
cf <- cforest(y~., data=br)
pt <- party:::prettytree(cf@ensemble[[1]], names(cf@data@get("input")))
nt <- new("BinaryTree")
nt@tree <- pt
nt@data <- cf@data
nt@responses <- cf@responses
plot(nt)

## bagging
set.seed(123)
rf.mod=randomForest(y~.,data=br[train_id,],mtry=23, ntree=2000, 
                    importance=T)


varImpPlot(rf.mod,type=1,pch=19)

# compute and display ROC curve
par(mfrow=c(1,1))
rf.roc=roc(rf_gridsearch$pred[,3],rf_gridsearch$pred[,5])
rf.roc
plot(rf.roc)

## predicting on out sample data
predicted_rf_test=predict(rf.mod,br[-train_id,])

# plot confusion matrix
conf.mtrx.rf=confusionMatrix(predicted_rf_test,br$y[-train_id])
conf.mtrx.rf$table
conf.mtrx.rf$table/length(br$y[-train_id])
par(mfrow=c(1,1))
barplot(conf.mtrx$table,col=c("light green","pink"),beside=T,xlab="actual")
legend("topright",legend=c("predicted 0","predicted 1"),
       col=c("light green","pink"),lwd=10)
