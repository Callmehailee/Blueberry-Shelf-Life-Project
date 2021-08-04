#Blueberry final model 

blueberry_dat <- read.csv("C:/Users/17038/Desktop/STAT5020W/Project/dataset/FirstYearBlueberry_Complete_Updated.csv")

#Standardized:
#only select the required quantitative variables:
blueberry_dat <- blueberry_dat[,c(7:13,16)]
#blueberry_dat_std <- as.data.frame(scale(blueberry_dat))
blueberry_dat[1:7] <- as.data.frame(scale(blueberry_dat[1:7]))
head(blueberry_dat)
attach(blueberry_dat)


#------------------Lasso Regression Model---------------------#
library(glmnet)
set.seed(5020) 


#We need to define the model equation
#From previous model results, we would like to include everything...
#need to remove the first index column


#Now, try adding some qualitative variables ---> logistic model
#harvest year, farm, blueberry type and cultivar type





X <- model.matrix(Final.Shelf.Life..Days. ~  DefectFree + Compression + Pucture + TSS + pH + TA + 
                    Weight + I(DefectFree^2) + I(Compression^2) + I(Pucture^2) + I(TSS^2) + I(pH^2) + I(TA^2) + 
                    I(Weight^2) + (DefectFree+Compression+Pucture+TSS+pH+TA+Weight)^2 , data=blueberry_dat)[,-1]

#Response variable:
Y <- blueberry_dat[,"Final.Shelf.Life..Days."]


#my_blueberry_dat <- sort(sample(nrow(blueberry_dat_std), nrow(blueberry_dat_std)*2/3))
#train<-blueberry_dat_std[my_blueberry_dat,]
#length(train)
#test<-blueberry_dat_std[-my_blueberry_dat,]

# Divide the dataset into 80% for training and 20% for testing 
train=sample(1: nrow(blueberry_dat), nrow(blueberry_dat)*0.8)
length(train)
test=(- train)
y.test=Y[test]



#First we need to find the amount of penalty, lambda by cross-validation. 
#We will search for the lambda that give the minimum  MSE

#Penalty type (alpha=1 is lasso 
#and alpha=0 is the ridge)

#Setting the range of lambda values:
lambda_seq <- 10^seq(2,-2, by = -0.1)

lambda.lasso <- glmnet(x=X[train,], y=Y[train], 
                             alpha = 1, lambda = lambda_seq) 
cv.lambda.lasso <- cv.glmnet(X[train,], y=Y[train], alpha = 1) 
plot(cv.lambda.lasso)  #MSE for several lambdas

cv.lambda.lasso                              #best lambda

#or 
best_lambda <- cv.lambda.lasso$lambda.min
best_lambda <- 5
#try best_lambda = 1

# Fit lasso with best lambda
lasso.pred=predict(lambda.lasso ,s=best_lambda ,newx=X[test ,]) #using testing dataset
# 
#rmse
RMSE <- sqrt(mean((lasso.pred -y.test)^2))  #when split the data into 2/3 for training and 1/3 for testing, the results get better

#R-squared"
Rsquare <- cor(y.test, lasso.pred)^2

plot(y.test,lasso.pred,col="#00000050",pch = 19,main = "Predicted vs. test final shelf-life (Using Lasso Model)", ylim = c(0,75), xlim = c(0,75),
     xlab = "Test final shelf-life", ylab= "Predicted final shelf-life")
abline(lm(lasso.pred~y.test),lty = 2,lwd = 2,col = "red")
text(x = 65,y = 26,labels = paste0("RMSE = ",round(RMSE,4),"\n","R-squared = ",round(Rsquare,4)))





summary(y.test)



#We can also see the impact of different lambdas in the estimated coefficients. 
#When lambda is very high, all the coefficients are shrunk exactly to zero.
plot(cv.lambda.lasso$glmnet.fit, 
     "lambda", label=FALSE)


#See selected variables suggested by lasso:
# Find selected variables
out=glmnet(X[train,],Y[train],alpha =1, lambda=lambda_seq)
lasso.coef=predict(out ,type ="coefficients", s= best_lambda )[1:36,]
lasso.coef








