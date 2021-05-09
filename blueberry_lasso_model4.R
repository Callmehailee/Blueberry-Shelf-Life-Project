#Change to your own path name here
blueberry_dat <- read.csv("C:/Users/17038/Desktop/STAT5020W/Project/dataset/FirstYearBlueberry_Complete_Updated.csv")

Year_factor <- as.factor(blueberry_dat$Year)
blueberry_dat$Year <- Year_factor

blueberry_dat_std <- cbind(blueberry_dat[,c(1,2,4,5,16)],as.data.frame(scale(blueberry_dat[,c(7:13)])))
head(blueberry_dat_std)
attach(blueberry_dat_std)
#View(blueberry_dat_std)

#------------------Lasso Regression Model---------------------#
library(glmnet)
set.seed(5020) 

# Divide the dataset into 2/3 for training and 1/3 for testing 
train=sample(1: nrow(blueberry_dat_std), nrow(blueberry_dat_std)*2/3)
#train.data <- blueberry_dat_std[train,]
test=(- train)
#test.data <- blueberry_dat_std[test,]


X <- model.matrix(Final.Shelf.Life..Days. ~ Year + Farm + Type + Cultivar + DefectFree + Compression + Pucture + TSS + pH + TA + 
                    Weight  + I(DefectFree^2) + I(Compression^2) + I(Pucture^2) + I(TSS^2) + I(pH^2) + I(TA^2) + 
                    I(Weight^2) + (DefectFree+Compression+Pucture+TSS+pH+TA+Weight)^2, data = blueberry_dat_std)[,-1]

#Response variable:
Y <- blueberry_dat_std[,"Final.Shelf.Life..Days."]
y.test=Y[test]


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


# Fit lasso with best lambda
lasso.pred=predict(lambda.lasso ,s=best_lambda ,newx=X[test ,]) #using testing dataset
# 

#Compute RMSE for model prediction performance evaluation
RMSE <- sqrt(mean((lasso.pred -y.test)^2))  #when split the data into 2/3 for training and 1/3 for testing, the results get better

summary(y.test)
range(y.test)



#R-squared/RMSE/plots"
Rsquare <- cor(y.test, lasso.pred)^2
round
plot(y.test,lasso.pred,col="#00000050",pch = 19,main = "Predicted vs. test final shelf-life (Using Lasso Model)", ylim = c(0,75), xlim = c(0,75),
     xlab = "Test final shelf-life", ylab= "Predicted final shelf-life")
abline(0,1, lty = 2, lwd = 2, col = "red")
text(x = 65,y = 26,labels = paste0("RMSE = ",round(RMSE,4),"\n","R-squared = ",round(Rsquare,4)))


plot(y.test,lasso.pred,col="#00000050",pch = 19,main = "Predicted vs. test final shelf-life (Using Lasso Model)", ylim = c(0,75), xlim = c(0,75),
     xlab = "Test final shelf-life", ylab= "Predicted final shelf-life", cex.lab = 1.5, cex.main = 2)
abline(0,1, lty = 2, lwd = 2, col = "red")
text(x = 63,y = 24,labels = paste0("RMSE = ",round(RMSE,4),"\n","R-squared = ",round(Rsquare,4),"\n", "lambda = ",round(best_lambda)), cex = 2)



plot(cv.lambda.lasso$glmnet.fit, 
     "lambda", label=FALSE)


#See selected variables suggested by lasso:
# Find selected variables
out=glmnet(X[train,],Y[train],alpha =1, lambda=lambda_seq)
lasso.coef=predict(out ,type ="coefficients", s= best_lambda )
lasso.coef


#Plot the residuals:
library(plotmo)
plotres(out, which = 4, caption = "Residual vs.Fitted and QQ plot")


