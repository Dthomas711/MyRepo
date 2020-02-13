Ames <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                   header = TRUE,
                   sep = ",")
library(dplyr)
Ames <- Ames %>%
  select(-c(18,19))

fit_all = lm(SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Street + LotShape + LandContour + LotConfig +
               LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle +
               YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + 
               MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond + 
               BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF +
               TotalBsmtSF +Heating +HeatingQC + CentralAir +Electrical + X1stFlrSF + X2ndFlrSF +
               LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr +
               KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType +
               GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive +
               WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + MiscVal +
               MoSold + YrSold + SaleType + SaleCondition, data=Ames)

fit_start = lm(SalePrice ~ 1, data=Ames)
step(fit_start, direction = "forward", scope=formula(fit_all))
fit_2 = lm(SalePrice ~ ExterQual, data=Ames)
step(fit_2, direction = "forward", scope=formula(fit_all))
fit_3= lm(SalePrice ~ ExterQual + GrLivArea, data=Ames) 
step(fit_3, direction = "forward", scope=formula(fit_all))
fit_4= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood, data=Ames) 
step(fit_4, direction = "forward", scope=formula(fit_all)) 
fit_5= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood + BsmtFinSF1, data=Ames) 
step(fit_5, direction = "forward", scope=formula(fit_all))
fit_6= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 + TotalBsmtSF, data=Ames) 
step(fit_6, direction = "forward", scope=formula(fit_all)) 
fit_7= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 + TotalBsmtSF + BsmtQual, data=Ames) 
step(fit_7, direction = "forward", scope=formula(fit_all)) 
fit_8= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 +  TotalBsmtSF + BsmtQual + BsmtExposure, data=Ames) 
step(fit_8, direction = "forward", scope=formula(fit_all)) 
fit_9= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 +  TotalBsmtSF + BsmtQual + BsmtExposure+ KitchenQual, data=Ames) 
step(fit_9, direction = "forward", scope=formula(fit_all)) 
fit_10= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 +  TotalBsmtSF + BsmtQual + BsmtExposure + KitchenQual + SaleCondition, data=Ames) 
step(fit_10, direction = "forward", scope=formula(fit_all))
fit_11= lm(SalePrice ~  ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 +  TotalBsmtSF + BsmtQual + BsmtExposure + KitchenQual + SaleCondition + LotFrontage, data=Ames) 
step(fit_11, direction = "forward", scope=formula(fit_all)) 
fit_12= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 +  TotalBsmtSF + BsmtQual + BsmtExposure + KitchenQual + SaleCondition + LotFrontage+ LotConfig, data=Ames) 
step(fit_12, direction = "forward", scope=formula(fit_all)) 
fit_13= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 +  TotalBsmtSF + BsmtQual + BsmtExposure + KitchenQual + SaleCondition + LotFrontage+ LotConfig + CentralAir, data=Ames) 
step(fit_13, direction = "forward", scope=formula(fit_all))
fit_14= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 +  TotalBsmtSF + BsmtQual + BsmtExposure + KitchenQual + SaleCondition + LotFrontage + LotConfig + CentralAir +  GarageCars, data=Ames) 
step(fit_14, direction = "forward", scope=formula(fit_all)) 
fit_15= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 +  TotalBsmtSF + BsmtQual + BsmtExposure + KitchenQual + SaleCondition + LotFrontage + LotConfig + CentralAir +  GarageCars + LotArea, data=Ames) 
step(fit_15, direction = "forward", scope=formula(fit_all)) 
final_fit = lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood + BsmtFinSF1 + TotalBsmtSF + BsmtQual + BsmtExposure + KitchenQual+ SaleCondition + LotFrontage + LotConfig + CentralAir + GarageCars + LotArea + LandContour, data = Ames)


fits <- list(fit_start, fit_2, fit_3, fit_4, fit_5, fit_6, fit_7, fit_8, fit_9, fit_10, fit_11, fit_12, fit_13, fit_14, fit_15, final_fit)
RMSE <- list()
complexity <- list(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)

for(i in fits){
  rss1 <- c(crossprod(i$residuals))
  mse1 <- rss1 / length(i$residuals)
  rmse1 <- sqrt(mse1)
  RMSE <- append(RMSE,rmse1)
}

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

plot(complexity,RMSE,main = "Complexity progression", xlab = "Complexity",ylab = "RMSE")

num_obs = nrow(Ames)

get_complexity = function(model) {
  length(coef(model)) - 1
}

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = Ames[train_index, ]
test_data = Ames[-train_index, ]

train_rmse = c(get_rmse(fit_start, train_data, "SalePrice"),
               get_rmse(fit_2, train_data, "SalePrice"),
               get_rmse(fit_3, train_data, "SalePrice"),
               get_rmse(fit_4, train_data, "SalePrice"),
               get_rmse(fit_5, train_data, "SalePrice"),
               get_rmse(fit_6, train_data, "SalePrice"),
               get_rmse(fit_7, train_data, "SalePrice"),
               get_rmse(fit_8, train_data, "SalePrice"),
               get_rmse(fit_9, train_data, "SalePrice"),
               get_rmse(fit_10, train_data, "SalePrice"),
               get_rmse(fit_11, train_data, "SalePrice"),
               get_rmse(fit_12, train_data, "SalePrice"),
               get_rmse(fit_13, train_data, "SalePrice"),
               get_rmse(fit_14, train_data, "SalePrice"),
               get_rmse(fit_15, train_data, "SalePrice"),
               get_rmse(final_fit, train_data, "SalePrice"))

test_rmse = c(get_rmse(fit_start, test_data, "SalePrice"),
              get_rmse(fit_2, test_data, "SalePrice"),
              get_rmse(fit_3, test_data, "SalePrice"),
              get_rmse(fit_4, test_data, "SalePrice"),
              get_rmse(fit_5, test_data, "SalePrice"),
              get_rmse(fit_6, test_data, "SalePrice"),
              get_rmse(fit_7, test_data, "SalePrice"),
              get_rmse(fit_8, test_data, "SalePrice"),
              get_rmse(fit_9, test_data, "SalePrice"),
              get_rmse(fit_10, test_data, "SalePrice"),
              get_rmse(fit_11, test_data, "SalePrice"),
              get_rmse(fit_12, test_data, "SalePrice"),
              get_rmse(fit_13, test_data, "SalePrice"),
              get_rmse(fit_14, test_data, "SalePrice"),
              get_rmse(fit_15, test_data, "SalePrice"),
              get_rmse(final_fit, test_data, "SalePrice"))

model_complexity = sapply(fits, get_complexity)

plot(model_complexity, train_rmse, type = "b",
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity, test_rmse, type = "b", col = "darkorange")

fit_1.1 = lm(SalePrice ~ 1, data=Ames)

fit_2.2 = lm(SalePrice ~ ExterQual, data=Ames)

fit_3.2= lm(SalePrice ~ ExterQual + GrLivArea, data=Ames) 

fit_4.2= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood, data=Ames) 

fit_5.2= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood + BsmtFinSF1, data=Ames) 

fit_6.2= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 + TotalBsmtSF, data=Ames) 

fit_7.2= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 + TotalBsmtSF + BsmtQual, data=Ames) 

fit_8.2= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 +  TotalBsmtSF + BsmtQual + BsmtExposure, data=Ames) 

fit_9.2= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 +  TotalBsmtSF + BsmtQual + BsmtExposure+ KitchenQual, data=Ames) 

fit_10.2= lm(SalePrice ~ ExterQual + GrLivArea + Neighborhood  + BsmtFinSF1 +  TotalBsmtSF + BsmtQual + BsmtExposure + KitchenQual + SaleCondition, data=Ames) 



fits1 <- list(fit_1.1, fit_2.2, fit_3.2, fit_4.2, fit_5.2, fit_6.2, fit_7.2, fit_8.2, fit_9.2, fit_10.2)
RMSE1 <- list()
complexity1 <- list(0,1,2,3,4,5,6,7,8,9,10,11,12,13)

for(i in fits){
  rss1.2 <- c(crossprod(i$residuals))
  mse1.2 <- rss1.2 / length(i$residuals)
  rmse1.2 <- sqrt(mse1.2)
  RMSE1 <- append(RMSE1,rmse1.2)
}

rmse1 = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

get_rmse1 = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

plot(complexity1,RMSE1,main = "Complexity progression", xlab = "Complexity",ylab = "RMSE")

num_obs1 = nrow(Ames)

get_complexity1 = function(model) {
  length(coef(model)) - 1
}

train_index1 = sample(num_obs1, size = trunc(0.50 * num_obs1))
train_data1 = Ames[train_index1, ]
test_data1 = Ames[-train_index1, ]

train_rmse1 = c(get_rmse1(fit_1.1, train_data1, "SalePrice"),
               get_rmse1(fit_2.2, train_data1, "SalePrice"),
               get_rmse1(fit_3.2, train_data1, "SalePrice"),
               get_rmse1(fit_4.2, train_data1, "SalePrice"),
               get_rmse1(fit_5.2, train_data1, "SalePrice"),
               get_rmse1(fit_6.2, train_data1, "SalePrice"),
               get_rmse1(fit_7.2, train_data1, "SalePrice"),
               get_rmse1(fit_8.2, train_data1, "SalePrice"),
               get_rmse1(fit_9.2, train_data1, "SalePrice"),
               get_rmse1(fit_10.2, train_data1, "SalePrice"))

test_rmse1 = c(get_rmse1(fit_1.1, test_data1, "SalePrice"),
              get_rmse1(fit_2.2, test_data1, "SalePrice"),
              get_rmse1(fit_3.2, test_data1, "SalePrice"),
              get_rmse1(fit_4.2, test_data1, "SalePrice"),
              get_rmse1(fit_5.2, test_data1, "SalePrice"),
              get_rmse1(fit_6.2, test_data1, "SalePrice"),
              get_rmse1(fit_7.2, test_data1, "SalePrice"),
              get_rmse1(fit_8.2, test_data1, "SalePrice"),
              get_rmse1(fit_9.2, test_data1, "SalePrice"),
              get_rmse1(fit_10.2, test_data1, "SalePrice"))

model_complexity1 = sapply(fits1, get_complexity1)

plot(model_complexity1, train_rmse1, type = "b",
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity1, test_rmse1, type = "b", col = "darkorange")

