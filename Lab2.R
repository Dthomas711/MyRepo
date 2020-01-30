ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")
Ames <- as.data.frame(ameslist)
Ames <- Ames[, sapply(Ames, class) != "factor"]
newlist <- Ames[, c("FullBath","LotArea", "OverallQual", "YearRemodAdd", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF",
                    "GrLivArea", "TotRmsAbvGrd","GarageArea", "PoolArea","MiscVal", "WoodDeckSF", "Fireplaces",
                    "HalfBath","SalePrice")]
pairs(newlist[,1:12], pch = 19)
newlist <- as.data.frame(newlist)
cor(newlist[,-12])
x <- Ames$SalePrice
y <- Ames$GrLivArea
plot(x, y, pch = 20, frame = TRUE, col = "red")
abline(lm(y ~ x, data = Ames), col = "blue")
costofgarage <- lm(Ames$SalePrice~Ames$GarageArea, data=Ames)
summary(costofgarage)
alllm <- lm(SalePrice~ Id+MSSubClass+LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+YearRemodAdd+
              MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+
              GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+
              TotRmsAbvGrd+Fireplaces+GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF+
              EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+MiscVal+MoSold+YrSold,data = Ames)
summary(alllm)
par(mfrow = c(2,2))
plot(alllm)
dif <- lm(SalePrice~ Id+MSSubClass+LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+YearRemodAdd+
            MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+
            GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+
            TotRmsAbvGrd+Fireplaces+GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF+
            EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+MiscVal+MoSold+YrSold + YearBuilt:YearRemodAdd +
            ScreenPorch*Fireplaces
          ,data = Ames)
summary(dif)
dif2 <- lm(SalePrice~ Id+MSSubClass+LotFrontage+ sqrt(LotArea) +OverallQual+OverallCond+YearBuilt+YearRemodAdd+
             MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+
             log(GrLivArea)+I(BsmtFullBath^2)+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+
             TotRmsAbvGrd+Fireplaces+GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF+
             EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+MiscVal+MoSold+YrSold
           ,data = Ames)
summary(dif2)