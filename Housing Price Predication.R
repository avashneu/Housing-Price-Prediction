rm(list = ls())
set.seed(123)

# Loading the packages

library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(reshape2)
library(caret)
library(olsrr)
library(ggpubr)
library(grid)
library(ggplotify)
library(glue)
library(mice)
library(class)
library(lattice)
library(ISLR)
library(ipred)
library(ggpubr)
library(factoextra)

# Loading the dataset

data <- fread('Data Group 21.csv')

ncol(data)
nrow(data)

head(data)

# Checking for columns with NA's

housing_prices <- data.table(data)
housing_prices

Filter(housing_prices[,lapply(.SD, is.na)][,lapply(.SD, sum)], f = function(x) {x > 0})

# Reducing the columns with lots of missing values
ncol(housing_prices)

housing_prices[,LotFrontage := NULL][,Alley := NULL][,FireplaceQu := NULL][,GarageType := NULL][,GarageYrBlt:= NULL][,GarageFinish:=NULL][,GarageQual:=NULL][,GarageCond:=NULL][,PoolQC:=NULL][,Fence:=NULL][,MiscFeature:= NULL]#[,Id:=NULL]

ncol(housing_prices)

Filter(housing_prices[,lapply(.SD, is.na)][,lapply(.SD, sum)], f = function(x) {x > 0})

# Removing 'NAs' as the loss in data is very minimal

housing_prices <- na.omit(housing_prices)

# Checking for continuous variables

continuous_variables <- colnames(Filter(housing_prices[,lapply(.SD, typeof)],f = function(x) {x == "integer"}))

continuous_variables

# Checking for columns with characters

Filter(housing_prices[,lapply(.SD, typeof)],f = function(x) {x == "character"})

# Plot Variable function for continuous variables

plt_variables <-function(variable_name,independent_variable,dependent_variable){
  
  plt1 <-  ggplot() + geom_point(aes(x = independent_variable, y = dependent_variable), color = 'green', size = 0.5, alpha = 1) + geom_smooth(aes(x = independent_variable, y = dependent_variable), method = 'lm',formula = y ~ x,se = F,size = 0.5, color = "red", na.rm = T)  + theme_bw()
  
  plt2 <-  ggplot() + geom_density(aes(x = independent_variable), color = 'blue', size = 1) + theme_bw()
  
  plt3 <-  ggplot() + geom_boxplot(aes(y = independent_variable), color = 'red', size = 0.5, alpha = 0.5) 
  
  plt <- grid.arrange(plt1, plt2, plt3 ,layout_matrix = rbind(c(1,1,1,2,2),c(1,1,1,3,3)), top = glue("Independent_Variable is {variable_name}."))
  
  return(plt) 
  
}

for (val in continuous_variables[2:36]) {
  x <- as.data.frame(select(housing_prices,!!!val))
  dev.new()
  plt_variables(val,x[,1],housing_prices$SalePrice)
}
dev.off()

# Correlation plot for continuous variables

correlation <- cor(select(housing_prices, !!!continuous_variables))

cor_data <- data.table(melt(correlation,keep.rownames = F))
cor_data

set_names(cor_data,nm = c("var1","var2","Correlation"))

cor_data %>% ggplot(aes(Var1,Var2,fill = value ,label = round(value,2))) + geom_tile(alpha = 1, color = 'white')+scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + geom_label(label.padding = unit(0, "lines"), label.size = 0) + theme(axis.text.x = element_text(angle = 90,size = 10), axis.text.y = element_text(size = 10)) 


## Feature Selection

ols_step_forward_p(lm(housing_prices[1:1000,],formula = SalePrice~.,),steps = 20)

# Basing on the Forward feature selection algorithm, correlation plot (to check for multi-collinearity) and significant features from initial model the following features are selected.

# "OverallQual" ,"GrLivArea","BsmtFinSF1","GarageCars","MSSubClass","YearBuilt" # "OverallCond","BedroomAbvGr","LotArea","MasVnrArea","BsmtFullBath"            # "TotRmsAbvGrd","ScreenPorch","WoodDeckSF","YearRemodAdd","TotalBsmtSF"        # "KitchenAbvGr","Fireplaces","PoolArea","FullBath"

# Multiple Linear model 1

reg_model <- lm(housing_prices[,c("Id","OverallQual" ,"GrLivArea","BsmtFinSF1","GarageCars","MSSubClass","YearBuilt","OverallCond","BedroomAbvGr","LotArea","MasVnrArea","BsmtFullBath","TotRmsAbvGrd","ScreenPorch","WoodDeckSF","YearRemodAdd","TotalBsmtSF","KitchenAbvGr","Fireplaces","PoolArea","FullBath","SalePrice")],formula = log(SalePrice)~.,)
summary(reg_model)


## Transformation of Variables

# outlier Treatment for LotArea

housing_prices <- housing_prices[housing_prices$LotArea %between% c(exp(8.5), exp(10)),]

# Plotting after outlier treatment using the created function
plt_variables("LotArea",housing_prices$LotArea,housing_prices$SalePrice)

## Separating Categorical and continuous features

# Names of categorical features
categorical_variables <- colnames(housing_prices)[(colnames(housing_prices) %in% continuous_variables)==FALSE]

# categorical Data
housing_prices_part <- housing_prices[,.SD,.SDcols = categorical_variables]

# Continuous Data
housing_prices_part2 <- housing_prices[,.SD,.SDcols = continuous_variables]

# Log transformation on continuous data
housing_prices_part2 <- housing_prices_part2[,lapply(.SD,log),.SDcols = continuous_variables[2:36]]

housing_prices_part2[,(continuous_variables[2:36]) := replace(.SD,.SD == -Inf, 0),.SDcols = continuous_variables[2:36]]

# Checking the Number of columns in the dataset before transformation
ncol(housing_prices)

# Recreating the dataset with transformed features
housing_prices <- cbind(housing_prices[,c("Id")],housing_prices_part,housing_prices_part2)

# Checking the number of columns in the dataset after transformation (Data check)
ncol(housing_prices)


## Reducing the test set to selected features and Dependent Variable
# Note 'Id' is a reference column not a feature

housing_prices <- housing_prices[,c("Id","OverallQual" ,"GrLivArea","BsmtFinSF1","GarageCars","MSSubClass","YearBuilt","OverallCond","BedroomAbvGr","LotArea","MasVnrArea","BsmtFullBath","TotRmsAbvGrd","ScreenPorch","WoodDeckSF","YearRemodAdd","TotalBsmtSF","KitchenAbvGr","Fireplaces","PoolArea","FullBath","SalePrice")]

# Creating dummy variables

dmy <- dummyVars(housing_prices, formula = ~.)
data_dummyvars <- data.table(predict(dmy, newdata = housing_prices))

## Creating Train and Test Sets (80 - 20 split)

training <- createDataPartition(data_dummyvars$SalePrice,p = 0.80, list=F)

train_new = data_dummyvars[training,]
test_new = data_dummyvars[-training,]

## Retraining model with selected and Transformed features
# Model 2
model <- lm(train_new,formula = SalePrice~.,)
summary(model)

# predicting sales Price on test set
test_new$SalePrice_predicted_dlr <- exp(predict(model, newdata = test_new))

## Clustering data using k-means
# Model 3
data_cluster <- data_dummyvars

# K-means clustering
clusters <- kmeans(housing_prices_part2[,c("LotArea","GrLivArea")],centers = 3)

# Plotting the clusters
fviz_cluster(clusters, data = housing_prices_part2[,c("LotArea","GrLivArea")],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# Appending the class of each data point
data_cluster$cluster <- clusters$cluster

# Re-splitting the clustered data into test and train
## the test and train set are kept same for both models
train_cluster <- data_cluster[training,]
test_cluster <- data_cluster[-training]


## Multiple Linear Regression  on Cluster 1

model_cls1 = lm(train_cluster[train_cluster$cluster == 1,-c("cluster","Id")], formula = SalePrice ~. )


summary(model_cls1)

# predicting the sale Price for cluster 1 data
SalePricecls1 <- exp(predict(model_cls1, newdata = test_cluster[test_cluster$cluster == 1,]))

# Appending the predicted Sale Price to dataset
test_cluster1 <- test_cluster[test_cluster$cluster == 1,] 
test_cluster1$SalePrice_predictedCls <- SalePricecls1


# Multiple Linear regression on Cluster 2

model_cls2 = lm(train_cluster[train_cluster$cluster == 2, -c("cluster","Id")], formula = SalePrice ~. )
summary(model_cls2)

# predicting Sale Price for Cluster 2
SalePricecls2 <- exp(predict(model_cls2, newdata = test_cluster[test_cluster$cluster == 2,]))

# Appending the Predicted Sale Price
test_cluster2 <- test_cluster[test_cluster$cluster == 2,] 
test_cluster2$SalePrice_predictedCls <- SalePricecls2

# Multiple Linear regression on Cluster 3

model_cls3 = lm(train_cluster[train_cluster$cluster == 3, -c("cluster","Id")], formula = SalePrice ~. )
summary(model_cls3)

# Predicting the sale price for cluster 3
SalePricecls3 <- exp(predict(model_cls3, newdata = test_cluster[test_cluster$cluster == 3,]))

# Appending the predicted sale price 
test_cluster3 <- test_cluster[test_cluster$cluster == 3,] 
test_cluster3$SalePrice_predictedCls <- SalePricecls3

# Joining the data from all the clusteres for comparision
test_saleprice_cls <- rbind(test_cluster1,test_cluster2,test_cluster3)

# Appending the Sale Price Predicted using Clustering + Regression back to the test dataset
test_new <- test_new[test_saleprice_cls[,c("Id","SalePrice_predictedCls")], on = "Id"]

# Transforming the actual sale price to original values 
## Due log transformation on SalePrice 

test_new$SalePrice <- exp(test_new$SalePrice)

# Appending the cluster data to test set
test_new <- test_new[test_cluster[,c("Id","cluster")], on = "Id"]

## Results Evaluation using MAE and RMSE

# MAE on cluster 3 data for Model 2 (Variable Transformation + Regression)
sum(abs(test_new[test_new$cluster == 3,]$SalePrice_predicted_dlr - test_new[test_new$cluster == 3,]$SalePrice))/nrow(test_new)

# MAE on cluster 3 data for Model 3 (Clustering + regression) 
sum(abs(test_new[test_new$cluster == 3,]$SalePrice_predictedCls - test_new[test_new$cluster == 3,]$SalePrice))/nrow(test_new)

# MAE on cluster 2 data for Model 2 (Variable Transformation + Regression)
sum(abs(test_new[test_new$cluster == 2,]$SalePrice_predicted_dlr - test_new[test_new$cluster == 2,]$SalePrice))/nrow(test_new)

# MAE on cluster 2 data for Model 3 (Clustering + regression)
sum(abs(test_new[test_new$cluster == 2,]$SalePrice_predictedCls - test_new[test_new$cluster == 2,]$SalePrice))/nrow(test_new)

# MAE on cluster 1 data for Model 2 (Variable Transformation + Regression)
sum(abs(test_new[test_new$cluster == 1,]$SalePrice_predicted_dlr - test_new[test_new$cluster == 1,]$SalePrice))/nrow(test_new)

# MAE on cluster 1 data for Model 3 (Clustering + regression)
sum(abs(test_new[test_new$cluster == 1,]$SalePrice_predictedCls - test_new[test_new$cluster == 1,]$SalePrice))/nrow(test_new)

# MAE on complete test data for Model 2 (Variable Transformation + Regression)
sum(abs(test_new$SalePrice_predicted_dlr - test_new$SalePrice))/nrow(test_new)

# MAE on complete test data for Model 3 (Clustering + regression)
sum(abs(test_new$SalePrice_predictedCls - test_new$SalePrice))/nrow(test_new)

# RMSE on Complete test data for Model 2 (Variable Transformation + Regression)
(sum((test_new$SalePrice_predicted_dlr - test_new$SalePrice)^2))^0.5/nrow(test_new)

# RMSE on complete test data for Model 3 (Clustering + regression)
(sum((test_new$SalePrice_predictedCls - test_new$SalePrice)^2))^0.5/nrow(test_new)


# Predicted vs Actual plot

test_new %>% ggplot() + geom_line(aes(x= SalePrice, y = SalePrice), color = 'red') + geom_point(aes(x = SalePrice, y = SalePrice_predicted_dlr), color = "green",alpha = 0.5) + geom_point(aes(x = SalePrice, y = SalePrice_predictedCls), color = "orange",alpha = 0.5) + labs(title = "Predicted Vs Actual Sale price",x = "SalePrice - Actual",y = "SalePrice - Predicted") +theme_bw()

## Additional exploration on clusters and Predicted data
# Correlation Plots on Cluster 1

correlation1 <- cor(test_cluster1[,-c("Id","cluster","SalePrice_predictedCls","BsmtFullBath")])

# Correlation data
cor_data <- data.table(melt(correlation1,keep.rownames = F))
cor_data

set_names(cor_data,nm = c("var1","var2","Correlation"))

# Creating correlation plot
cor_data %>% ggplot(aes(Var1,Var2,fill = value ,label = round(value,2))) + geom_tile(alpha = 1, color = 'white')+scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + geom_label(label.padding = unit(0, "lines"), label.size = 0) + theme(axis.text.x = element_text(angle = 90,size = 10), axis.text.y = element_text(size = 10))

# Correlation plot for cluster 2
correlation2 <- cor(test_cluster2[,-c("Id","cluster","SalePrice_predictedCls","PoolArea")])

# Correlation Data
cor_data <- data.table(melt(correlation2,keep.rownames = F))
cor_data

set_names(cor_data,nm = c("var1","var2","Correlation"))

# Creating correlation Plot
cor_data %>% ggplot(aes(Var1,Var2,fill = value ,label = round(value,2))) + geom_tile(alpha = 1, color = 'white')+scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + geom_label(label.padding = unit(0, "lines"), label.size = 0) + theme(axis.text.x = element_text(angle = 90,size = 10), axis.text.y = element_text(size = 10))
