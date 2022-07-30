sup_pri_train  =  read.csv(file="C://Users//Sanskruti Jajoo//Desktop//STUDY//DMA//Lab//sup_pri_data.csv",  header=TRUE,  sep="",  stringsAsFactors  =  FALSE)
sup_pri_test  =  read.csv(file="C://Users//Sanskruti Jajoo//Desktop//STUDY//DMA//Lab//sup_pri_data_val.csv",  header=TRUE,  sep="",  stringsAsFactors  =  FALSE)

productVariation = function (init_price, init_quant, years, a, b){
  sup_pri <- matrix(NA, years, 3)
  sup_pri[1,1] <- 1
  sup_pri[1,2] <- init_price
  sup_pri[1,3] <- init_quant
  
  for (i in 2:years){
    sup_pri[i, 1] <- i
    sup_pri[i, 2] <- sup_pri[i-1, 2] - a *(sup_pri[i-1, 3]-500) # update value of price based on i - 1
    sup_pri[i, 3] <- sup_pri[i-1, 3] + b *(sup_pri[i-1, 2]-100) # update value of quantity based on i - 1
  }
  
  df_sp <- as.data.frame(sup_pri)
  colnames(df_sp) <- c("Year", "Price", "Supply")
  return(df_sp)
}

result1 <- productVariation(100, 500, 25, 0.1, 0.2)
result2 <- productVariation(200, 500, 25, 0.1, 0.2)
result3 <- productVariation(100, 600, 25, 0.1, 0.2)
result4 <- productVariation(100, 400, 25, 0.1, 0.2)
library(ggplot2)

ggplot(result2)+
  labs(color = "Variables") +
  geom_line(aes(x=Year, y=Supply, col="Quantity")) +
  geom_line(aes(x=Year, y=Price, col="Price"))


# Q2.1 Explore the correlation between Price and Supply. Be careful, 
# the observations are not paired in the dataset. 
# It might be wise to measure the correlation between averages per year instead.

ggplot(result2)+
  labs(color = "Variables") +
  geom_line(aes(x=Year, y=Supply, col="Quantity")) +
  geom_line(aes(x=Year, y=Price, col="Price"))

cor(x=aggregate(sup_pri_train$Price, list(sup_pri_train$Year), FUN=mean)$x,y=aggregate(sup_pri_train$Supply, list(sup_pri_train$Year), FUN=mean)$x)

primary.model <- lm(Price ~ Supply, data = sup_pri_train)
summary(primary.model)

# Q2.3 Formulate secondary models. You do not have access to extraneous 
# variables or confounders,so you can instead play with the complexity 
# of the functional form of your formula. 
# For example,inserting quadratic and cubic terms:

secondary.model.1 <- lm(Price ~ Supply + I(Supply^2) + I(Supply^3), data
                        = sup_pri_train)
summary(secondary.model.1)

sup_pri_train$Decade <- as.integer(sup_pri_train$Year / 10)
secondary.model.2 <- lm(Price ~ Supply + I(Decade^3), data = sup_pri_train)
summary(secondary.model.2)

install.packages('Metrics')
library(Metrics)

sup_pri_train$primary_model=predict(primary.model, sup_pri_train)
sup_pri_train$model1=predict(secondary.model.1, sup_pri_train)
sup_pri_train$model2=predict(secondary.model.2, sup_pri_train)

rmse(sup_pri_train$Price, sup_pri_train$primary_model)
rmse(sup_pri_train$Price, sup_pri_train$model1)
rmse(sup_pri_train$Price, sup_pri_train$model2)

install.packages("groupdata2")

library(groupdata2)
library(dplyr)

x= fold(
  sup_pri_train,
  k = 5,
  method = 'n_dist'
)

sup_pri_train$folds=x$.folds


k_fold_validation=function(foldNo){
  df_val=sup_pri_train[sup_pri_train$folds==foldNo,]
  model3 <- lm(Price ~ Supply + I(Decade^3), data = df_val)
  df_val$model3=predict(model3, df_val)
  rmse_val=rmse(sup_pri_train$Price, df_val$model3)
  print(foldNo)
  print(rmse_val)
  
  model4 <- lm(Price ~ Supply + I(Supply^2) + I(Supply^3), data = df_val)
  df_val$model4=predict(model4, df_val)
  rmse_val=rmse(sup_pri_train$Price, df_val$model4)
  print(foldNo)
  print(rmse_val)
  print(summary(model3)$coefficients[,4])
  print(summary(model4)$coefficients[,4])
}
x <- c(1,2,3,4,5)
for(val in x)k_fold_validation(val)




