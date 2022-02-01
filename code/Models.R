library(dplyr)
library(stringr)
library(ggplot2)
library(purrr)
library(car)
library(mgcv)
library(plotROC)
library(randomForest)
library(caret)
library(xgboost)
library(deldir)
library(cutpointr)
library(InformationValue)
library(nflfastR)


######
##Read in preprocessed tracking/plays_punts .csv files, if necessary
tracking_all <- read.csv("tracking_all.csv")
tracking_subset_first <- read.csv("tracking_subset.first.csv")
tracking_subset_end <- read.csv("tracking_subset.end.csv")
plays_punts <- read.csv("plays_punts.csv")

#Train/test split: 2020 as a holdout
set.seed(12345)
training_index <- which(plays_punts$season != 2020)
training <- plays_punts[training_index,]
test <- plays_punts[-training_index,]  

#Variables of model interest (chosen based on domain knowledge)
model_vars_returned <- c("area_voronoi", "dist_to_ball", "gun_vise_diff", "hangTime", "kick_type_bin", "kick_direction_wrong",
                         "kickLength", "mean_control", "kickReturnYardage", "num_rushers_bin", "num_safeties_bin",
                         "operationTime", "perc_over", "random", "return_direction_wrong",  "sd_control", 
                         "seventyfifth_control", "snapDetail")

######
#RANDOM FOREST
######

#Initial random forest to identify optimal # of trees
rf_cont_1_all <- randomForest(kickReturnYardage ~ . ,data = training %>%
                                select(model_vars_returned), mtry = 5)


plot(rf_cont_1_all) #Levels off near 100

set.seed(123)

#Tune mtry with tuneRF
rf_cont_1_all_tune <- tuneRF(x = training %>%
         select(model_vars_returned, -kickReturnYardage), y = training$kickReturnYardage,
       plot = T, ntreeTry = 100, stepFactor = 0.5)  #Ideal mtry appears to be 5

#Second (and final) random forest model, using ntree of 100 and mtry of 5
rf_cont_2_all <- randomForest(kickReturnYardage ~., data = training %>%
               select(model_vars_returned), ntree = 100, mtry = 5, importance = T)

#Basic variable importance plot
varImpPlot(rf_cont_2_all)

#OOB performance on training data for random forest
rf_all_predict <- predict(rf_cont_2_all)

#RMSE of Random forest model on training data
rmse_rf_all <- sqrt(mean(rf_all_predict - training$kickReturnYardage)^2)


######
#Extreme Gradient Boosting
######

#Create train and test model matrix objects, dummifying (1/0) any categorical variables.
train_x_xgb <- model.matrix(kickReturnYardage ~ ., data = training %>%
                             select(model_vars_returned))[, -1]
train_y_xgb <- training$kickReturnYardage

test_x_xgb <- model.matrix(kickReturnYardage ~ ., data = test %>%
                             select(model_vars_returned))[, -1]
plays_punts_x_xgb <- model.matrix(kickReturnYardage ~ ., data = plays_punts %>%
                                   select(model_vars_returned))[, -1]

#10-fold CV to find ideal nrounds (appears to be 6)
set.seed(123)
xgb_1 <- xgb.cv(data = train_x_xgb, label = train_y_xgb, subsample = 0.5, nrounds = 100, nfold = 10)

#Tuning grid for caret
tune_grid <- expand.grid(
  nrounds = 6,
  eta = c(0.1, 0.15, 0.2, 0.25, 0.3),
  max_depth = c(1:10),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.25, 0.5, 0.75, 1)
)

#Tune XGBoost
xgb.caret <- train(x = train_x_xgb, y = train_y_xgb,
                        method = "xgbTree",
                        tuneGrid = tune_grid,
                        trControl = trainControl(method = 'cv', number = 10))

#Final tuned XGBoost model
xgb_final <- xgboost(data = train_x_xgb, label = train_y_xgb, subsample = 0.5, nrounds = 6, eta = 0.25, max_depth = 5)

#Variable importance for XGBoost
xgb_imp <- xgb.importance(feature_names = colnames(train_x_xgb),model = xgb_final)
xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x_xgb), model = xgb_final))

#XGBoost Predictions and RMSE on training data
xgb_all_predict <- predict(xgb_final, train_x_xgb)
rmse_xgb_all <- sqrt(mean(xgb_all_predict - training$kickReturnYardage)^2)

#Creating an ensemble prediction, averaging the random forest and XGBoost prediction
rf_xgb_mean <- (xgb_all_predict + rf_all_predict)/2
rmse_rf_xgb_mean <- sqrt(mean(rf_xgb_mean - training$kickReturnYardage)^2)

### At this juncture, given that the performance of all models appeared to be relatively similar, I opted to use 
  #the ensemble model. 
###

#2020 predictions using ensemble model.
rf_xgb_mean_test <- (predict(xgb_final, test_x_xgb)  + predict(rf_cont_2_all, test))/2
rf_mean_test <- (predict(rf_cont_2_all, test)) 
xgb_mean_test <- (predict(xgb_final, test_x_xgb))


#Final RMSE, MAE, MAPE
rmse_ensemble <- sqrt(mean(rf_xgb_mean_test - test$kickReturnYardage)^2)
rmse_rf <- sqrt(mean(rf_mean_test - test$kickReturnYardage)^2)
rmse_xgb <- sqrt(mean(xgb_mean_test - test$kickReturnYardage)^2)

mae_ensemble <- mean(abs(rf_xgb_mean_test - test$kickReturnYardage)) 
mae_rf <- mean(abs(rf_mean_test - test$kickReturnYardage)) 
mae_xgb <- mean(abs(xgb_mean_test - test$kickReturnYardage)) 

mape_ensemble <- (1/length(test$kick_contact_bin))* mean(abs(rf_xgb_mean_test - test$kickReturnYardage)) * 100
mape_rf <- (1/length(test$kick_contact_bin))*mean(abs(rf_mean_test - test$kickReturnYardage)) * 100
mape_xgb <- (1/length(test$kick_contact_bin))*mean(abs(xgb_mean_test - test$kickReturnYardage))  * 100

results_df <- data.frame('Model' = c('Ensemble', 'RF', 'XGB'),
                         'RMSE' = c(rmse_ensemble, rmse_rf, rmse_xgb),
                         'MAE' = c(mae_ensemble, mae_rf, mae_xgb),
                         'MAPE' = c(mape_ensemble, mape_rf, mape_xgb))
write.csv(results_df, 'ensemble_model_results.csv')

#Add expected return to plays_punts
plays_punts$expected_return <- (predict(xgb_final, plays_punts_x_xgb)  + predict(rf_cont_2_all, plays_punts))/2

write.csv("plays_punts_with_expected.csv")


####################
######## Model Importance:
###################

## Random Forest
varImpPlot(rf_cont_2_all)
rf_imp <- data.frame(imp = rf_cont_2_all$importance[,1], var = rownames(rf_cont_2_all$importance))
rf_imp <- rf_imp %>%
  mutate(fill = case_when(imp > rf_imp$imp[rf_imp$var == "random"] ~ "More Imp. than Random",
                          TRUE ~ "Less Imp. than Random"))
rf_imp$var <- c("Voronoi Area", "Dist. to Ball", "# Gunners - # Vises",
                "Punt Hangtime", "Punt Type", "Punt Direction Wrong", 
                "Mean Control", "Punt Length",
                "# of Rushers", "# of Safeties", "Operation Time", "% > 0.5",
                "Random Variable", "Return Direction Wrong", "Std. Dev. Control",
                "75th Percentile Control", "Snap Detail")

rf_imp_plot <- ggplot(rf_imp, aes(reorder(var, imp), imp, fill = fill)) + geom_bar(stat = "identity") +
  coord_flip() + scale_fill_manual(values = c("lightgrey", "#A7C7E7"), guide = "none") + theme_minimal() +
  ylab("Importance (% Inc. MSE)") + xlab("Feature") + ggtitle("",subtitle = "Random Forest")

xgb_imp <- xgb_imp %>%
  mutate(fill = case_when(Gain > xgb_imp$Gain[xgb_imp$Feature == "random"] ~ "More Imp. than Random",
                          TRUE ~ "Less Imp. than Random"))

xgb_imp$Feature <- c("Voronoi Area", "Dist. to Ball", "Return Direction Wrong",
                     "Random Variable", "Std. Dev. Control", "Operation Time",
                     "Punt Hangtime", "% > 0.5", "Kick Length", "75th Percentile Control",
                     "# Gunners - # Vises", "Mean Control", "# of Rushers", "Kick Type")
xgb_imp_plot <- ggplot(xgb_imp, aes(reorder(Feature, Gain), Gain, fill = fill)) + geom_bar(stat = "identity") +
  coord_flip() + scale_fill_manual(values = c("lightgrey", "#A7C7E7"), guide = "none") + theme_minimal() +
  xlab(" ") + ylab("Importance (Gain)") + ggtitle("",subtitle = "XGBoost") 


##### FINAL FEATURE IMPORTANCE GRAPH
feature_importance <- plot_grid(rf_imp_plot, xgb_imp_plot) + ggtitle("Feature Importance")


