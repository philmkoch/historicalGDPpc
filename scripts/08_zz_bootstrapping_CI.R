
data_misc <- data

data_misc$oos_pred <- NA
data_misc$oos_pred_lower <- NA
data_misc$oos_pred_upper <- NA
data_misc$oos_pred_level <- NA
data_misc$oos_pred_level_lower <- NA
data_misc$oos_pred_level_upper <- NA

bootstrap_iteration <- function(u, fulldata_sub, oosdata, startcolumn, training_grid_modelperformance, cctrl1) {
  resampled_indices <- sample(1:nrow(fulldata_sub), replace = TRUE)
  fulldata_sub_resampled <- fulldata_sub[resampled_indices,]
  
  test_class_cv_model <- train(fulldata_sub_resampled[,startcolumn:ncol(fulldata_sub_resampled)], log10(fulldata_sub_resampled$GDPpc), method = "glmnet", 
                               trControl = cctrl1, metric = "RMSE", standardize = TRUE,
                               tuneGrid = training_grid_modelperformance)
  
  best_lambda <- test_class_cv_model$bestTune$lambda
  best_alpha <- test_class_cv_model$bestTune$alpha
  
  best_model <- glmnet(fulldata_sub_resampled[,startcolumn:ncol(fulldata_sub_resampled)], log10(fulldata_sub_resampled$GDPpc), alpha = best_alpha, lambda = best_lambda, standardize = TRUE)
  
  predictions <- predict(best_model, newx = as.matrix(oosdata[,startcolumn:(ncol(oosdata)-6)]))
  
  return(predictions)
}

for(p in 1:5){
  set.seed(p)
  
  # Parallel setup
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, {
    library(glmnet)
    library(caret)
    set.seed(1234)
  })
  
  # Main code
  fulldata_sub <- subset(labeled_data, histperiod == p)
  oosdata <- subset(data_misc, histperiod == p)
  
  if(p == 2){
    update_GDPpc_t0(fulldata_sub, data_misc, labeled_data, 1500, "oos_pred")
    update_GDPpc_t0(oosdata, data_misc, labeled_data, 1500, "oos_pred")
  } else if(p == 3){
    update_GDPpc_t0(fulldata_sub, data_misc, labeled_data, 1750, "oos_pred")
    update_GDPpc_t0(oosdata, data_misc, labeled_data, 1750, "oos_pred")
  } else if(p == 4){
    update_GDPpc_t0(fulldata_sub, data_misc, labeled_data, 1850, "oos_pred")
    update_GDPpc_t0(oosdata, data_misc, labeled_data, 1850, "oos_pred")
  } else if(p == 5){
    update_GDPpc_t0(fulldata_sub, data_misc, labeled_data, 1950, "oos_pred")
    update_GDPpc_t0(oosdata, data_misc, labeled_data, 1950, "oos_pred")
  }
  
  cctrl1 <- trainControl(method="cv", number = min(k, nrow(fulldata_sub)))
  
  fulldata_sub <- subset(fulldata_sub, !is.na(diversity_died) & !is.na(diversity) 
                         & !is.na(ubiquity_died) & !is.na(ubiquity)
                         & !is.na(diversity_immigrated) & !is.na(diversity_emigrated)
                         & !is.na(ubiquity_immigrated) & !is.na(ubiquity_emigrated) & !is.na(GDPpc_t0))
  
  # Run bootstrap iterations in parallel
  bootstrap_predictions_list <- parLapply(cl, 1:n_bootstraps, bootstrap_iteration, 
                                          fulldata_sub = fulldata_sub, oosdata = oosdata, 
                                          startcolumn = startcolumn, 
                                          training_grid_modelperformance = training_grid_modelperformance, 
                                          cctrl1 = cctrl1)
  
  # Stop the cluster
  stopCluster(cl)
  
  # Combine the results into a matrix
  bootstrap_predictions <- do.call(cbind, bootstrap_predictions_list)
  bootstrap_predictions <- as.data.frame(bootstrap_predictions)
  
  rownames(bootstrap_predictions) <- rownames(oosdata)
  bootstrap_predictions$mean <- rowMeans(bootstrap_predictions, na.rm=TRUE)
  bootstrap_predictions$lower <- apply(bootstrap_predictions, 1, quantile, probs=bootstrap_CI[1], na.rm=TRUE)
  bootstrap_predictions$upper <- apply(bootstrap_predictions, 1, quantile, probs=bootstrap_CI[2], na.rm=TRUE)
  
  # Update data_misc with the bootstrap predictions
  data_misc$oos_pred <- ifelse(is.na(data_misc$oos_pred), vlookup(data_misc$ID, bootstrap_predictions, lookup_column = "rownames", result_column = "mean"), data_misc$oos_pred)
  data_misc$oos_pred_lower <- ifelse(is.na(data_misc$oos_pred_lower), vlookup(data_misc$ID, bootstrap_predictions, lookup_column = "rownames", result_column = "lower"), data_misc$oos_pred_lower)
  data_misc$oos_pred_upper <- ifelse(is.na(data_misc$oos_pred_upper), vlookup(data_misc$ID, bootstrap_predictions, lookup_column = "rownames", result_column = "upper"), data_misc$oos_pred_upper)
  
}

data_misc$oos_pred_level <- 10^data_misc$oos_pred
data_misc$oos_pred_level_lower <- 10^data_misc$oos_pred_lower
data_misc$oos_pred_level_upper <- 10^data_misc$oos_pred_upper