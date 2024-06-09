test_data_preds <- test_data
test_data_preds$prediction_abs <- NA

training_data_preds <- training_data
training_data_preds$prediction <- NA
training_data_preds$prediction_abs <- NA

# Function to update GDPpc_t0
update_GDPpc_t0 <- function(data_subset, data, labeled_data, year, column_name_preds) {
  for(m in 1:nrow(data_subset)){
    # GDPpc_t0 already describes available source data of a location at the start of the historical era (see 05_add_SVD_and_ECI.R)
    # Here, we look for alternatives if this is not available
    # First look for an out-of-sample prediction of the location. If unavailable, check for source data of country_0. If unavailable, check for out-of-sample prediction of country_0.
    tempGDP <- vlookup(paste(data_subset$country[m], year, sep="_"), data, lookup_column = "ID2", result_column = column_name_preds)
    if(is.na(tempGDP)){
      tempGDP <- log10(vlookup(paste(data_subset$country_0[m], year, sep="_"), labeled_data, lookup_column = "ID2", result_column = "GDPpc"))
    }
    if(is.na(tempGDP)){
      tempGDP <- vlookup(paste(data_subset$country_0[m], year, sep="_"), data, lookup_column = "ID2", result_column = column_name_preds)
    }
    if(!is.na(tempGDP)){
      data_subset$GDPpc_t0[m] <- ifelse(is.na(vlookup(paste(data_subset$country[m], year, sep="_"), labeled_data, lookup_column = "ID2", result_column = "GDPpc")), 
                                        tempGDP, 
                                        data_subset$GDPpc_t0[m])
    }
    rm(tempGDP)
  }
}

for(selperiod in 1:5){
  
  startcolumn <- 14
  
  training_data_sub <- subset(training_data, histperiod == selperiod)
  test_data_sub <- subset(test_data, histperiod == selperiod)
  
  if(selperiod == 2){
    update_GDPpc_t0(test_data_sub, test_data_preds, labeled_data, 1500, "prediction")
    update_GDPpc_t0(training_data_sub, training_data_preds, labeled_data, 1500, "prediction")
  } else if(selperiod == 3){
    update_GDPpc_t0(test_data_sub, test_data_preds, labeled_data, 1750, "prediction")
    update_GDPpc_t0(training_data_sub, training_data_preds, labeled_data, 1750, "prediction")
  } else if(selperiod == 4){
    update_GDPpc_t0(test_data_sub, test_data_preds, labeled_data, 1850, "prediction")
    update_GDPpc_t0(training_data_sub, training_data_preds, labeled_data, 1850, "prediction")
  } else if(selperiod == 5){
    update_GDPpc_t0(test_data_sub, test_data_preds, labeled_data, 1950, "prediction")
    update_GDPpc_t0(training_data_sub, training_data_preds, labeled_data, 1950, "prediction")
  }
  
  
  # Your existing processing here, for example:
  cctrl1 <- trainControl(method="cv", number = min(k, nrow(training_data_sub)))
  training_data_sub <- subset(training_data_sub, complete.cases(training_data_sub[,startcolumn:ncol(training_data_sub)]))
  
  cols <- match("year1300", colnames(training_data_sub)):match("year2000", colnames(training_data_sub))
  training_data_sub[ , cols] <- apply(training_data_sub[ , cols], 2,            # Specify own function within apply
                                      function(x) as.numeric(as.character(x)))
  
  # More data processing and model training
  test_class_cv_model <- train(training_data_sub[,startcolumn:ncol(training_data_sub)], log10(training_data_sub$GDPpc), method = "glmnet", 
                               trControl = cctrl1, metric = "MAE", standardize = TRUE, tuneGrid = training_grid_modelperformance)
  
  best_lambda <- test_class_cv_model$bestTune$lambda
  best_alpha <- test_class_cv_model$bestTune$alpha
  
  best_model <- glmnet(training_data_sub[,startcolumn:ncol(training_data_sub)], log10(training_data_sub$GDPpc), alpha = best_alpha, lambda = best_lambda, standardize = TRUE
  )
  
  test_data_sub[ , cols] <- apply(test_data_sub[ , cols], 2,            # Specify own function within apply
                                  function(x) as.numeric(as.character(x))) 
  
  test_data_sub$prediction <- predict(best_model, newx = as.matrix(test_data_sub[,startcolumn:ncol(training_data_sub)]))
  test_data_sub$prediction_abs <- 10^test_data_sub$prediction
  
  test_data_preds$prediction <- ifelse(is.na(test_data_preds$prediction), vlookup(test_data_preds$ID, test_data_sub, lookup_column = "ID", result_column = "prediction"), test_data_preds$prediction)
  test_data_preds$prediction_abs <- ifelse(is.na(test_data_preds$prediction_abs), vlookup(test_data_preds$ID, test_data_sub, lookup_column = "ID", result_column = "prediction_abs"), test_data_preds$prediction_abs)
  
  if(rescale_regions == "Y"){
    
    test_data_preds$country_0_period <- paste(test_data_preds$country_0, test_data_preds$period, sep="_")
    countries_0 <- unique(test_data_preds$country_0_period)
    
    test_data_preds_countrylevel <- subset(test_data_preds, country == country_0)
    
    test_data_preds_regional <- subset(test_data_preds, country != country_0)
    
    test_data_preds_regional_rescaled <- data.frame()
    for(c in countries_0){
      
      refGDPpc <- ifelse(is.na(vlookup(c, test_data_sub, lookup_column = "ID", result_column = "GDPpc")), 
                         subset(test_data_preds_countrylevel, test_data_preds_countrylevel$country_0_period == c)$prediction_abs,
                         vlookup(c, test_data_sub, lookup_column = "ID", result_column = "GDPpc"))
      
      regionalGDPpc <- subset(test_data_preds_regional, test_data_preds_regional$country_0_period == c)
      
      if(nrow(regionalGDPpc) > 0 & nrow(subset(test_data_preds_countrylevel, test_data_preds_countrylevel$country_0_period == c)) > 0){
        
        if(normalization_option == "log"){
          regionalGDPpc$prediction_abs <- regionalGDPpc$prediction_abs * (refGDPpc / 
                                                                            weighted.mean(regionalGDPpc$prediction_abs, w = 10^regionalGDPpc$births + 10^regionalGDPpc$deaths)
          )
        }
        if(normalization_option == "ihs"){
          regionalGDPpc$prediction_abs <- regionalGDPpc$prediction_abs * (refGDPpc / 
                                                                            weighted.mean(regionalGDPpc$prediction_abs, w = asinh(regionalGDPpc$births) + asinh(regionalGDPpc$deaths))
          )
        }
        
      }
      test_data_preds_regional_rescaled <- rbind(test_data_preds_regional_rescaled, regionalGDPpc)
    }
    
    test_data_preds <- rbind(test_data_preds_countrylevel, test_data_preds_regional_rescaled)
    test_data_preds$country_0_period <- NULL
    
  }
  
  
  
  
  # Make predictions for training data as well to use as input in the next period
  training_data_sub$prediction <- predict(best_model, newx = as.matrix(training_data_sub[,startcolumn:ncol(training_data_sub)]))
  training_data_sub$prediction_abs <- 10^training_data_sub$prediction
  
  training_data_preds$prediction <- ifelse(is.na(training_data_preds$prediction), vlookup(training_data_preds$ID, training_data_sub, lookup_column = "ID", result_column = "prediction"), training_data_preds$prediction)
  training_data_preds$prediction_abs <- ifelse(is.na(training_data_preds$prediction_abs), vlookup(training_data_preds$ID, training_data_sub, lookup_column = "ID", result_column = "prediction_abs"), training_data_preds$prediction_abs)
  
  if(rescale_regions == "Y"){
    
    training_data_preds$country_0_period <- paste(training_data_preds$country_0, training_data_preds$period, sep="_")
    countries_0 <- unique(training_data_preds$country_0_period)
    
    training_data_preds_countrylevel <- subset(training_data_preds, country == country_0)
    
    training_data_preds_regional <- subset(training_data_preds, country != country_0)
    
    training_data_preds_regional_rescaled <- data.frame()
    for(c in countries_0){
      
      refGDPpc <- ifelse(is.na(vlookup(c, training_data_sub, lookup_column = "ID", result_column = "GDPpc")), 
                         subset(training_data_preds_countrylevel, training_data_preds_countrylevel$country_0_period == c)$prediction_abs,
                         vlookup(c, training_data_sub, lookup_column = "ID", result_column = "GDPpc"))
      
      regionalGDPpc <- subset(training_data_preds_regional, training_data_preds_regional$country_0_period == c)
      
      if(nrow(regionalGDPpc) > 0 & nrow(subset(training_data_preds_countrylevel, training_data_preds_countrylevel$country_0_period == c)) > 0){
        
        if(normalization_option == "log"){
          regionalGDPpc$prediction_abs <- regionalGDPpc$prediction_abs * (refGDPpc / 
                                                                            weighted.mean(regionalGDPpc$prediction_abs, w = 10^regionalGDPpc$births + 10^regionalGDPpc$deaths)
          )
        }
        if(normalization_option == "ihs"){
          regionalGDPpc$prediction_abs <- regionalGDPpc$prediction_abs * (refGDPpc / 
                                                                            weighted.mean(regionalGDPpc$prediction_abs, w = asinh(regionalGDPpc$births) + asinh(regionalGDPpc$deaths))
          )
        }
        
      }
      training_data_preds_regional_rescaled <- rbind(training_data_preds_regional_rescaled, regionalGDPpc)
    }
    
    training_data_preds <- rbind(training_data_preds_countrylevel, training_data_preds_regional_rescaled)
    training_data_preds$country_0_period <- NULL
    
  }
  
  
}


test_data <- test_data_preds