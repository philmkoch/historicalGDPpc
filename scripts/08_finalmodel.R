
training_grid_finalmodel <- expand.grid(alpha = seq(0, 1, by = 0.01),
                                        lambda = seq(0.01,1,by = 0.01))

  data$oos_pred <- NA
  data$oos_pred_lower <- NA
  data$oos_pred_upper <- NA
  data$oos_pred_level <- NA
  data$oos_pred_level_lower <- NA
  data$oos_pred_level_upper <- NA
  
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
  
  for(p in 1:5){
    set.seed(p)
    
    startcolumn <- 14
    
    fulldata_sub <- subset(labeled_data, histperiod == p)
    oosdata <- subset(data, histperiod == p)
    
    if(p == 2){
      update_GDPpc_t0(fulldata_sub, data, labeled_data, 1500, "oos_pred")
      update_GDPpc_t0(oosdata, data, labeled_data, 1500, "oos_pred")
    } else if(p == 3){
      update_GDPpc_t0(fulldata_sub, data, labeled_data, 1750, "oos_pred")
      update_GDPpc_t0(oosdata, data, labeled_data, 1750, "oos_pred")
    } else if(p == 4){
      update_GDPpc_t0(fulldata_sub, data, labeled_data, 1850, "oos_pred")
      update_GDPpc_t0(oosdata, data, labeled_data, 1850, "oos_pred")
    } else if(p == 5){
      update_GDPpc_t0(fulldata_sub, data, labeled_data, 1950, "oos_pred")
      update_GDPpc_t0(oosdata, data, labeled_data, 1950, "oos_pred")
    }
    
    cctrl1 <- trainControl(method="cv", number = min(k, nrow(fulldata_sub))#, returnResamp="all"
    )
    
    fulldata_sub <- subset(fulldata_sub, is.na(diversity_died) == F & is.na(diversity) == F 
                           & is.na(ubiquity_died) == F & is.na(ubiquity) == F
                           & is.na(diversity_immigrated) == F & is.na(diversity_emigrated) == F
                           & is.na(ubiquity_immigrated) == F & is.na(ubiquity_emigrated) == F & is.na(GDPpc_t0) == F)
    
    cols <- match("year1300", colnames(fulldata_sub)):match("year2000", colnames(fulldata_sub))
    fulldata_sub[ , cols] <- apply(fulldata_sub[ , cols], 2,            # Specify own function within apply
                                        function(x) as.numeric(as.character(x))) 
    
    set.seed(p)
    test_class_cv_model <- train(fulldata_sub[,startcolumn:ncol(fulldata_sub)], log10(fulldata_sub$GDPpc), method = "glmnet", 
                                 trControl = cctrl1, metric = "MAE", standardize = TRUE
                                 , tuneGrid = training_grid_finalmodel)
    
    ggplot(test_class_cv_model) + theme_light()
    ggsave(paste0("./genfiles_", version, "/figures_SI/LASSO_results/optimization_period_", p, ".png"), width = 15, height = 5)
    
    best_lambda <- test_class_cv_model$bestTune$lambda
    best_alpha <- test_class_cv_model$bestTune$alpha
    
    write.csv2(best_lambda, paste0("./genfiles_", version, "/figures_SI/LASSO_results/lambda_", p, ".csv"))
    write.csv2(best_alpha, paste0("./genfiles_", version, "/figures_SI/LASSO_results/alpha_", p, ".csv"))
    
    best_model <- glmnet(fulldata_sub[,startcolumn:ncol(fulldata_sub)], log10(fulldata_sub$GDPpc), alpha = best_alpha, lambda = best_lambda, standardize = TRUE
    )
    
    modelsummary(best_model
                 #, stars = c('*' = .1, '**' = .05, '***' = .01)
                 , output=paste0("./genfiles_", version, "/figures_SI/LASSO_results/model_period_", p, ".docx"))
    
    coefs <- as.data.frame(as.matrix(coef(best_model)))
    coefs$name <- rownames(coefs)
    
    coefs <- subset(coefs, s0 != 0 & name != "(Intercept)")
    
    coefs %>% 
      mutate(name = fct_reorder(name, s0)) %>%
      ggplot(aes(x=name, y=s0)) +
      geom_bar(stat="identity", width = 0.7, fill = "darkblue") +
      coord_flip() +
      xlab("") + ylab("Coefficient") + labs(fill="") +
      theme_light()
    ggsave(paste0("./genfiles_", version, "/figures_SI/LASSO_results/coefficients_period_", p, ".png"), width = 5, height = nrow(coefs) / 4.5, limitsize = FALSE)
    
    preds <- as.data.frame(predict(best_model, newx = as.matrix(oosdata[,startcolumn:ncol(fulldata_sub)])))
    colnames(preds) <- c("oos_pred")
    rownames(preds) <- oosdata$ID
    
    preds$period <- oosdata$period
    preds$country <- oosdata$country
    preds$country_0 <- oosdata$country_0
    
    if(rescale_regions == "Y"){
      
      preds$oos_pred_level <- 10^preds$oos_pred
      
      preds$country_0_period <- paste(preds$country_0, preds$period, sep="_")
      countries_0 <- unique(preds$country_0_period)
      
      preds$country_period <- paste(preds$country, preds$period, sep="_")
      
      preds$births <- vlookup(preds$country_period, data, result_column = "births")
      preds$deaths <- vlookup(preds$country_period, data, result_column = "deaths")
      
      preds_countrylevel <- subset(preds, country == country_0)
      
      preds_regional <- subset(preds, country != country_0)
      
      preds_regional_rescaled <- data.frame()
      for(c in countries_0){
        
        refGDPpc <- ifelse(is.na(vlookup(c, fulldata_sub, lookup_column = "ID", result_column = "GDPpc")), 
                           subset(preds_countrylevel, preds_countrylevel$country_0_period == c)$oos_pred_level,
                           vlookup(c, fulldata_sub, lookup_column = "ID", result_column = "GDPpc"))
        
        regionalGDPpc <- subset(preds_regional, preds_regional$country_0_period == c)
        
        if(nrow(regionalGDPpc) > 0 & nrow(subset(preds_countrylevel, preds_countrylevel$country_0_period == c)) > 0){
          
          if(normalization_option == "log"){
            regionalGDPpc$oos_pred_level <- regionalGDPpc$oos_pred_level * (refGDPpc / 
                                                                              weighted.mean(regionalGDPpc$oos_pred_level, w = 10^regionalGDPpc$births + 10^regionalGDPpc$deaths - 2)
            )
          }
          if(normalization_option == "ihs"){
            regionalGDPpc$oos_pred_level <- regionalGDPpc$oos_pred_level * (refGDPpc / 
                                                                              weighted.mean(regionalGDPpc$oos_pred_level, w = asinh(regionalGDPpc$births) + asinh(regionalGDPpc$deaths))
            )
          }
          
        }
        preds_regional_rescaled <- rbind(preds_regional_rescaled, regionalGDPpc)
      }
      
      preds <- rbind(preds_countrylevel, preds_regional_rescaled)
      preds$country_0_period <- NULL
      
      preds$oos_pred <- log10(preds$oos_pred_level)
    }
    
    data$oos_pred <- ifelse(is.na(data$oos_pred), vlookup(data$ID, preds, lookup_column = "rownames", result_column = 1), data$oos_pred)
    

  # SHAPLEY VALUES
  if(shapley == "Y"){
    
    coefs <- as.data.frame(as.matrix(coef(best_model)))
    coefs_sub <- subset(coefs, coefs[, 1] != 0)
    
    # Prediction wrapper for glmnet models
    pfun <- function(object, newdata) {
      predict.glmnet(object, newx = newdata)
    }
    
    baseline_prediction <- mean(preds$oos_pred, na.rm = TRUE)
    
    num_cores <- detectCores() - 1
    
    # Create a cluster
    cl <- makeCluster(num_cores)
    
    # Export necessary variables and libraries to the cluster
    clusterExport(cl, c("best_model", "oosdata", "startcolumn", "fulldata_sub", "pfun"))
    clusterEvalQ(cl, {
      library(fastshap)
      library(glmnet)
      set.seed(123)
    })
    
    # Parallelized version of the code
    shapley_list <- parLapply(cl, 1:nrow(oosdata), function(m) {
      expl <- fastshap::explain(
        best_model, 
        X = as.matrix(oosdata[, startcolumn:ncol(fulldata_sub)]), 
        pred_wrapper = pfun, 
        nsim = 100, 
        newdata = as.matrix(oosdata[m, startcolumn:ncol(fulldata_sub)])
      )
    })
    
    # Stop the cluster
    stopCluster(cl)
    
    shapleyvals <- do.call(rbind, shapley_list)
    rownames(shapleyvals) <- rownames(oosdata)
    
    # Process Shapley values
    shapleyvals <- abs(shapleyvals)
    years_tobeselected <- unique(as.character(oosdata$year))
    
    for (y in years_tobeselected) {
      shapleyvals_sub <- shapleyvals[as.character(oosdata$year) == y, ]
      write.csv2(shapleyvals_sub, paste0("./genfiles_", version, "/figures_SI/shapleyvalues_year_", y, ".csv"))
      
      shaps <- as.data.frame(colMeans(shapleyvals_sub, na.rm = TRUE))
      shaps <- subset(shaps, shaps[, 1] > 0)
      shaps$name <- rownames(shaps)
      
      shaps$group <- ifelse(vlookup(rownames(shaps), coefs, lookup_column = "rownames", result_column = 1) < 0, "negative",
                            ifelse(vlookup(rownames(shaps), coefs, lookup_column = "rownames", result_column = 1) > 0, "positive", NA))
      
      colors <- c("negative" = "#a41f20ff", "positive" = "#52c559ff")
      shaps$period <- str_sub(shaps$name, start = -4)
      shaps$period <- ifelse(str_sub(shaps$name, end = -5, start = -8) %in% c("fore", "upto"), "incl", shaps$period)
      
      years_not_to_be <- years_tobeselected[years_tobeselected != y]
      shaps <- subset(shaps, !period %in% years_not_to_be)
      
      if (nrow(shaps) < 16) {
        p <- shaps %>% 
          mutate(name = fct_reorder(name, shaps[, 1])) %>%
          ggplot(aes(x = name, y = shaps[, 1], fill = group)) +
          geom_bar(stat = "identity", width = 0.7) +
          scale_fill_manual(values = colors) +
          scale_y_continuous(trans = "asn") +
          coord_flip() +
          xlab("") + ylab("Shapley value") + labs(fill = "") +
          theme_light()
      } else {
        shaps <- shaps[order(shaps[, 1], decreasing = TRUE), ][1:15, ]
        p <- shaps %>% 
          mutate(name = fct_reorder(name, shaps[, 1])) %>%
          ggplot(aes(x = name, y = shaps[, 1], fill = group)) +
          geom_bar(stat = "identity", width = 0.7) +
          scale_fill_manual(values = colors) +
          scale_y_continuous(trans = "asn") +
          coord_flip() +
          xlab("") + ylab("Shapley value") + labs(fill = "") +
          theme_light()
      }
      
      ggsave(paste0("./genfiles_", version, "/figures_SI/shapleyvalues_barchart_year_", y, ".svg"), plot = p, width = 5, height = 5)
    }
    
    
  }
  
}

data$oos_pred_level <- 10^data$oos_pred
  

# add confidence intervals by bootstrapping

if(standarderrors == "Y"){
  source("./scripts/08_zz_bootstrapping_CI.R")
  
  lowerbound_share <- data_misc$oos_pred_level_lower / data_misc$oos_pred_level
  upperbound_share <- data_misc$oos_pred_level_upper / data_misc$oos_pred_level
  
  data$oos_pred_level_lower <- data$oos_pred_level * lowerbound_share
  data$oos_pred_level_upper <- data$oos_pred_level * upperbound_share
  
  data$oos_pred_lower <- log10(data$oos_pred_level_lower)
  data$oos_pred_upper <- log10(data$oos_pred_level_upper)
}



