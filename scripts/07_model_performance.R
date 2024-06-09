training_grid_modelperformance <- expand.grid(alpha = seq(0, 1, by = 0.05),
                                              lambda = seq(0.01,1,by = 0.02))

locations <- labeled_data %>% dplyr::group_by(country) %>% count() %>% as.data.frame()
countries <- subset(locations, str_length(country) == 3)

countries_training_long <- sample_n(subset(countries, n >= 9), size = round(nrow(subset(countries, n >= 9)) * 0.8, digits = 0))
countries_test_long <- subset(subset(countries, n >= 9), (country %in% countries_training_long$country) == F)

countries_training_short <- sample_n(subset(countries, n < 9), size = round(nrow(subset(countries, n < 9)) * 0.8, digits = 0))
countries_test_short <- subset(subset(countries, n < 9), (country %in% countries_training_short$country) == F)

countries_training <- rbind(countries_training_long, countries_training_short)
countries_test <- rbind(countries_test_long, countries_test_short)

countries_test$abbrev <- c("IT", "PT", "AL", "HR", "LV", "NO", "RO", "SI")
countries_test$country <- c("ITA", "PRT", "ALB", "HRV", "LVA", "NOR", "ROU", "SVN")

locations$test <- 0
for(i in 1:nrow(locations)){
  locations$test[i] <- ifelse(str_sub(locations$country[i], end = 2) %in% countries_test$abbrev
                              | str_sub(locations$country[i], end = 3) %in% countries_test$abbrev
                              | str_sub(locations$country[i], end = 3) %in% countries_test$country
                              | str_sub(locations$country[i], end = 4) %in% countries_test$abbrev
                              , 1, 0)
}

training_data <- subset(labeled_data, country %in% subset(locations, test == 0)$country)
test_data <- subset(labeled_data, country %in% subset(locations, test == 1)$country)

test_data_orig <- test_data

# This adds predictions based on the LASSO model to the test data set
source("./scripts/07a_LASSO_for_modelperformance.R")

test_data$RMSE <- (test_data$prediction_abs - test_data$GDPpc)^2
test_data$RMSE_logs <- (test_data$prediction - log10(test_data$GDPpc))^2

test_data$AME <- abs(test_data$prediction_abs - test_data$GDPpc)

RMSE_overall <- test_data %>% dplyr::summarize(sqrt(mean(RMSE, na.rm=T)))
RMSE_overall$avgGDPpc <- test_data %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 

AME_overall <- test_data %>% dplyr::summarize(mean(AME, na.rm=T))
AME_overall$avgGDPpc <- test_data %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 

RMSE_tab <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(sqrt(mean(RMSE, na.rm=T))) %>% as.data.frame()
RMSE_logs_tab <- sqrt(test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(sqrt(mean(RMSE_logs, na.rm=T))) %>% as.data.frame())

AME_tab <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(mean(AME, na.rm=T)) %>% as.data.frame()

avgGDPpc <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 

RMSE_tab$avgGDPpc <- avgGDPpc$`mean(GDPpc, na.rm = T)`
RMSE_tab$share <- RMSE_tab$`sqrt(mean(RMSE, na.rm = T))` / RMSE_tab$avgGDPpc

AME_tab$avgGDPpc <- avgGDPpc$`mean(GDPpc, na.rm = T)`
AME_tab$share <- AME_tab$`mean(AME, na.rm = T)` / AME_tab$avgGDPpc

fullmodel <- ggplot(test_data, aes(y=GDPpc, x=prediction_abs)) + stat_poly_eq(formula = y~x, data=test_data, aes(label = after_stat(rr.label)), parse = TRUE) +
  geom_smooth(method="lm", se=F, linetype="dashed", color="grey") + geom_abline(slope = 1, intercept = 0, color = "grey") + geom_point(size=1.5, color = "darkorange1") + geom_text(label=test_data$ID2, check_overlap=T, size=2.5, nudge_y = -0.02) +
  scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + 
  theme_light() + geom_smooth(method="lm", se=F, linetype="dashed", color="grey") +
  labs(x="prediction", y="log(GDP per capita)", title = "Full model")

fullmodel_below2000 <- ggplot(subset(test_data, as.numeric(as.character(year)) < 2000), aes(y=GDPpc, x=prediction_abs)) + stat_poly_eq(formula = y~x, data=subset(test_data, as.numeric(as.character(year)) < 2000), aes(label = after_stat(rr.label)), parse = TRUE) +
  geom_smooth(method="lm", se=F, linetype="dashed", color="grey") + geom_abline(slope = 1, intercept = 0, color = "grey") + geom_point(size=1.5, color = "darkorange1") + geom_text(label=subset(test_data, as.numeric(as.character(year)) < 2000)$ID2, check_overlap=T, size=2.5, nudge_y = -0.02) +
  scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + 
  theme_light() + geom_smooth(method="lm", se=F, linetype="dashed", color="grey") +
  labs(x="prediction", y="log(GDP per capita)", title = "Full model")

RMSE_overall_fullmodel <- RMSE_overall
RMSE_tab_fullmodel <- RMSE_tab
AME_overall_fullmodel <- AME_overall
AME_tab_fullmodel <- AME_tab



# BASELINE TAKING ONLY FIXED EFFECTS
test_data$prediction <- NA
test_data$prediction_abs <- NA

training_data$prediction <- NA
training_data$prediction_abs <- NA

for(p in 1:5){
  training_data_sub <- subset(training_data, histperiod == p)
  test_data_sub <- subset(test_data, histperiod == p)
  
  if(p == 2){
    update_GDPpc_t0(test_data_sub, test_data, labeled_data, 1500, "prediction")
    update_GDPpc_t0(training_data_sub, training_data, labeled_data, 1500, "prediction")
  } else if(p == 3){
    update_GDPpc_t0(test_data_sub, test_data, labeled_data, 1750, "prediction")
    update_GDPpc_t0(training_data_sub, training_data, labeled_data, 1750, "prediction")
  } else if(p == 4){
    update_GDPpc_t0(test_data_sub, test_data, labeled_data, 1850, "prediction")
    update_GDPpc_t0(training_data_sub, training_data, labeled_data, 1850, "prediction")
  } else if(p == 5){
    update_GDPpc_t0(test_data_sub, test_data, labeled_data, 1950, "prediction")
    update_GDPpc_t0(training_data_sub, training_data, labeled_data, 1950, "prediction")
  }
  
  model <- feols(as.formula("log10(GDPpc) ~ GDPpc_t0 + as.factor(year) + as.factor(UN_subregion)")
                 , data = training_data_sub
  )
  
  # prediction performance across periods
  test_data_sub$prediction <- predict(model, newdata = test_data_sub)
  test_data_sub$prediction_abs <- 10^predict(model, newdata = test_data_sub)
  
  test_data$prediction <- ifelse(is.na(test_data$prediction), vlookup(test_data$ID, test_data_sub, lookup_column = "ID", result_column = "prediction"), test_data$prediction)
  test_data$prediction_abs <- ifelse(is.na(test_data$prediction_abs), vlookup(test_data$ID, test_data_sub, lookup_column = "ID", result_column = "prediction_abs"), test_data$prediction_abs)
  
  # prediction performance across periods
  training_data_sub$prediction <- predict(model, newdata = training_data_sub)
  training_data_sub$prediction_abs <- 10^predict(model, newdata = training_data_sub)
  
  training_data$prediction <- ifelse(is.na(training_data$prediction), vlookup(training_data$ID, training_data_sub, lookup_column = "ID", result_column = "prediction"), training_data$prediction)
  training_data$prediction_abs <- ifelse(is.na(training_data$prediction_abs), vlookup(training_data$ID, training_data_sub, lookup_column = "ID", result_column = "prediction_abs"), training_data$prediction_abs)
  
}

test_data$RMSE <- (test_data$prediction_abs - test_data$GDPpc)^2
test_data$RMSE_logs <- (test_data$prediction - log10(test_data$GDPpc))^2

test_data$AME <- abs(test_data$prediction_abs - test_data$GDPpc)

RMSE_overall <- test_data %>% dplyr::summarize(sqrt(mean(RMSE, na.rm=T)))
RMSE_overall$avgGDPpc <- test_data %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 

AME_overall <- test_data %>% dplyr::summarize(mean(AME, na.rm=T))
AME_overall$avgGDPpc <- test_data %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 

RMSE_tab <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(sqrt(mean(RMSE, na.rm=T))) %>% as.data.frame()
RMSE_logs_tab <- sqrt(test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(sqrt(mean(RMSE_logs, na.rm=T))) %>% as.data.frame())

AME_tab <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(mean(AME, na.rm=T)) %>% as.data.frame()

avgGDPpc <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 

RMSE_tab$avgGDPpc <- avgGDPpc$`mean(GDPpc, na.rm = T)`
RMSE_tab$share <- RMSE_tab$`sqrt(mean(RMSE, na.rm = T))` / RMSE_tab$avgGDPpc

AME_tab$avgGDPpc <- avgGDPpc$`mean(GDPpc, na.rm = T)`
AME_tab$share <- AME_tab$`mean(AME, na.rm = T)` / AME_tab$avgGDPpc

baselinemodel <- ggplot(test_data, aes(y=GDPpc, x=prediction_abs)) + stat_poly_eq(formula = y~x, data=test_data, aes(label = after_stat(rr.label)), parse = TRUE) +
  geom_smooth(method="lm", se=F, linetype="dashed", color="grey") + geom_abline(slope = 1, intercept = 0, color = "grey") + geom_point(size=1.5, color = "darkorange1") + geom_text(label=test_data$ID2, check_overlap=T, size=2.5, nudge_y = -0.02) +
  scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + #annotate("text", x = 1400, y = 50000, label = paste0("RMSE = ", round(RMSE_overall[1,1], digits = 1))) +
  theme_light() + geom_smooth(method="lm", se=F, linetype="dashed", color="grey") +
  labs(x="prediction", y="log(GDP per capita)", title = "Baseline model")

baselinemodel_below2000 <- ggplot(subset(test_data, as.numeric(as.character(year)) < 2000), aes(y=GDPpc, x=prediction_abs)) + stat_poly_eq(formula = y~x, data=subset(test_data, as.numeric(as.character(year)) < 2000), aes(label = after_stat(rr.label)), parse = TRUE) +
  geom_smooth(method="lm", se=F, linetype="dashed", color="grey") + geom_abline(slope = 1, intercept = 0, color = "grey") + geom_point(size=1.5, color = "darkorange1") + geom_text(label=subset(test_data, as.numeric(as.character(year)) < 2000)$ID2, check_overlap=T, size=2.5, nudge_y = -0.02) +
  scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + #annotate("text", x = 1400, y = 50000, label = paste0("RMSE = ", round(RMSE_overall[1,1], digits = 1))) +
  theme_light() + geom_smooth(method="lm", se=F, linetype="dashed", color="grey") +
  labs(x="prediction", y="log(GDP per capita)", title = "Baseline model")

RMSE_tab_fullmodel$group <- "Full model"
RMSE_tab_fullmodel$century <- c("1300-1500", "1550-1750", "1800-1850", "1900-1950", "2000")

RMSE_tab$group <- "Naive model"
RMSE_tab$century <- c("1300-1500", "1550-1750", "1800-1850", "1900-1950", "2000")
RMSE_tab_full <- rbind(RMSE_tab, RMSE_tab_fullmodel)

ggplot(RMSE_tab_full, aes(x=share*100, y = as.factor(histperiod), fill = as.factor(group))) + geom_bar(stat = "identity", position = "dodge", width = 0.7) + theme_light() +
  labs(y="Century", x="RMSE, % of average GDP per capita")

AME_tab_fullmodel$group <- "Full model"
AME_tab_fullmodel$century <- c("1300-1500", "1550-1750", "1800-1850", "1900-1950", "2000")

AME_tab$group <- "Naive model"
AME_tab$century <- c("1300-1500", "1550-1750", "1800-1850", "1900-1950", "2000")

AME_tab_full <- rbind(AME_tab, AME_tab_fullmodel)

ggplot(AME_tab_full, aes(x=share*100, y = as.factor(histperiod), fill = as.factor(group))) + geom_bar(stat = "identity", position = "dodge", width = 0.7) + theme_light() +
  labs(y="Century", x="AME, % of average GDP per capita")

plot_grid(baselinemodel, fullmodel, labels = "AUTO", ncol = 2)
ggsave(paste0("./genfiles_", version, "/figures/Fig2_AB.svg"), width = 8, height = 4)



# MODEL PERFORMANCE FOR MULTIPLE DRAWS

# SPLITTING DATA

locations <- labeled_data %>% group_by(country) %>% count() %>% as.data.frame()
locations$country_0 <- vlookup(locations$country, labeled_data, lookup_column = "country", result_column = "country_0")
countries <- subset(locations, str_length(country) == 3)

AME_distribution_naive <- data.frame(matrix(NA, ncol=2, nrow=n_draws))
colnames(AME_distribution_naive) <- c("value", "countries")
AME_distribution_full <- data.frame(matrix(NA, ncol=2, nrow=n_draws))
colnames(AME_distribution_full) <- c("value", "countries")
R2_distribution_naive <- data.frame(matrix(NA, ncol=1, nrow=n_draws))
colnames(R2_distribution_naive) <- "value"
R2_distribution_full <- data.frame(matrix(NA, ncol=1, nrow=n_draws))
colnames(R2_distribution_full) <- "value"

AME_percentury_naive <- data.frame()
AME_percentury_full <- data.frame()

for(iteration in 1:n_draws){
  set.seed(iteration)
  
  countries_training_long <- sample_n(subset(countries, n >= 9), size = round(nrow(subset(countries, n >= 9)) * 0.8, digits = 0))
  countries_test_long <- subset(subset(countries, n >= 9), (country %in% countries_training_long$country) == F)
  
  countries_training_short <- sample_n(subset(countries, n < 9), size = round(nrow(subset(countries, n < 9)) * 0.8, digits = 0))
  countries_test_short <- subset(subset(countries, n < 9), (country %in% countries_training_short$country) == F)
  
  countries_training <- rbind(countries_training_long, countries_training_short)
  countries_test <- rbind(countries_test_long, countries_test_short)
  
  locations$test <- 0
  
  locations$test <- ifelse(locations$country_0 %in% countries_test$country_0, 1, 0)
  
  training_data <- subset(labeled_data, country %in% subset(locations, test == 0)$country)
  test_data <- subset(labeled_data, country %in% subset(locations, test == 1)$country)
  
  test_data_orig <- test_data
  
  AME_distribution_naive$countries[iteration] <- str_c(unique(countries_test$country_0), collapse=",")
  AME_distribution_full$countries[iteration] <- str_c(unique(countries_test$country_0), collapse=",")
  
  tryCatch(
    expr= {
      
      # This adds predictions based on the LASSO model to the test data set
      source("./scripts/07a_LASSO_for_modelperformance.R")
      
      test_data$RMSE <- (test_data$prediction_abs - test_data$GDPpc)^2
      test_data$RMSE_logs <- (test_data$prediction - log10(test_data$GDPpc))^2
      
      test_data$AME <- abs(test_data$prediction_abs - test_data$GDPpc)
      
      RMSE_overall <- test_data %>% dplyr::summarize(sqrt(mean(RMSE, na.rm=T)))
      RMSE_overall$avgGDPpc <- test_data %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 
      
      AME_overall <- test_data %>% dplyr::summarize(mean(AME, na.rm=T))
      AME_overall$avgGDPpc <- test_data %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 
      
      RMSE_tab <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(sqrt(mean(RMSE, na.rm=T))) %>% as.data.frame()
      RMSE_logs_tab <- sqrt(test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(sqrt(mean(RMSE_logs, na.rm=T))) %>% as.data.frame())
      
      AME_tab <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(mean(AME, na.rm=T)) %>% as.data.frame()
      
      avgGDPpc <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 
      
      RMSE_tab$avgGDPpc <- avgGDPpc$`mean(GDPpc, na.rm = T)`
      RMSE_tab$share <- RMSE_tab$`sqrt(mean(RMSE, na.rm = T))` / RMSE_tab$avgGDPpc
      
      AME_tab$avgGDPpc <- avgGDPpc$`mean(GDPpc, na.rm = T)`
      AME_tab$share <- AME_tab$`mean(AME, na.rm = T)` / AME_tab$avgGDPpc
      
      R2_distribution_full$value[iteration] <- summary(lm(log(GDPpc) ~ log(prediction_abs), test_data))$r.squared
      AME_distribution_full$value[iteration] <- AME_overall$`mean(AME, na.rm = T)` / AME_overall$avgGDPpc
      
      
      # BASELINE TAKING ONLY FIXED EFFECTS
      test_data$prediction <- NA
      test_data$prediction_abs <- NA
      
      training_data$prediction <- NA
      training_data$prediction_abs <- NA
      
      for(p in 1:5){
        training_data_sub <- subset(training_data, histperiod == p)
        test_data_sub <- subset(test_data, histperiod == p)
        
        if(p == 2){
          update_GDPpc_t0(test_data_sub, test_data, labeled_data, 1500, "prediction")
          update_GDPpc_t0(training_data_sub, training_data, labeled_data, 1500, "prediction")
        } else if(p == 3){
          update_GDPpc_t0(test_data_sub, test_data, labeled_data, 1750, "prediction")
          update_GDPpc_t0(training_data_sub, training_data, labeled_data, 1750, "prediction")
        } else if(p == 4){
          update_GDPpc_t0(test_data_sub, test_data, labeled_data, 1850, "prediction")
          update_GDPpc_t0(training_data_sub, training_data, labeled_data, 1850, "prediction")
        } else if(p == 5){
          update_GDPpc_t0(test_data_sub, test_data, labeled_data, 1950, "prediction")
          update_GDPpc_t0(training_data_sub, training_data, labeled_data, 1950, "prediction")
        }
        
        model <- feols(as.formula("log10(GDPpc) ~ GDPpc_t0 + as.factor(year) + as.factor(UN_subregion)")
                       , data = training_data_sub
        )
        
        # prediction performance across periods
        test_data_sub$prediction <- predict(model, newdata = test_data_sub)
        test_data_sub$prediction_abs <- 10^predict(model, newdata = test_data_sub)
        
        test_data$prediction <- ifelse(is.na(test_data$prediction), vlookup(test_data$ID, test_data_sub, lookup_column = "ID", result_column = "prediction"), test_data$prediction)
        test_data$prediction_abs <- ifelse(is.na(test_data$prediction_abs), vlookup(test_data$ID, test_data_sub, lookup_column = "ID", result_column = "prediction_abs"), test_data$prediction_abs)
        
        training_data_sub$prediction <- predict(model, newdata = training_data_sub)
        training_data_sub$prediction_abs <- 10^predict(model, newdata = training_data_sub)
        
        training_data$prediction <- ifelse(is.na(training_data$prediction), vlookup(training_data$ID, training_data_sub, lookup_column = "ID", result_column = "prediction"), training_data$prediction)
        training_data$prediction_abs <- ifelse(is.na(training_data$prediction_abs), vlookup(training_data$ID, training_data_sub, lookup_column = "ID", result_column = "prediction_abs"), training_data$prediction_abs)
        
      }
      
      test_data$RMSE <- (test_data$prediction_abs - test_data$GDPpc)^2
      test_data$RMSE_logs <- (test_data$prediction - log10(test_data$GDPpc))^2
      
      test_data$AME <- abs(test_data$prediction_abs - test_data$GDPpc)
      
      RMSE_overall <- test_data %>% dplyr::summarize(sqrt(mean(RMSE, na.rm=T)))
      RMSE_overall$avgGDPpc <- test_data %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 
      
      AME_overall <- test_data %>% dplyr::summarize(mean(AME, na.rm=T))
      AME_overall$avgGDPpc <- test_data %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 
      
      RMSE_tab <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(sqrt(mean(RMSE, na.rm=T))) %>% as.data.frame()
      RMSE_logs_tab <- sqrt(test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(sqrt(mean(RMSE_logs, na.rm=T))) %>% as.data.frame())
      
      AME_tab <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(mean(AME, na.rm=T)) %>% as.data.frame()
      
      avgGDPpc <- test_data %>% dplyr::group_by(histperiod) %>% dplyr::summarize(mean(GDPpc, na.rm=T)) 
      
      RMSE_tab$avgGDPpc <- avgGDPpc$`mean(GDPpc, na.rm = T)`
      RMSE_tab$share <- RMSE_tab$`sqrt(mean(RMSE, na.rm = T))` / RMSE_tab$avgGDPpc
      
      AME_tab$avgGDPpc <- avgGDPpc$`mean(GDPpc, na.rm = T)`
      AME_tab$share <- AME_tab$`mean(AME, na.rm = T)` / AME_tab$avgGDPpc
      
      R2_distribution_naive$value[iteration] <- summary(lm(log(GDPpc) ~ log(prediction_abs), test_data))$r.squared
      AME_distribution_naive$value[iteration] <- AME_overall$`mean(AME, na.rm = T)` / AME_overall$avgGDPpc
      
    },
    error = function(e){NULL}
  )
  
  print(iteration)
}

AME_distribution_full$group <- "Full model"
AME_distribution_naive$group <- "Baseline model"
AME_distribution <- rbind(AME_distribution_full, AME_distribution_naive)
AME_distribution$value <- as.numeric(AME_distribution$value)

R2_distribution_full$group <- "Full model"
R2_distribution_naive$group <- "Baseline model"
R2_distribution <- rbind(R2_distribution_full, R2_distribution_naive)
R2_distribution$value <- as.numeric(R2_distribution$value)

kw_AME <- kruskal.test(value~group, data=AME_distribution)
kw_R2 <- kruskal.test(value~group, data=R2_distribution)

mu <- ddply(AME_distribution, "group", summarise, grp.mean=median(value, na.rm=T))
AME_distribution_plot <- ggplot(AME_distribution, aes(x=value, fill=group)) + geom_density(alpha = 0.3) + theme_light() +
  labs(y="Density", x="AME, % of average GDP per capita", fill="", color ="") + #xlim(0.15,0.5) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed") + 
  geom_text(data=mu, aes(x=grp.mean, y=0.9, label=paste("Median =", round(grp.mean, 3))), 
                                          color="black", vjust=-0.5, angle=90)


mu <- ddply(R2_distribution, "group", summarise, grp.mean=median(value, na.rm=T))
R2_distribution_plot <- ggplot(R2_distribution, aes(x=value, fill=group)) + geom_density(alpha = 0.3) + theme_light() +
  labs(y="Density", x="R-squared", fill="", color ="") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed") + 
  geom_text(data=mu, aes(x=grp.mean, y=0.8, label=paste("Median =", round(grp.mean, 3))), 
            color="black", vjust=-0.5, angle=90)

# mean(as.numeric(AME_distribution_full$value), na.rm=T)
# mean(as.numeric(AME_distribution_naive$value), na.rm=T)
# 
# mean(as.numeric(R2_distribution_full$value), na.rm=T)
# mean(as.numeric(R2_distribution_naive$value), na.rm=T)

write.csv2(R2_distribution, file=paste0("./genfiles_", version, "/figures_SI/data_Fig2_C.csv"))
write.csv2(AME_distribution, file=paste0("./genfiles_", version, "/figures_SI/data_Fig2_D.csv"))

plot_grid(R2_distribution_plot, AME_distribution_plot, labels = "AUTO", ncol = 2)
ggsave(paste0("./genfiles_", version, "/figures/Fig2_CD.svg"), width = 10, height = 5)


