for(l in 1:length(data_list)){
  
  data <- cbind(data_list[[l]][, c("name", "bplace_ID", "occupation", "local")],1)
  
  # aggregate data
  data <- aggregate(data[,5], by=list(data$bplace_ID, data$occupation), FUN=sum)
  
  mat <- reshape(data, direction = "wide", idvar = "Group.1", timevar="Group.2")
  
  misc <- mat[,-1]
  rownames(misc) <- mat[,1]
  colnames(misc) <- str_sub(colnames(misc), start=3)
  misc <- subset(misc, str_length(rownames(misc)) > 2)
  saveRDS(misc, file=paste0("./misc/occupationnumbers_dfs/LAOUENAN_regions_births_period", l, ".rds"))
  rm(misc)
  
}

for(l in 1:length(data_list)){
  
  data <- cbind(data_list[[l]][, c("name", "bplace_ID", "occupation", "local")],1)
  
  data <- subset(data, local == F)
  
  # aggregate data
  data <- aggregate(data[,5], by=list(data$bplace_ID, data$occupation), FUN=sum)
  
  mat <- reshape(data, direction = "wide", idvar = "Group.1", timevar="Group.2")
  
  misc <- mat[,-1]
  rownames(misc) <- mat[,1]
  colnames(misc) <- str_sub(colnames(misc), start=3)
  misc <- subset(misc, str_length(rownames(misc)) > 2)
  saveRDS(misc, file=paste0("./misc/occupationnumbers_dfs/LAOUENAN_regions_emigrants_period", l, ".rds"))
  rm(misc)
  
}

for(l in 1:length(data_list)){
  
  data <- cbind(data_list[[l]][, c("name", "dplace_ID", "occupation", "local")],1)
  
  # aggregate data
  data <- aggregate(data[,5], by=list(data$dplace_ID, data$occupation), FUN=sum)
  
  mat <- reshape(data, direction = "wide", idvar = "Group.1", timevar="Group.2")
  
  misc <- mat[,-1]
  rownames(misc) <- mat[,1]
  colnames(misc) <- str_sub(colnames(misc), start=3)
  misc <- subset(misc, str_length(rownames(misc)) > 2)
  saveRDS(misc, file=paste0("./misc/occupationnumbers_dfs/LAOUENAN_regions_deaths_period", l, ".rds"))
  #rm(misc)
  
}

for(l in 1:length(data_list)){
  
  data <- cbind(data_list[[l]][, c("name", "dplace_ID", "occupation", "local")],1)
  
  data <- subset(data, local == F)
  
  # aggregate data
  data <- aggregate(data[,5], by=list(data$dplace_ID, data$occupation), FUN=sum)
  
  mat <- reshape(data, direction = "wide", idvar = "Group.1", timevar="Group.2")
  
  misc <- mat[,-1]
  rownames(misc) <- mat[,1]
  colnames(misc) <- str_sub(colnames(misc), start=3)
  misc <- subset(misc, str_length(rownames(misc)) > 2)
  saveRDS(misc, file=paste0("./misc/occupationnumbers_dfs/LAOUENAN_regions_immigrants_period", l, ".rds"))
  #rm(misc)
  
}

# COUNTRIES
for(l in 1:length(data_list)){
  
  data <- cbind(data_list[[l]][, c("name", "bplace_countrycode_v2", "bplace_NUTS3", "occupation", "local")],1)
  
  # aggregate data
  data <- aggregate(data[,6], by=list(data$bplace_countrycode_v2, data$occupation), FUN=sum)
  
  mat <- reshape(data, direction = "wide", idvar = "Group.1", timevar="Group.2")
  
  mat <- subset(mat, countrycode(mat$Group.1, origin = "iso3c", destination = "continent") == "Europe" | mat$Group.1 == "USA" | mat$Group.1 == "CAN" | is.na(countrycode(mat$Group.1, origin = "iso3c", destination = "continent")))
  
  misc <- mat[,-1]
  rownames(misc) <- mat[,1]
  colnames(misc) <- str_sub(colnames(misc), start=3)
  misc <- subset(misc, str_length(rownames(misc)) > 2)
  saveRDS(misc, file=paste0("./misc/occupationnumbers_dfs/LAOUENAN_country_births_period", l, ".rds"))
  
}

for(l in 1:length(data_list)){
  
  data <- cbind(data_list[[l]][, c("name", "bplace_countrycode_v2", "bplace_NUTS3", "dplace_countrycode_v2", "dplace_NUTS3", "occupation", "local")],1)
  
  data <- subset(data, local == F)
  
  # aggregate data
  data <- aggregate(data[,8], by=list(data$bplace_countrycode_v2, data$occupation), FUN=sum)
  
  mat <- reshape(data, direction = "wide", idvar = "Group.1", timevar="Group.2")
  
  mat <- subset(mat, countrycode(mat$Group.1, origin = "iso3c", destination = "continent") == "Europe" | mat$Group.1 == "USA" | mat$Group.1 == "CAN" | is.na(countrycode(mat$Group.1, origin = "iso3c", destination = "continent")))
  
  misc <- mat[,-1]
  rownames(misc) <- mat[,1]
  colnames(misc) <- str_sub(colnames(misc), start=3)
  misc <- subset(misc, str_length(rownames(misc)) > 2)
  saveRDS(misc, file=paste0("./misc/occupationnumbers_dfs/LAOUENAN_country_emigrants_period", l, ".rds"))
  
}

for(l in 1:length(data_list)){
  
  data <- cbind(data_list[[l]][, c("name", "dplace_countrycode_v2", "dplace_NUTS3", "occupation", "local")],1)
  
  # aggregate data
  data <- aggregate(data[,6], by=list(data$dplace_countrycode_v2, data$occupation), FUN=sum)
  
  mat <- reshape(data, direction = "wide", idvar = "Group.1", timevar="Group.2")
  
  mat <- subset(mat, countrycode(mat$Group.1, origin = "iso3c", destination = "continent") == "Europe" | mat$Group.1 == "USA" | mat$Group.1 == "CAN" | is.na(countrycode(mat$Group.1, origin = "iso3c", destination = "continent")))
  
  misc <- mat[,-1]
  rownames(misc) <- mat[,1]
  colnames(misc) <- str_sub(colnames(misc), start=3)
  misc <- subset(misc, str_length(rownames(misc)) > 2)
  saveRDS(misc, file=paste0("./misc/occupationnumbers_dfs/LAOUENAN_country_deaths_period", l, ".rds"))
  
}


for(l in 1:length(data_list)){
  
  data <- cbind(data_list[[l]][, c("name", "bplace_countrycode_v2", "bplace_NUTS3", "dplace_countrycode_v2", "dplace_NUTS3", "occupation", "local")],1)
  
  data <- subset(data, local == F)
  
  # aggregate data
  data <- aggregate(data[,8], by=list(data$dplace_countrycode_v2, data$occupation), FUN=sum)
  
  mat <- reshape(data, direction = "wide", idvar = "Group.1", timevar="Group.2")
  
  mat <- subset(mat, countrycode(mat$Group.1, origin = "iso3c", destination = "continent") == "Europe" | mat$Group.1 == "USA" | mat$Group.1 == "CAN" | is.na(countrycode(mat$Group.1, origin = "iso3c", destination = "continent")))
  
  misc <- mat[,-1]
  rownames(misc) <- mat[,1]
  colnames(misc) <- str_sub(colnames(misc), start=3)
  misc <- subset(misc, str_length(rownames(misc)) > 2)
  saveRDS(misc, file=paste0("./misc/occupationnumbers_dfs/LAOUENAN_country_immigrants_period", l, ".rds"))
  
}

# CONTINUE PROCESSING

occunumbers_births_df <- data.frame()
occunumbers_deaths_df <- data.frame()
occunumbers_emigrants_df <- data.frame()
occunumbers_immigrants_df <- data.frame()
for(l in 1:15){
  
  misc1 <- readRDS(paste0("./misc/occupationnumbers_dfs/LAOUENAN_country_births_period", l, ".rds")) 
  misc1$period_ID <- l
  misc1$countrycode <- rownames(misc1)
  misc1$ID <- paste(misc1$countrycode, misc1$period_ID, sep="_")
  
  misc5 <- readRDS(paste0("./misc/occupationnumbers_dfs/LAOUENAN_country_emigrants_period", l, ".rds")) 
  misc5$period_ID <- l
  misc5$countrycode <- rownames(misc5)
  misc5$ID <- paste(misc5$countrycode, misc5$period_ID, sep="_")
  
  misc2 <- readRDS(paste0("./misc/occupationnumbers_dfs/LAOUENAN_country_deaths_period", l, ".rds")) 
  misc2$period_ID <- l
  misc2$countrycode <- rownames(misc2)
  misc2$ID <- paste(misc2$countrycode, misc2$period_ID, sep="_")
  
  misc6 <- readRDS(paste0("./misc/occupationnumbers_dfs/LAOUENAN_country_immigrants_period", l, ".rds")) 
  misc6$period_ID <- l
  misc6$countrycode <- rownames(misc6)
  misc6$ID <- paste(misc6$countrycode, misc6$period_ID, sep="_")
  
  misc3 <- readRDS(paste0("./misc/occupationnumbers_dfs/LAOUENAN_regions_births_period", l, ".rds")) 
  misc3$period_ID <- l
  misc3$countrycode <- rownames(misc3)
  misc3$ID <- paste(misc3$countrycode, misc3$period_ID, sep="_")
  
  misc7 <- readRDS(paste0("./misc/occupationnumbers_dfs/LAOUENAN_regions_emigrants_period", l, ".rds")) 
  misc7$period_ID <- l
  misc7$countrycode <- rownames(misc7)
  misc7$ID <- paste(misc7$countrycode, misc7$period_ID, sep="_")
  
  misc4 <- readRDS(paste0("./misc/occupationnumbers_dfs/LAOUENAN_regions_deaths_period", l, ".rds")) 
  misc4$period_ID <- l
  misc4$countrycode <- rownames(misc4)
  misc4$ID <- paste(misc4$countrycode, misc4$period_ID, sep="_")
  
  misc8 <- readRDS(paste0("./misc/occupationnumbers_dfs/LAOUENAN_regions_immigrants_period", l, ".rds")) 
  misc8$period_ID <- l
  misc8$countrycode <- rownames(misc8)
  misc8$ID <- paste(misc8$countrycode, misc8$period_ID, sep="_")
  
  misc_births <- bind_rows(misc1, misc3)
  misc_deaths <- bind_rows(misc2, misc4)
  misc_emigrants <- bind_rows(misc5, misc7)
  misc_immigrants <- bind_rows(misc6, misc8)
  
  occunumbers_births_df <- bind_rows(occunumbers_births_df, misc_births)
  occunumbers_deaths_df <- bind_rows(occunumbers_deaths_df, misc_deaths)
  occunumbers_emigrants_df <- bind_rows(occunumbers_emigrants_df, misc_emigrants)
  occunumbers_immigrants_df <- bind_rows(occunumbers_immigrants_df, misc_immigrants)
  
}  

colnames(occunumbers_births_df) <- paste(colnames(occunumbers_births_df), "born", sep="_")
colnames(occunumbers_deaths_df) <- paste(colnames(occunumbers_deaths_df), "died", sep="_")
colnames(occunumbers_emigrants_df) <- paste(colnames(occunumbers_emigrants_df), "emigrated", sep="_")
colnames(occunumbers_immigrants_df) <- paste(colnames(occunumbers_immigrants_df), "immigrated", sep="_")

# replace NA's with zeros
occunumbers_births_df[is.na(occunumbers_births_df)] <- 0
occunumbers_deaths_df[is.na(occunumbers_deaths_df)] <- 0
occunumbers_emigrants_df[is.na(occunumbers_emigrants_df)] <- 0
occunumbers_immigrants_df[is.na(occunumbers_immigrants_df)] <- 0

occunumbers_df <- merge(occunumbers_births_df, occunumbers_deaths_df, by.x ="ID_born", by.y = "ID_died", all=T)
occunumbers_df <- merge(occunumbers_df, occunumbers_emigrants_df, by.x ="ID_born", by.y = "ID_emigrated", all=T)
occunumbers_df <- merge(occunumbers_df, occunumbers_immigrants_df, by.x ="ID_born", by.y = "ID_immigrated", all=T)

occunumbers_df[is.na(occunumbers_df)] <- 0

colnames(occunumbers_df) <- str_replace_all(colnames(occunumbers_df), "/", "_")

# DEFINE STARTING POINT TO MERGE OTHER DATA TO IT

fulldata <- occunumbers_df

fulldata$period_ID_born <- NULL
fulldata$countrycode_born <- NULL
fulldata$period_ID_died <- NULL
fulldata$countrycode_died <- NULL
fulldata$period_ID_emigrated <- NULL
fulldata$countrycode_emigrated <- NULL
fulldata$period_ID_immigrated <- NULL
fulldata$countrycode_immigrated <- NULL


colnames(fulldata)[1] <- "ID"

# These changes relate to the border adjustments

if("SCO_10" %in% fulldata$ID | "WAL_10" %in% fulldata$ID){
  fulldata[fulldata$ID == "GBR_10", 2:ncol(fulldata)] <- t(t(fulldata[fulldata$ID == "GBR_10", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "SCO_10", 2:ncol(fulldata)]) +
                                                             t(fulldata[fulldata$ID == "WAL_10", 2:ncol(fulldata)]) + + t(fulldata[fulldata$ID == "NIR_10", 2:ncol(fulldata)]))
}
if("SCO_11" %in% fulldata$ID | "WAL_11" %in% fulldata$ID){
  fulldata[fulldata$ID == "GBR_11", 2:ncol(fulldata)] <- t(t(fulldata[fulldata$ID == "GBR_11", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "SCO_11", 2:ncol(fulldata)]) +
                                                             t(fulldata[fulldata$ID == "WAL_11", 2:ncol(fulldata)]) + + t(fulldata[fulldata$ID == "NIR_11", 2:ncol(fulldata)]))
}
if("NLD_2_12" %in% fulldata$ID){
  fulldata[fulldata$ID == "NLD_12", 2:ncol(fulldata)] <- t(t(fulldata[fulldata$ID == "NLD_12", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "NLD_2_12", 2:ncol(fulldata)]))
}
if("NLD_2_13" %in% fulldata$ID){
  fulldata[fulldata$ID == "NLD_13", 2:ncol(fulldata)] <- t(t(fulldata[fulldata$ID == "NLD_13", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "NLD_2_13", 2:ncol(fulldata)]))
}
if("POL_2_13" %in% fulldata$ID){
  fulldata[fulldata$ID == "POL_13", 2:ncol(fulldata)] <- t(t(fulldata[fulldata$ID == "POL_13", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "POL_2_13", 2:ncol(fulldata)]))
}
if("POL_2_14" %in% fulldata$ID){
  fulldata[fulldata$ID == "POL_14", 2:ncol(fulldata)] <- t(t(fulldata[fulldata$ID == "POL_14", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "POL_2_14", 2:ncol(fulldata)]))
}
if("S_ITA_13" %in% fulldata$ID){
  fulldata[fulldata$ID == "ITA_13", 2:ncol(fulldata)] <- t(t(fulldata[fulldata$ID == "ITA_13", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "S_ITA_13", 2:ncol(fulldata)]))
}
if("S_ITA_14" %in% fulldata$ID){
  fulldata[fulldata$ID == "ITA_14", 2:ncol(fulldata)] <- t(t(fulldata[fulldata$ID == "ITA_14", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "S_ITA_14", 2:ncol(fulldata)]))
}

# Reassigning those born in CSK in 150 years before 2000 to CZE or SVK by aggregating regions in CZE and SVK
# And vice versa for 1850 and 1900
if(sum(t(fulldata[fulldata$ID == "CZE_12", 2:ncol(fulldata)])) > 0 & sum(t(fulldata[fulldata$ID == "SVK_12", 2:ncol(fulldata)])) > 0){
  fulldata[fulldata$ID == "CSK_12", 2:ncol(fulldata)] <- t(t(fulldata[fulldata$ID == "CSK_12", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "CZE_12", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "SVK_12", 2:ncol(fulldata)]))
}
if(sum(t(fulldata[fulldata$ID == "CZE_13", 2:ncol(fulldata)])) > 0 & sum(t(fulldata[fulldata$ID == "SVK_13", 2:ncol(fulldata)])) > 0){
  fulldata[fulldata$ID == "CSK_13", 2:ncol(fulldata)] <- t(t(fulldata[fulldata$ID == "CSK_13", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "CZE_13", 2:ncol(fulldata)]) + t(fulldata[fulldata$ID == "SVK_13", 2:ncol(fulldata)]))
}

CZE_subdata <- subset(fulldata, ID %in% c("CZ01_15", "CZ02_15", "CZ03_15", "CZ04_15", "CZ05_15", "CZ06_15", "CZ07_15", "CZ08_15"))
SVK_subdata <- subset(fulldata, ID %in% c("SK01_15", "SK02_15", "SK03_15", "SK04_15"))

fulldata[fulldata$ID == "CZE_15", 2:ncol(fulldata)] <- colSums(CZE_subdata[,2:ncol(CZE_subdata)])
fulldata[fulldata$ID == "SVK_15", 2:ncol(fulldata)] <- colSums(SVK_subdata[,2:ncol(SVK_subdata)])

fulldata <- subset(fulldata, ID != "SCO_10" 
                   & ID != "WAL_10"
                   & ID != "NIR_10"
                   & ID != "SCO_11" 
                   & ID != "WAL_11"
                   & ID != "NIR_11"
                   & ID != "NLD_2_12"
                   & ID != "NLD_2_13"
                   & ID != "POL_2_13"
                   & ID != "POL_2_14"
                   & ID != "S_ITA_13"
                   & ID != "S_ITA_14"
                   & ID != "CZE_12"
                   & ID != "CZE_13"
                   & ID != "SVK_12"
                   & ID != "SVK_13"
                   & ID != "CSK_15")

fulldata$period <- str_sub(fulldata$ID, start = -2)
fulldata$period <- str_replace(fulldata$period, "_", "")

fulldata$country <- str_sub(fulldata$ID, end = -3)
fulldata$country <- ifelse(str_sub(fulldata$country, start = -1) == "_", str_sub(fulldata$country, end = str_length(fulldata$country) - 1), fulldata$country)

fulldata$year <- ifelse(fulldata$period == 1, 1300, 
                        ifelse(fulldata$period == 2, 1350, 
                               ifelse(fulldata$period == 3, 1400, 
                                      ifelse(fulldata$period == 4, 1450, 
                                             ifelse(fulldata$period == 5, 1500, 
                                                    ifelse(fulldata$period == 6, 1550, 
                                                           ifelse(fulldata$period == 7, 1600, 
                                                                  ifelse(fulldata$period == 8, 1650, 
                                                                         ifelse(fulldata$period == 9, 1700,
                                                                                ifelse(fulldata$period == 10, 1750,
                                                                                       ifelse(fulldata$period == 11, 1800, 
                                                                                              ifelse(fulldata$period == 12, 1850, 
                                                                                                     ifelse(fulldata$period == 13, 1900,
                                                                                                            ifelse(fulldata$period == 14, 1950, 
                                                                                                                   ifelse(fulldata$period == 15, 2000, NA)))))))))))))))
fulldata$ID2 <- paste(fulldata$country, fulldata$year, sep="_")

fulldata$births <- NA
fulldata$deaths <- NA
fulldata$emigrants <- NA
fulldata$immigrants <- NA
fulldata$diversity <- NA
fulldata$diversity_deaths <- NA
fulldata$diversity_emigrants <- NA
fulldata$diversity_immigrants <- NA

fulldata$period_ID_born <- NULL
fulldata$countrycode_born <- NULL
fulldata$period_ID_died <- NULL
fulldata$countrycode_died <- NULL

fulldata <- subset(fulldata, duplicated(fulldata$ID) == F)

for(i in 1:nrow(fulldata)){
  fulldata$births[i] <- sum(fulldata[i, str_sub(colnames(fulldata), start = -4) == "born"], na.rm = T)
  fulldata$deaths[i] <- sum(fulldata[i, str_sub(colnames(fulldata), start = -4) == "died"], na.rm = T)
  fulldata$emigrants[i] <- sum(fulldata[i, str_sub(colnames(fulldata), start = -9) == "emigrated"], na.rm = T)
  fulldata$immigrants[i] <- sum(fulldata[i, str_sub(colnames(fulldata), start = -10) == "immigrated"], na.rm = T)
  fulldata$diversity[i] <- sum(fulldata[i, str_sub(colnames(fulldata), start = -4) == "born"] > 0)
  fulldata$diversity_deaths[i] <- sum(fulldata[i, str_sub(colnames(fulldata), start = -4) == "died"] > 0)
  fulldata$diversity_emigrants[i] <- sum(fulldata[i, str_sub(colnames(fulldata), start = -9) == "emigrated"] > 0)
  fulldata$diversity_immigrants[i] <- sum(fulldata[i, str_sub(colnames(fulldata), start = -10) == "immigrated"] > 0)
}

fulldata$births_squared <- fulldata$births^2
fulldata$deaths_squared <- fulldata$deaths^2
fulldata$emigrants_squared <- fulldata$emigrants^2
fulldata$immigrants_squared <- fulldata$immigrants^2

fulldata$ubiquity <- NA
fulldata$ubiquity_deaths <- NA
fulldata$ubiquity_immigrants <- NA
fulldata$ubiquity_emigrants <- NA
for(p in 1:15){
  tempdat <- subset(fulldata, period == p)
  
  births <- tempdat$diversity
  deaths <- tempdat$diversity_deaths
  emigrants <- tempdat$diversity_emigrants
  immigrants <- tempdat$diversity_immigrants
  
  rownames(tempdat) <- tempdat$ID
  
  tempdat[tempdat > 0] <- 1
  
  ubiquity_born <- rowSums(t(tempdat[str_sub(colnames(tempdat), start = -4) == "born"]))
  ubiquity_died <- rowSums(t(tempdat[str_sub(colnames(tempdat), start = -4) == "died"]))
  ubiquity_emigrated <- rowSums(t(tempdat[str_sub(colnames(tempdat), start = -9) == "emigrated"]))
  ubiquity_immigrated <- rowSums(t(tempdat[str_sub(colnames(tempdat), start = -10) == "immigrated"]))
  
  tempdat_born <- tempdat[str_sub(colnames(tempdat), start = -4) == "born"]
  tempdat_died <- tempdat[str_sub(colnames(tempdat), start = -4) == "died"]
  tempdat_emigrated <- tempdat[str_sub(colnames(tempdat), start = -9) == "emigrated"]
  tempdat_immigrated <- tempdat[str_sub(colnames(tempdat), start = -10) == "immigrated"]
  
  avgubi_born <- as.matrix(tempdat_born) %*% ubiquity_born / births
  avgubi_born[avgubi_born == "NaN"] <- 0
  
  avgubi_emigrated <- as.matrix(tempdat_emigrated) %*% ubiquity_emigrated / emigrants
  avgubi_emigrated[avgubi_emigrated == "NaN"] <- 0
  
  avgubi_died <- as.matrix(tempdat_died) %*% ubiquity_died / deaths
  avgubi_died[avgubi_died == "NaN"] <- 0
  
  avgubi_immigrated <- as.matrix(tempdat_immigrated) %*% ubiquity_immigrated / immigrants
  avgubi_immigrated[avgubi_immigrated == "NaN"] <- 0
  
  fulldata$ubiquity <- ifelse(is.na(fulldata$ubiquity), vlookup(fulldata$ID, as.data.frame(avgubi_born), lookup_column = "rownames", result_column = 1), fulldata$ubiquity)
  fulldata$ubiquity_deaths <- ifelse(is.na(fulldata$ubiquity_deaths), vlookup(fulldata$ID, as.data.frame(avgubi_died), lookup_column = "rownames", result_column = 1), fulldata$ubiquity_deaths)
  fulldata$ubiquity_emigrants <- ifelse(is.na(fulldata$ubiquity_emigrants), vlookup(fulldata$ID, as.data.frame(avgubi_emigrated), lookup_column = "rownames", result_column = 1), fulldata$ubiquity_emigrants)
  fulldata$ubiquity_immigrants <- ifelse(is.na(fulldata$ubiquity_immigrants), vlookup(fulldata$ID, as.data.frame(avgubi_immigrated), lookup_column = "rownames", result_column = 1), fulldata$ubiquity_immigrants)
  
}

# Add average age of famous individual born and died

# only use individuals born before 1940 to get proper estimates for 20th century to
fulldata$avg_age_births <- NA
fulldata$avg_age_deaths <- NA
fulldata$avg_age_emigrants <- NA
fulldata$avg_age_immigrants <- NA

# regions
for(l in 1:length(data_list)){
  data <- cbind(data_list[[l]][, c("name", "bplace_ID", "dplace_ID", "age", "local")],1)
  
  misc_age_bplace <- data %>% group_by(bplace_ID) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
  misc_age_bplace$ID <- paste(misc_age_bplace$bplace_ID, l, sep="_")
  
  fulldata$avg_age_births <- ifelse(is.na(fulldata$avg_age_births), vlookup(fulldata$ID, misc_age_bplace, lookup_column = "ID"), fulldata$avg_age_births)
  
  misc_age_emi <- data %>% subset(local == F) %>% group_by(bplace_ID) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
  misc_age_emi$ID <- paste(misc_age_emi$bplace_ID, l, sep="_")
  
  fulldata$avg_age_emigrants <- ifelse(is.na(fulldata$avg_age_emigrants), vlookup(fulldata$ID, misc_age_emi, lookup_column = "ID"), fulldata$avg_age_emigrants)
  
  misc_age_dplace <- data %>% group_by(dplace_ID) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
  misc_age_dplace$ID <- paste(misc_age_dplace$dplace_ID, l, sep="_")
  
  fulldata$avg_age_deaths <- ifelse(is.na(fulldata$avg_age_deaths), vlookup(fulldata$ID, misc_age_dplace, lookup_column = "ID"), fulldata$avg_age_deaths)
  
  misc_age_immi <- data %>% subset(local == F) %>% group_by(dplace_ID) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
  misc_age_immi$ID <- paste(misc_age_immi$dplace_ID, l, sep="_")
  
  fulldata$avg_age_immigrants <- ifelse(is.na(fulldata$avg_age_immigrants), vlookup(fulldata$ID, misc_age_immi, lookup_column = "ID"), fulldata$avg_age_immigrants)
  
  if(l != 15){
  }else{
    data <- cbind(data_list[[l]][, c("name", "birth", "bplace_ID", "dplace_ID", "age", "local")],1)
    data <- subset(data, birth < 1940)
    
    misc_age_bplace <- data %>% group_by(bplace_ID) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_bplace$ID <- paste(misc_age_bplace$bplace_ID, l, sep="_")
    
    fulldata$avg_age_births <- ifelse(is.na(fulldata$avg_age_births), vlookup(fulldata$ID, misc_age_bplace, lookup_column = "ID"), fulldata$avg_age_births)
    
    misc_age_emi <- data %>% subset(local == F) %>% group_by(bplace_ID) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_emi$ID <- paste(misc_age_emi$bplace_ID, l, sep="_")
    
    fulldata$avg_age_emigrants <- ifelse(is.na(fulldata$avg_age_emigrants), vlookup(fulldata$ID, misc_age_emi, lookup_column = "ID"), fulldata$avg_age_emigrants)
    
    misc_age_dplace <- data %>% group_by(dplace_ID) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_dplace$ID <- paste(misc_age_dplace$dplace_ID, l, sep="_")
    
    fulldata$avg_age_deaths <- ifelse(is.na(fulldata$avg_age_deaths), vlookup(fulldata$ID, misc_age_dplace, lookup_column = "ID"), fulldata$avg_age_deaths)
    
    misc_age_immi <- data %>% subset(local == F) %>% group_by(dplace_ID) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_immi$ID <- paste(misc_age_immi$dplace_ID, l, sep="_")
    
    fulldata$avg_age_immigrants <- ifelse(is.na(fulldata$avg_age_immigrants), vlookup(fulldata$ID, misc_age_immi, lookup_column = "ID"), fulldata$avg_age_immigrants)
  }
  
}

# countries
for(l in 1:length(data_list)){
  
  if(l != 15){
    data <- cbind(data_list[[l]][, c("name", "bplace_countrycode", "bplace_NUTS3", "dplace_countrycode", "dplace_NUTS3", "age", "local")],1)
    
    source("./scripts/01a_zz_borderadjustments.R")
    
    misc_age_bplace <- data %>% group_by(bplace_countrycode_v2) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_bplace$ID <- paste(misc_age_bplace$bplace_countrycode_v2, l, sep="_")
    
    fulldata$avg_age_births <- ifelse(is.na(fulldata$avg_age_births), vlookup(fulldata$ID, misc_age_bplace, lookup_column = "ID"), fulldata$avg_age_births)
    
    misc_age_dplace <- data %>% group_by(dplace_countrycode_v2) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_dplace$ID <- paste(misc_age_dplace$dplace_countrycode_v2, l, sep="_")
    
    fulldata$avg_age_deaths <- ifelse(is.na(fulldata$avg_age_deaths), vlookup(fulldata$ID, misc_age_dplace, lookup_column = "ID"), fulldata$avg_age_deaths)
    
    misc_age_emi <- data %>% subset(local == F) %>% group_by(bplace_countrycode_v2) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_emi$ID <- paste(misc_age_emi$bplace_countrycode_v2, l, sep="_")
    
    fulldata$avg_age_emigrants <- ifelse(is.na(fulldata$avg_age_emigrants), vlookup(fulldata$ID, misc_age_emi, lookup_column = "ID"), fulldata$avg_age_emigrants)
    
    misc_age_immi <- data %>% subset(local == F) %>% group_by(dplace_countrycode_v2) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_immi$ID <- paste(misc_age_immi$dplace_countrycode_v2, l, sep="_")
    
    fulldata$avg_age_immigrants <- ifelse(is.na(fulldata$avg_age_immigrants), vlookup(fulldata$ID, misc_age_immi, lookup_column = "ID"), fulldata$avg_age_immigrants)
    
  }else{
    data <- cbind(data_list[[l]][, c("name", "birth", "bplace_countrycode", "dplace_countrycode", "age", "local")],1)
    data <- subset(data, birth < 1940)
    
    misc_age_bplace <- data %>% group_by(bplace_countrycode) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_bplace$ID <- paste(misc_age_bplace$bplace_countrycode, l, sep="_")
    
    fulldata$avg_age_births <- ifelse(is.na(fulldata$avg_age_births), vlookup(fulldata$ID, misc_age_bplace, lookup_column = "ID"), fulldata$avg_age_births)
    
    misc_age_dplace <- data %>% group_by(dplace_countrycode) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_dplace$ID <- paste(misc_age_dplace$dplace_countrycode, l, sep="_")
    
    fulldata$avg_age_deaths <- ifelse(is.na(fulldata$avg_age_deaths), vlookup(fulldata$ID, misc_age_dplace, lookup_column = "ID"), fulldata$avg_age_deaths)
    
    misc_age_emi <- data %>% subset(local == F) %>% group_by(bplace_countrycode) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_emi$ID <- paste(misc_age_emi$bplace_countrycode, l, sep="_")
    
    fulldata$avg_age_emigrants <- ifelse(is.na(fulldata$avg_age_emigrants), vlookup(fulldata$ID, misc_age_emi, lookup_column = "ID"), fulldata$avg_age_emigrants)
    
    misc_age_immi <- data %>% subset(local == F) %>% group_by(dplace_countrycode) %>% dplyr::summarize(mean(age, na.rm=T)) %>% as.data.frame()
    misc_age_immi$ID <- paste(misc_age_immi$dplace_countrycode, l, sep="_")
    
    fulldata$avg_age_immigrants <- ifelse(is.na(fulldata$avg_age_immigrants), vlookup(fulldata$ID, misc_age_immi, lookup_column = "ID"), fulldata$avg_age_immigrants)
    
  }
}

age_by_period_births <- fulldata %>% group_by(period) %>% dplyr::summarize(mean(avg_age_births, na.rm=T)) %>% as.data.frame()
age_by_period_deaths <- fulldata %>% group_by(period) %>% dplyr::summarize(mean(avg_age_deaths, na.rm=T)) %>% as.data.frame()
age_by_period_immigrants <- fulldata %>% group_by(period) %>% dplyr::summarize(mean(avg_age_immigrants, na.rm=T)) %>% as.data.frame()
age_by_period_emigrants <- fulldata %>% group_by(period) %>% dplyr::summarize(mean(avg_age_emigrants, na.rm=T)) %>% as.data.frame()

age_by_period_births_lessthan5 <- subset(fulldata, births < 5) %>% group_by(period) %>% dplyr::summarize(mean(avg_age_births, na.rm=T)) %>% as.data.frame()
age_by_period_deaths_lessthan5 <- subset(fulldata, deaths < 5) %>% group_by(period) %>% dplyr::summarize(mean(avg_age_deaths, na.rm=T)) %>% as.data.frame()
age_by_period_immigrants_lessthan5 <- subset(fulldata, immigrants < 5) %>% group_by(period) %>% dplyr::summarize(mean(avg_age_immigrants, na.rm=T)) %>% as.data.frame()
age_by_period_emigrants_lessthan5 <- subset(fulldata, emigrants < 5) %>% group_by(period) %>% dplyr::summarize(mean(avg_age_emigrants, na.rm=T)) %>% as.data.frame()

# This generates many NA's as well as quite distorted estimates for small samples. 
# Hence, we will do the following:
# We will impute the minimum average age for the regions with less than 5 observations
# If still NA because no one born before 1940, we impute the average

fulldata$avg_age_births <- ifelse(fulldata$births < 5, vlookup(fulldata$period, age_by_period_births_lessthan5), fulldata$avg_age_births)
fulldata$avg_age_deaths <- ifelse(fulldata$deaths < 5, vlookup(fulldata$period, age_by_period_deaths_lessthan5), fulldata$avg_age_deaths)
fulldata$avg_age_immigrants <- ifelse(fulldata$immigrants < 5, vlookup(fulldata$period, age_by_period_immigrants_lessthan5), fulldata$avg_age_immigrants)
fulldata$avg_age_emigrants <- ifelse(fulldata$emigrants < 5, vlookup(fulldata$period, age_by_period_emigrants_lessthan5), fulldata$avg_age_emigrants)

fulldata$avg_age_births <- ifelse(is.na(fulldata$avg_age_births), vlookup(fulldata$period, age_by_period_births), fulldata$avg_age_births)
fulldata$avg_age_deaths <- ifelse(is.na(fulldata$avg_age_deaths), vlookup(fulldata$period, age_by_period_deaths), fulldata$avg_age_deaths)
fulldata$avg_age_immigrants <- ifelse(is.na(fulldata$avg_age_immigrants), vlookup(fulldata$period, age_by_period_immigrants), fulldata$avg_age_immigrants)
fulldata$avg_age_emigrants <- ifelse(is.na(fulldata$avg_age_emigrants), vlookup(fulldata$period, age_by_period_emigrants), fulldata$avg_age_emigrants)

fulldata$period <- as.numeric(fulldata$period)
fulldata <- fulldata[order(fulldata$country, fulldata$period),]

fulldata <- fulldata[!duplicated(fulldata$ID),]

fulldata <- fulldata[, c(1, match("period", colnames(fulldata)):ncol(fulldata), 2:(match("period", colnames(fulldata))-1))]

fulldata$year1300 <- ifelse(fulldata$year == 1300, 1, 0)
fulldata$year1350 <- ifelse(fulldata$year == 1350, 1, 0)
fulldata$year1400 <- ifelse(fulldata$year == 1400, 1, 0)
fulldata$year1450 <- ifelse(fulldata$year == 1450, 1, 0)
fulldata$year1500 <- ifelse(fulldata$year == 1500, 1, 0)
fulldata$year1550 <- ifelse(fulldata$year == 1550, 1, 0)
fulldata$year1600 <- ifelse(fulldata$year == 1600, 1, 0)
fulldata$year1650 <- ifelse(fulldata$year == 1650, 1, 0)
fulldata$year1700 <- ifelse(fulldata$year == 1700, 1, 0)
fulldata$year1750 <- ifelse(fulldata$year == 1750, 1, 0)
fulldata$year1800 <- ifelse(fulldata$year == 1800, 1, 0)
fulldata$year1850 <- ifelse(fulldata$year == 1850, 1, 0)
fulldata$year1900 <- ifelse(fulldata$year == 1900, 1, 0)
fulldata$year1950 <- ifelse(fulldata$year == 1950, 1, 0)
fulldata$year2000 <- ifelse(fulldata$year == 2000, 1, 0)

fulldata$country_0 <- NA

fulldata$country_0 <- ifelse(is.na(fulldata$country_0) & str_length(fulldata$country) == 3, fulldata$country, fulldata$country_0)
fulldata$country_0 <- ifelse(is.na(fulldata$country_0) & str_length(fulldata$country) == 4, countrycode(str_sub(fulldata$country, end = 2), origin = "iso2c", destination = "iso3c"), fulldata$country_0)

fulldata$country_0 <- ifelse(is.na(fulldata$country_0) & fulldata$country == "POL_2", "POL", fulldata$country_0)
fulldata$country_0 <- ifelse(is.na(fulldata$country_0) & fulldata$country == "S_ITA", "ITA", fulldata$country_0)
fulldata$country_0 <- ifelse(is.na(fulldata$country_0) & fulldata$country == "NLD_2", "NLD", fulldata$country_0)

fulldata$country_0 <- ifelse(fulldata$country == "SCO", "GBR", fulldata$country_0)
fulldata$country_0 <- ifelse(fulldata$country == "WAL", "GBR", fulldata$country_0)
fulldata$country_0 <- ifelse(fulldata$country == "NIR", "GBR", fulldata$country_0)
fulldata$country_0 <- ifelse(fulldata$country == "CSK", "CZE", fulldata$country_0)

fulldata$country_0 <- ifelse(str_sub(fulldata$country, end = 2) == "US", "USA", fulldata$country_0)
fulldata$country_0 <- ifelse(str_sub(fulldata$country, end = 2) == "CN", "CAN", fulldata$country_0)

fulldata$country_0 <- ifelse(is.na(fulldata$country_0) & str_length(fulldata$country) == 4 & str_sub(fulldata$country, end = 2) == "UK", "GBR", fulldata$country_0)
fulldata$country_0 <- ifelse(is.na(fulldata$country_0) & str_length(fulldata$country) == 4 & str_sub(fulldata$country, end = 2) == "EL", "GRC", fulldata$country_0)

fulldata$country_0 <- ifelse(is.na(fulldata$country_0), str_sub(fulldata$country, end = 3), fulldata$country_0)

fulldata <- subset(fulldata, country_0 != "CYP")
fulldata <- subset(fulldata, country != "US33260")

fulldata$UN_subregion <- countrycode(fulldata$country_0, origin = "iso3c", destination = "un.regionsub.name")
fulldata$UN_subregion <- ifelse(fulldata$country_0 == "XKO", "Southern Europe", fulldata$UN_subregion)

# UN SUBREGIONS CLASSIFICATION
fulldata$UN_subregion_broad <- fulldata$UN_subregion

for(m in 1:nrow(fulldata)){
  fulldata$UN_subregion[m] <- paste0(fulldata$UN_subregion[m], " in ", fulldata$year[m])
}

fulldata$UN_subregion <- ifelse(as.numeric(as.character(fulldata$year)) <= 1800 & (str_detect(fulldata$UN_subregion, "Southern Europe") == T | str_detect(fulldata$UN_subregion, "Western Asia") == T), "Southern Europe up to 1800",fulldata$UN_subregion)
fulldata$UN_subregion <- ifelse(as.numeric(as.character(fulldata$year)) <= 1800 & str_detect(fulldata$UN_subregion, "Northern Europe") == T, "Northern Europe up to 1800",fulldata$UN_subregion)
fulldata$UN_subregion <- ifelse(as.numeric(as.character(fulldata$year)) < 1800 & str_detect(fulldata$UN_subregion, "Northern Europe") == F & str_detect(fulldata$UN_subregion, "Southern Europe") == F, "Western and Eastern Europe or Northern America before 1800", fulldata$UN_subregion)
fulldata$UN_subregion <- ifelse(as.numeric(as.character(fulldata$year)) == 1800 & (str_detect(fulldata$UN_subregion, "Western Europe") == T ), "Western Europe in 1800", fulldata$UN_subregion)
fulldata$UN_subregion <- ifelse(as.numeric(as.character(fulldata$year)) == 1800 & (str_detect(fulldata$UN_subregion, "Eastern Europe") == T ), "Eastern Europe in 1800", fulldata$UN_subregion)

fulldata$UN_subregion <- ifelse(as.numeric(as.character(fulldata$year)) >= 1950 & (str_detect(fulldata$UN_subregion, "Eastern Europe") == T | fulldata$country_0 == "EST" | fulldata$country_0 == "LTU" | fulldata$country_0 == "LVA"), paste0("Soviet Union in ", fulldata$year),fulldata$UN_subregion)
fulldata$UN_subregion <- ifelse(str_detect(fulldata$UN_subregion, "Western Asia"), paste0("Southern Europe in ", fulldata$year), fulldata$UN_subregion)

fulldata$UN_subregion <- ifelse(fulldata$UN_subregion == "Western and Eastern Europe or Northern America before 1800" & (fulldata$country_0 == "USA" | fulldata$country_0 == "CAN"), "Northern America before 1800", fulldata$UN_subregion)
fulldata$UN_subregion <- ifelse(fulldata$UN_subregion == "Western and Eastern Europe or Northern America before 1800", "Western and Eastern Europe before 1800", fulldata$UN_subregion)

fulldata$UN_subregion <- ifelse(fulldata$UN_subregion == "Western and Eastern Europe before 1800" & (str_detect(fulldata$UN_subregion_broad, "Western Europe") == T ), "Western Europe before 1800", fulldata$UN_subregion)
fulldata$UN_subregion <- ifelse(fulldata$UN_subregion == "Western and Eastern Europe before 1800", "Eastern Europe before 1800", fulldata$UN_subregion)

# special treatment of Baltic states
# We treat them as Northern Europe prior to 1750, Eastern Europe between 1750 and 1950, and (impacted by the) Soviet Union in 1950 and 2000.
fulldata$UN_subregion <- ifelse((fulldata$country_0 == "EST" | fulldata$country_0 == "LTU" | fulldata$country_0 == "LVA") & as.numeric(as.character(fulldata$year)) == 1750, "Northern Europe up to 1800", fulldata$UN_subregion)
fulldata$UN_subregion <- ifelse((fulldata$country_0 == "EST" | fulldata$country_0 == "LTU" | fulldata$country_0 == "LVA") & as.numeric(as.character(fulldata$year)) == 1800, "Eastern Europe in 1800", fulldata$UN_subregion)
fulldata$UN_subregion <- ifelse((fulldata$country_0 == "EST" | fulldata$country_0 == "LTU" | fulldata$country_0 == "LVA") & as.numeric(as.character(fulldata$year)) == 1850, "Eastern Europe in 1850", fulldata$UN_subregion)
fulldata$UN_subregion <- ifelse((fulldata$country_0 == "EST" | fulldata$country_0 == "LTU" | fulldata$country_0 == "LVA") & as.numeric(as.character(fulldata$year)) == 1900, "Eastern Europe in 1900", fulldata$UN_subregion)

# Change variable country_0 back
# Otherwise the rescaling later does not work properly
fulldata$country_0 <- ifelse(fulldata$country == "POL_2", "POL_2", fulldata$country_0)
fulldata$country_0 <- ifelse(str_sub(fulldata$country, end = 2) == "PL" & fulldata$country != "PL21" & fulldata$period < 13, "POL_2", fulldata$country_0)

fulldata$country_0 <- ifelse(fulldata$country == "S_ITA", "S_ITA", fulldata$country_0)
fulldata$country_0 <- ifelse(fulldata$period < 13 & fulldata$country %in% c("ITF1", "ITF2", "ITF3", "ITF4", "ITF5", "ITF6", "ITG1", "ITG2", "ITI4"), "S_ITA", fulldata$country_0)
  
fulldata$country_0 <- ifelse(fulldata$country == "NLD_2", "NLD_2", fulldata$country_0)
fulldata$country_0 <- ifelse(fulldata$period < 12 & fulldata$country %in% c("NL11", "NL12", "NL13", "NL21", "NL22", "NL23", "NL31", "NL34", "NL41", "NL42"), "NLD_2", fulldata$country_0)

fulldata$country_0 <- ifelse(fulldata$country == "SCO", "SCO", fulldata$country_0)
fulldata$country_0 <- ifelse(fulldata$country == "WAL", "WAL", fulldata$country_0)
fulldata$country_0 <- ifelse(fulldata$country == "NIR", "NIR", fulldata$country_0)

fulldata$country_0 <- ifelse(fulldata$period >= 12 & fulldata$period < 15 & fulldata$country_0 %in% c("CZE", "SVK"), "CSK", fulldata$country_0)