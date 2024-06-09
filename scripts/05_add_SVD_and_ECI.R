# do RCA calcs
source("./scripts/05_zz_RCA.R")
dataRCA <- output

# exchange NA's for 0's
datatemp <- dataRCA[,33:ncol(dataRCA)]
datatemp[is.na(datatemp)] <- 0
dataRCA <- cbind(dataRCA[,1:32], datatemp)
rm(datatemp)

if(HPI_incl == "Y"){
  for(i in 1:nrow(data)){
    data$births[i] <- sum(data[i, str_sub(colnames(data), start = -4) == "born"], na.rm = T)
    data$deaths[i] <- sum(data[i, str_sub(colnames(data), start = -4) == "died"], na.rm = T)
    data$emigrants[i] <- sum(data[i, str_sub(colnames(data), start = -9) == "emigrated"], na.rm = T)
    data$immigrants[i] <- sum(data[i, str_sub(colnames(data), start = -10) == "immigrated"], na.rm = T)
  }
  
  # data$GDP <- data$GDPpc * (data$births + data$deaths)
}

data$births_squared <- data$births^2
data$deaths_squared <- data$deaths^2
data$emigrants_squared <- data$emigrants^2
data$immigrants_squared <- data$immigrants^2

if(normalization_option == "log"){
  for(m in c(13:(ncol(data)-15))){
    data[,m] <- log10(data[,m]+1)
  } 
}

if(normalization_option == "ihs"){
  for(m in c(13:(ncol(data)-15))){
    data[,m] <- asinh(data[,m])
  } 
}
  
data$SVD_factor1_born <- NA
data$SVD_factor2_born <- NA
data$SVD_factor3_born <- NA
data$SVD_factor4_born <- NA
data$SVD_factor5_born <- NA

data$SVD_factor1_died <- NA
data$SVD_factor2_died <- NA
data$SVD_factor3_died <- NA
data$SVD_factor4_died <- NA
data$SVD_factor5_died <- NA

data$SVD_factor1_emigrated <- NA
data$SVD_factor2_emigrated <- NA
data$SVD_factor3_emigrated <- NA
data$SVD_factor4_emigrated <- NA
data$SVD_factor5_emigrated <- NA

data$SVD_factor1_immigrated <- NA
data$SVD_factor2_immigrated <- NA
data$SVD_factor3_immigrated <- NA
data$SVD_factor4_immigrated <- NA
data$SVD_factor5_immigrated <- NA

for(p in 1:15){
  
  misc <- subset(data, period == p)
  
  misc3 <- misc[,1:20]
  
  # births
  misc2 <- misc[,str_sub(colnames(misc), start = -4) == "born"]
  rownames(misc2) <- misc3$ID
  
  cols <- as.data.frame(colSums(misc2))
  rows <- as.data.frame(rowSums(misc2))
  
  if(p < 8){
    cutoffrow <- 3
    cutoffcol <- 3
  }
  if(p > 7 & p < 15){
    cutoffrow <- 5
    cutoffcol <- 5
  }
  if(p == 15){
    cutoffrow <- 10
    cutoffcol <- 10
  }
  
  if(normalization_option == "log"){
  cols <- subset(cols, cols[,1] >= log10(1+cutoffcol))
  }
  if(normalization_option == "ihs"){
    cols <- subset(cols, cols[,1] >= asinh(cutoffcol))
  }
  
  misc2 <- as.data.frame(t(subset(t(misc2), colnames(misc2) %in% rownames(cols))))
  
  svd <- svd(misc2)
  
  misc2 <- cbind(misc2, svd$u[,1:5])
  colnames(misc2)[(ncol(misc2)-4):ncol(misc2)] <- c(paste0("SVD_factor1_born"),
                                                    paste0("SVD_factor2_born"),
                                                    paste0("SVD_factor3_born"),
                                                    paste0("SVD_factor4_born"),
                                                    paste0("SVD_factor5_born"))
  
  misc3 <- merge(misc3, misc2, all.x=T, by.x="ID", by.y="row.names")
  
  # emigrants
  misc2 <- misc[,str_sub(colnames(misc), start = -9) == "emigrated"]
  rownames(misc2) <- misc3$ID
  
  cols <- as.data.frame(colSums(misc2))
  rows <- as.data.frame(rowSums(misc2))
  
  if(p < 8){
    cutoffrow <- 3
    cutoffcol <- 3
  }
  if(p > 7 & p < 15){
    cutoffrow <- 5
    cutoffcol <- 5
  }
  if(p == 15){
    cutoffrow <- 10
    cutoffcol <- 10
  }
  
  
  if(normalization_option == "log"){
    cols <- subset(cols, cols[,1] >= log10(1+cutoffcol))
  }
  if(normalization_option == "ihs"){
    cols <- subset(cols, cols[,1] >= asinh(cutoffcol))
  }
  
  misc2 <- as.data.frame(t(subset(t(misc2), colnames(misc2) %in% rownames(cols))))
  
  svd <- svd(misc2)
  
  misc2 <- cbind(misc2, svd$u[,1:5])
  colnames(misc2)[(ncol(misc2)-4):ncol(misc2)] <- c(paste0("SVD_factor1_emigrated"),
                                                    paste0("SVD_factor2_emigrated"),
                                                    paste0("SVD_factor3_emigrated"),
                                                    paste0("SVD_factor4_emigrated"),
                                                    paste0("SVD_factor5_emigrated"))
  
  misc3 <- merge(misc3, misc2, all.x=T, by.x="ID", by.y="row.names")
  
  # deaths
  misc2 <- misc[,str_sub(colnames(misc), start = -4) == "died"]
  rownames(misc2) <- misc3$ID
  
  cols <- as.data.frame(colSums(misc2))
  rows <- as.data.frame(rowSums(misc2))
  
  if(p < 8){
    cutoffrow <- 3
    cutoffcol <- 3
  }
  if(p > 7 & p < 15){
    cutoffrow <- 5
    cutoffcol <- 5
  }
  if(p == 15){
    cutoffrow <- 10
    cutoffcol <- 10
  }
  
  if(normalization_option == "log"){
    cols <- subset(cols, cols[,1] >= log10(1+cutoffcol))
  }
  if(normalization_option == "ihs"){
    cols <- subset(cols, cols[,1] >= asinh(cutoffcol))
  }
  
  misc2 <- as.data.frame(t(subset(t(misc2), colnames(misc2) %in% rownames(cols))))
  
  svd <- svd(misc2)
  
  misc2 <- cbind(misc2, svd$u[,1:5])
  colnames(misc2)[(ncol(misc2)-4):ncol(misc2)] <- c(paste0("SVD_factor1_died"),
                                                    paste0("SVD_factor2_died"),
                                                    paste0("SVD_factor3_died"),
                                                    paste0("SVD_factor4_died"),
                                                    paste0("SVD_factor5_died"))
  
  misc3 <- merge(misc3, misc2, all.x=T, by.x="ID", by.y="row.names")
  
  # immigrants
  misc2 <- misc[,str_sub(colnames(misc), start = -10) == "immigrated"]
  rownames(misc2) <- misc3$ID
  
  cols <- as.data.frame(colSums(misc2))
  rows <- as.data.frame(rowSums(misc2))
  
  if(p < 8){
    cutoffrow <- 3
    cutoffcol <- 3
  }
  if(p > 7 & p < 15){
    cutoffrow <- 5
    cutoffcol <- 5
  }
  if(p == 15){
    cutoffrow <- 10
    cutoffcol <- 10
  }
  
  if(normalization_option == "log"){
    cols <- subset(cols, cols[,1] >= log10(1+cutoffcol))
  }
  if(normalization_option == "ihs"){
    cols <- subset(cols, cols[,1] >= asinh(1+cutoffcol))
  }
  
  misc2 <- as.data.frame(t(subset(t(misc2), colnames(misc2) %in% rownames(cols))))
  
  svd <- svd(misc2)
  
  misc2 <- cbind(misc2, svd$u[,1:5])
  colnames(misc2)[(ncol(misc2)-4):ncol(misc2)] <- c(paste0("SVD_factor1_immigrated"),
                                                    paste0("SVD_factor2_immigrated"),
                                                    paste0("SVD_factor3_immigrated"),
                                                    paste0("SVD_factor4_immigrated"),
                                                    paste0("SVD_factor5_immigrated"))
  
  misc3 <- merge(misc3, misc2, all.x=T, by.x="ID", by.y="row.names")
  
  data$SVD_factor1_born <- ifelse(is.na(data$SVD_factor1_born), vlookup(data$ID, misc3, result_column = "SVD_factor1_born"), data$SVD_factor1_born)
  data$SVD_factor2_born <- ifelse(is.na(data$SVD_factor2_born), vlookup(data$ID, misc3, result_column = "SVD_factor2_born"), data$SVD_factor2_born)
  data$SVD_factor3_born <- ifelse(is.na(data$SVD_factor3_born), vlookup(data$ID, misc3, result_column = "SVD_factor3_born"), data$SVD_factor3_born)
  data$SVD_factor4_born <- ifelse(is.na(data$SVD_factor4_born), vlookup(data$ID, misc3, result_column = "SVD_factor4_born"), data$SVD_factor4_born)
  data$SVD_factor5_born <- ifelse(is.na(data$SVD_factor5_born), vlookup(data$ID, misc3, result_column = "SVD_factor5_born"), data$SVD_factor5_born)
  
  data$SVD_factor1_emigrated <- ifelse(is.na(data$SVD_factor1_emigrated), vlookup(data$ID, misc3, result_column = "SVD_factor1_emigrated"), data$SVD_factor1_emigrated)
  data$SVD_factor2_emigrated <- ifelse(is.na(data$SVD_factor2_emigrated), vlookup(data$ID, misc3, result_column = "SVD_factor2_emigrated"), data$SVD_factor2_emigrated)
  data$SVD_factor3_emigrated <- ifelse(is.na(data$SVD_factor3_emigrated), vlookup(data$ID, misc3, result_column = "SVD_factor3_emigrated"), data$SVD_factor3_emigrated)
  data$SVD_factor4_emigrated <- ifelse(is.na(data$SVD_factor4_emigrated), vlookup(data$ID, misc3, result_column = "SVD_factor4_emigrated"), data$SVD_factor4_emigrated)
  data$SVD_factor5_emigrated <- ifelse(is.na(data$SVD_factor5_emigrated), vlookup(data$ID, misc3, result_column = "SVD_factor5_emigrated"), data$SVD_factor5_emigrated)
  
  data$SVD_factor1_died <- ifelse(is.na(data$SVD_factor1_died), vlookup(data$ID, misc3, result_column = "SVD_factor1_died"), data$SVD_factor1_died)
  data$SVD_factor2_died <- ifelse(is.na(data$SVD_factor2_died), vlookup(data$ID, misc3, result_column = "SVD_factor2_died"), data$SVD_factor2_died)
  data$SVD_factor3_died <- ifelse(is.na(data$SVD_factor3_died), vlookup(data$ID, misc3, result_column = "SVD_factor3_died"), data$SVD_factor3_died)
  data$SVD_factor4_died <- ifelse(is.na(data$SVD_factor4_died), vlookup(data$ID, misc3, result_column = "SVD_factor4_died"), data$SVD_factor4_died)
  data$SVD_factor5_died <- ifelse(is.na(data$SVD_factor5_died), vlookup(data$ID, misc3, result_column = "SVD_factor5_died"), data$SVD_factor5_died)
  
  data$SVD_factor1_immigrated <- ifelse(is.na(data$SVD_factor1_immigrated), vlookup(data$ID, misc3, result_column = "SVD_factor1_immigrated"), data$SVD_factor1_immigrated)
  data$SVD_factor2_immigrated <- ifelse(is.na(data$SVD_factor2_immigrated), vlookup(data$ID, misc3, result_column = "SVD_factor2_immigrated"), data$SVD_factor2_immigrated)
  data$SVD_factor3_immigrated <- ifelse(is.na(data$SVD_factor3_immigrated), vlookup(data$ID, misc3, result_column = "SVD_factor3_immigrated"), data$SVD_factor3_immigrated)
  data$SVD_factor4_immigrated <- ifelse(is.na(data$SVD_factor4_immigrated), vlookup(data$ID, misc3, result_column = "SVD_factor4_immigrated"), data$SVD_factor4_immigrated)
  data$SVD_factor5_immigrated <- ifelse(is.na(data$SVD_factor5_immigrated), vlookup(data$ID, misc3, result_column = "SVD_factor5_immigrated"), data$SVD_factor5_immigrated)
  
}

data <- data %>% select(ID:racedriver_immigrated, SVD_factor1_born:SVD_factor5_immigrated, year1300:year2000)

factorcols <- c("year1300", "year1350", "year1400", "year1450", "year1500", "year1550", "year1600", "year1650", "year1700", "year1750", "year1800", "year1850", "year1900", "year1950", "year2000")
data[factorcols] <- lapply(data[factorcols], factor)
rm(factorcols)

# implement dummies for subregion_time factors
data$southern_europe_upto1800 <- ifelse(data$UN_subregion == "Southern Europe up to 1800", 1, 0)
data$southern_europe_1850 <- ifelse(data$UN_subregion == "Southern Europe in 1850", 1, 0)
data$southern_europe_1900 <- ifelse(data$UN_subregion == "Southern Europe in 1900", 1, 0)
data$southern_europe_1950 <- ifelse(data$UN_subregion == "Southern Europe in 1950", 1, 0)
data$southern_europe_2000 <- ifelse(data$UN_subregion == "Southern Europe in 2000", 1, 0)
data$western_europe_before1800 <- ifelse(data$UN_subregion == "Western Europe before 1800", 1, 0)
data$western_europe_1800 <- ifelse(data$UN_subregion == "Western Europe in 1800", 1, 0)
data$eastern_europe_before1800 <- ifelse(data$UN_subregion == "Eastern Europe before 1800", 1, 0)
data$eastern_europe_1800 <- ifelse(data$UN_subregion == "Eastern Europe in 1800", 1, 0)
data$western_europe_1850 <- ifelse(data$UN_subregion == "Western Europe in 1850", 1, 0)
data$western_europe_1900 <- ifelse(data$UN_subregion == "Western Europe in 1900", 1, 0)
data$western_europe_1950 <- ifelse(data$UN_subregion == "Western Europe in 1950", 1, 0)
data$western_europe_2000 <- ifelse(data$UN_subregion == "Western Europe in 2000", 1, 0)
data$eastern_europe_1850 <- ifelse(data$UN_subregion == "Eastern Europe in 1850", 1, 0)
data$eastern_europe_1900 <- ifelse(data$UN_subregion == "Eastern Europe in 1900", 1, 0)
data$soviet_1950 <- ifelse(data$UN_subregion == "Soviet Union in 1950", 1, 0)
data$soviet_2000 <- ifelse(data$UN_subregion == "Soviet Union in 2000", 1, 0)
data$northern_europe_upto1800 <- ifelse(data$UN_subregion == "Northern Europe up to 1800", 1, 0)
data$northern_europe_1850 <- ifelse(data$UN_subregion == "Northern Europe in 1850", 1, 0)
data$northern_europe_1900 <- ifelse(data$UN_subregion == "Northern Europe in 1900", 1, 0)
data$northern_europe_1950 <- ifelse(data$UN_subregion == "Northern Europe in 1950", 1, 0)
data$northern_europe_2000 <- ifelse(data$UN_subregion == "Northern Europe in 2000", 1, 0)
data$northern_america_before1800 <- ifelse(data$UN_subregion == "Northern America before 1800", 1, 0)
data$northern_america_1800 <- ifelse(data$UN_subregion == "Northern America in 1800", 1, 0)
data$northern_america_1850 <- ifelse(data$UN_subregion == "Northern America in 1850", 1, 0)
data$northern_america_1900 <- ifelse(data$UN_subregion == "Northern America in 1900", 1, 0)
data$northern_america_1950 <- ifelse(data$UN_subregion == "Northern America in 1950", 1, 0)
data$northern_america_2000 <- ifelse(data$UN_subregion == "Northern America in 2000", 1, 0)

data$SVD_factor1_born_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor1_born, 0)
data$SVD_factor1_born <- NULL

data$SVD_factor2_born_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor2_born, 0)
data$SVD_factor2_born <- NULL

data$SVD_factor3_born_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor3_born, 0)
data$SVD_factor3_born <- NULL

data$SVD_factor4_born_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor4_born, 0)
data$SVD_factor4_born <- NULL

data$SVD_factor5_born_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor5_born, 0)
data$SVD_factor5_born <- NULL

data$SVD_factor1_emigrated_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor1_emigrated, 0)
data$SVD_factor1_emigrated <- NULL

data$SVD_factor2_emigrated_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor2_emigrated, 0)
data$SVD_factor2_emigrated <- NULL

data$SVD_factor3_emigrated_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor3_emigrated, 0)
data$SVD_factor3_emigrated <- NULL

data$SVD_factor4_emigrated_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor4_emigrated, 0)
data$SVD_factor4_emigrated <- NULL

data$SVD_factor5_emigrated_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor5_emigrated, 0)
data$SVD_factor5_emigrated <- NULL

data$SVD_factor1_died_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor1_died, 0)
data$SVD_factor1_died <- NULL

data$SVD_factor2_died_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor2_died, 0)
data$SVD_factor2_died <- NULL

data$SVD_factor3_died_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor3_died, 0)
data$SVD_factor3_died <- NULL

data$SVD_factor4_died_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor4_died, 0)
data$SVD_factor4_died <- NULL

data$SVD_factor5_died_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor5_died, 0)
data$SVD_factor5_died <- NULL

data$SVD_factor1_immigrated_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor1_immigrated, 0)
data$SVD_factor1_immigrated <- NULL

data$SVD_factor2_immigrated_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor2_immigrated, 0)
data$SVD_factor2_immigrated <- NULL

data$SVD_factor3_immigrated_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor3_immigrated, 0)
data$SVD_factor3_immigrated <- NULL

data$SVD_factor4_immigrated_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor4_immigrated, 0)
data$SVD_factor4_immigrated <- NULL

data$SVD_factor5_immigrated_year1300 <- ifelse(data$year1300 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1350 <- ifelse(data$year1350 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1400 <- ifelse(data$year1400 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1450 <- ifelse(data$year1450 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1500 <- ifelse(data$year1500 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1550 <- ifelse(data$year1550 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1600 <- ifelse(data$year1600 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1650 <- ifelse(data$year1650 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1700 <- ifelse(data$year1700 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1750 <- ifelse(data$year1750 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1800 <- ifelse(data$year1800 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1850 <- ifelse(data$year1850 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1900 <- ifelse(data$year1900 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year1950 <- ifelse(data$year1950 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated_year2000 <- ifelse(data$year2000 == 1, data$SVD_factor5_immigrated, 0)
data$SVD_factor5_immigrated <- NULL

# rename some columns
data <- data %>%
  dplyr::rename("diversity_died" = "diversity_deaths", "diversity_immigrated" = "diversity_immigrants",
         "diversity_emigrated" = "diversity_emigrants",
         "ubiquity_died" = "ubiquity_deaths", "ubiquity_immigrated" = "ubiquity_immigrants",
         "ubiquity_emigrated" = "ubiquity_emigrants",
         "avg_age_born" = "avg_age_births", "avg_age_died" = "avg_age_deaths",
         "avg_age_immigrated" = "avg_age_immigrants", "avg_age_emigrated" = "avg_age_emigrants"
  )

data$histperiod <- ifelse(data$year < 1501, 1,
                       ifelse(data$year > 1500 & data$year < 1800,2, 
                              ifelse(data$year > 1799 & data$year < 1900, 3,
                                     ifelse(data$year > 1899 & data$year < 1951, 4,
                                            ifelse(data$year > 1950 & data$year < 2001, 5,
                                                   NA)))))

data$year <- as.factor(data$year)

cols <- match("year1300", colnames(data)):match("year2000", colnames(data))
data[ , cols] <- apply(data[ , cols], 2,            # Specify own function within apply
                       function(x) as.numeric(as.character(x)))

# INCLUDE ECI

binarydata <- dataRCA

tempdata <- binarydata[,33:ncol(binarydata)]

tempdata <- tempdata[,(str_sub(colnames(tempdata), start = -4) == "born" | str_sub(colnames(tempdata), start = -4) == "died" | str_sub(colnames(tempdata), start = -10) == "immigrated" | str_sub(colnames(tempdata), start = -9) == "emigrated")]
if(normalization_option == "ihs"){
  tempdata[tempdata < asinh(1)] <- 0
  tempdata[tempdata > asinh(1)] <- 1
}
if(normalization_option == "log"){
  tempdata[tempdata < log10(1+1)] <- 0
  tempdata[tempdata > log10(1+1)] <- 1
}

binarydata <- cbind(binarydata[1:32], tempdata, binarydata[,(str_sub(colnames(binarydata), end = 3) == "SVD")], binarydata[,(ncol(binarydata)-14):ncol(binarydata)])

data$ECI_births <- NA
data$ECI_deaths <- NA
data$ECI_immigrants <- NA
data$ECI_emigrants <- NA

for(p in 1:15){
  
  misc <- subset(binarydata, period == p)
  
  misc2 <- misc[,(str_sub(colnames(misc), start = -4) == "born" | str_sub(colnames(misc), start = -4) == "died" | str_sub(colnames(misc), start = -10) == "immigrated" | str_sub(colnames(misc), start = -9) == "emigrated")]
  misc2 <- misc2[,(str_sub(colnames(misc2), end = 3) != "SVD")]
  
  misc_births <- misc2[,(str_sub(colnames(misc2), start = -4) == "born")]
  misc_deaths <- misc2[,(str_sub(colnames(misc2), start = -4) == "died")]
  misc_immigrants <- misc2[,(str_sub(colnames(misc2), start = -10) == "immigrated")]
  misc_emigrants <- misc2[,(str_sub(colnames(misc2), start = -9) == "emigrated")]
  
  if(p < 8){
    cutoffrow <- 8
    cutoffcol <- 8
  }
  if(p > 7 & p < 15){
    cutoffrow <- 20
    cutoffcol <- 20
  }
  if(p == 15){
    cutoffrow <- 50
    cutoffcol <- 50 
  }
  
  misc_births <- subset(misc_births, misc$births >= cutoffrow)
  misc_births <- t(subset(t(misc_births), colSums(misc_births) >= cutoffcol))
  
  misc_deaths <- subset(misc_deaths, misc$deaths >= cutoffrow)
  misc_deaths <- t(subset(t(misc_deaths), colSums(misc_deaths) >= cutoffcol))
  
  misc_immigrants <- subset(misc_immigrants, misc$immigrants >= cutoffrow)
  misc_immigrants <- t(subset(t(misc_immigrants), colSums(misc_immigrants) >= cutoffcol))
  
  misc_emigrants <- subset(misc_emigrants, misc$emigrants >= cutoffrow)
  misc_emigrants <- t(subset(t(misc_emigrants), colSums(misc_emigrants) >= cutoffcol))
  
  ECI_births <- as.data.frame(morc(misc_births, steps = 20))
  ECI_deaths <- as.data.frame(morc(misc_deaths, steps = 20))
  ECI_immigrants <- as.data.frame(morc(misc_immigrants, steps = 20))
  ECI_emigrants <- as.data.frame(morc(misc_emigrants, steps = 20))
  
  data$ECI_births <- ifelse(is.na(data$ECI_births), vlookup(data$ID, ECI_births, lookup_column = "rownames", result_column = 1),  data$ECI_births)
  data$ECI_deaths <- ifelse(is.na(data$ECI_deaths), vlookup(data$ID, ECI_deaths, lookup_column = "rownames", result_column = 1),  data$ECI_deaths)
  data$ECI_immigrants <- ifelse(is.na(data$ECI_immigrants), vlookup(data$ID, ECI_immigrants, lookup_column = "rownames", result_column = 1),  data$ECI_immigrants)
  data$ECI_emigrants <- ifelse(is.na(data$ECI_emigrants), vlookup(data$ID, ECI_emigrants, lookup_column = "rownames", result_column = 1),  data$ECI_emigrants)
  
}

data$ECI_births <- ifelse(is.na(data$ECI_births), 0, data$ECI_births)
data$ECI_deaths <- ifelse(is.na(data$ECI_deaths), 0, data$ECI_deaths)
data$ECI_immigrants <- ifelse(is.na(data$ECI_immigrants), 0, data$ECI_immigrants)
data$ECI_emigrants <- ifelse(is.na(data$ECI_emigrants), 0, data$ECI_emigrants)

data$ECI_births_year1300 <- ifelse(data$year1300 == 1, data$ECI_births, 0)
data$ECI_births_year1350 <- ifelse(data$year1350 == 1, data$ECI_births, 0)
data$ECI_births_year1400 <- ifelse(data$year1400 == 1, data$ECI_births, 0)
data$ECI_births_year1450 <- ifelse(data$year1450 == 1, data$ECI_births, 0)
data$ECI_births_year1500 <- ifelse(data$year1500 == 1, data$ECI_births, 0)
data$ECI_births_year1550 <- ifelse(data$year1550 == 1, data$ECI_births, 0)
data$ECI_births_year1600 <- ifelse(data$year1600 == 1, data$ECI_births, 0)
data$ECI_births_year1650 <- ifelse(data$year1650 == 1, data$ECI_births, 0)
data$ECI_births_year1700 <- ifelse(data$year1700 == 1, data$ECI_births, 0)
data$ECI_births_year1750 <- ifelse(data$year1750 == 1, data$ECI_births, 0)
data$ECI_births_year1800 <- ifelse(data$year1800 == 1, data$ECI_births, 0)
data$ECI_births_year1850 <- ifelse(data$year1850 == 1, data$ECI_births, 0)
data$ECI_births_year1900 <- ifelse(data$year1900 == 1, data$ECI_births, 0)
data$ECI_births_year1950 <- ifelse(data$year1950 == 1, data$ECI_births, 0)
data$ECI_births_year2000 <- ifelse(data$year2000 == 1, data$ECI_births, 0)
data$ECI_births <- NULL

data$ECI_deaths_year1300 <- ifelse(data$year1300 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1350 <- ifelse(data$year1350 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1400 <- ifelse(data$year1400 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1450 <- ifelse(data$year1450 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1500 <- ifelse(data$year1500 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1550 <- ifelse(data$year1550 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1600 <- ifelse(data$year1600 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1650 <- ifelse(data$year1650 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1700 <- ifelse(data$year1700 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1750 <- ifelse(data$year1750 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1800 <- ifelse(data$year1800 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1850 <- ifelse(data$year1850 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1900 <- ifelse(data$year1900 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year1950 <- ifelse(data$year1950 == 1, data$ECI_deaths, 0)
data$ECI_deaths_year2000 <- ifelse(data$year2000 == 1, data$ECI_deaths, 0)
data$ECI_deaths <- NULL

data$ECI_immigrants_year1300 <- ifelse(data$year1300 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1350 <- ifelse(data$year1350 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1400 <- ifelse(data$year1400 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1450 <- ifelse(data$year1450 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1500 <- ifelse(data$year1500 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1550 <- ifelse(data$year1550 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1600 <- ifelse(data$year1600 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1650 <- ifelse(data$year1650 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1700 <- ifelse(data$year1700 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1750 <- ifelse(data$year1750 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1800 <- ifelse(data$year1800 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1850 <- ifelse(data$year1850 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1900 <- ifelse(data$year1900 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year1950 <- ifelse(data$year1950 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants_year2000 <- ifelse(data$year2000 == 1, data$ECI_immigrants, 0)
data$ECI_immigrants <- NULL

data$ECI_emigrants_year1300 <- ifelse(data$year1300 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1350 <- ifelse(data$year1350 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1400 <- ifelse(data$year1400 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1450 <- ifelse(data$year1450 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1500 <- ifelse(data$year1500 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1550 <- ifelse(data$year1550 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1600 <- ifelse(data$year1600 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1650 <- ifelse(data$year1650 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1700 <- ifelse(data$year1700 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1750 <- ifelse(data$year1750 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1800 <- ifelse(data$year1800 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1850 <- ifelse(data$year1850 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1900 <- ifelse(data$year1900 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year1950 <- ifelse(data$year1950 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants_year2000 <- ifelse(data$year2000 == 1, data$ECI_emigrants, 0)
data$ECI_emigrants <- NULL



# ADDING INCOME LEVELS AT THE START OF THE PERIOD 
# I.e. before 1300, 1500, 1750, 1850, and 1950
# Based on the source data we have available

t1_south <- 1519 # GDPpc of Spain in 1295 according to Maddison
t1_north <- 1155 # GDPpc of England in 1295 according to Maddison
t1_rest <- 1219 # GDPpc of France in 1295 according to Maddison

t2 <- subset(data, year == 1500 & !is.na(GDPpc))
t2_south <- t2 %>% 
  filter(country_0 %in% c("ITA", "ESP")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t2_north <- t2 %>% 
  filter(country_0 %in% c("GBR", "SWE")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t2_west <- t2 %>% 
  filter(country_0 %in% c("BEL", "DEU", "FRA", "NLD")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t2_east <- t2 %>% 
  filter(country_0 %in% c("POL")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t2_america <- t2 %>% 
  summarize(mean(GDPpc)) %>%
  as.numeric()

t3 <- subset(data, year == 1750 & !is.na(GDPpc) & country == country_0)
t3_south <- t3 %>% 
  filter(country_0 %in% c("ITA", "ESP")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t3_north <- t3 %>% 
  filter(country_0 %in% c("GBR", "SWE")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t3_west <- t3 %>% 
  filter(country_0 %in% c("BEL", "DEU", "FRA", "NLD")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t3_east <- t3 %>% 
  filter(country_0 %in% c("POL")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t3_america <- subset(data, year == 1700 & !is.na(GDPpc) & country == country_0) %>% 
  filter(country_0 %in% c("USA")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()

t4 <- subset(data, year == 1850 & !is.na(GDPpc)  & country == country_0)
t4_south <- t4 %>% 
  filter(UN_subregion_broad %in% c("Southern Europe")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t4_north <- t4 %>% 
  filter(UN_subregion_broad %in% c("Northern Europe")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t4_west <- t4 %>% 
  filter(UN_subregion_broad %in% c("Western Europe")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t4_east <- t4 %>% 
  filter(UN_subregion_broad %in% c("Eastern Europe", "Western Asia")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t4_america <- t4 %>% 
  filter(UN_subregion_broad %in% c("Northern America")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()

t5 <- subset(data, year == 1950 & !is.na(GDPpc)  & country == country_0)
t5_south <- t5 %>% 
  filter(UN_subregion_broad %in% c("Southern Europe")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t5_north <- t5 %>% 
  filter(UN_subregion_broad %in% c("Northern Europe")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t5_west <- t5 %>% 
  filter(UN_subregion_broad %in% c("Western Europe")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t5_east <- t5 %>% 
  filter(UN_subregion_broad %in% c("Eastern Europe", "Western Asia")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()
t5_america <- t5 %>% 
  filter(UN_subregion_broad %in% c("Northern America")) %>%
  summarize(mean(GDPpc)) %>%
  as.numeric()

data$GDPpc_t0 <- NA

data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 1 & data$UN_subregion_broad == "Southern Europe", t1_south, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 1 & data$UN_subregion_broad == "Northern Europe", t1_north, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 1 & data$UN_subregion_broad %in% c("Western Europe", "Eastern Europe", "Western Asia", "Northern America"), t1_rest, data$GDPpc_t0)

data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 2 & data$UN_subregion_broad == "Southern Europe", t2_south, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 2 & data$UN_subregion_broad == "Northern Europe", t2_north, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 2 & data$UN_subregion_broad == "Western Europe", t2_west, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 2 & data$UN_subregion_broad %in% c("Eastern Europe", "Western Asia"), t2_east, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 2 & data$UN_subregion_broad == "Northern America", t2_america, data$GDPpc_t0)

data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 3 & data$UN_subregion_broad == "Southern Europe", t3_south, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 3 & data$UN_subregion_broad == "Northern Europe", t3_north, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 3 & data$UN_subregion_broad == "Western Europe", t3_west, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 3 & data$UN_subregion_broad %in% c("Eastern Europe", "Western Asia"), t3_east, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 3 & data$UN_subregion_broad == "Northern America", t3_america, data$GDPpc_t0)

data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 4 & data$UN_subregion_broad == "Southern Europe", t4_south, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 4 & data$UN_subregion_broad == "Northern Europe", t4_north, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 4 & data$UN_subregion_broad == "Western Europe", t4_west, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 4 & data$UN_subregion_broad %in% c("Eastern Europe", "Western Asia"), t4_east, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 4 & data$UN_subregion_broad == "Northern America", t4_america, data$GDPpc_t0)

data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 5 & (data$UN_subregion_broad == "Southern Europe" | data$UN_subregion == "Southern Europe in 2000"), t5_south, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 5 & data$UN_subregion_broad == "Northern Europe", t5_north, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 5 & data$UN_subregion_broad == "Western Europe", t5_west, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 5 & data$UN_subregion_broad %in% c("Eastern Europe", "Western Asia"), t5_east, data$GDPpc_t0)
data$GDPpc_t0 <- ifelse(is.na(data$GDPpc_t0) & data$histperiod == 5 & data$UN_subregion_broad == "Northern America", t5_america, data$GDPpc_t0)

for(m in 1:nrow(data)){
  
  # Is there a GDP level at the start of the period?
  # Either in the exact same country / region, or the country_0
  # For the first period, we stick with supranational averages due to too few observations before 1300
  tempGDP <- ifelse(data$histperiod[m] == 2, vlookup(paste(data$country[m], 1500, sep="_"), data, lookup_column = "ID2", result_column = "GDPpc"),
                           ifelse(data$histperiod[m] == 3, vlookup(paste(data$country[m], 1750, sep="_"), data, lookup_column = "ID2", result_column = "GDPpc"),
                                  ifelse(data$histperiod[m] == 4, vlookup(paste(data$country[m], 1850, sep="_"), data, lookup_column = "ID2", result_column = "GDPpc"),
                                         ifelse(data$histperiod[m] == 5, vlookup(paste(data$country[m], 1950, sep="_"), data, lookup_column = "ID2", result_column = "GDPpc"), NA))))
  
  data$GDPpc_t0[m] <- ifelse(!is.na(tempGDP), tempGDP, data$GDPpc_t0[m]) 
  
  rm(tempGDP)
}

data$GDPpc_t0 <- log10(data$GDPpc_t0)

data <- data %>% select(ID:period, histperiod, year:population, GDPpc_t0, births:ECI_emigrants_year2000)


