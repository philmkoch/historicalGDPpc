# RCA calcs Version June

output <- data.frame()

data2 <- data

data2$diversity <- NA
data2$diversity_deaths <- NA
data2$diversity_immigrants <- NA
data2$diversity_emigrants <- NA
data2$ubiquity <- NA
data2$ubiquity_deaths <- NA
data2$ubiquity_immigrants <- NA
data2$ubiquity_emigrants <- NA

for(p in 1:15){
  
  misc <- subset(data2, period == p)
  
  misc3 <- misc[,1:grep("avg_age_immigrants", colnames(misc))]
  
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
  
  cols <- subset(cols, cols[,1] >= cutoffcol)
  rows <- subset(rows, rows[,1] >= cutoffrow)
  
  misc2 <- subset(misc2, rownames(misc2) %in% rownames(rows))
  misc2 <- as.data.frame(t(subset(t(misc2), colnames(misc2) %in% rownames(cols))))
  
  miscRCA <- misc2
  for(i in 1:nrow(misc2)){
    for(j in 1:ncol(misc2)){
      miscRCA[i,j] <- (miscRCA[i,j] / cols[j,1]) / (rows[i,1] / sum(cols[,1]))
    }
  }
  
  miscRCA[miscRCA > quantile(reshape2::melt(miscRCA)$value, probs = 0.95)] <- quantile(reshape2::melt(miscRCA)$value, probs = 0.95)
  
  miscRCA <- asinh(miscRCA)
  
  svd <- svd(miscRCA)
  
  miscRCA <- cbind(miscRCA, svd$u[,1:5])
  colnames(miscRCA)[(ncol(miscRCA)-4):ncol(miscRCA)] <- c(paste0("SVD_factor1_born"),
                                                          paste0("SVD_factor2_born"),
                                                          paste0("SVD_factor3_born"),
                                                          paste0("SVD_factor4_born"),
                                                          paste0("SVD_factor5_born"))
  
  miscRCAbin <- miscRCA
  miscRCAbin[miscRCAbin >= asinh(1)] <- 1
  miscRCAbin[miscRCAbin < asinh(1)] <- 0
  
  # diversity
  RCAdiversity <- as.data.frame(morc(miscRCAbin, steps = 0))
  data2$diversity <- ifelse(is.na(data2$diversity), vlookup(data2$ID, RCAdiversity, lookup_column = "rownames", result_column = 1), data2$diversity)
  
  # average ubiquity
  RCAubiquity <- as.data.frame(morc(miscRCAbin, steps = 1))
  data2$ubiquity <- ifelse(is.na(data2$ubiquity), vlookup(data2$ID, RCAubiquity, lookup_column = "rownames", result_column = 1), data2$ubiquity)
  
  misc3 <- merge(misc3, miscRCA, all.x=T, by.x="ID", by.y="row.names")
  
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
  
  cols <- subset(cols, cols[,1] >= cutoffcol)
  rows <- subset(rows, rows[,1] >= cutoffrow)
  
  misc2 <- subset(misc2, rownames(misc2) %in% rownames(rows))
  misc2 <- as.data.frame(t(subset(t(misc2), colnames(misc2) %in% rownames(cols))))
  
  miscRCA <- misc2
  for(i in 1:nrow(misc2)){
    for(j in 1:ncol(misc2)){
      miscRCA[i,j] <- (miscRCA[i,j] / cols[j,1]) / (rows[i,1] / sum(cols[,1]))
    }
  }
  
  miscRCA[miscRCA > quantile(reshape2::melt(miscRCA)$value, probs = 0.95)] <- quantile(reshape2::melt(miscRCA)$value, probs = 0.95)
  
  miscRCA <- asinh(miscRCA)
  
  svd <- svd(miscRCA)
  
  miscRCA <- cbind(miscRCA, svd$u[,1:5])
  colnames(miscRCA)[(ncol(miscRCA)-4):ncol(miscRCA)] <- c(paste0("SVD_factor1_died"),
                                                          paste0("SVD_factor2_died"),
                                                          paste0("SVD_factor3_died"),
                                                          paste0("SVD_factor4_died"),
                                                          paste0("SVD_factor5_died"))
  
  miscRCAbin <- miscRCA
  miscRCAbin[miscRCAbin >= asinh(1)] <- 1
  miscRCAbin[miscRCAbin < asinh(1)] <- 0
  
  # diversity
  RCAdiversity <- as.data.frame(morc(miscRCAbin, steps = 0))
  data2$diversity_deaths <- ifelse(is.na(data2$diversity_deaths), vlookup(data2$ID, RCAdiversity, lookup_column = "rownames", result_column = 1), data2$diversity_deaths)
  
  # average ubiquity
  RCAubiquity <- as.data.frame(morc(miscRCAbin, steps = 1))
  data2$ubiquity_deaths <- ifelse(is.na(data2$ubiquity_deaths), vlookup(data2$ID, RCAubiquity, lookup_column = "rownames", result_column = 1), data2$ubiquity_deaths)
  
  misc3 <- merge(misc3, miscRCA, all.x=T, by.x="ID", by.y="row.names")
  
  
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
  
  cols <- subset(cols, cols[,1] >= cutoffcol)
  rows <- subset(rows, rows[,1] >= cutoffrow)
  
  misc2 <- subset(misc2, rownames(misc2) %in% rownames(rows))
  misc2 <- as.data.frame(t(subset(t(misc2), colnames(misc2) %in% rownames(cols))))
  
  miscRCA <- misc2
  for(i in 1:nrow(misc2)){
    for(j in 1:ncol(misc2)){
      miscRCA[i,j] <- (miscRCA[i,j] / cols[j,1]) / (rows[i,1] / sum(cols[,1]))
    }
  }
  
  miscRCA[miscRCA > quantile(reshape2::melt(miscRCA)$value, probs = 0.95)] <- quantile(reshape2::melt(miscRCA)$value, probs = 0.95)
  
  miscRCA <- asinh(miscRCA)
  
  svd <- svd(miscRCA)
  
  miscRCA <- cbind(miscRCA, svd$u[,1:5])
  colnames(miscRCA)[(ncol(miscRCA)-4):ncol(miscRCA)] <- c(paste0("SVD_factor1_immigrated"),
                                                          paste0("SVD_factor2_immigrated"),
                                                          paste0("SVD_factor3_immigrated"),
                                                          paste0("SVD_factor4_immigrated"),
                                                          paste0("SVD_factor5_immigrated"))
  
  miscRCAbin <- miscRCA
  miscRCAbin[miscRCAbin >= asinh(1)] <- 1
  miscRCAbin[miscRCAbin < asinh(1)] <- 0
  
  # diversity
  RCAdiversity <- as.data.frame(morc(miscRCAbin, steps = 0))
  data2$diversity_immigrants <- ifelse(is.na(data2$diversity_immigrants), vlookup(data2$ID, RCAdiversity, lookup_column = "rownames", result_column = 1), data2$diversity_immigrants)
  
  # average ubiquity
  RCAubiquity <- as.data.frame(morc(miscRCAbin, steps = 1))
  data2$ubiquity_immigrants <- ifelse(is.na(data2$ubiquity_immigrants), vlookup(data2$ID, RCAubiquity, lookup_column = "rownames", result_column = 1), data2$ubiquity_immigrants)
  
  misc3 <- merge(misc3, miscRCA, all.x=T, by.x="ID", by.y="row.names")
  
  
  
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
  
  cols <- subset(cols, cols[,1] >= cutoffcol)
  rows <- subset(rows, rows[,1] >= cutoffrow)
  
  misc2 <- subset(misc2, rownames(misc2) %in% rownames(rows))
  misc2 <- as.data.frame(t(subset(t(misc2), colnames(misc2) %in% rownames(cols))))
  
  miscRCA <- misc2
  for(i in 1:nrow(misc2)){
    for(j in 1:ncol(misc2)){
      miscRCA[i,j] <- (miscRCA[i,j] / cols[j,1]) / (rows[i,1] / sum(cols[,1]))
    }
  }
  
  miscRCA[miscRCA > quantile(reshape2::melt(miscRCA)$value, probs = 0.95)] <- quantile(reshape2::melt(miscRCA)$value, probs = 0.95)
  
  miscRCA <- asinh(miscRCA)
  
  svd <- svd(miscRCA)
  
  miscRCA <- cbind(miscRCA, svd$u[,1:5])
  colnames(miscRCA)[(ncol(miscRCA)-4):ncol(miscRCA)] <- c(paste0("SVD_factor1_emigrated"),
                                                          paste0("SVD_factor2_emigrated"),
                                                          paste0("SVD_factor3_emigrated"),
                                                          paste0("SVD_factor4_emigrated"),
                                                          paste0("SVD_factor5_emigrated"))
  
  miscRCAbin <- miscRCA
  miscRCAbin[miscRCAbin >= asinh(1)] <- 1
  miscRCAbin[miscRCAbin < asinh(1)] <- 0
  
  # diversity
  RCAdiversity <- as.data.frame(morc(miscRCAbin, steps = 0))
  data2$diversity_emigrants <- ifelse(is.na(data2$diversity_emigrants), vlookup(data2$ID, RCAdiversity, lookup_column = "rownames", result_column = 1), data2$diversity_emigrants)
  
  # average ubiquity
  RCAubiquity <- as.data.frame(morc(miscRCAbin, steps = 1))
  data2$ubiquity_emigrants <- ifelse(is.na(data2$ubiquity_emigrants), vlookup(data2$ID, RCAubiquity, lookup_column = "rownames", result_column = 1), data2$ubiquity_emigrants)
  
  misc3 <- merge(misc3, miscRCA, all.x=T, by.x="ID", by.y="row.names")
  
  
  output <- bind_rows(output, misc3)
  
}

output <- cbind(data2[,1:32], output[,33:ncol(output)], data2[,(ncol(data2)-14):ncol(data2)])
