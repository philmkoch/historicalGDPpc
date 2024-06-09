# We manually adjusted occupations on level 3 in the database by Laouenan et al. to improve coverage and consistency
# The following two files reflect the manual changes
occs_science_ADAP <- read.csv2("./misc/occupations_manuallychecked/occs_science_ADAP.csv")
occus_misc <- read.csv2("./misc/occupations_manuallychecked/occs_misc_ADAP.csv")

occs_science_ADAP$new <- ifelse(occs_science_ADAP$new == "", "science/other", occs_science_ADAP$new)

data$occupation <- vlookup(data$level3_main_occ, occs_science_ADAP, lookup_column = "x", result_column = "new")

occus_misc <- subset(occus_misc, new != "")

list_occs_level3_nonscience <- c("actor", "writer", "painter", "singer", "music", "composer", "poet", "artist", "conductor", "explorer",
                                 "journalist", "architect", "football", "athletic", "baseball", "hockey", "basketball", "cricket", "tennis", "racedriver", "politician", 
                                 "lawyer", "priest", "chess", "sculptor", "photographer", "inventor", "business", "producer", "dancer", "designer", "activist")

data$occupation <- ifelse(is.na(data$occupation) & data$level3_main_occ %in% list_occs_level3_nonscience, data$level3_main_occ, data$occupation)

data$occupation <- ifelse(is.na(data$occupation), vlookup(data$level3_main_occ, occus_misc, lookup_column = "level3_main_occ", result_column = "new"), data$occupation)

data$occupation <- ifelse(is.na(data$occupation) & data$level2_main_occ == "Military", "military", data$occupation)
data$occupation <- ifelse(is.na(data$occupation) & data$level2_main_occ == "Corporate/Executive/Business (large)", "business", data$occupation)
data$occupation <- ifelse(is.na(data$occupation) & data$level2_main_occ == "Nobility", "nobility", data$occupation)

data$occupation <- ifelse(is.na(data$occupation) & data$level3_main_occ == "film" & str_detect(data$level3_all_occ, "actor"), "actor", data$occupation)
data$occupation <- ifelse(is.na(data$occupation) & data$level3_main_occ == "film" & str_detect(data$level3_all_occ, "writer"), "writer", data$occupation)

occus <- as.data.frame(unique(subset(data, !is.na(occupation))$occupation))
colnames(occus) <- "occupation"

# There are sill 121K individuals we could not assign an occupation to based on the rules in the prior lines of code
# Hence, we exploit the column level3_all_occ in the database and check whether one fo the 49 occupations appears in this column
# If multiple ones appear we choose the one occupation that appears the most often
# We follow a similar logic for the "science/other" category and remove individuals we cannot assign to a specific science occupation (since this category is too obscure otherwise)

remainingNAs <- subset(data, is.na(occupation))
for(k in 1:nrow(remainingNAs)){
  
  occus$no <- str_count(remainingNAs$level3_all_occ[k], occus$occupation)
  occus <- occus[order(occus$no, decreasing = T),]
  
  if(occus$no[1] > 0){
    remainingNAs$occupation[k] <- occus$occupation[1]
  }
  
}

data <- rbind(subset(data, !is.na(occupation)), remainingNAs)
data <- subset(data, !is.na(occupation))

tempdata <- subset(data, occupation == "science/other")

for(i in 1:nrow(tempdata)){
  
  strlocation <- as.data.frame(str_locate(tempdata$level3_all_occ[i], occus$occupation))
  rownames(strlocation) <- occus$occupation
  strlocation <- strlocation[order(strlocation$start),]
  seloccup <- rownames(strlocation)[1]
  
  tempdata$occupation[i] <- seloccup
  rm(seloccup)
  
}

data$occupation <- ifelse(data$occupation == "science/other", vlookup(data$wikidata_code, tempdata, result_column = "occupation"), data$occupation)

data <- subset(data, occupation != "science/other")
occus <- subset(occus, occupation != "science/other")

data$level2_main_occ <- ifelse(data$level2_main_occ == "Culture-core", "Culture", data$level2_main_occ)
data$level2_main_occ <- ifelse(data$level2_main_occ == "Culture-periphery", "Culture", data$level2_main_occ)

write.csv2(data, file="./raw_data/famous_individuals/cross-verified-database_withGEOGRAPHY_andOCCUPATIONS.csv", row.names = F)

# CREATING TREEMAP OF OCCUPATIONS

occudata <- data %>% group_by(level2_main_occ, occupation) %>% dplyr::summarize(n=n()) %>% data.frame()
occudata <- occudata[order(occudata$n, decreasing = T),]

occudata2 <- occudata[,c("occupation", "n")] %>% group_by(occupation) %>% dplyr::summarize(sum(n)) %>% data.frame()
occudata2$level2_main_occ <- vlookup(occudata2$occupation, occudata, lookup_column = "occupation", result_column = "level2_main_occ")

occudata <- occudata2

occudata$occupation2 <- paste0(occudata$occupation, " (", occudata$sum.n., ")")

pdf(file=paste0("./genfiles_", version, "/figures_SI/treemap_occus.pdf"), width=8, height = 6)
treemap(occudata, index = c("level2_main_occ", "occupation2"), vSize = "sum.n.", type="index",
        fontsize.labels = c(20, 12), palette = "Paired", overlap.labels = 1, title = "")
dev.off()


occus_sumstat <- data %>% group_by(occupation) %>% count() %>% as.data.frame()
write.csv2(occus_sumstat, file=paste0("./genfiles_", version, "/figures_SI/occupations_sumstat.csv"))

data7 <- subset(data, data$birth >= 1300 - lag_parameter & data$birth < 1300)
data8 <- subset(data, data$birth >= 1350 - lag_parameter & data$birth < 1350)
data9 <- subset(data, data$birth >= 1400 - lag_parameter & data$birth < 1400)
data10 <- subset(data, data$birth >= 1450 - lag_parameter & data$birth < 1450)
data11 <- subset(data, data$birth >= 1500 - lag_parameter & data$birth < 1500)
data12 <- subset(data, data$birth >= 1550 - lag_parameter & data$birth < 1550)
data13 <- subset(data, data$birth >= 1600 - lag_parameter & data$birth < 1600)
data14 <- subset(data, data$birth >= 1650 - lag_parameter & data$birth < 1650)
data15 <- subset(data, data$birth >= 1700 - lag_parameter & data$birth < 1700)
data16 <- subset(data, data$birth >= 1750 - lag_parameter & data$birth < 1750)
data17 <- subset(data, data$birth >= 1800 - lag_parameter & data$birth < 1800)
data18 <- subset(data, data$birth >= 1850 - lag_parameter & data$birth < 1850)
data19 <- subset(data, data$birth >= 1900 - lag_parameter & data$birth < 1900)
data20 <- subset(data, data$birth >= 1950 - lag_parameter & data$birth < 1950)
data21 <- subset(data, data$birth >= 2000 - lag_parameter & data$birth < 2000)

data_list <- list(data7, data8, data9, data10, data11, data12,
                  data13, data14, data15, data16, data17, data18, data19, data20, data21)
rm(data7, data8, data9, data10, data11, data12,
   data13, data14, data15, data16, data17, data18, data19, data20, data21)


# TABLE
tab <- as.data.frame(matrix(NA, nrow=length(data_list)))
tab$Period <- seq(1:length(data_list))
tab$Description <- c("13th century",
                     NA,
                     "14th century",
                     NA,
                     "15th century", 
                     NA,
                     "16th century", 
                     NA,
                     "17th century", 
                     NA,
                     "18th century", 
                     NA,
                     "19th century", 
                     NA,
                     "20th century")
tab$`included if born after` <- NA
for(i in 1:length(data_list)){
  tab$`included if born after`[i] <- round(min(data_list[[i]]$birth, na.rm=T), digits=-1)
}
#tab$`included if born after`[6] <- 1915
#tab$`included if died before` <- 500
for(i in 1:length(data_list)){
  tab$`included if born before`[i] <- max(data_list[[i]]$birth, na.rm=T)
}
tab$`No. of observations` <- NA
for(i in 1:length(data_list)){
  tab$`No. of observations`[i] <- nrow(data_list[[i]])
}
tab <- tab[,-1]
write.csv2(tab, row.names = F, file=paste0("./genfiles_", version, "/figures_SI/table_observations_halfcenturies.csv"))


