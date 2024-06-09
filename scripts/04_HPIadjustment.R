laouenan <- read.csv2("./raw_data/famous_individuals/cross-verified-database_withGEOGRAPHY_andOCCUPATIONS.csv")

# build index of importance based on
# wiki editions
# page views
# age
laouenan$wiki_eds <- log(laouenan$number_wiki_editions)
laouenan$wiki_reads <- log(laouenan$wiki_readers_2015_2018, base = 10)
laouenan$age_log <- log(2023-laouenan$birth, base = 4)

laouenan$HPI <- ifelse(laouenan$birth < (2023-70), laouenan$wiki_eds + laouenan$wiki_reads + laouenan$age_log,
                       laouenan$wiki_eds + laouenan$wiki_reads + laouenan$age_log - (70-(2023-laouenan$birth))/7)
laouenan$HPI <- ifelse(is.infinite(laouenan$HPI), NA, laouenan$HPI)
laouenan$HPI <- ifelse(is.na(laouenan$HPI), min(laouenan$HPI, na.rm=T), laouenan$HPI)

laouenan$HPI <- 100 * (laouenan$HPI - min(laouenan$HPI)) / (max(laouenan$HPI) - min(laouenan$HPI))

laouenan$period <- ifelse(laouenan$birth >= 1300-lag_parameter & laouenan$birth < 1300, 1,
                          ifelse(laouenan$birth >= 1350-lag_parameter & laouenan$birth < 1350, 2,
                                 ifelse(laouenan$birth >= 1400-lag_parameter & laouenan$birth < 1400, 3,
                                        ifelse(laouenan$birth >= 1450-lag_parameter & laouenan$birth < 1450, 4,
                                               ifelse(laouenan$birth >= 1500-lag_parameter & laouenan$birth < 1500, 5,
                                                      ifelse(laouenan$birth >= 1550-lag_parameter & laouenan$birth < 1550, 6,
                                                             ifelse(laouenan$birth >= 1600-lag_parameter & laouenan$birth < 1600, 7,
                                                                    ifelse(laouenan$birth >= 1650-lag_parameter & laouenan$birth < 1650, 8,
                                                                           ifelse(laouenan$birth >= 1700-lag_parameter & laouenan$birth < 1700, 9,
                                                                                  ifelse(laouenan$birth >= 1750-lag_parameter & laouenan$birth < 1750, 10,
                                                                                         ifelse(laouenan$birth >= 1800-lag_parameter & laouenan$birth < 1800, 11,
                                                                                                ifelse(laouenan$birth >= 1850-lag_parameter & laouenan$birth < 1850, 12,
                                                                                                       ifelse(laouenan$birth >= 1900-lag_parameter & laouenan$birth < 1900, 13,
                                                                                                              ifelse(laouenan$birth >= 1950-lag_parameter & laouenan$birth < 1950, 14,
                                                                                                                     ifelse(laouenan$birth >= 2000-lag_parameter & laouenan$birth < 2000, 15, NA)))))))))))))))

# COMPARISON TO PANTHEON
pantheon <- read.csv("./raw_data/famous_individuals/Pantheon_v2020.csv")
laouenan$HPI_pantheon <- vlookup(laouenan$wikidata_code, pantheon, lookup_column = "wd_id", result_column = "hpi")

ggplot(laouenan, aes(x=HPI_pantheon, y=HPI)) + geom_point() + stat_poly_eq(formula = y~x, data=laouenan, aes(label = after_stat(rr.label)), parse = TRUE) +
  theme_light() + geom_point(size=1.5, color = "darkorange1") + labs(x="HPI (Pantheon)", y="HPI with data by Laouenan et al.") #+ geom_text(label = laouenan$wikidata_code)
ggsave(paste0("./genfiles_", version, "/figures_SI/HPI_comparison_to_pantheon.png"), width = 5, height = 5)

# REGIONS

HPI_born_ikt <- laouenan %>% group_by(bplace_ID, occupation, period) %>% dplyr::summarize(mean(HPI)) %>% data.frame()

HPI_born_ikt <- subset(HPI_born_ikt, is.na(bplace_ID) == F)

HPI_born_ikt$ID <- paste(HPI_born_ikt$bplace_ID, HPI_born_ikt$period, sep="_")
HPI_born_ikt$occupation <- paste(HPI_born_ikt$occupation, "born", sep="_")

occs <- unique(HPI_born_ikt$occupation)
rows <- unique(data$ID)
HPI_born_ikt$rowsoccs <- paste(HPI_born_ikt$ID, HPI_born_ikt$occupation, sep="_")

for(i in rows){
  for(j in occs){
    if(paste(i,j, sep="_") %in% HPI_born_ikt$rowsoccs){
      data[i,j] <- data[i,j] * subset(HPI_born_ikt, ID == i & occupation == j)$mean.HPI.
    }
  }
}

HPI_emi_ikt <- laouenan %>% subset(local == F) %>% group_by(bplace_ID, occupation, period) %>% dplyr::summarize(mean(HPI)) %>% data.frame()

HPI_emi_ikt <- subset(HPI_emi_ikt, is.na(bplace_ID) == F)

HPI_emi_ikt$ID <- paste(HPI_emi_ikt$bplace_ID, HPI_emi_ikt$period, sep="_")
HPI_emi_ikt$occupation <- paste(HPI_emi_ikt$occupation, "emigrated", sep="_")

occs <- unique(HPI_emi_ikt$occupation)
rows <- unique(data$ID)
HPI_emi_ikt$rowsoccs <- paste(HPI_emi_ikt$ID, HPI_emi_ikt$occupation, sep="_")

for(i in rows){
  for(j in occs){
    if(paste(i,j, sep="_") %in% HPI_emi_ikt$rowsoccs){
      data[i,j] <- data[i,j] * subset(HPI_emi_ikt, ID == i & occupation == j)$mean.HPI.
    }
  }
}


HPI_died_ikt <- laouenan %>% group_by(dplace_ID, occupation, period) %>% dplyr::summarize(mean(HPI)) %>% data.frame()

HPI_died_ikt <- subset(HPI_died_ikt, is.na(dplace_ID) == F)

HPI_died_ikt$ID <- paste(HPI_died_ikt$dplace_ID, HPI_died_ikt$period, sep="_")
HPI_died_ikt$occupation <- paste(HPI_died_ikt$occupation, "died", sep="_")

occs <- unique(HPI_died_ikt$occupation)
rows <- unique(data$ID)
HPI_died_ikt$rowsoccs <- paste(HPI_died_ikt$ID, HPI_died_ikt$occupation, sep="_")

for(i in rows){
  for(j in occs){
    if(paste(i,j, sep="_") %in% HPI_died_ikt$rowsoccs){
      data[i,j] <- data[i,j] * subset(HPI_died_ikt, ID == i & occupation == j)$mean.HPI.
    }
  }
}

HPI_immi_ikt <- laouenan %>% subset(local == F) %>% group_by(dplace_ID, occupation, period) %>% dplyr::summarize(mean(HPI)) %>% data.frame()

HPI_immi_ikt <- subset(HPI_immi_ikt, is.na(dplace_ID) == F)

HPI_immi_ikt$ID <- paste(HPI_immi_ikt$dplace_ID, HPI_immi_ikt$period, sep="_")
HPI_immi_ikt$occupation <- paste(HPI_immi_ikt$occupation, "immigrated", sep="_")

occs <- unique(HPI_immi_ikt$occupation)
rows <- unique(data$ID)
HPI_immi_ikt$rowsoccs <- paste(HPI_immi_ikt$ID, HPI_immi_ikt$occupation, sep="_")

for(i in rows){
  for(j in occs){
    if(paste(i,j, sep="_") %in% HPI_immi_ikt$rowsoccs){
      data[i,j] <- data[i,j] * subset(HPI_immi_ikt, ID == i & occupation == j)$mean.HPI.
    }
  }
}



# COUNTRIES

HPI_born_ikt <- laouenan %>% group_by(bplace_countrycode_v2, occupation, period) %>% dplyr::summarize(mean(HPI)) %>% data.frame()

HPI_born_ikt <- subset(HPI_born_ikt, is.na(bplace_countrycode_v2) == F)

HPI_born_ikt$ID <- paste(HPI_born_ikt$bplace_countrycode_v2, HPI_born_ikt$period, sep="_")
HPI_born_ikt$occupation <- paste(HPI_born_ikt$occupation, "born", sep="_")

occs <- unique(HPI_born_ikt$occupation)
rows <- unique(data$ID)
HPI_born_ikt$rowsoccs <- paste(HPI_born_ikt$ID, HPI_born_ikt$occupation, sep="_")

for(i in rows){
  for(j in occs){
    if(paste(i,j, sep="_") %in% HPI_born_ikt$rowsoccs){
      data[i,j] <- data[i,j] * subset(HPI_born_ikt, ID == i & occupation == j)$mean.HPI.
    }
  }
}

HPI_emi_ikt <- laouenan %>% subset(local == F) %>% group_by(bplace_countrycode_v2, occupation, period) %>% dplyr::summarize(mean(HPI)) %>% data.frame()

HPI_emi_ikt <- subset(HPI_emi_ikt, is.na(bplace_countrycode_v2) == F)

HPI_emi_ikt$ID <- paste(HPI_emi_ikt$bplace_countrycode_v2, HPI_emi_ikt$period, sep="_")
HPI_emi_ikt$occupation <- paste(HPI_emi_ikt$occupation, "emigrated", sep="_")

occs <- unique(HPI_emi_ikt$occupation)
rows <- unique(data$ID)
HPI_emi_ikt$rowsoccs <- paste(HPI_emi_ikt$ID, HPI_emi_ikt$occupation, sep="_")

for(i in rows){
  for(j in occs){
    if(paste(i,j, sep="_") %in% HPI_emi_ikt$rowsoccs){
      data[i,j] <- data[i,j] * subset(HPI_emi_ikt, ID == i & occupation == j)$mean.HPI.
    }
  }
}


HPI_died_ikt <- laouenan %>% group_by(dplace_countrycode_v2, occupation, period) %>% dplyr::summarize(mean(HPI)) %>% data.frame()

HPI_died_ikt <- subset(HPI_died_ikt, is.na(dplace_countrycode_v2) == F)

HPI_died_ikt$ID <- paste(HPI_died_ikt$dplace_countrycode_v2, HPI_died_ikt$period, sep="_")
HPI_died_ikt$occupation <- paste(HPI_died_ikt$occupation, "died", sep="_")

occs <- unique(HPI_died_ikt$occupation)
rows <- unique(data$ID)
HPI_died_ikt$rowsoccs <- paste(HPI_died_ikt$ID, HPI_died_ikt$occupation, sep="_")

for(i in rows){
  for(j in occs){
    if(paste(i,j, sep="_") %in% HPI_died_ikt$rowsoccs){
      data[i,j] <- data[i,j] * subset(HPI_died_ikt, ID == i & occupation == j)$mean.HPI.
    }
  }
}

HPI_immi_ikt <- laouenan %>% subset(local == F) %>% group_by(dplace_countrycode_v2, occupation, period) %>% dplyr::summarize(mean(HPI)) %>% data.frame()

HPI_immi_ikt <- subset(HPI_immi_ikt, is.na(dplace_countrycode_v2) == F)

HPI_immi_ikt$ID <- paste(HPI_immi_ikt$dplace_countrycode_v2, HPI_immi_ikt$period, sep="_")
HPI_immi_ikt$occupation <- paste(HPI_immi_ikt$occupation, "immigrated", sep="_")

occs <- unique(HPI_immi_ikt$occupation)
rows <- unique(data$ID)
HPI_immi_ikt$rowsoccs <- paste(HPI_immi_ikt$ID, HPI_immi_ikt$occupation, sep="_")

for(i in rows){
  for(j in occs){
    if(paste(i,j, sep="_") %in% HPI_immi_ikt$rowsoccs){
      data[i,j] <- data[i,j] * subset(HPI_immi_ikt, ID == i & occupation == j)$mean.HPI.
    }
  }
}
