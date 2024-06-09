
sumstat <- as.data.frame(matrix(ncol = 8, nrow = 26))

names(sumstat) <- c("Variable", "Period", "labeled observations", "a", "b", "unlabeled observations", "c", "d")

sumstat[1, 3:5] <- c("N", "mean", "sd")
sumstat[1, 6:8] <- c("N", "mean", "sd")

sumstat$Variable[c(2,7,12,17,22)] <- c("GDPpc [2000]", "births", "deaths", "immigrants", "emigrants")
sumstat$Period[2:26] <- c("1300-1500", "1550-1750", "1800-1850", "1900-1950", "2000",
                          "1300-1500", "1550-1750", "1800-1850", "1900-1950", "2000",
                          "1300-1500", "1550-1750", "1800-1850", "1900-1950", "2000",
                          "1300-1500", "1550-1750", "1800-1850", "1900-1950", "2000",
                          "1300-1500", "1550-1750", "1800-1850", "1900-1950", "2000"
                          )

data$histperiod <- ifelse(data$year < 1501, 1,
                          ifelse(data$year > 1500 & data$year < 1800,2, 
                                 ifelse(data$year > 1799 & data$year < 1900, 3,
                                        ifelse(data$year > 1899 & data$year < 1951, 4,
                                               ifelse(data$year > 1950 & data$year < 2001, 5,
                                                      NA)))))

labeled_data <- subset(data, is.na(GDPpc)==F & str_length(country) != 4 & str_length(country) < 6)
unlabeled_data <- subset(data, is.na(GDPpc)  & str_length(country) != 4 & str_length(country) < 6)

data2000 <- subset(labeled_data, year == 2000)

labeled_data$GDPpc_2000 <- vlookup(labeled_data$country, data2000, lookup_column = "country", result_column = "GDPpc")
unlabeled_data$GDPpc_2000 <- vlookup(unlabeled_data$country, data2000, lookup_column = "country", result_column = "GDPpc")

# fill the data with summary statistics

for(p in 1:4){
  summary_table <- labeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(labeled_data, histperiod == p & !is.na(GDPpc_2000))),
      Mean_Var1 = round(mean(GDPpc_2000, na.rm=T), digits=1),
      SD_Var1 = round(sd(GDPpc_2000, na.rm=T), digits=1)
    )
  sumstat[2+p-1, 3:5] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- labeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(labeled_data, histperiod == p & !is.na(births))),
      Mean_Var1 = round(mean(births), digits=1),
      SD_Var1 = round(sd(births), digits=1)
    )
  sumstat[7+p-1, 3:5] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- labeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(labeled_data, histperiod == p & !is.na(deaths))),
      Mean_Var1 = round(mean(deaths), digits=1),
      SD_Var1 = round(sd(deaths), digits=1)
    )
  sumstat[12+p-1, 3:5] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- labeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(labeled_data, histperiod == p & !is.na(immigrants))),
      Mean_Var1 = round(mean(immigrants), digits=1),
      SD_Var1 = round(sd(immigrants), digits=1)
    )
  sumstat[17+p-1, 3:5] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- labeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(labeled_data, histperiod == p & !is.na(emigrants))),
      Mean_Var1 = round(mean(emigrants), digits=1),
      SD_Var1 = round(sd(emigrants), digits=1)
    )
  sumstat[22+p-1, 3:5] <- summary_table[1,]
}


for(p in 1:4){
  summary_table <- unlabeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(unlabeled_data, histperiod == p & !is.na(GDPpc_2000))),
      Mean_Var1 = round(mean(GDPpc_2000, na.rm=T), digits=1),
      SD_Var1 = round(sd(GDPpc_2000, na.rm=T), digits=1)
    )
  sumstat[2+p-1, 6:8] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- unlabeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(unlabeled_data, histperiod == p & !is.na(births))),
      Mean_Var1 = round(mean(births), digits=1),
      SD_Var1 = round(sd(births), digits=1)
    )
  sumstat[7+p-1, 6:8] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- unlabeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(unlabeled_data, histperiod == p & !is.na(deaths))),
      Mean_Var1 = round(mean(deaths), digits=1),
      SD_Var1 = round(sd(deaths), digits=1)
    )
  sumstat[12+p-1, 6:8] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- unlabeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(unlabeled_data, histperiod == p & !is.na(immigrants))),
      Mean_Var1 = round(mean(immigrants), digits=1),
      SD_Var1 = round(sd(immigrants), digits=1)
    )
  sumstat[17+p-1, 6:8] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- unlabeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(unlabeled_data, histperiod == p & !is.na(emigrants))),
      Mean_Var1 = round(mean(emigrants), digits=1),
      SD_Var1 = round(sd(emigrants), digits=1)
    )
  sumstat[22+p-1, 6:8] <- summary_table[1,]
}

ft <- flextable(sumstat)

# Save as word file
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = paste("./genfiles_", version, "/figures_SI/summarytable_countries.docx", sep=""))

# REGIONS
labeled_data <- subset(data, is.na(GDPpc)==F & (str_length(country) == 4 | str_length(country) > 5))
unlabeled_data <- subset(data, is.na(GDPpc)  & (str_length(country) == 4 | str_length(country) > 5))

data2000 <- subset(labeled_data, year == 2000)

labeled_data$GDPpc_2000 <- vlookup(labeled_data$country, data2000, lookup_column = "country", result_column = "GDPpc")
unlabeled_data$GDPpc_2000 <- vlookup(unlabeled_data$country, data2000, lookup_column = "country", result_column = "GDPpc")

# fill the data with summary statistics

for(p in 1:4){
  summary_table <- labeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(labeled_data, histperiod == p & !is.na(GDPpc_2000))),
      Mean_Var1 = round(mean(GDPpc_2000, na.rm=T), digits=1),
      SD_Var1 = round(sd(GDPpc_2000, na.rm=T), digits=1)
    )
  sumstat[2+p-1, 3:5] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- labeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(labeled_data, histperiod == p & !is.na(births))),
      Mean_Var1 = round(mean(births), digits=1),
      SD_Var1 = round(sd(births), digits=1)
    )
  sumstat[7+p-1, 3:5] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- labeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(labeled_data, histperiod == p & !is.na(deaths))),
      Mean_Var1 = round(mean(deaths), digits=1),
      SD_Var1 = round(sd(deaths), digits=1)
    )
  sumstat[12+p-1, 3:5] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- labeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(labeled_data, histperiod == p & !is.na(immigrants))),
      Mean_Var1 = round(mean(immigrants), digits=1),
      SD_Var1 = round(sd(immigrants), digits=1)
    )
  sumstat[17+p-1, 3:5] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- labeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(labeled_data, histperiod == p & !is.na(emigrants))),
      Mean_Var1 = round(mean(emigrants), digits=1),
      SD_Var1 = round(sd(emigrants), digits=1)
    )
  sumstat[22+p-1, 3:5] <- summary_table[1,]
}


for(p in 1:4){
  summary_table <- unlabeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(unlabeled_data, histperiod == p & !is.na(GDPpc_2000))),
      Mean_Var1 = round(mean(GDPpc_2000, na.rm=T), digits=1),
      SD_Var1 = round(sd(GDPpc_2000, na.rm=T), digits=1)
    )
  sumstat[2+p-1, 6:8] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- unlabeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(unlabeled_data, histperiod == p & !is.na(births))),
      Mean_Var1 = round(mean(births), digits=1),
      SD_Var1 = round(sd(births), digits=1)
    )
  sumstat[7+p-1, 6:8] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- unlabeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(unlabeled_data, histperiod == p & !is.na(deaths))),
      Mean_Var1 = round(mean(deaths), digits=1),
      SD_Var1 = round(sd(deaths), digits=1)
    )
  sumstat[12+p-1, 6:8] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- unlabeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(unlabeled_data, histperiod == p & !is.na(immigrants))),
      Mean_Var1 = round(mean(immigrants), digits=1),
      SD_Var1 = round(sd(immigrants), digits=1)
    )
  sumstat[17+p-1, 6:8] <- summary_table[1,]
}

for(p in 1:5){
  summary_table <- unlabeled_data %>%
    filter(histperiod == p) %>%
    summarise(
      count_Var1 = nrow(subset(unlabeled_data, histperiod == p & !is.na(emigrants))),
      Mean_Var1 = round(mean(emigrants), digits=1),
      SD_Var1 = round(sd(emigrants), digits=1)
    )
  sumstat[22+p-1, 6:8] <- summary_table[1,]
}

ft <- flextable(sumstat)

# Save as word file
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = paste("./genfiles_", version, "/figures_SI/summarytable_regions.docx", sep=""))


data$histperiod <- NULL