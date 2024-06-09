
# matching geodata obtained from QGIS
births_NUTS <- read.csv("./raw_data/famous_individuals/DATASET_births_withNUTS.csv")
deaths_NUTS <- read.csv("./raw_data/famous_individuals/DATASET_deaths_withNUTS.csv")
births_NUTS3 <- read.csv("./raw_data/famous_individuals/DATASET_births_withNUTS3.csv")
deaths_NUTS3 <- read.csv("./raw_data/famous_individuals/DATASET_deaths_withNUTS3.csv")
births_MSA <- read.csv("./raw_data/famous_individuals/DATASET_births_withMSA.csv")
births_MSA$GEOID <- paste0("US", births_MSA$GEOID)
deaths_MSA <- read.csv("./raw_data/famous_individuals/DATASET_deaths_withMSA.csv")
deaths_MSA$GEOID <- paste0("US", deaths_MSA$GEOID)

births_country <- read.csv("./raw_data/famous_individuals/DATASET_births_countries.csv")
deaths_country <- read.csv("./raw_data/famous_individuals/DATASET_deaths_countries.csv")

data$bplace_ID <- vlookup(data$wikidata_code, births_NUTS, lookup_column = "wikidata_code", result_column = "NUTS_ID")
data$bplace_ID <- ifelse(is.na(data$bplace_ID), vlookup(data$wikidata_code, births_MSA, lookup_column = "wikidata_code", result_column = "GEOID"), data$bplace_ID)

data$dplace_ID <- vlookup(data$wikidata_code, deaths_NUTS, lookup_column = "wikidata_code", result_column = "NUTS_ID")
data$dplace_ID <- ifelse(is.na(data$dplace_ID), vlookup(data$wikidata_code, deaths_MSA, lookup_column = "wikidata_code", result_column = "GEOID"), data$dplace_ID)

data$bplace_NUTS3 <- vlookup(data$wikidata_code, births_NUTS3, lookup_column = "wikidata_code", result_column = "NUTS_ID")
data$dplace_NUTS3 <- vlookup(data$wikidata_code, deaths_NUTS3, lookup_column = "wikidata_code", result_column = "NUTS_ID")

data$bplace_country <- vlookup(data$wikidata_code, births_country, lookup_column = "wikidata_code", result_column = "iso3")
data$dplace_country <- vlookup(data$wikidata_code, deaths_country, lookup_column = "wikidata_code", result_column = "iso3")

# add 12th century
births_NUTS_12 <- read.csv("./raw_data/famous_individuals/DATASET_births_withNUTS_12thcentury.csv")
deaths_NUTS_12 <- read.csv("./raw_data/famous_individuals/DATASET_deaths_withNUTS_12thcentury.csv")
births_NUTS3_12 <- read.csv("./raw_data/famous_individuals/DATASET_births_withNUTS3_12thcentury.csv")
deaths_NUTS3_12 <- read.csv("./raw_data/famous_individuals/DATASET_deaths_withNUTS3_12thcentury.csv")
births_country_12 <- read.csv("./raw_data/famous_individuals/DATASET_births_countries_12thcentury.csv")
deaths_country_12 <- read.csv("./raw_data/famous_individuals/DATASET_deaths_countries_12thcentury.csv")

data$bplace_ID <- ifelse(is.na(data$bplace_ID), vlookup(data$wikidata_code, births_NUTS_12, lookup_column = "wikidata_code", result_column = "NUTS_ID"), data$bplace_ID)
data$dplace_ID <- ifelse(is.na(data$dplace_ID), vlookup(data$wikidata_code, deaths_NUTS_12, lookup_column = "wikidata_code", result_column = "NUTS_ID"), data$dplace_ID)

data$bplace_NUTS3 <- ifelse(is.na(data$bplace_NUTS3), vlookup(data$wikidata_code, births_NUTS3_12, lookup_column = "wikidata_code", result_column = "NUTS_ID"), data$bplace_NUTS3)
data$dplace_NUTS3 <- ifelse(is.na(data$dplace_NUTS3), vlookup(data$wikidata_code, deaths_NUTS3_12, lookup_column = "wikidata_code", result_column = "NUTS_ID"), data$dplace_NUTS3)

data$bplace_country <- ifelse(is.na(data$bplace_country), vlookup(data$wikidata_code, births_country_12, lookup_column = "wikidata_code", result_column = "iso3"), data$bplace_country)
data$dplace_country <- ifelse(is.na(data$dplace_country), vlookup(data$wikidata_code, deaths_country_12, lookup_column = "wikidata_code", result_column = "iso3"), data$dplace_country)

# Aggregating to Inner and Outer London
data$bplace_ID <- ifelse(str_sub(data$bplace_ID, end = 4) == "UKI3" | str_sub(data$bplace_ID, end = 4) == "UKI4", "UKI1", data$bplace_ID)
data$dplace_ID <- ifelse(str_sub(data$dplace_ID, end = 4) == "UKI3" | str_sub(data$dplace_ID, end = 4) == "UKI4", "UKI1", data$dplace_ID)

data$bplace_ID <- ifelse(str_sub(data$bplace_ID, end = 4) == "UKI5" | str_sub(data$bplace_ID, end = 4) == "UKI6" | str_sub(data$bplace_ID, end = 4) == "UKI7", "UKI2", data$bplace_ID)
data$dplace_ID <- ifelse(str_sub(data$dplace_ID, end = 4) == "UKI5" | str_sub(data$dplace_ID, end = 4) == "UKI6" | str_sub(data$dplace_ID, end = 4) == "UKI7", "UKI2", data$dplace_ID)

data$bplace_NUTS3 <- ifelse(str_sub(data$bplace_NUTS3, end = 4) == "UKI3" | str_sub(data$bplace_NUTS3, end = 4) == "UKI4", "UKI10", data$bplace_NUTS3)
data$dplace_NUTS3 <- ifelse(str_sub(data$dplace_NUTS3, end = 4) == "UKI3" | str_sub(data$dplace_NUTS3, end = 4) == "UKI4", "UKI10", data$dplace_NUTS3)

data$bplace_NUTS3 <- ifelse(str_sub(data$bplace_NUTS3, end = 4) == "UKI5" | str_sub(data$bplace_NUTS3, end = 4) == "UKI6" | str_sub(data$bplace_NUTS3, end = 4) == "UKI7", "UKI20", data$bplace_NUTS3)
data$dplace_NUTS3 <- ifelse(str_sub(data$dplace_NUTS3, end = 4) == "UKI5" | str_sub(data$dplace_NUTS3, end = 4) == "UKI6" | str_sub(data$dplace_NUTS3, end = 4) == "UKI7", "UKI20", data$dplace_NUTS3)

# This seems to be a generic geocode that is incorrect
data$bplace_ID <- ifelse(data$bplo1 == -112 & data$bpla1 == 44, NA, data$bplace_ID)
data$dplace_ID <- ifelse(data$dplo1 == -112 & data$dpla1 == 44, NA, data$dplace_ID)

# ADDING CANADA
sf_use_s2(FALSE)
shpfile <- read_sf("./misc/SHP_files/shapefile_Kanada_MSAs/lcma000a16a_e.shp")
shpfile$CMAPUID <- paste0("CN", shpfile$CMAPUID)

shpfile_CAN_0 <- read_sf("./misc/SHP_files/shapefile_Kanada/gadm41_CAN_0.shp")

# births
data_misc <- subset(data, is.na(bpla1) == F & is.na(bplo1) == F)
geodata <- st_as_sf(data_misc, coords = c('bplo1', 'bpla1'))

st_crs(geodata) <- "WGS84"
shpfile <- st_transform(shpfile, crs = st_crs(geodata))

births_CAN_MSA <- st_contains(shpfile, geodata)
births_CAN_MSA <- as.data.frame(births_CAN_MSA)
shpfile_df <- as.data.frame(shpfile)
births_CAN_MSA$code <- shpfile_df[births_CAN_MSA$row.id,"CMAPUID"]
births_CAN_MSA$wikicode <- data_misc[births_CAN_MSA$col.id,"wikidata_code"]

data$bplace_ID <- ifelse(is.na(data$bplace_ID), vlookup(data$wikidata_code, births_CAN_MSA, lookup_column = "wikicode", result_column = "code"), data$bplace_ID)

# deaths
data_misc <- subset(data, is.na(dpla1) == F & is.na(dplo1) == F)
geodata <- st_as_sf(data_misc, coords = c('dplo1', 'dpla1'))

st_crs(geodata) <- "WGS84"
shpfile <- st_transform(shpfile, crs = st_crs(geodata))

deaths_CAN_MSA <- st_contains(shpfile, geodata)
deaths_CAN_MSA <- as.data.frame(deaths_CAN_MSA)
shpfile_df <- as.data.frame(shpfile)
deaths_CAN_MSA$code <- shpfile_df[deaths_CAN_MSA$row.id,"CMAPUID"]
deaths_CAN_MSA$wikicode <- data_misc[deaths_CAN_MSA$col.id,"wikidata_code"]

data$dplace_ID <- ifelse(is.na(data$dplace_ID), vlookup(data$wikidata_code, deaths_CAN_MSA, lookup_column = "wikicode", result_column = "code"), data$dplace_ID)


# births
data_misc <- subset(data, is.na(bpla1) == F & is.na(bplo1) == F)
geodata <- st_as_sf(data_misc, coords = c('bplo1', 'bpla1'))

st_crs(geodata) <- "WGS84"
shpfile_CAN_0 <- st_transform(shpfile_CAN_0, crs = st_crs(geodata))

births_CAN_MSA <- st_contains(shpfile_CAN_0, geodata)
births_CAN_MSA <- as.data.frame(births_CAN_MSA)

births_CAN_MSA$wikicode <- data_misc[births_CAN_MSA$col.id,"wikidata_code"]
births_CAN_MSA$code <- "CAN"

data$bplace_country <- ifelse(is.na(data$bplace_country), vlookup(data$wikidata_code, births_CAN_MSA, lookup_column = "wikicode", result_column = "code"), data$bplace_country)

# deaths
data_misc <- subset(data, is.na(dpla1) == F & is.na(dplo1) == F)
geodata <- st_as_sf(data_misc, coords = c('dplo1', 'dpla1'))

st_crs(geodata) <- "WGS84"
shpfile_CAN_0 <- st_transform(shpfile_CAN_0, crs = st_crs(geodata))

births_CAN_MSA <- st_contains(shpfile_CAN_0, geodata)
births_CAN_MSA <- as.data.frame(births_CAN_MSA)

births_CAN_MSA$wikicode <- data_misc[births_CAN_MSA$col.id,"wikidata_code"]
births_CAN_MSA$code <- "CAN"

data$dplace_country <- ifelse(is.na(data$dplace_country), vlookup(data$wikidata_code, births_CAN_MSA, lookup_column = "wikicode", result_column = "code"), data$dplace_country)

data <- subset(data, !is.na(data$bplace_ID) | !is.na(data$dplace_ID))

# Some assignments seem to be incorrect, so we correct them manually in the following
# Also, estimates in the Maddison project do not align fully with today's borders (see discussion in the manuscript and the SI Appendix)
# Hence we take this into account

data$b_iso2 <- str_sub(data$bplace_ID, end = 2)
data$d_iso2 <- str_sub(data$dplace_ID, end = 2)

data$bplace_country_alternative <- countrycode(data$b_iso2, origin = "iso2c", destination = "iso3c")
data$dplace_country_alternative <- countrycode(data$d_iso2, origin = "iso2c", destination = "iso3c")

data$bplace_country <- ifelse(is.na(data$bplace_country), data$bplace_country_alternative, data$bplace_country)
data$dplace_country <- ifelse(is.na(data$dplace_country), data$dplace_country_alternative, data$dplace_country)

data$bplace_country_alternative <- NULL
data$dplace_country_alternative <- NULL

data$bplace_country <- ifelse(data$bplace_country == "MCO", "FRA", data$bplace_country)
data$dplace_country <- ifelse(data$dplace_country == "MCO", "FRA", data$dplace_country)

data$bplace_country <- ifelse(data$bplace_country == "VAT", "ITA", data$bplace_country)
data$dplace_country <- ifelse(data$dplace_country == "VAT", "ITA", data$dplace_country)

data$bplace_ID <- ifelse(data$bplace_country == "VAT", "ITI4", data$bplace_ID)
data$dplace_ID <- ifelse(data$dplace_country == "VAT", "ITI4", data$dplace_ID)

data$bplace_country <- ifelse(data$bplace_country %in% c("AND", "ANT", "ATF", "GIB", "IMY", "IOT", "FRO", "LIE", "SJM", "SMR"), NA, data$bplace_country)
data$dplace_country <- ifelse(data$dplace_country %in% c("AND", "ANT", "ATF", "GIB", "IMY", "IOT", "FRO", "LIE", "SJM", "SMR"), NA, data$dplace_country)

data$local <- ifelse(data$bplace_ID == data$dplace_ID, TRUE, FALSE)
data$local[is.na(data$local)] <- FALSE
data$migrant <- ifelse(data$local == FALSE, TRUE, FALSE)

data$age <- data$death - data$birth

# country assignments seem to be not 100% correct
temp <- subset(data, is.na(bplace_country) & !is.na(bplace_ID))
temp$bplace_country <- ifelse(str_sub(temp$bplace_ID, end = 2) == "US", "USA", temp$bplace_country)
temp$bplace_country <- ifelse(str_sub(temp$bplace_ID, end = 3) == "RUS", "RUS", temp$bplace_country)
temp$bplace_country <- ifelse(str_sub(temp$bplace_ID, end = 3) == "UKR", "UKR", temp$bplace_country)
temp$bplace_country <- ifelse(is.na(temp$bplace_country), countrycode(str_sub(temp$bplace_ID, end = 2), origin = "iso2c", destination = "iso3c"), temp$bplace_country)
temp$bplace_country <- ifelse(str_sub(temp$bplace_ID, end = 2) == "UK", "GBR", temp$bplace_country)
temp$bplace_country <- ifelse(str_sub(temp$bplace_ID, end = 2) == "EL", "GRC", temp$bplace_country)

data$bplace_country <- ifelse(is.na(data$bplace_country), vlookup(data$wikidata_code, temp, lookup_column = "wikidata_code", result_column = "bplace_country"), data$bplace_country)

temp <- subset(data, is.na(dplace_country) & !is.na(dplace_ID))
temp$dplace_country <- ifelse(str_sub(temp$dplace_ID, end = 2) == "US", "USA", temp$dplace_country)
temp$dplace_country <- ifelse(str_sub(temp$dplace_ID, end = 3) == "RUS", "RUS", temp$dplace_country)
temp$dplace_country <- ifelse(str_sub(temp$dplace_ID, end = 3) == "UKR", "UKR", temp$dplace_country)
temp$dplace_country <- ifelse(is.na(temp$dplace_country), countrycode(str_sub(temp$dplace_ID, end = 2), origin = "iso2c", destination = "iso3c"), temp$dplace_country)
temp$dplace_country <- ifelse(str_sub(temp$dplace_ID, end = 2) == "UK", "GBR", temp$dplace_country)
temp$dplace_country <- ifelse(str_sub(temp$dplace_ID, end = 2) == "EL", "GRC", temp$dplace_country)

data$dplace_country <- ifelse(is.na(data$dplace_country), vlookup(data$wikidata_code, temp, lookup_column = "wikidata_code", result_column = "dplace_country"), data$dplace_country)

data$bplace_countrycode <- data$bplace_country
data$dplace_countrycode <- data$dplace_country

data_orig <- data

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

data_complete <- data.frame()
for(l in 1:length(data_list)){
  data <- data_list[[l]]
  
  source("./scripts/01a_zz_borderadjustments.R")
  
  data_complete <- rbind(data_complete, data)
}

data_complete$bplace_countrycode_v2 <- ifelse(data_complete$bplace_countrycode == "USA" | data_complete$bplace_countrycode == "CAN" | data_complete$bplace_countrycode == "RUS" | data_complete$bplace_countrycode == "UKR" | data_complete$bplace_countrycode == "BLR" | data_complete$bplace_countrycode == "MDA",
                                              data_complete$bplace_countrycode, data_complete$bplace_countrycode_v2)
data_complete$dplace_countrycode_v2 <- ifelse(data_complete$dplace_countrycode == "USA" | data_complete$bplace_countrycode == "CAN" | data_complete$dplace_countrycode == "RUS" | data_complete$dplace_countrycode == "UKR" | data_complete$dplace_countrycode == "BLR" | data_complete$dplace_countrycode == "MDA",
                                              data_complete$dplace_countrycode, data_complete$dplace_countrycode_v2)

data <- data_orig

data$bplace_countrycode_v2 <- vlookup(data$wikidata_code, data_complete, result_column = "bplace_countrycode_v2")
data$dplace_countrycode_v2 <- vlookup(data$wikidata_code, data_complete, result_column = "dplace_countrycode_v2")

data$local_country <- ifelse(data$bplace_countrycode_v2 == data$dplace_countrycode_v2, TRUE, FALSE)
data$local_country[is.na(data$local_country)] <- FALSE




# The following creates a reference list for regions and countries that will be used later

contEurope <- st_read("./misc/SHP_files/shapefile_continentalEurope_NUTS2/continentalEurope.shp")

contEurope$NUTS_ID <- ifelse(str_sub(contEurope$NUTS_ID, end = 4) == "UKI3" | str_sub(contEurope$NUTS_ID, end = 4) == "UKI4", "UKI1", contEurope$NUTS_ID)
contEurope$NUTS_ID <- ifelse(str_sub(contEurope$NUTS_ID, end = 4) == "UKI5" | str_sub(contEurope$NUTS_ID, end = 4) == "UKI6" | str_sub(contEurope$NUTS_ID, end = 4) == "UKI7", "UKI2", contEurope$NUTS_ID)

NUTS_ref <- as.data.frame(contEurope[,c("NUTS_ID", "NAME_1", "NAME_LATN")])
NUTS_ref$name_final <- ifelse(is.na(NUTS_ref$NAME_1), NUTS_ref$NAME_LATN, NUTS_ref$NAME_1)

# create full reference list including US
GEO_ref <- NUTS_ref[,c("NUTS_ID", "name_final")]
colnames(GEO_ref) <- c("ID", "NAME")

MSA_ref <- read.csv2("./misc/SHP_files/MSA_ref.csv")
colnames(MSA_ref) <- c("ID", "NAME")
MSA_ref$ID <- paste0("US", MSA_ref$ID)
GEO_ref <- rbind(GEO_ref, MSA_ref)

CAN_ref <- read_sf("./misc/SHP_files/shapefile_Kanada_MSAs/lcma000a16a_e.shp")
CAN_ref$CMAPUID <- paste0("CN", CAN_ref$CMAPUID)
CAN_ref <- as.data.frame(CAN_ref)
colnames(CAN_ref)[2:3] <- c("ID", "NAME")
GEO_ref <- rbind(GEO_ref, CAN_ref[,c("ID", "NAME")])

write.xlsx2(GEO_ref, append=F, file="./misc/GEO_REF_EUROPE_PLUS_US.xlsx", row.names = F)

RCA_cutoff <- 1

# load shape file
world <- st_read("/Users/philippkoch/Documents/Projects/_DATASETS/SHP_files/shapefile_world_countries/WB_countries_Admin0_10m.shp")

# create country reference file
country_ref <- as.data.frame(unique(data$bplace_country))
colnames(country_ref) <- "code"
country_ref$name <- countrycode(country_ref$code, origin = "iso3c", destination = "country.name")
country_ref <- country_ref[,c("name", "code")]
# add a few countries
country_ref <- rbind(country_ref, c("Scotland", "SCO"),
                     c("Wales", "WAL"),
                     c("Northern Ireland", "NIR"),
                     c("Southern Italy", "S_ITA"),
                     c("Netherlands w/o Holland", "NLD_2"),
                     c("Poland w/o Krakow", "POL_2"),
                     c("Czechoslovakia", "CSK"))

country_ref <- subset(country_ref, !is.na(name))
write.xlsx2(country_ref, append=F, file="./misc/GEO_REF_COUNTRIES.xlsx", row.names = F)
