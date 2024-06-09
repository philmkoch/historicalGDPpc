
# MAPS AND RESULTS
# Load shape files
contEurope <- st_read("./misc/SHP_files/shapefile_continentalEurope_NUTS2/continentalEurope.shp")
contEurope$NUTS_ID <- ifelse(str_sub(contEurope$NUTS_ID, end = 4) == "UKI3" | str_sub(contEurope$NUTS_ID, end = 4) == "UKI4", "UKI1", contEurope$NUTS_ID)
contEurope$NUTS_ID <- ifelse(str_sub(contEurope$NUTS_ID, end = 4) == "UKI5" | str_sub(contEurope$NUTS_ID, end = 4) == "UKI6" | str_sub(contEurope$NUTS_ID, end = 4) == "UKI7", "UKI2", contEurope$NUTS_ID)
USAMSA <- st_read("./misc/SHP_files/shapefile_MSA_USA/tl_2019_us_cbsa.shp")
world <- st_read("./misc/SHP_files/shapefile_world_countries/world-administrative-boundaries.shp")
world <- subset(world, continent == "Europe" | iso3 == "USA" | iso3 == "CAN")
NUTS3 <- st_read("./misc/SHP_files/shapefile_NUTS/NUTS3/NUTS3.shp")
CAN <- read_sf("./misc/SHP_files/shapefile_Kanada_MSAs/lcma000a16a_e.shp")

# load GEO_ref
GEO_ref <- read.xlsx2(file="./misc/GEO_REF_EUROPE_PLUS_US.xlsx", sheetIndex = 1)

data$NUTS_name <- NA
data$NUTS_name <- ifelse(is.na(data$NUTS_name), vlookup(data$country, GEO_ref), data$NUTS_name)
data$NUTS_name <- ifelse(is.na(data$NUTS_name), countrycode(data$country, origin = "iso3c", destination = "country.name"), data$NUTS_name)
data$NUTS_name <- ifelse(data$country == "WAL", "Wales", data$NUTS_name)
data$NUTS_name <- ifelse(data$country == "XKO", "Kosovo", data$NUTS_name)
data$NUTS_name <- ifelse(is.na(data$NUTS_name) & data$country_0 == "USA", vlookup(str_sub(data$country, start = 3), USAMSA, lookup_column = "GEOID", result_column = "NAMELSAD"),data$NUTS_name)

# REGIONS
for(l in unique(data$year)){
  
  misc <- subset(data, year == l)
  
  # add ECI
  contEurope$GDPpc <- vlookup(contEurope$NUTS_ID, misc, result_column = "GDPpc", lookup_column = "country")
  contEurope$GDPpc <- ifelse(is.na(contEurope$GDPpc), vlookup(contEurope$NUTS_ID, misc, result_column = "oos_pred_level", lookup_column = "country"), contEurope$GDPpc)
  
  USAMSA$GDPpc <- vlookup(paste0("US", USAMSA$GEOID), misc, result_column = "GDPpc", lookup_column = "country")
  USAMSA$GDPpc <- ifelse(is.na(USAMSA$GDPpc), vlookup(paste0("US", USAMSA$GEOID), misc, result_column = "oos_pred_level", lookup_column = "country"), USAMSA$GDPpc)
  
  CAN$GDPpc <- vlookup(paste0("CN", CAN$CMAPUID), misc, result_column = "GDPpc", lookup_column = "country")
  CAN$GDPpc <- ifelse(is.na(CAN$GDPpc), vlookup(paste0("CN", CAN$CMAPUID), misc, result_column = "oos_pred_level", lookup_column = "country"), CAN$GDPpc)
  
  if(sum(is.na(USAMSA$GDPpc)) < nrow(USAMSA)){
    ggplot(USAMSA) + geom_sf(data=world, fill = "white", size = 0.5, color="black") + geom_sf(aes(fill = GDPpc), size=0.1) +
      scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value="white", trans = 'log10') +
      xlim(-130, -60) + ylim(25,55) +
      theme_void() + theme(legend.position = "right") +
      geom_sf(data = CAN, aes(fill = GDPpc), size=0.1) +
      labs(title = paste(misc$year[1]), fill = "Predicted GDP per capita") + 
      geom_sf(data = contEurope, aes(fill = GDPpc), size=0.1) 
    ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/regions/NorthAmerica_", l, ".png", sep=""), width = 10, height = 8.5)
    
  }
  
  if(sum(is.na(contEurope$GDPpc)) < nrow(contEurope)){
    
    ggplot(USAMSA) + geom_sf(aes(fill = GDPpc), size=0.1) +
      scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value="white", trans='log10') +
      theme_void() + theme(legend.position = "right") +
      geom_sf(data = contEurope, aes(fill = GDPpc), size=0.1) +
      labs(title = paste(misc$year[1]), fill = "Predicted GDP per capita") + 
      xlim(-10, 50) + ylim(35,71.5) + geom_sf(data=world, fill = NA, size = 0.3, color="black")
    ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/regions/Europe_", l, ".png", sep=""), width = 10, height = 8.5)
    
  }
}



# COUNTRY MAPS WITH ADJUSTED BORDERS

for(t in c(1300, 1350, 1400, 1450, 1500, 1550, 1600, 1650, 1700)){
  
  data1300 <- subset(data, year == t & (str_length(country) == 3 | str_length(country) == 5))
  
  # assign GDPpc values to NUTS3 regions
  NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
  NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
  NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)
  
  # create country codes for special cases
  list_S_ITA <- c("ITI4", "ITG", "ITF")
  NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_S_ITA | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_S_ITA, "S_ITA", NUTS3$CNTR_iso3)
  
  NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) == "UKL", "WAL", NUTS3$CNTR_iso3)
  NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) == "UKM", "SCO", NUTS3$CNTR_iso3)
  NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) == "UKN", "NIR", NUTS3$CNTR_iso3)
  
  NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 2) == "PL" & str_sub(NUTS3$NUTS_ID, end = 4) != "PL21", "POL_2", NUTS3$CNTR_iso3)
  
  NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 2) == "NL" & str_sub(NUTS3$NUTS_ID, end = 4) != "NL32" & str_sub(NUTS3$NUTS_ID, end = 4) != "NL33", "NLD_2", NUTS3$CNTR_iso3)
  
  NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1300, lookup_column = "country", result_column = "GDPpc")
  
  NUTS3$GDPpc <- ifelse(is.na(NUTS3$GDPpc), vlookup(NUTS3$CNTR_iso3, data1300, lookup_column = "country", result_column = "oos_pred_level"), NUTS3$GDPpc)
  
  list_white <- c("DEF07", "DEF0C")
  NUTS3$GDPpc <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_white, NA, NUTS3$GDPpc)
  
  world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA" | world$iso3 == "USA"| world$iso3 == "CAN", 
                        vlookup(world$iso3, data1300, lookup_column = "country", result_column = "GDPpc"), NA)
  
  world$GDPpc <- ifelse(is.na(world$GDPpc) & (world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA" | world$iso3 == "USA"| world$iso3 == "CAN"), 
                        vlookup(world$iso3, data1300, lookup_column = "country", result_column = "oos_pred_level"), world$GDPpc)
  
  ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
    theme_void() + xlim(-10, 50) + ylim(35,71.5) +
    geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
    scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
    scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
    labs(title = paste(t), fill = "GDP per capita") +
    geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
  ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/countries/Europe_", t, ".png", sep=""), width = 10, height = 8.5)
  
  if(t >= 1500){
    ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
      theme_void() + xlim(-130, -60) + ylim(25,55) +
      geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
      scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
      scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
      labs(title = paste(t), fill = "GDP per capita") +
      geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
    ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/countries/NorthAmerica_", t, ".png", sep=""), width = 10, height = 8.5)
  }
  
}


for(t in c(1750, 1800)){
  
  data1300 <- subset(data, year == t & (str_length(country) == 3 | str_length(country) == 5))
  
  # assign GDPpc values to NUTS3 regions
  NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
  NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
  NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)
  
  # create country codes for special cases
  list_S_ITA <- c("ITI4", "ITG", "ITF")
  NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_S_ITA | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_S_ITA, "S_ITA", NUTS3$CNTR_iso3)
  
  NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 2) == "PL" & str_sub(NUTS3$NUTS_ID, end = 4) != "PL21", "POL_2", NUTS3$CNTR_iso3)
  
  NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 2) == "NL" & str_sub(NUTS3$NUTS_ID, end = 4) != "NL32" & str_sub(NUTS3$NUTS_ID, end = 4) != "NL33", "NLD_2", NUTS3$CNTR_iso3)
  
  NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1300, lookup_column = "country", result_column = "GDPpc")
  
  NUTS3$GDPpc <- ifelse(is.na(NUTS3$GDPpc), vlookup(NUTS3$CNTR_iso3, data1300, lookup_column = "country", result_column = "oos_pred_level"), NUTS3$GDPpc)
  
  list_white <- c("DEF07", "DEF0C")
  NUTS3$GDPpc <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_white, NA, NUTS3$GDPpc)
  
  world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA" | world$iso3 == "USA"| world$iso3 == "CAN", 
                        vlookup(world$iso3, data1300, lookup_column = "country", result_column = "GDPpc"), NA)
  
  world$GDPpc <- ifelse(is.na(world$GDPpc) & (world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA" | world$iso3 == "USA"| world$iso3 == "CAN"), 
                        vlookup(world$iso3, data1300, lookup_column = "country", result_column = "oos_pred_level"), world$GDPpc)
  
  ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
    theme_void() + xlim(-10, 50) + ylim(35,71.5) +
    geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
    scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
    scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
    labs(title = paste(t), fill = "GDP per capita") +
    geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
  ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/countries/Europe_", t, ".png", sep=""), width = 10, height = 8.5)
  
  ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
    theme_void() + xlim(-130, -60) + ylim(25,55) +
    geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
    scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
    scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
    labs(title = paste(t), fill = "GDP per capita") +
    geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
  ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/countries/NorthAmerica_", t, ".png", sep=""), width = 10, height = 8.5)
}


data1850 <- subset(data, year == 1850 & (str_length(country) == 3 | str_length(country) == 5))

# assign GDPpc values to NUTS3 regions
NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)

# create country codes for special cases
list_S_ITA <- c("ITI4", "ITG", "ITF")
NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_S_ITA | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_S_ITA, "S_ITA", NUTS3$CNTR_iso3)

list_csk <- c("SK", "CZ")
NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 2) %in% list_csk, "CSK", NUTS3$CNTR_iso3)

NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 2) == "PL" & str_sub(NUTS3$NUTS_ID, end = 4) != "PL21", "POL_2", NUTS3$CNTR_iso3)

NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1850, lookup_column = "country", result_column = "GDPpc")

NUTS3$GDPpc <- ifelse(is.na(NUTS3$GDPpc), vlookup(NUTS3$CNTR_iso3, data1850, lookup_column = "country", result_column = "oos_pred_level"), NUTS3$GDPpc)

list_white <- ""
NUTS3$GDPpc <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white, NA, NUTS3$GDPpc)

world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA" | world$iso3 == "USA"| world$iso3 == "CAN", 
                      vlookup(world$iso3, data1850, lookup_column = "country", result_column = "GDPpc"), NA)

world$GDPpc <- ifelse(is.na(world$GDPpc) & (world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA" | world$iso3 == "USA"| world$iso3 == "CAN"), 
                      vlookup(world$iso3, data1850, lookup_column = "country", result_column = "oos_pred_level"), world$GDPpc)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "1850", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/countries/Europe_", 1850, ".png", sep=""), width = 10, height = 8.5)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-130, -60) + ylim(25,55) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "1850", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/countries/NorthAmerica_", 1850, ".png", sep=""), width = 10, height = 8.5)

for(t in c(1900, 1950)){
  
  data1300 <- subset(data, year == t & (str_length(country) == 3 | str_length(country) == 5))
  
  # assign GDPpc values to NUTS3 regions
  NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
  NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
  NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)
  
  # create country codes for special cases
  list_csk <- c("SK", "CZ")
  NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 2) %in% list_csk, "CSK", NUTS3$CNTR_iso3)
  
  NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1300, lookup_column = "country", result_column = "GDPpc")
  
  NUTS3$GDPpc <- ifelse(is.na(NUTS3$GDPpc), vlookup(NUTS3$CNTR_iso3, data1300, lookup_column = "country", result_column = "oos_pred_level"), NUTS3$GDPpc)
  
  list_white <- ""
  NUTS3$GDPpc <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_white, NA, NUTS3$GDPpc)
  
  world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA" | world$iso3 == "USA"| world$iso3 == "CAN", 
                        vlookup(world$iso3, data1300, lookup_column = "country", result_column = "GDPpc"), NA)
  
  world$GDPpc <- ifelse(is.na(world$GDPpc) & (world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA" | world$iso3 == "USA"| world$iso3 == "CAN"), 
                        vlookup(world$iso3, data1300, lookup_column = "country", result_column = "oos_pred_level"), world$GDPpc)
  
  ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
    theme_void() + xlim(-10, 50) + ylim(35,71.5) +
    geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
    scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
    scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
    labs(title = paste(t), fill = "GDP per capita") +
    geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
  ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/countries/Europe_", t, ".png", sep=""), width = 10, height = 8.5)
  
  ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
    theme_void() + xlim(-130, -60) + ylim(25,55) +
    geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
    scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
    scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
    labs(title = paste(t), fill = "GDP per capita") +
    geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
  ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/countries/NorthAmerica_", t, ".png", sep=""), width = 10, height = 8.5)
  
}

data1300 <- subset(data, year == 2000 & (str_length(country) == 3 | str_length(country) == 5))

# assign GDPpc values to NUTS3 regions
NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)

NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1300, lookup_column = "country", result_column = "GDPpc")

NUTS3$GDPpc <- ifelse(is.na(NUTS3$GDPpc), vlookup(NUTS3$CNTR_iso3, data1300, lookup_column = "country", result_column = "oos_pred_level"), NUTS3$GDPpc)

list_white <- ""
NUTS3$GDPpc <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_white, NA, NUTS3$GDPpc)

world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA" | world$iso3 == "USA"| world$iso3 == "CAN", 
                      vlookup(world$iso3, data1300, lookup_column = "country", result_column = "GDPpc"), NA)

world$GDPpc <- ifelse(is.na(world$GDPpc) & (world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA" | world$iso3 == "USA"| world$iso3 == "CAN"), 
                      vlookup(world$iso3, data1300, lookup_column = "country", result_column = "oos_pred_level"), world$GDPpc)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "2000", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/countries/Europe_", 2000, ".png", sep=""), width = 10, height = 8.5)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-130, -60) + ylim(25,55) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "2000", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(paste("./genfiles_", version, "/maps/Maddison+ML/countries/NorthAmerica_", 2000, ".png", sep=""), width = 10, height = 8.5)







data$GDPpc_publishable <- ifelse(is.na(data$GDPpc) == F, data$GDPpc, data$oos_pred_level)
data$flag <- ifelse(is.na(data$GDPpc) == F, "source data", "out-of-sample prediction")

data$GDPpc_lower <- ifelse(is.na(data$GDPpc) == T, data$oos_pred_level_lower, NA)
data$GDPpc_upper <- ifelse(is.na(data$GDPpc) == T, data$oos_pred_level_upper, NA)

# CITY COMPARISON

comp_regions <- c("RUS.43_1",
                  "DE30",
                  "ITC4",
                  "NL32",
                  "UKI1",
                  "US35620",
                  "PT17",
                  "CN35535")

temp <- subset(data, country %in% comp_regions & as.numeric(as.character(year)) < 1950)
ggplot(temp, aes(x=as.numeric(as.character(year)), y=GDPpc_publishable, color = NUTS_name)) + geom_line() + geom_point(size=1) +
  scale_y_continuous(trans = 'log10', breaks = c(1000, 2000, 3000, 5000, 10000, 30000)) +
  theme_light() + labs(x="Year", y="GDP per capita", color = NULL)
ggsave(paste("./genfiles_", version, "/figures/Fig3_A.svg", sep=""), width = 8, height = 4)


comp_regions_US <- c("US39300",
                     "US35620",
                     "US14460",
                     "US10580",
                     "US12580",
                     "US14460",
                     "US14860",
                     "US16700",
                     #"US20100",
                     "US25540",
                     #"US31340",
                     #"US35300",
                     "US35620",
                     "US35980",
                     "US37980")

temp <- subset(data, country %in% comp_regions_US & as.numeric(as.character(year)) < 1950)
ggplot(temp, aes(x=as.numeric(as.character(year)), y=GDPpc_publishable, color = NUTS_name)) + geom_line() + geom_point(size=1) +
  scale_y_continuous(trans = 'log10', breaks = c(1000, 2000, 3000, 5000, 10000, 30000)) +
  theme_light() + labs(x="Year", y="GDP per capita", color = NULL)
ggsave(paste("./genfiles_", version, "/figures/Fig4_H.svg", sep=""), width = 8, height = 5)




##########################################################################

#         TESTING THE LITTLE DIVERGENCE            #

##########################################################################

# reduce to relevant variables

data_rel <- data[,c("ID", "country", "NUTS_name", "year", "population", "GDPpc", "GDPpc_publishable", "oos_pred", "oos_pred_level", "births", "deaths")]

data_rel$north <- ifelse(str_length(data_rel$country) == 4 & str_sub(data_rel$country, end=2) %in% c("UK", "NL", "BE"), 1, 0)
data_rel$south <- ifelse(str_length(data_rel$country) == 4 & str_sub(data_rel$country, end=2) %in% c("IT", "PT", "ES"#, "DE", "PL", "SE"
), 1, 0)

data_rel$geo <- ifelse(data_rel$north == 1, "north", ifelse(data_rel$south == 1, "south", NA))
data_rel <- subset(data_rel, !is.na(geo) #& !is.na(population)
)

data_rel$population_famous <- 10^data_rel$births + 10^data_rel$deaths - 2

# minimum and maximum approach
agg_series <- data_rel %>% dplyr::group_by(year, geo) %>% dplyr::summarize(weighted.quantile(GDPpc_publishable, w = population_famous, prob = 0.9)) %>% as.data.frame()
colnames(agg_series)[3] <- "max"
agg_series_min <- data_rel %>% dplyr::group_by(year, geo) %>% dplyr::summarize(weighted.quantile(GDPpc_publishable, w = population_famous, prob = 0.1)) %>% as.data.frame()
agg_series$min <- agg_series_min[,3]

agg_series_mean <- data_rel %>% dplyr::group_by(year, geo) %>% dplyr::summarize(weighted.mean(GDPpc_publishable, w = population_famous, na.rm=T)) %>% as.data.frame()
agg_series$mean <- agg_series_mean[,3]

ggplot(subset(agg_series, as.numeric(as.character(year)) < 1851), aes(x=as.numeric(as.character(year)), y=mean, color=geo)) + geom_line(size = 1) + geom_ribbon(aes(ymin = min, ymax = max, fill = geo), alpha = 0.1, linetype = "dashed", size = 0.2) + 
  theme_light() + xlim(1300, 1850) + labs(x = "year", y = "log(GDP per capita)", color = NULL, fill =NULL) + scale_y_continuous(trans='log10', breaks = c(1000, 2000, 3000, 5000, 10000, 30000), limits = c(1000, max(subset(agg_series, as.numeric(as.character(year)) < 1851)$max)))
ggsave(paste("./genfiles_", version, "/figures/Fig3_B.svg", sep=""), width = 6, height = 4)


##########################################################################

#         VALIDATION DATA FROM TÜBINGEN            #

##########################################################################

validationdata <- data.frame()

wellbeing <- read.csv2("./raw_data/validation_data/wellbeing.csv")
height <- read.csv2("./raw_data/validation_data/height.csv")

wellbeing$iso3 <- countrycode(wellbeing$country.name, origin = "country.name", destination = "iso3c")
height$iso3 <- countrycode(height$country.name, origin = "country.name", destination = "iso3c")

wellbeing$ID <- paste(wellbeing$iso3, wellbeing$year, sep="_")
height$ID <- paste(height$iso3, height$year, sep="_")

# HEIGHT, average in the 18th century
misc <- subset(height, year > 1699 & year < 1801)
misc <- misc %>% dplyr::group_by(iso3) %>% dplyr::summarize(mean(value)) %>% data.frame()
colnames(misc) <- c("iso3", "height")
misc$ID <- paste(misc$iso3, "1800", sep="_")

misc$GDPpc_maddison <- vlookup(misc$ID, data, lookup_column = "ID2", result_column = "GDPpc")
misc$GDPpc_oos <- vlookup(misc$ID, data, lookup_column = "ID2", result_column = "oos_pred_level")

misc$labeled <- ifelse(is.na(misc$GDPpc_maddison), FALSE, TRUE)

misc %>% ggplot(aes(x=GDPpc_oos, y=height)) + scale_x_continuous(trans = 'log') + scale_y_continuous(trans = 'log') + stat_poly_eq(formula = y~x, data=misc, aes(label = after_stat(rr.label)), parse = TRUE) + geom_point(size=1.5, color = "darkorange1", alpha = 0) + 
  theme_light() + geom_smooth(method="lm", se=F, linetype="dashed", color="grey", alpha = 0.2) + geom_text(label = misc$iso3) +
  labs(x = "Predicted GDP per capita in 1800", y = "Average height, 1700-1800")
ggsave(paste("./genfiles_", version, "/figures/Fig3_F.svg", sep=""), width = 5, height = 5)

misc %>% ggplot(aes(x=GDPpc_oos, y=height, color = labeled)) + scale_x_continuous(trans = 'log') + scale_y_continuous(trans = 'log') + stat_poly_eq(formula = y~x, data=misc, aes(label = after_stat(rr.label)), parse = TRUE) + geom_point(size=1.5, color = "darkorange1", alpha = 0) + 
  theme_light() + geom_smooth(method="lm", se=F, linetype="dashed", alpha = 0.2) + geom_text(label = misc$iso3) +
  labs(x = "Predicted GDP per capita in 1800", y = "Average height, 1700-1800")
ggsave(paste("./genfiles_", version, "/figures_SI/Fig_bodyheight_labeled.svg", sep=""), width = 5, height = 5)


# WELLBEING, 1850
misc <- subset(wellbeing, year == 1850)
misc <- misc %>% dplyr::group_by(iso3) %>% dplyr::summarize(mean(value)) %>% data.frame()
colnames(misc) <- c("iso3", "wellbeing")
misc$ID <- paste(misc$iso3, "1850", sep="_")

misc$GDPpc_maddison <- vlookup(misc$ID, data, lookup_column = "ID2", result_column = "GDPpc")
misc$GDPpc_oos <- vlookup(misc$ID, data, lookup_column = "ID2", result_column = "oos_pred_level")

misc$labeled <- ifelse(is.na(misc$GDPpc_maddison), FALSE, TRUE)

misc %>% ggplot(aes(x=GDPpc_oos, y=wellbeing)) + scale_x_continuous(trans = 'log', breaks = c(2000, 3000, 6000)) + stat_poly_eq(formula = y~x, data=misc, aes(label = after_stat(rr.label)), parse = TRUE) + geom_point(size=1.5, color = "darkorange1", alpha=0) + 
  theme_light() + geom_smooth(method="lm", se=F, linetype="dashed", color="grey", alpha = 0.2) + geom_text(label = misc$iso3) +
  labs(x = "Predicted GDP per capita in 1850", y = "Wellbeing, 1850")
ggsave(paste("./genfiles_", version, "/figures/Fig3_G.svg", sep=""), width = 5, height = 5)

misc %>% ggplot(aes(x=GDPpc_oos, y=wellbeing, color = labeled)) + scale_x_continuous(trans = 'log', breaks = c(2000, 3000, 6000)) + stat_poly_eq(formula = y~x, data=misc, aes(label = after_stat(rr.label)), parse = TRUE) + geom_point(size=1.5, color = "darkorange1", alpha=0) + 
  theme_light() + geom_smooth(method="lm", se=F, linetype="dashed", alpha = 0.2) + geom_text(label = misc$iso3) +
  labs(x = "Predicted GDP per capita in 1850", y = "Wellbeing, 1850")
ggsave(paste("./genfiles_", version, "/figures_SI/Fig_wellbeing_labeled.svg", sep=""), width = 5, height = 5)

## CHURCHES 

churches <- readRDS("./raw_data/validation_data/churchbuilding.rds")

# data describes church building between t and t-20
# We redefine it such that it describes church building between t and t+20
churches$decade2 <- churches$decade - 20

churches$decade2 <- ifelse(churches$decade2 == 1340, 1350, churches$decade2)
churches$decade2 <- ifelse(churches$decade2 == 1440, 1450, churches$decade2)

churches$ID2 <- paste(churches$NUTS2, churches$decade2, sep="_")

churches$sum.im3.post <- churches$sum.im3.
churches$sum.im3.post <- ifelse(churches$decade2 == 1350, churches$sum.im3.post / 2 + vlookup(paste(churches$NUTS2, 1360, sep="_"), churches, lookup_column = "ID2", result_column = "sum.im3.post") / 2, churches$sum.im3.post)
churches$sum.im3.post <- ifelse(churches$decade2 == 1450, churches$sum.im3.post / 2 + vlookup(paste(churches$NUTS2, 1460, sep="_"), churches, lookup_column = "ID2", result_column = "sum.im3.post") / 2, churches$sum.im3.post)

churches$oos_GDPpc <- vlookup(churches$ID2, data, lookup_column = "ID2", result_column = "oos_pred_level")

subset(churches, is.na(oos_GDPpc) == F & sum.im3.post > 0) %>% ggplot(aes(x=oos_GDPpc, y=sum.im3.post, color = as.factor(decade2))) + geom_point(alpha=0) + scale_x_continuous(trans = "log", breaks = c(800, 1100, 2200, 3000), labels=c("800", "1100", "2200", "3000")) + scale_y_continuous(trans = "log", breaks = c(0, 1000, 20000, 400000), labels=c("0", "1000", "20000", "400000")) +
  geom_smooth(method="lm", alpha = 0.2, linetype = "dashed", se=F) + theme_light() + labs(x = "Predicted GDP per capita, log scale", y = "Church building in following 20 years, m3, log scale") + stat_poly_eq(formula = y~x, data=subset(churches, is.na(oos_GDPpc) == F), aes(label = after_stat(rr.label)), parse = TRUE) + scale_color_discrete(name="") + geom_text(label=subset(churches, is.na(oos_GDPpc) == F & sum.im3.post > 0)$NUTS2, size= 3, check_overlap = F)
ggsave(paste("./genfiles_", version, "/figures/Fig3_H.svg", sep=""), width = 6, height = 5)


# URBANIZATION

# the population variable in the dataset is taken from Buringh (2021) describing city-level urban population
countryleveldata <- subset(data, str_length(country) != 4 & str_length(country) < 6)

countryleveldata$urbanization <- countryleveldata$population / 1000 / countryleveldata$pop_maddison

countryleveldata$labeled <- ifelse(is.na(countryleveldata$GDPpc), FALSE, TRUE)

subset(countryleveldata, as.numeric(as.character(year)) < 2000 & is.na(urbanization) == F) %>% ggplot(aes(x=oos_pred_level, y=urbanization)) + geom_point(size=1.5, color = "darkorange1", alpha = 0.5) + 
  scale_x_continuous(trans = "log", breaks = c(1000, 2000, 3000, 5000, 10000), labels=c("1000", "2000", "3000", "5000", "10000")) + 
  geom_smooth(method="lm", alpha = 0.2, linetype = "dashed", color="grey", se=F) + geom_text(label=subset(countryleveldata, as.numeric(as.character(year)) < 2000  & is.na(urbanization) == F)$ID2, size= 3, check_overlap = T) + theme_light() + labs(x = "Predicted GDP per capita, log scale", y = "Urbanization rate") + stat_poly_eq(formula = y~x, data=subset(countryleveldata, as.numeric(as.character(year)) < 2000  & is.na(urbanization) == F), aes(label = after_stat(rr.label)), parse = TRUE) + scale_color_discrete(name="") 
ggsave(paste("./genfiles_", version, "/figures/Fig3_E.svg", sep=""), width = 6, height = 5)

subset(countryleveldata, as.numeric(as.character(year)) < 2000 & is.na(urbanization) == F) %>% ggplot(aes(x=oos_pred_level, y=urbanization, color=as.factor(labeled))) + geom_point(alpha=0.5) + 
  scale_x_continuous(trans = "log", breaks = c(1000, 2000, 3000, 5000, 10000), labels=c("1000", "2000", "3000", "5000", "10000")) + 
  geom_smooth(method="lm", alpha = 0.2, linetype = "dashed", se=F) + geom_text(label=subset(countryleveldata, as.numeric(as.character(year)) < 2000  & is.na(urbanization) == F)$ID2, size= 3, check_overlap = T) + theme_light() + labs(x = "Predicted GDP per capita, log scale", y = "Urbanization rate") + stat_poly_eq(formula = y~x, data=subset(countryleveldata, as.numeric(as.character(year)) < 2000  & is.na(urbanization) == F), aes(label = after_stat(rr.label)), parse = TRUE) + scale_color_discrete(name="") 
ggsave(paste("./genfiles_", version, "/figures_SI/Fig_urbanization_labeled.svg", sep=""), width = 6, height = 5)


##########################################################################

#         REPLICATION AJR 2005            #

##########################################################################


# Regions with Atlantic ports
atlantic_regions <- c("UKK4", "UKI1", "UKI2", "UKK1", "UKM8", "NL32", "NL33", "NL34", "FRD2", "FRI2", "ES61", "ES13", "PT11", "PT17")
mediterranean_regions <- c("ES62", "ES52", "ES51", "FRJ1", "FRL0", "HR03", "EL61", "EL63", "ITC2", "ITC3", "ITF1", "ITF3", "ITF4", "ITG1", "ITG2", "ITH3", "ITH4", "ITH5", "ITI1", "ITI3", "ITI4")
western_europe_acemoglu <- c("AUT", "BEL", "GBR", "DEN", "FIN", "FRA", "DEU", "IRL", "ITA", "NLD", "NOR", "PRT", "ESP", "SWE", "CHE")
atlantic_countries <- c("GBR", "NLD", "FRA", "ESP", "PRT")

data$population_famous <- 10^data$births + 10^data$deaths - 2

# country-plot, Acemoglu Fig 2B
data_plot <- data %>%
  filter(period <= 12 & str_length(country) != 4 & str_length(country) < 6) %>%
  mutate(
    region = case_when(
      country_0 %in% atlantic_countries ~ "Atlantic Port countries",
      country_0 %in% western_europe_acemoglu & country_0 %in% atlantic_countries == F  ~ "Western European countries without Atlantic port",
      country_0 %in% western_europe_acemoglu == F & country_0 %in% c("USA", "CAN") == F ~ "Eastern Europe"
    )
  ) %>%
  group_by(year, region) %>%
  summarize(mean_GDPpc = weighted.mean(GDPpc_publishable, population_famous, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(region))

ggplot(data_plot, aes(x = year, y = mean_GDPpc, group = region, color = region)) +
  geom_line() +
  geom_point(size=0.5) +
  labs(x = "Year",
       y = "GDP per capita") +
  scale_color_manual(values = c("Atlantic Port countries" = "blue", 
                                "Western European countries without Atlantic port" = "darkorange2", 
                                "Eastern Europe" = "red")) +
  scale_x_discrete(breaks = c(1300, 1400, 1500, 1600, 1700, 1800),
                     labels = c("1300", "1400", "1500", "1600", "1700", "1800")) +
  theme_light() +
  scale_y_continuous(trans = 'log', breaks = c(1500, 2000, 2500, 3000),
                   labels = c("1500", "2000", "2500", "3000"))
ggsave(paste("./genfiles_", version, "/figures/Fig3_C.svg", sep=""), width = 9, height = 4)

# region-plot, Acemoglu Fig 4/5
data_plot <- data %>%
  filter(period <= 12 & (str_length(country) == 4 | str_length(country) > 5)) %>%
  mutate(
    region = case_when(
      country %in% atlantic_regions[1:8] ~ "Regions with Atlantic port in UK or NLD",
      country %in% atlantic_regions[8:14] ~ "Regions with Atlantic port in FRA, PRT, or ESP",
      country %in% mediterranean_regions ~ "Regions with Mediterranean port",
      country_0 %in% western_europe_acemoglu & country %in% atlantic_regions == F & country %in% mediterranean_regions == F  ~ "Western European regions without Atlantic or Mediterranean port",
      country_0 %in% western_europe_acemoglu == F & country %in% mediterranean_regions == F & country_0 %in% c("USA", "CAN") == F ~ "Eastern European regions"
    )
  ) %>%
  group_by(year, region) %>%
  summarize(mean_GDPpc = weighted.mean(GDPpc_publishable, population_famous, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(region))

ggplot(data_plot, aes(x = year, y = mean_GDPpc, group = region, color = region)) +
  geom_line() +
  geom_point(size=0.5) +
  labs(x = "Year",
       y = "GDP per capita") +
  scale_color_manual(values = c("Regions with Atlantic port in UK or NLD" = "blue", 
                                "Regions with Atlantic port in FRA, PRT, or ESP" = "lightblue",
                                "Regions with Mediterranean port" = "green",
                                "Western European regions without Atlantic or Mediterranean port" = "darkorange2", 
                                "Eastern European regions" = "red")) +
  scale_x_discrete(breaks = c(1300, 1400, 1500, 1600, 1700, 1800),
                   labels = c("1300", "1400", "1500", "1600", "1700", "1800")) +
  theme_light() +
  scale_y_continuous(trans = 'log', breaks = c(1000, 2000, 3000, 4000, 5000, 6000),
                     labels = c("1000", "2000", "3000", "4000", "5000", "6000"))
ggsave(paste("./genfiles_", version, "/figures/Fig3_D.svg", sep=""), width = 8, height = 4)


##########################################################################

#         REPLICATION Acemoglu et al. 2011           #

##########################################################################

treated_regions <- c("DEB1", "DEB3", "DEA5", "DEA3", "DE91", "DEE0", "DE73", "DE92")
control_west_of_Elbe <- c("DE12", "DE21", "DE71", "DED2", "DE11")
control_east_of_Elbe <- c("DE40", "RUS.21_1", "PL42", "PL51", "DE80", "DEF0")

data_plot <- data %>%
  filter(period <= 13 & period >= 9 & (str_length(country) == 4 | str_length(country) > 5)) %>%
  mutate(
    region = case_when(
      country %in% treated_regions ~ "Treated regions",
      country %in% control_west_of_Elbe | country %in% control_east_of_Elbe ~ "Control group",
      )
  ) %>%
  group_by(year, region) %>%
  summarize(mean_GDPpc = weighted.mean(GDPpc_publishable, population_famous, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(region))

ggplot(data_plot, aes(x = year, y = mean_GDPpc, group = region, color = region)) +
  geom_line() +
  geom_point(size=0.5) +
  labs(x = "Year",
       y = "GDP per capita") +
  scale_color_manual(values = c("Treated regions" = "blue", 
                                "Control group" = "red"
                                )) +
  theme_light() +
  scale_y_continuous(trans = 'log', breaks = c(2000, 3000, 4000),
                     labels = c("2000", "3000", "4000"))
ggsave(paste("./genfiles_", version, "/figures_SI/Fig_Acemoglu_et_al_2011.svg", sep=""), width = 8, height = 4)


##########################################################################

#         ECONOMIC COMPLEXITY RANKINGS            #

##########################################################################


ECIs <- colnames(data[,str_sub(colnames(data), end = 3) == "ECI"])

for(fac in ECIs){
  
  temp <- data[,c("ID", "country", "NUTS_name", "period", "births", "deaths", "immigrants", "emigrants", fac)]
  temp <- temp[order(temp[,fac], decreasing = T),]
  temp <- subset(temp, period == temp$period[1])
  temp <- temp[c(1:30),]
  
  temp$NUTS_name <- ifelse(is.na(temp$NUTS_name) & temp$country == "POL_2", "Poland (w/o Krakow)", temp$NUTS_name)
  temp$NUTS_name <- ifelse(is.na(temp$NUTS_name) & temp$country == "NLD_2", "Netherlands (w/o Holland)", temp$NUTS_name)
  temp$NUTS_name <- ifelse(is.na(temp$NUTS_name) & temp$country == "S_ITA", "South Italy", temp$NUTS_name)
  temp$NUTS_name <- ifelse(is.na(temp$NUTS_name) & temp$country == "SCO", "Scotland", temp$NUTS_name)
  temp$NUTS_name <- ifelse(is.na(temp$NUTS_name) & temp$country == "NIR", "Northern Ireland", temp$NUTS_name)
  
  temp %>% 
    mutate(NUTS_name = fct_reorder(NUTS_name, temp[,fac])) %>%
    ggplot(aes(x=NUTS_name, y=temp[,fac])) +
    geom_bar(stat="identity", width = 0.7, fill = "darkblue") +
    coord_flip() +
    xlab("") + ylab("ECI") + labs(fill="") +
    theme_light()
  ggsave(paste("./genfiles_", version, "/figures_SI/ECI/", fac, ".png", sep=""), width = 5, height = nrow(temp) / 4.5)
  
  
}

##########################################################################

#         EXPLORING SVD FACTORS            #

##########################################################################

SVDs <- colnames(data[,str_sub(colnames(data), end = 3) == "SVD"])

for(fac in SVDs){
  
  temp <- data[,c("ID", "country", "period", "births", "deaths", "immigrants", "emigrants", fac)]
  temp <- subset(temp, temp[,fac] != 0)
  
  ggplot(temp, aes(x=temp[,fac], y=births)) + geom_point(color = "darkorange2", alpha=0) + theme_light() + geom_text(label=temp$country, size= 3, check_overlap = F) +
    labs(x=paste(fac), y ="births of famous individuals")
  ggsave(paste("./genfiles_", version, "/figures_SI/SVD/", fac, ".png", sep=""), width = 7, height = 6)
  
}


# EXPORT DATASETS
data2 <- data

data2$NUTS_name <- ifelse(data2$country == "CSK", "Czechoslovakia", data2$NUTS_name)
data2$NUTS_name <- ifelse(data2$country == "MDA", "Moldova", data2$NUTS_name)
data2$NUTS_name <- ifelse(data2$country == "NLD_2", "Netherlands (without Holland)", data2$NUTS_name)
data2$NUTS_name <- ifelse(data2$country == "POL_2", "Poland (without Krakow)", data2$NUTS_name)
data2$NUTS_name <- ifelse(data2$country == "S_ITA", "Southern Italy", data2$NUTS_name)
data2$NUTS_name <- ifelse(data2$country == "SCO", "Scotland", data2$NUTS_name)

data2 <- data2 %>% dplyr::rename("name" = "NUTS_name")
data2$GDPpc <- NULL
data2 <- data2 %>% dplyr::rename("GDPpc" = "GDPpc_publishable")
data2 <- data2 %>% dplyr::mutate_at(vars(GDPpc, GDPpc_lower, GDPpc_upper), round, 2)

write.xlsx2(data2[,c("ID2", "country", "country_0", "name", "year", "GDPpc", "flag", "GDPpc_lower", "GDPpc_upper")], 
            file=paste0("./genfiles_", version, "/oos_predictions.xlsx"), row.names = F, append = F)
