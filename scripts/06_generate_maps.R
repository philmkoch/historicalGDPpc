
contEurope <- st_read("./misc/SHP_files/shapefile_continentalEurope_NUTS2/continentalEurope.shp")
USAMSA <- st_read("./misc/SHP_files/shapefile_MSA_USA/tl_2019_us_cbsa.shp")
world <- st_read("./misc/SHP_files/shapefile_world_countries/world-administrative-boundaries.shp")
world <- subset(world, continent == "Europe" | iso3 == "USA" | iso3 == "CAN")
NUTS3 <- st_read("./misc/SHP_files/shapefile_NUTS/NUTS3/NUTS3.shp")

data1300 <- subset(data, year == 1300 & !is.na(GDPpc))

# assign GDPpc values to NUTS3 regions
NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)

NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1300, lookup_column = "country", result_column = "GDPpc")
list_white <- c("UKN", "UKM", "UKL", "FRM", "FRY", "ITF", "ITG", "ITI4")
NUTS3$GDPpc <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white, NA, NUTS3$GDPpc)

world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA", 
                      vlookup(world$iso3, data1300, lookup_column = "country", result_column = "GDPpc"), NA)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "1300", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1300.svg"), width = 10, height = 8.5)

data1400 <- subset(data, year == 1400 & !is.na(GDPpc))

# assign GDPpc values to NUTS3 regions
NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)

NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1400, lookup_column = "country", result_column = "GDPpc")
list_white <- c("UKN", "UKM", "UKL", "FRM", "FRY",  "ITF", "ITG", "ITI4", "NL1", "NL2", "NL34", "NL4", "PL22", "PL4", "PL5", "PL6", "PL7", "PL8", "PL9")
NUTS3$GDPpc <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white, NA, NUTS3$GDPpc)

world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA", 
                      vlookup(world$iso3, data1400, lookup_column = "country", result_column = "GDPpc"), NA)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "1400", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1400.svg"), width = 10, height = 8.5)

data1500 <- subset(data, year == 1500 & !is.na(GDPpc))

# assign GDPpc values to NUTS3 regions
NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)

list_white <- c("UKN", "UKM", "UKL", "FRM", "ITF", "ITG", "ITI4", "DEF07", "DEF0C", "NL1", "NL2", "NL34", "NL4", "PL22", "PL4", "PL5", "PL6", "PL7", "PL8", "PL9")
# add regions from Poland and Belgium to Germany
list_german <- c("PL42", "PL43", "PL51", "PL52", "BE336", "PL224", "PL228", "PL22B", "PL22C", "PL229", "PL227")

NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 4) %in% list_german | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_german, "DEU", NUTS3$CNTR_iso3)

NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1500, lookup_column = "country", result_column = "GDPpc")
NUTS3$GDPpc <- ifelse(((str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white) & NUTS3$CNTR_iso3 != "DEU") | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_white, NA, NUTS3$GDPpc)

world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA", 
                      vlookup(world$iso3, data1500, lookup_column = "country", result_column = "GDPpc"), NA)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "1500", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1500.svg"), width = 10, height = 8.5)



data1600 <- subset(data, year == 1600 & !is.na(GDPpc))

# assign GDPpc values to NUTS3 regions
NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)

list_white <- c("UKN", "UKM", "UKL", "FRM","FRY",  "ITF", "ITG", "ITI4", "DEF07", "DEF0C", "NL1", "NL2", "NL34", "NL4", "PL22", "PL4", "PL5", "PL6", "PL7", "PL8", "PL9")
# add regions from Poland and Belgium to Germany
list_german <- c("PL42", "PL43", "PL51", "PL52", "BE336", "PL224", "PL228", "PL22B", "PL22C", "PL229", "PL227")

NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 4) %in% list_german | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_german, "DEU", NUTS3$CNTR_iso3)

NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1600, lookup_column = "country", result_column = "GDPpc")
NUTS3$GDPpc <- ifelse(((str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white) & NUTS3$CNTR_iso3 != "DEU") | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_white, NA, NUTS3$GDPpc)

world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA", 
                      vlookup(world$iso3, data1600, lookup_column = "country", result_column = "GDPpc"), NA)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "1600", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1600.svg"), width = 10, height = 8.5)


data1700 <- subset(data, year == 1700 & !is.na(GDPpc))

# assign GDPpc values to NUTS3 regions
NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)

list_white <- c("UKN", "UKM", "UKL", "FRM","FRY",  "ITF", "ITG", "ITI4", "DEF07", "DEF0C", "NL1", "NL2", "NL34", "NL4", "PL22", "PL4", "PL5", "PL6", "PL7", "PL8", "PL9")
# add regions from Poland and Belgium to Germany
list_german <- c("PL42", "PL43", "PL51", "PL52", "BE336", "PL224", "PL228", "PL22B", "PL22C", "PL229", "PL227")

NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 4) %in% list_german | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_german, "DEU", NUTS3$CNTR_iso3)

NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1700, lookup_column = "country", result_column = "GDPpc")
NUTS3$GDPpc <- ifelse(((str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white) & NUTS3$CNTR_iso3 != "DEU") | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_white, NA, NUTS3$GDPpc)

world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA", 
                      vlookup(world$iso3, data1700, lookup_column = "country", result_column = "GDPpc"), NA)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "1700", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1700.svg"), width = 10, height = 8.5)

data1800 <- subset(data, year == 1750 & !is.na(GDPpc))

# assign GDPpc values to NUTS3 regions
NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)

list_white <- c("FRM", "ITF", "ITG", "ITI4", "FRY", "DEF07", "DEF0C", "NL1", "NL2", "NL34", "NL4", "PL22", "PL4", "PL5", "PL6", "PL7", "PL8", "PL9")
# add regions from Poland and Belgium to Germany
list_german <- c("PL42", "PL43", "PL51", "PL52", "BE336", "PL224", "PL228", "PL22B", "PL22C", "PL229", "PL227")

NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 4) %in% list_german | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_german, "DEU", NUTS3$CNTR_iso3)

NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1800, lookup_column = "country", result_column = "GDPpc")
NUTS3$GDPpc <- ifelse(((str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white) & NUTS3$CNTR_iso3 != "DEU") | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_white, NA, NUTS3$GDPpc)

world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA", 
                      vlookup(world$iso3, data1800, lookup_column = "country", result_column = "GDPpc"), NA)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "1750", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1750_Europe.svg"), width = 10, height = 8.5)

world$GDPpc <- vlookup(world$iso3, data1800, lookup_column = "country", result_column = "GDPpc")
ggplot(world) + geom_sf(aes(fill = GDPpc), color="black", linewidth = 0.2) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') + 
  theme_void() + xlim(-130, -60) + ylim(25,55) +
  labs(title = "1750", fill = "GDP per capita") 
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1750_NorthAmerica.svg"), width = 10, height = 8.5)


# REGIONAL MAP
contEurope$GDPpc <- vlookup(contEurope$NUTS_ID, data1800, result_column = "GDPpc", lookup_column = "country")

ggplot(contEurope) + geom_sf(aes(fill = GDPpc), size=0.1) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value="white", trans='log10', limits = c(min(data1800$GDPpc, na.rm=T), max(data1800$GDPpc, na.rm=T))) +
  theme_void() + theme(legend.position = "right") +
  labs(title = "1750", fill = "Predicted GDP per capita") + 
  xlim(-10, 50) + ylim(35,71.5) + geom_sf(data=world, fill = NA, size = 0.3, color="black")
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1750_Europe_REGIONAL.svg"), width = 10, height = 8.5)

data1800 <- subset(data, year == 1800 & !is.na(GDPpc))

# assign GDPpc values to NUTS3 regions
NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)

list_white <- c("FRM", "ITF", "ITG", "ITI4", "FRY", "DEF07", "DEF0C", "NL1", "NL2", "NL34", "NL4", "PL22", "PL4", "PL5", "PL6", "PL7", "PL8", "PL9")
# add regions from Poland and Belgium to Germany
list_german <- c("PL42", "PL43", "PL51", "PL52", "BE336", "PL224", "PL228", "PL22B", "PL22C", "PL229", "PL227")

NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 4) %in% list_german | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_german, "DEU", NUTS3$CNTR_iso3)

NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1800, lookup_column = "country", result_column = "GDPpc")
NUTS3$GDPpc <- ifelse(((str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white) & NUTS3$CNTR_iso3 != "DEU") | str_sub(NUTS3$NUTS_ID, end = 5) %in% list_white, NA, NUTS3$GDPpc)

world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA", 
                      vlookup(world$iso3, data1800, lookup_column = "country", result_column = "GDPpc"), NA)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "1800", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1800_Europe.svg"), width = 10, height = 8.5)

world$GDPpc <- vlookup(world$iso3, data1800, lookup_column = "country", result_column = "GDPpc")
ggplot(world) + geom_sf(aes(fill = GDPpc), color="black", linewidth = 0.2) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') + 
  theme_void() + xlim(-130, -60) + ylim(25,55) +
  labs(title = "1800", fill = "GDP per capita") 
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1800_NorthAmerica.svg"), width = 10, height = 8.5)


# REGIONAL MAP
contEurope$GDPpc <- vlookup(contEurope$NUTS_ID, data1800, result_column = "GDPpc", lookup_column = "country")

ggplot(contEurope) + geom_sf(aes(fill = GDPpc), size=0.1) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value="white", trans='log10', limits = c(min(data1800$GDPpc, na.rm=T), max(data1800$GDPpc, na.rm=T))) +
  theme_void() + theme(legend.position = "right") +
  labs(title = "1800", fill = "Predicted GDP per capita") + 
  xlim(-10, 50) + ylim(35,71.5) + geom_sf(data=world, fill = NA, size = 0.3, color="black")
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1800_Europe_REGIONAL.svg"), width = 10, height = 8.5)


data1850 <- subset(data, year == 1850 & !is.na(GDPpc))

# assign GDPpc values to NUTS3 regions
NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)

list_white <- c("FRM", "ITF", "ITG", "ITI4","FRY",  "PL22", "PL4", "PL5", "PL6", "PL7", "PL8", "PL9")
# add regions from Poland and Belgium to Germany
list_csk <- c("SK", "CZ")

NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 2) %in% list_csk, "CSK", NUTS3$CNTR_iso3)

NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1850, lookup_column = "country", result_column = "GDPpc")
NUTS3$GDPpc <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white, NA, NUTS3$GDPpc)

world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA", vlookup(world$iso3, data1850, lookup_column = "country", result_column = "GDPpc"), NA)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "1850", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1850_Europe.svg"), width = 10, height = 8.5)

world$GDPpc <- vlookup(world$iso3, data1850, lookup_column = "country", result_column = "GDPpc")
ggplot(world) + geom_sf(aes(fill = GDPpc), color="black", linewidth = 0.2) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') + 
  theme_void() + xlim(-130, -60) + ylim(25,55) +
  labs(title = "1850", fill = "GDP per capita") 
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1850_NorthAmerica.svg"), width = 10, height = 8.5)

# REGIONAL MAP
contEurope$GDPpc <- vlookup(contEurope$NUTS_ID, data1850, result_column = "GDPpc", lookup_column = "country")

ggplot(contEurope) + geom_sf(aes(fill = GDPpc), size=0.1) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value="white", trans='log10', limits = c(min(data1850$GDPpc, na.rm=T), max(data1850$GDPpc, na.rm=T))) +
  theme_void() + theme(legend.position = "right") +
  labs(title = "1850", fill = "Predicted GDP per capita") + 
  xlim(-10, 50) + ylim(35,71.5) + geom_sf(data=world, fill = NA, size = 0.3, color="black")
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1850_Europe_REGIONAL.svg"), width = 10, height = 8.5)

data1900 <- subset(data, year == 1900 & !is.na(GDPpc))

# assign GDPpc values to NUTS3 regions
NUTS3$CNTR_iso3 <- countrycode(NUTS3$CNTR_CODE, origin = "iso2c", destination = "iso3c")
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "EL", "GRC", NUTS3$CNTR_iso3)
NUTS3$CNTR_iso3 <- ifelse(NUTS3$CNTR_CODE == "UK", "GBR", NUTS3$CNTR_iso3)

list_white <- c("FRM", "ITG2","FRY")
# add regions from Poland and Belgium to Germany
list_csk <- c("SK", "CZ")

NUTS3$CNTR_iso3 <- ifelse(str_sub(NUTS3$NUTS_ID, end = 2) %in% list_csk, "CSK", NUTS3$CNTR_iso3)

NUTS3$GDPpc <- vlookup(NUTS3$CNTR_iso3, data1900, lookup_column = "country", result_column = "GDPpc")
NUTS3$GDPpc <- ifelse(str_sub(NUTS3$NUTS_ID, end = 3) %in% list_white | str_sub(NUTS3$NUTS_ID, end = 4) %in% list_white, NA, NUTS3$GDPpc)

world$GDPpc <- ifelse(world$iso3 == "RUS" | world$iso3 == "UKR" | world$iso3 == "BLR" | world$iso3 == "MDA", vlookup(world$iso3, data1900, lookup_column = "country", result_column = "GDPpc"), NA)

ggplot(world) + geom_sf(aes(fill = GDPpc, color = GDPpc)) +
  theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_sf(data = NUTS3, aes(fill = GDPpc, color = GDPpc)) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10') +
  scale_color_gradient(low="#DCEDC8", high="#33691E", na.value=NA, trans='log10', guide = "none") +
  labs(title = "1900", fill = "GDP per capita") +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.2)
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1900_Europe.svg"), width = 10, height = 8.5)

# REGIONAL MAP
contEurope$GDPpc <- vlookup(contEurope$NUTS_ID, data1900, result_column = "GDPpc", lookup_column = "country")

ggplot(contEurope) + geom_sf(aes(fill = GDPpc), size=0.1) +
  scale_fill_gradient(low="#DCEDC8", high="#33691E", na.value="white", trans='log10', limits = c(min(data1900$GDPpc, na.rm=T), max(data1900$GDPpc, na.rm=T))) +
  theme_void() + theme(legend.position = "right") +
  labs(title = "1900", fill = "Predicted GDP per capita") + 
  xlim(-10, 50) + ylim(35,71.5) + geom_sf(data=world, fill = NA, size = 0.3, color="black")
ggsave(file=paste0("./genfiles_", version, "/maps/Maddison/map_1900_Europe_REGIONAL.svg"), width = 10, height = 8.5)


# MAPS LAOUENAN DATA
peopledata <- read.csv2("./raw_data/famous_individuals/cross-verified-database_withGEOGRAPHY.csv")

peopledata_births <- subset(peopledata, !is.na(bplace_ID))
peopledata_deaths <- subset(peopledata, !is.na(dplace_ID))

ggplot(world) + geom_sf(fill = "white", color = "black", linewidth = 0.2) + theme_void() + xlim(-10, 50) + ylim(35,71.5) +
  geom_point(data = peopledata_births, aes(x = bplo1, y = bpla1), size = 0.2,  color = "darkgreen", alpha = 0.05) +
  geom_point(data = peopledata_deaths, aes(x = dplo1, y = dpla1), size = 0.2,  color = "darkred", alpha = 0.05)
ggsave(file=paste0("./genfiles_", version, "/maps/famous_individuals/map_TOTAL_BIRTHSDEATHS_EU.svg"), width = 10, height = 8.5)
ggplot(world) + geom_sf(fill = "white", color = "black", linewidth = 0.2) + theme_void() + xlim(-130, -60) + ylim(25,55) +
  geom_point(data = peopledata_births, aes(x = bplo1, y = bpla1), size = 0.6,  color = "darkgreen", alpha = 0.05)+
  geom_point(data = peopledata_deaths, aes(x = dplo1, y = dpla1), size = 0.6,  color = "darkred", alpha = 0.05) 
ggsave(file=paste0("./genfiles_", version, "/maps/famous_individuals/map_TOTAL_BIRTHSDEATHS_USA.svg"), width = 10, height = 8.5)


years <- seq(1300, 1350, 50)
for(k in years){
  temp_births <- subset(peopledata_births, birth < k & birth > k-150)
  temp_deaths <- subset(peopledata_deaths, birth < k & birth > k-150)
  
  ggplot(world) + geom_sf(fill = "white", color = "black", linewidth = 0.2) + theme_void() + xlim(-130, -60) + ylim(25,55) +
    geom_point(data = temp_births, aes(x = bplo1, y = bpla1), size = 1.5,  color = "darkgreen", alpha = 0.1)+
    geom_point(data = temp_deaths, aes(x = dplo1, y = dpla1), size = 1.5,  color = "darkred", alpha = 0.1) 
  ggsave(file=paste0("./genfiles_", version, "/maps/famous_individuals/map_BIRTHSDEATHS_USA_", k, ".svg"), width = 10, height = 8.5)
  
  
}


years <- seq(1700, 1750, 50)
for(k in years){
  temp_births <- subset(peopledata_births, birth < k & birth > k-150)
  temp_deaths <- subset(peopledata_deaths, birth < k & birth > k-150)
  
  ggplot(world) + geom_sf(fill = "white", color = "black", linewidth = 0.2) + theme_void() + xlim(-130, -60) + ylim(25,55) +
    geom_point(data = temp_births, aes(x = bplo1, y = bpla1), size = 1,  color = "darkgreen", alpha = 0.1)+
    geom_point(data = temp_deaths, aes(x = dplo1, y = dpla1), size = 1,  color = "darkred", alpha = 0.1) 
  ggsave(file=paste0("./genfiles_", version, "/maps/famous_individuals/map_BIRTHSDEATHS_USA_", k, ".png"), width = 10, height = 8.5)
  
  
}

