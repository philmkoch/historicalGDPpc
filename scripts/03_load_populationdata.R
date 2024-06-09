
# uncleaned population data
countrydata_pop <- read.csv2("./raw_data/population/urban_population_NUTS0.csv")

countrydata_pop$country <- countrycode(countrydata_pop$geo_unit, origin = "iso2c", destination = "iso3c")
countrydata_pop$country <- ifelse(countrydata_pop$geo_unit == "EL", "GRC", 
                                  ifelse(countrydata_pop$geo_unit == "UK", "GBR", countrydata_pop$country))
countrydata_pop$period <- ifelse(countrydata_pop$year == 1300, 1,
                                 ifelse(countrydata_pop$year == 1350, 2,
                                        ifelse(countrydata_pop$year == 1400, 3,
                                               ifelse(countrydata_pop$year == 1450, 4,
                                                      ifelse(countrydata_pop$year == 1500, 5,
                                                             ifelse(countrydata_pop$year == 1550, 6,
                                                                    ifelse(countrydata_pop$year == 1600, 7,
                                                                           ifelse(countrydata_pop$year == 1650, 8,
                                                                                  ifelse(countrydata_pop$year == 1700, 9,
                                                                                         ifelse(countrydata_pop$year == 1750, 10,
                                                                                                ifelse(countrydata_pop$year == 1800, 11,
                                                                                                       ifelse(countrydata_pop$year == 1850, 12,
                                                                                                              ifelse(countrydata_pop$year == 1900, 13,
                                                                                                                     ifelse(countrydata_pop$year == 1950, 14,
                                                                                                                            ifelse(countrydata_pop$year == 2000, 15, NA)))))))))))))))
countrydata_pop$ID <- paste(countrydata_pop$country, countrydata_pop$period, sep="_")
countrydata_pop$population <- countrydata_pop$population * 1000

countrydata_pop <- subset(countrydata_pop, is.na(period) == F)

countrydata_pop$population <- as.numeric(countrydata_pop$population)
countrydata_pop <- countrydata_pop[,c(-1,-2,-3)]
countrydata_pop <- countrydata_pop[,c("ID", "country", "period", "population")]

# load NUTS3 level data and then correct for changing borders
urbandata <- read.csv2("./raw_data/population/urban_population_NUTS3.csv")
urbandata$period <- ifelse(urbandata$year == 1300, 1,
                           ifelse(urbandata$year == 1350, 2,
                                  ifelse(urbandata$year == 1400, 3,
                                         ifelse(urbandata$year == 1450, 4,
                                                ifelse(urbandata$year == 1500, 5,
                                                       ifelse(urbandata$year == 1550, 6,
                                                              ifelse(urbandata$year == 1600, 7,
                                                                     ifelse(urbandata$year == 1650, 8,
                                                                            ifelse(urbandata$year == 1700, 9,
                                                                                   ifelse(urbandata$year == 1750, 10,
                                                                                          ifelse(urbandata$year == 1800, 11,
                                                                                                 ifelse(urbandata$year == 1850, 12,
                                                                                                        ifelse(urbandata$year == 1900, 13,
                                                                                                               ifelse(urbandata$year == 1950, 14,
                                                                                                                      ifelse(urbandata$year == 2000, 15, NA)))))))))))))))
urbandata <- subset(urbandata, is.na(period)==F & population > 0)
urbandata$population <- urbandata$population*1000

# adjust population levels
for(l in 1:15){
  
  source("./scripts/03_zz_borderadjustments.R")
  
}

# add data in countrydata_pop to countrydata.p
fulldata$population <- vlookup(fulldata$ID, countrydata_pop, lookup_column = "ID", result_column = "population")

# add population data for Russia / Ukraine / Belarus
misc <- read.csv("./raw_data/population/urban_population_Buringh2021_inclNUTS_EUROPE.csv")
misc <- aggregate(misc$inhabitants.in.000.s, by = list(misc$country, misc$year), FUN = sum)
colnames(misc) <- c("country", "year", "population")
misc$period <- ifelse(misc$year == 1300, 1,
                      ifelse(misc$year == 1350, 2,
                             ifelse(misc$year == 1400, 3,
                                    ifelse(misc$year == 1450, 4,
                                           ifelse(misc$year == 1500, 5,
                                                  ifelse(misc$year == 1550, 6,
                                                         ifelse(misc$year == 1600, 7,
                                                                ifelse(misc$year == 1650, 8,
                                                                       ifelse(misc$year == 1700, 9,
                                                                              ifelse(misc$year == 1750, 10,
                                                                                     ifelse(misc$year == 1800, 11,
                                                                                            ifelse(misc$year == 1850, 12,
                                                                                                   ifelse(misc$year == 1900, 13,
                                                                                                          ifelse(misc$year == 1950, 14,
                                                                                                                 ifelse(misc$year == 2000, 15, NA)))))))))))))))
misc$population <- misc$population*1000
misc$iso <- countrycode(misc$country, origin = "country.name", destination = "iso3c")
misc$iso <- ifelse(misc$country == "Kosovo", "XKO", misc$iso)
misc$iso <- ifelse(misc$country == "Romenia", "ROU", misc$iso)
misc$ID <- paste(misc$iso, misc$period, sep="_")

fulldata$population <- ifelse(is.na(fulldata$population), vlookup(fulldata$ID, misc, lookup_column = "ID", result_column = "population"), fulldata$population)

# add US data manually (from OWID)
fulldata$population[fulldata$ID == "USA_15"] <- 6000000 * 0.0607
fulldata$population[fulldata$ID == "USA_16"] <- 23600000 * 0.1541
fulldata$population[fulldata$ID == "USA_17"] <- 78760000 * 0.3998
fulldata$population[fulldata$ID == "USA_18"] <- 148280000 * 0.6415
fulldata$population[fulldata$ID == "USA_19"] <- 282400000 * 0.7906


# load NUTS3 level data and then correct for changing borders
urbandata <- read.csv2("./raw_data/population/urban_population_NUTS2.csv")
urbandata$period <- ifelse(urbandata$year == 1300, 1,
                           ifelse(urbandata$year == 1350, 2,
                                  ifelse(urbandata$year == 1400, 3,
                                         ifelse(urbandata$year == 1450, 4,
                                                ifelse(urbandata$year == 1500, 5,
                                                       ifelse(urbandata$year == 1550, 6,
                                                              ifelse(urbandata$year == 1600, 7,
                                                                     ifelse(urbandata$year == 1650, 8,
                                                                            ifelse(urbandata$year == 1700, 9,
                                                                                   ifelse(urbandata$year == 1750, 10,
                                                                                          ifelse(urbandata$year == 1800, 11,
                                                                                                 ifelse(urbandata$year == 1850, 12,
                                                                                                        ifelse(urbandata$year == 1900, 13,
                                                                                                               ifelse(urbandata$year == 1950, 14,
                                                                                                                      ifelse(urbandata$year == 2000, 15, NA)))))))))))))))

urbandata <- subset(urbandata, population > 0)
urbandata$population <- urbandata$population*1000
urbandata$ID <- paste(urbandata$geo_unit, urbandata$year, sep="_")

fulldata$population <- ifelse(is.na(fulldata$population), vlookup(fulldata$ID2, urbandata, lookup_column = "ID", result_column = "population"), fulldata$population)
fulldata$population <- as.numeric(fulldata$population)

##########################################################################

#         POPULATION PRXOY            #

##########################################################################


fulldata$population_famous <- fulldata$births + fulldata$deaths

ggplot(fulldata, aes(x=population, y=population_famous)) +  geom_point(size=1.5, color = "darkorange1") + scale_y_continuous(trans='log', breaks = c(3, 20, 150)) + scale_x_continuous(trans='log', breaks = c(8000, 160000, 3000000)) + stat_poly_eq(formula = y~x, data=fulldata, aes(label = after_stat(rr.label)), parse = TRUE) +
  theme_light() + geom_smooth(method="lm", se=F, linetype="dashed", color="grey", alpha = 0.2) +
  labs(x = "Historical urban population estimate, log scale", y = "Number of famous biographies \n (births + deaths), weighted with HPI, log scale")
ggsave(paste("./genfiles_", version, "/figures_SI/population_proxy.png", sep=""), width = 7, height = 6)
