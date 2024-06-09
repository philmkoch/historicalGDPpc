# Data from Maddison project on countries
# Including slight adjustments as described in the manuscript and the SI Appendix
# That is, we augment the data if observations close to a missing year exist, e.g. we use AUT's value in 1820 for the year 1800
histGDP <- read.csv2("./raw_data/GDP/historicalGDPdata.csv")  

# replacing SUN with RUS prior to 1920
histGDP$countrycode <- ifelse(histGDP$countrycode == "SUN" & as.numeric(histGDP$year) <= 1920, "RUS", histGDP$countrycode)

histGDP$ID <- paste(histGDP$countrycode, histGDP$year, sep="_")

fulldata$GDPpc <- vlookup(fulldata$ID2, histGDP, lookup_column = "ID", result_column = "gdppc")
fulldata$pop_maddison <- vlookup(fulldata$ID2, histGDP, lookup_column = "ID", result_column = "pop")

# We collect regional GDP data for the year 2000 from contemporary sources (Eurostat etc.).
# Soruces are described in the manuscript
# We adjust these values to match 2011 USD
newgdppcdat <- read.csv2("./raw_data/GDP/DATA_regionalGDPpc_2011USD_PPP.csv")
newgdppcdat$ID <- paste(newgdppcdat$ID, "2000", sep="_")

fulldata$GDPpc <- ifelse(!is.na(vlookup(fulldata$ID2, newgdppcdat)), vlookup(fulldata$ID2, newgdppcdat), fulldata$GDPpc)
rm(newgdppcdat, histGDP)

# We collect historical regional GDP per capita data from various academic sources
# Sources are cited in the manuscript
additionaldata <- read.csv2("./raw_data/GDP/DATA_regional_historical.csv")
fulldata$GDPpc <- ifelse(is.na(fulldata$GDPpc), vlookup(fulldata$ID2, additionaldata), fulldata$GDPpc)

# We create a variable describing total GDP in a location by using births and deaths as population proxies
fulldata$GDP <- fulldata$GDPpc * (fulldata$births + fulldata$deaths)
