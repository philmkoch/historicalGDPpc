
if(l < max(urbandata$period)-5){
  
  # GBR
  countrydata_pop$population[countrydata_pop$country == "GBR" & countrydata_pop$period == l] <- as.numeric(countrydata_pop$population[countrydata_pop$country == "GBR" & countrydata_pop$period == l]) -
    sum(urbandata$population[str_sub(urbandata$geo_unit, end=3) == "UKN" & urbandata$period == l]) -
    sum(urbandata$population[str_sub(urbandata$geo_unit, end=3) == "UKM" & urbandata$period == l]) -
    sum(urbandata$population[str_sub(urbandata$geo_unit, end=3) == "UKL" & urbandata$period == l])
  countrydata_pop <- rbind(countrydata_pop,
                           c(paste("NIR", l, sep="_"), "NIR", l, sum(urbandata$population[str_sub(urbandata$geo_unit, end=3) == "UKN" & urbandata$period == l])),
                           c(paste("SCO", l, sep="_"), "SCO", l, sum(urbandata$population[str_sub(urbandata$geo_unit, end=3) == "UKM" & urbandata$period == l])),
                           c(paste("WAL", l, sep="_"), "WAL", l, sum(urbandata$population[str_sub(urbandata$geo_unit, end=3) == "UKL" & urbandata$period == l]))
  )
  
  # NLD
  countrydata_pop$population[countrydata_pop$country == "NLD" & countrydata_pop$period == l] <- as.numeric(countrydata_pop$population[countrydata_pop$country == "NLD" & countrydata_pop$period == l]) -
    sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "NL" & str_sub(urbandata$geo_unit, end=4) != "NL32" & str_sub(urbandata$geo_unit, end=4) != "NL33" & urbandata$period == l])
  countrydata_pop <- rbind(countrydata_pop,
                           c(paste("NLD_2", l, sep="_"), "NLD_2", l, sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "NL" & str_sub(urbandata$geo_unit, end=4) != "NL32" & str_sub(urbandata$geo_unit, end=4) != "NL33" & urbandata$period == l]))
  )
  
  
  # DEU
  countrydata_pop$population[countrydata_pop$country == "DEU" & countrydata_pop$period == l] <- as.numeric(countrydata_pop$population[countrydata_pop$country == "DEU" & countrydata_pop$period == l]) +
    sum(urbandata$population[(str_sub(urbandata$geo_unit, end = 4) == "PL42"
                             | str_sub(urbandata$geo_unit, end = 4) == "PL43"
                             | str_sub(urbandata$geo_unit, end = 4) == "PL51"
                             | str_sub(urbandata$geo_unit, end = 4) == "PL52"
                             | urbandata$geo_unit == "BE336"
                             | urbandata$geo_unit == "PL224"
                             | urbandata$geo_unit == "PL228"
                             | urbandata$geo_unit == "PL22B"
                             | urbandata$geo_unit == "PL22C"
                             | urbandata$geo_unit == "PL229"
                             | urbandata$geo_unit == "PL227")
                             & urbandata$period == l]) -
    sum(urbandata$population[(urbandata$geo_unit == "DEF07" | urbandata$geo_unit == "DEF0C") & urbandata$period == l])
  
  
  # POL
  countrydata_pop$population[countrydata_pop$country == "POL" & countrydata_pop$period == l] <- sum(urbandata$population[str_sub(urbandata$geo_unit, end=4) == "PL21" & urbandata$period == l])
  countrydata_pop <- rbind(countrydata_pop, 
                           c(paste("POL_2", l, sep="_"), "POL_2", l, 
                             sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "PL" & urbandata$period == l]) -
                               sum(urbandata$population[(str_sub(urbandata$geo_unit, end = 4) == "PL42"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL21"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL43"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL51"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL52"
                                                         | urbandata$geo_unit == "PL224"
                                                         | urbandata$geo_unit == "PL228"
                                                         | urbandata$geo_unit == "PL22B"
                                                         | urbandata$geo_unit == "PL22C"
                                                         | urbandata$geo_unit == "PL229"
                                                         | urbandata$geo_unit == "PL227")
                                                        & urbandata$period == l])
                             )
  )
  
  # ITA
  countrydata_pop$population[countrydata_pop$country == "ITA" & countrydata_pop$period == l] <- as.numeric(countrydata_pop$population[countrydata_pop$country == "ITA" & countrydata_pop$period == l]) -
    sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "IT" & str_sub(urbandata$geo_unit, end=3) != "ITC" & str_sub(urbandata$geo_unit, end=3) != "ITH" 
                             & str_sub(urbandata$geo_unit, end=4) != "ITI1" & str_sub(urbandata$geo_unit, end=4) != "ITI2" & str_sub(urbandata$geo_unit, end=4) != "ITI3" & urbandata$period == l])
  countrydata_pop <- rbind(countrydata_pop,
                           c(paste("S_ITA", l, sep="_"), "S_ITA", l, sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "IT" & str_sub(urbandata$geo_unit, end=3) != "ITC" & str_sub(urbandata$geo_unit, end=3) != "ITH" 
                                                                                                  & str_sub(urbandata$geo_unit, end=4) != "ITI1" & str_sub(urbandata$geo_unit, end=4) != "ITI2" & str_sub(urbandata$geo_unit, end=4) != "ITI3" & urbandata$period == l]))
  )
  
}


if(l == max(urbandata$period)-5 | l == max(urbandata$period)-4){
  
  # NLD
  countrydata_pop$population[countrydata_pop$country == "NLD" & countrydata_pop$period == l] <- as.numeric(countrydata_pop$population[countrydata_pop$country == "NLD" & countrydata_pop$period == l]) -
    sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "NL" & str_sub(urbandata$geo_unit, end=4) != "NL32" & str_sub(urbandata$geo_unit, end=4) != "NL33" & urbandata$period == l])
  countrydata_pop <- rbind(countrydata_pop,
                           c(paste("NLD_2", l, sep="_"), "NLD_2", l, sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "NL" & str_sub(urbandata$geo_unit, end=4) != "NL32" & str_sub(urbandata$geo_unit, end=4) != "NL33" & urbandata$period == l]))
  )
  
  
  # DEU
  countrydata_pop$population[countrydata_pop$country == "DEU" & countrydata_pop$period == l] <- as.numeric(countrydata_pop$population[countrydata_pop$country == "DEU" & countrydata_pop$period == l]) +
    sum(urbandata$population[(str_sub(urbandata$geo_unit, end = 4) == "PL42"
                              | str_sub(urbandata$geo_unit, end = 4) == "PL43"
                              | str_sub(urbandata$geo_unit, end = 4) == "PL51"
                              | str_sub(urbandata$geo_unit, end = 4) == "PL52"
                              | urbandata$geo_unit == "BE336"
                              | urbandata$geo_unit == "PL224"
                              | urbandata$geo_unit == "PL228"
                              | urbandata$geo_unit == "PL22B"
                              | urbandata$geo_unit == "PL22C"
                              | urbandata$geo_unit == "PL229"
                              | urbandata$geo_unit == "PL227")
                             & urbandata$period == l]) -
    sum(urbandata$population[(urbandata$geo_unit == "DEF07" | urbandata$geo_unit == "DEF0C") & urbandata$period == l])
  
  
  # POL
  countrydata_pop$population[countrydata_pop$country == "POL" & countrydata_pop$period == l] <- sum(urbandata$population[str_sub(urbandata$geo_unit, end=4) == "PL21" & urbandata$period == l])
  countrydata_pop <- rbind(countrydata_pop, 
                           c(paste("POL_2", l, sep="_"), "POL_2", l, 
                             sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "PL" & urbandata$period == l]) -
                               sum(urbandata$population[(str_sub(urbandata$geo_unit, end = 4) == "PL42"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL21"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL43"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL51"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL52"
                                                         | urbandata$geo_unit == "PL224"
                                                         | urbandata$geo_unit == "PL228"
                                                         | urbandata$geo_unit == "PL22B"
                                                         | urbandata$geo_unit == "PL22C"
                                                         | urbandata$geo_unit == "PL229"
                                                         | urbandata$geo_unit == "PL227")
                                                        & urbandata$period == l])
                           )
  )
  
  # ITA
  countrydata_pop$population[countrydata_pop$country == "ITA" & countrydata_pop$period == l] <- as.numeric(countrydata_pop$population[countrydata_pop$country == "ITA" & countrydata_pop$period == l]) -
    sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "IT" & str_sub(urbandata$geo_unit, end=3) != "ITC" & str_sub(urbandata$geo_unit, end=3) != "ITH" 
                             & str_sub(urbandata$geo_unit, end=4) != "ITI1" & str_sub(urbandata$geo_unit, end=4) != "ITI2" & str_sub(urbandata$geo_unit, end=4) != "ITI3" & urbandata$period == l])
  countrydata_pop <- rbind(countrydata_pop,
                           c(paste("S_ITA", l, sep="_"), "S_ITA", l, sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "IT" & str_sub(urbandata$geo_unit, end=3) != "ITC" & str_sub(urbandata$geo_unit, end=3) != "ITH" 
                                                                                                  & str_sub(urbandata$geo_unit, end=4) != "ITI1" & str_sub(urbandata$geo_unit, end=4) != "ITI2" & str_sub(urbandata$geo_unit, end=4) != "ITI3" & urbandata$period == l]))
  )
  
}

if(l == max(urbandata$period)-3){
  
  
  # POL
  countrydata_pop$population[countrydata_pop$country == "POL" & countrydata_pop$period == l] <- sum(urbandata$population[str_sub(urbandata$geo_unit, end=4) == "PL21" & urbandata$period == l])
  countrydata_pop <- rbind(countrydata_pop, 
                           c(paste("POL_2", l, sep="_"), "POL_2", l, 
                             sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "PL" & urbandata$period == l]) -
                               sum(urbandata$population[(str_sub(urbandata$geo_unit, end = 4) == "PL42"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL21"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL43"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL51"
                                                         | str_sub(urbandata$geo_unit, end = 4) == "PL52"
                                                         | urbandata$geo_unit == "PL224"
                                                         | urbandata$geo_unit == "PL228"
                                                         | urbandata$geo_unit == "PL22B"
                                                         | urbandata$geo_unit == "PL22C"
                                                         | urbandata$geo_unit == "PL229"
                                                         | urbandata$geo_unit == "PL227")
                                                        & urbandata$period == l])
                           )
  )
  
  # ITA
  countrydata_pop$population[countrydata_pop$country == "ITA" & countrydata_pop$period == l] <- as.numeric(countrydata_pop$population[countrydata_pop$country == "ITA" & countrydata_pop$period == l]) -
    sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "IT" & str_sub(urbandata$geo_unit, end=3) != "ITC" & str_sub(urbandata$geo_unit, end=3) != "ITH" 
                             & str_sub(urbandata$geo_unit, end=4) != "ITI1" & str_sub(urbandata$geo_unit, end=4) != "ITI2" & str_sub(urbandata$geo_unit, end=4) != "ITI3" & urbandata$period == l])
  countrydata_pop <- rbind(countrydata_pop,
                           c(paste("S_ITA", l, sep="_"), "S_ITA", l, sum(urbandata$population[str_sub(urbandata$geo_unit, end=2) == "IT" & str_sub(urbandata$geo_unit, end=3) != "ITC" & str_sub(urbandata$geo_unit, end=3) != "ITH" 
                                                                                              & str_sub(urbandata$geo_unit, end=4) != "ITI1" & str_sub(urbandata$geo_unit, end=4) != "ITI2" & str_sub(urbandata$geo_unit, end=4) != "ITI3" & urbandata$period == l]))
  )
  
  # CSK
  countrydata_pop <- rbind(countrydata_pop,
                           c(paste("CSK", l, sep="_"), "CSK", l, sum(urbandata$population[(str_sub(urbandata$geo_unit, end=2) == "CZ" | str_sub(urbandata$geo_unit, end=2) == "SK") & urbandata$period == l]))
  )
  countrydata_pop <- subset(countrydata_pop, ID != paste("CZE", l, sep="_") & ID != paste("SVK", l, sep="_"))
  
}

if(l == max(urbandata$period)-2 | l == max(urbandata$period)-1){
  
  # CSK
  countrydata_pop <- rbind(countrydata_pop,
                           c(paste("CSK", l, sep="_"), "CSK", l, sum(urbandata$population[(str_sub(urbandata$geo_unit, end=2) == "CZ" | str_sub(urbandata$geo_unit, end=2) == "SK") & urbandata$period == l]))
  )
  countrydata_pop <- subset(countrydata_pop, ID != paste("CZE", l, sep="_") & ID != paste("SVK", l, sep="_"))
  
}