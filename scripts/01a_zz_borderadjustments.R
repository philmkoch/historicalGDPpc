
if(l < length(data_list) - 5){
  
  data$bplace_countrycode_v2 <- data$bplace_countrycode
  
  # GBR
  data$bplace_countrycode_v2 <- ifelse(str_sub(data$bplace_NUTS3, end=3) == "UKN", "NIR",
                                       ifelse(str_sub(data$bplace_NUTS3, end=3) == "UKM", "SCO",
                                              ifelse(str_sub(data$bplace_NUTS3, end=3) == "UKL", "WAL",data$bplace_countrycode_v2)))
  
  # NLD
  data$bplace_countrycode_v2 <- ifelse(data$bplace_countrycode == "NLD" & (str_sub(data$bplace_NUTS3, end=4) != "NL32" & str_sub(data$bplace_NUTS3, end=4) != "NL33"), "NLD_2", data$bplace_countrycode_v2)
  
  # ITA
  data$bplace_countrycode_v2 <- ifelse(data$bplace_countrycode == "ITA" & (str_sub(data$bplace_NUTS3, end=3) != "ITC" & str_sub(data$bplace_NUTS3, end=3) != "ITH"
                                                                           & str_sub(data$bplace_NUTS3, end=4) != "ITI1" & str_sub(data$bplace_NUTS3, end=4) != "ITI2" & str_sub(data$bplace_NUTS3, end=4) != "ITI3"), "S_ITA", 
                                       data$bplace_countrycode_v2)
  
  # POL
  data$bplace_countrycode_v2 <- ifelse(data$bplace_countrycode == "POL" & str_sub(data$bplace_NUTS3, end=4) != "PL21", "POL_2", data$bplace_countrycode_v2)
  
  # DEU
  data$bplace_countrycode_v2 <- ifelse(str_sub(data$bplace_NUTS3, end=4) == "PL42"
                                       | str_sub(data$bplace_NUTS3, end=4) == "PL43"
                                       | str_sub(data$bplace_NUTS3, end=4) == "PL51"
                                       | str_sub(data$bplace_NUTS3, end=4) == "PL52"
                                       | data$bplace_NUTS3 == "BE336"
                                       | data$bplace_NUTS3 == "PL224"
                                       | data$bplace_NUTS3 == "PL228"
                                       | data$bplace_NUTS3 == "PL22B"
                                       | data$bplace_NUTS3 == "PL22C"
                                       | data$bplace_NUTS3 == "PL229"
                                       | data$bplace_NUTS3 == "PL227", 
                                       "DEU", 
                                       data$bplace_countrycode_v2)
  data$bplace_countrycode_v2 <- ifelse(data$bplace_NUTS3 == "DEF07" | data$bplace_NUTS3 == "DEF0C", NA,
                                       data$bplace_countrycode_v2)
  
}

if(l == length(data_list) - 4 | l == length(data_list) - 5){
  
  data$bplace_countrycode_v2 <- data$bplace_countrycode
  
  # GBR
  # data$bplace_countrycode_v2 <- ifelse(str_sub(data$bplace_NUTS3, end=3) == "UKN", "NIR",
  #                                      ifelse(str_sub(data$bplace_NUTS3, end=3) == "UKM", "SCO",
  #                                             ifelse(str_sub(data$bplace_NUTS3, end=3) == "UKL", "WAL",data$bplace_countrycode_v2)))
  
  # NLD
  data$bplace_countrycode_v2 <- ifelse(data$bplace_countrycode == "NLD" & (str_sub(data$bplace_NUTS3, end=4) != "NL32" & str_sub(data$bplace_NUTS3, end=4) != "NL33"), "NLD_2", data$bplace_countrycode_v2)
  
  # ITA
  data$bplace_countrycode_v2 <- ifelse(data$bplace_countrycode == "ITA" & (str_sub(data$bplace_NUTS3, end=3) != "ITC" & str_sub(data$bplace_NUTS3, end=3) != "ITH"
                                                                           & str_sub(data$bplace_NUTS3, end=4) != "ITI1" & str_sub(data$bplace_NUTS3, end=4) != "ITI2" & str_sub(data$bplace_NUTS3, end=4) != "ITI3"), "S_ITA", 
                                       data$bplace_countrycode_v2)
  
  # POL
  data$bplace_countrycode_v2 <- ifelse(data$bplace_countrycode == "POL" & str_sub(data$bplace_NUTS3, end=4) != "PL21", "POL_2", data$bplace_countrycode_v2)
  
  # DEU
  data$bplace_countrycode_v2 <- ifelse(str_sub(data$bplace_NUTS3, end=4) == "PL42"
                                       | str_sub(data$bplace_NUTS3, end=4) == "PL43"
                                       | str_sub(data$bplace_NUTS3, end=4) == "PL51"
                                       | str_sub(data$bplace_NUTS3, end=4) == "PL52"
                                       | data$bplace_NUTS3 == "BE336"
                                       | data$bplace_NUTS3 == "PL224"
                                       | data$bplace_NUTS3 == "PL228"
                                       | data$bplace_NUTS3 == "PL22B"
                                       | data$bplace_NUTS3 == "PL22C"
                                       | data$bplace_NUTS3 == "PL229"
                                       | data$bplace_NUTS3 == "PL227", 
                                       "DEU", 
                                       data$bplace_countrycode_v2)
  data$bplace_countrycode_v2 <- ifelse(data$bplace_NUTS3 == "DEF07" | data$bplace_NUTS3 == "DEF0C", NA,
                                       data$bplace_countrycode_v2)
  
}

if(l == length(data_list) - 3){
  
  data$bplace_countrycode_v2 <- data$bplace_countrycode
  
  # ITA
  data$bplace_countrycode_v2 <- ifelse(data$bplace_countrycode == "ITA" & (str_sub(data$bplace_NUTS3, end=3) != "ITC" & str_sub(data$bplace_NUTS3, end=3) != "ITH"
                                                                           & str_sub(data$bplace_NUTS3, end=4) != "ITI1" & str_sub(data$bplace_NUTS3, end=4) != "ITI2" & str_sub(data$bplace_NUTS3, end=4) != "ITI3"), "S_ITA", 
                                       data$bplace_countrycode_v2)
  
  # POL
  data$bplace_countrycode_v2 <- ifelse(data$bplace_countrycode == "POL" & str_sub(data$bplace_NUTS3, end=4) != "PL21", "POL_2", data$bplace_countrycode_v2)
  
  # CSK
  data$bplace_countrycode_v2 <- ifelse(data$bplace_countrycode == "CZE" | data$bplace_countrycode == "SVK", "CSK", data$bplace_countrycode_v2)
  
  
}

if(l == length(data_list) - 2){
  
  data$bplace_countrycode_v2 <- data$bplace_countrycode
  
  # CSK
  data$bplace_countrycode_v2 <- ifelse(data$bplace_countrycode == "CZE" | data$bplace_countrycode == "SVK", "CSK", data$bplace_countrycode_v2)
  
}

if(l == length(data_list) - 1){
  
  data$bplace_countrycode_v2 <- data$bplace_countrycode
  
  # CSK
  data$bplace_countrycode_v2 <- ifelse(data$bplace_countrycode == "CZE" | data$bplace_countrycode == "SVK", "CSK", data$bplace_countrycode_v2)
  
}

if(l == length(data_list)){
  
  data$bplace_countrycode_v2 <- data$bplace_countrycode
  
}


if(l < length(data_list) - 5){
  
  data$dplace_countrycode_v2 <- data$dplace_countrycode
  
  # GBR
  data$dplace_countrycode_v2 <- ifelse(str_sub(data$dplace_NUTS3, end=3) == "UKN", "NIR",
                                       ifelse(str_sub(data$dplace_NUTS3, end=3) == "UKM", "SCO",
                                              ifelse(str_sub(data$dplace_NUTS3, end=3) == "UKL", "WAL",data$dplace_countrycode_v2)))
  
  # NLD
  data$dplace_countrycode_v2 <- ifelse(data$dplace_countrycode == "NLD" & (str_sub(data$dplace_NUTS3, end=4) != "NL32" & str_sub(data$dplace_NUTS3, end=4) != "NL33"), "NLD_2", data$dplace_countrycode_v2)
  
  # ITA
  data$dplace_countrycode_v2 <- ifelse(data$dplace_countrycode == "ITA" & (str_sub(data$dplace_NUTS3, end=3) != "ITC" & str_sub(data$dplace_NUTS3, end=3) != "ITH"
                                                                           & str_sub(data$dplace_NUTS3, end=4) != "ITI1" & str_sub(data$dplace_NUTS3, end=4) != "ITI2" & str_sub(data$dplace_NUTS3, end=4) != "ITI3"), "S_ITA", 
                                       data$dplace_countrycode_v2)
  
  # POL
  data$dplace_countrycode_v2 <- ifelse(data$dplace_countrycode == "POL" & str_sub(data$dplace_NUTS3, end=4) != "PL21", "POL_2", data$dplace_countrycode_v2)
  
  # DEU
  data$dplace_countrycode_v2 <- ifelse(str_sub(data$dplace_NUTS3, end=4) == "PL42"
                                       | str_sub(data$dplace_NUTS3, end=4) == "PL43"
                                       | str_sub(data$dplace_NUTS3, end=4) == "PL51"
                                       | str_sub(data$dplace_NUTS3, end=4) == "PL52"
                                       | data$dplace_NUTS3 == "BE336"
                                       | data$dplace_NUTS3 == "PL224"
                                       | data$dplace_NUTS3 == "PL228"
                                       | data$dplace_NUTS3 == "PL22B"
                                       | data$dplace_NUTS3 == "PL22C"
                                       | data$dplace_NUTS3 == "PL229"
                                       | data$dplace_NUTS3 == "PL227", 
                                       "DEU", 
                                       data$dplace_countrycode_v2)
  data$dplace_countrycode_v2 <- ifelse(data$dplace_NUTS3 == "DEF07" | data$dplace_NUTS3 == "DEF0C", NA,
                                       data$dplace_countrycode_v2)
  
}

if(l == length(data_list) - 4 | l == length(data_list) - 5){
  
  data$dplace_countrycode_v2 <- data$dplace_countrycode
  
  # GBR
  # data$dplace_countrycode_v2 <- ifelse(str_sub(data$dplace_NUTS3, end=3) == "UKN", "NIR",
  #                                      ifelse(str_sub(data$dplace_NUTS3, end=3) == "UKM", "SCO",
  #                                             ifelse(str_sub(data$dplace_NUTS3, end=3) == "UKL", "WAL",data$dplace_countrycode_v2)))
  
  # NLD
  data$dplace_countrycode_v2 <- ifelse(data$dplace_countrycode == "NLD" & (str_sub(data$dplace_NUTS3, end=4) != "NL32" & str_sub(data$dplace_NUTS3, end=4) != "NL33"), "NLD_2", data$dplace_countrycode_v2)
  
  # ITA
  data$dplace_countrycode_v2 <- ifelse(data$dplace_countrycode == "ITA" & (str_sub(data$dplace_NUTS3, end=3) != "ITC" & str_sub(data$dplace_NUTS3, end=3) != "ITH"
                                                                           & str_sub(data$dplace_NUTS3, end=4) != "ITI1" & str_sub(data$dplace_NUTS3, end=4) != "ITI2" & str_sub(data$dplace_NUTS3, end=4) != "ITI3"), "S_ITA", 
                                       data$dplace_countrycode_v2)
  
  # POL
  data$dplace_countrycode_v2 <- ifelse(data$dplace_countrycode == "POL" & str_sub(data$dplace_NUTS3, end=4) != "PL21", "POL_2", data$dplace_countrycode_v2)
  
  # DEU
  data$dplace_countrycode_v2 <- ifelse(str_sub(data$dplace_NUTS3, end=4) == "PL42"
                                       | str_sub(data$dplace_NUTS3, end=4) == "PL43"
                                       | str_sub(data$dplace_NUTS3, end=4) == "PL51"
                                       | str_sub(data$dplace_NUTS3, end=4) == "PL52"
                                       | data$dplace_NUTS3 == "BE336"
                                       | data$dplace_NUTS3 == "PL224"
                                       | data$dplace_NUTS3 == "PL228"
                                       | data$dplace_NUTS3 == "PL22B"
                                       | data$dplace_NUTS3 == "PL22C"
                                       | data$dplace_NUTS3 == "PL229"
                                       | data$dplace_NUTS3 == "PL227", 
                                       "DEU", 
                                       data$dplace_countrycode_v2)
  data$dplace_countrycode_v2 <- ifelse(data$dplace_NUTS3 == "DEF07" | data$dplace_NUTS3 == "DEF0C", NA,
                                       data$dplace_countrycode_v2)
  
}

if(l == length(data_list) - 3){
  
  data$dplace_countrycode_v2 <- data$dplace_countrycode
  
  # ITA
  data$dplace_countrycode_v2 <- ifelse(data$dplace_countrycode == "ITA" & (str_sub(data$dplace_NUTS3, end=3) != "ITC" & str_sub(data$dplace_NUTS3, end=3) != "ITH"
                                                                           & str_sub(data$dplace_NUTS3, end=4) != "ITI1" & str_sub(data$dplace_NUTS3, end=4) != "ITI2" & str_sub(data$dplace_NUTS3, end=4) != "ITI3"), "S_ITA", 
                                       data$dplace_countrycode_v2)
  
  # POL
  data$dplace_countrycode_v2 <- ifelse(data$dplace_countrycode == "POL" & str_sub(data$dplace_NUTS3, end=4) != "PL21", "POL_2", data$dplace_countrycode_v2)
  
  # CSK
  data$dplace_countrycode_v2 <- ifelse(data$dplace_countrycode == "CZE" | data$dplace_countrycode == "SVK", "CSK", data$dplace_countrycode_v2)
  
  
}

if(l == length(data_list) - 2){
  
  data$dplace_countrycode_v2 <- data$dplace_countrycode
  
  # CSK
  data$dplace_countrycode_v2 <- ifelse(data$dplace_countrycode == "CZE" | data$dplace_countrycode == "SVK", "CSK", data$dplace_countrycode_v2)
  
}

if(l == length(data_list) - 1){
  
  data$dplace_countrycode_v2 <- data$dplace_countrycode
  
  # CSK
  data$dplace_countrycode_v2 <- ifelse(data$dplace_countrycode == "CZE" | data$dplace_countrycode == "SVK", "CSK", data$dplace_countrycode_v2)
  
}

if(l == length(data_list)){
  
  data$dplace_countrycode_v2 <- data$dplace_countrycode
  
}

