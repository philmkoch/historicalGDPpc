
# loading the raw data by Laouenan et al.
data <- read.csv("./raw_data/famous_individuals/cross-verified-database.csv")

# filtering for individuals with at least 2 language editions
data <- subset(data, number_wiki_editions > 1 &
                 birth > 1099)

# Assigning geography to places of birth and death of famous individuals (NUTS2, oblasts, MSAs, countries, etc...)
source("./scripts/01a_assign_geography.R")

# Assigning and cleaning occupations
# this also saves some descriptive statistics at the end
source("./scripts/01b_assign_occupations.R")

# Now we aggregate the data on famous individuals to a panel dataset, where one line refers to all individuals in a location at a specific time
source("./scripts/01c_aggregating_to_panel.R")
