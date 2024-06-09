rm(list=ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(ggplot2, glmnet, reshape2, haven, irr, future.apply, ICC, broom, stringr, knitr, kableExtra, officer, flextable, cowplot, EconGeo, modi, eurostat, sf, raster, DataVisualizations, plotly, xlsx, plm, fixest, countrycode, ggpmisc, car, plyr, pals, htmlwidgets, forcats, ds4psy, rsample, dplyr, rpart, rpart.plot, ipred, caret, BMS, Matrix, BMA, data.table, modelsummary, sandwich, boot, sp, rgdal, treemap, RColorBrewer, parallel, foreach, doParallel)

set.seed(8765)

# load custom function for data processing
source("./misc/R_functions/vlookup.R")

# PARAMETER
lag_parameter <- 150
normalization_option <- "log"
k <- 10      # number of folds
HPI_incl <- "Y" # Should HPI be included in the derivation of features? "Y" yes or "N" no
rescale_regions <- "Y" # Should regional estimates be rescaled to match country-level data? "Y" yes or "N" no
shapley <- "Y" # Should shapley values be computed? "Y" yes or "N" no
standarderrors <- "Y" # Should standard errors be computed? "Y" yes or "N" no
n_bootstraps <- 250 # number of bootstraps for computing standard errors
bootstrap_CI <- c(0.1, 0.9) # confidence interval for standard errors
n_draws <- 500 # number of draws to assess model performance

version <- paste0(Sys.Date(), "_COMPLETE")

# Create a date-specific directory
if(dir.exists(paste0("./genfiles_", version))){
  unlink(paste0("./genfiles_", version), recursive = T)
}
dir.create(paste0("./genfiles_", version))
dir.create(paste0("./genfiles_", version, "/figures"))
dir.create(paste0("./genfiles_", version, "/figures_SI"))
dir.create(paste0("./genfiles_", version, "/figures_SI/ECI"))
dir.create(paste0("./genfiles_", version, "/figures_SI/LASSO_results"))
dir.create(paste0("./genfiles_", version, "/figures_SI/SVD"))
dir.create(paste0("./genfiles_", version, "/maps"))
dir.create(paste0("./genfiles_", version, "/maps/famous_individuals"))
dir.create(paste0("./genfiles_", version, "/maps/Maddison"))
dir.create(paste0("./genfiles_", version, "/maps/Maddison+ML"))
dir.create(paste0("./genfiles_", version, "/maps/Maddison+ML/countries"))
dir.create(paste0("./genfiles_", version, "/maps/Maddison+ML/regions"))

source("./scripts/01_load_famousindividuals.R")

source("./scripts/02_load_GDPdata.R")

source("./scripts/03_load_populationdata.R")

# reorder columns
data <- fulldata %>% select(ID, ID2, period, year, country, country_0:population, births:avg_age_immigrants, architect_born:year2000)

rownames(data) <- data$ID

# # We do not make out-of-sample predictions for locations with very few births or deaths
# # Hence, we remove those observations from the dataset
data <- subset(data, !is.na(GDPpc) |
                 ( (period < 8 & births >= 3 & deaths >= 3) |
                     (period > 7 & period < 15 & births >= 5 & deaths >= 5) |
                     (period == 15 & births >= 10 & deaths >= 10))
)

source("./scripts/misc_summarytable.R")

if(HPI_incl == "Y"){
  source("./scripts/04_HPIadjustment.R")
}

source("./scripts/05_add_SVD_and_ECI.R")

source("./scripts/06_generate_maps.R")

saveRDS(data, "./misc/data_inputToML.rds")

data <- readRDS("./misc/data_inputToML.rds")

labeled_data <- subset(data, is.na(GDPpc)==F)
unlabeled_data <- subset(data, is.na(GDPpc))

# MODEL PERFORMANCE

source("./scripts/07_model_performance.R")

# FULL MODEL AND RESULTS

source("./scripts/08_finalmodel.R")

source("./scripts/09_descriptives.R")
