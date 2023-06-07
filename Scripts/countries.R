library(readr)
library(haven)
natgov <- read_sav('Data_preproc/euro2018.sav') #Eurobarometer 90.2 (2018)

summary(natgov$qc5_6)
hist(natgov$qc5_6)
natgov$isocntry

