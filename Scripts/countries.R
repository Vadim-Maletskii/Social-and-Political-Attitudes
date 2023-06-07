library(readr)
library(haven)
euro2018<- read_sav('Data_preproc/euro2018.sav') #Eurobarometer 90.2 (2018)

euro2018 <- subset(euro2018, !grepl("5", qc5_6))

summary(euro2018$qc5_6)
hist(euro2018$qc5_6)
euro2018$isocntry

