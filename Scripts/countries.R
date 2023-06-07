library(readr)
library(haven)
euro2018<- read_sav('Data_preproc/euro2018.sav') #Eurobarometer 90.2 (2018)

euro2018 <- subset(euro2018, !grepl("5", qc5_6))

summary(euro2018$qc5_6)
hist(euro2018$qc5_6)
euro2018$isocntry

means <- aggregate(qc5_6 ~ isocntry, data = euro2018, FUN = mean)

print(means)

# Sort the means in ascending order
means <- means[order(means$qc5_6), ]

# Create a bar plot of the means in ascending order
barplot(means$qc5_6, names.arg = means$isocntry,
        xlab = "isocntry", ylab = "Mean qc5_6",
        main = "Means of qc5_6 across isocntry ", las = 2)

euro = read_sav('Data_preproc/euro.sav')

# depended: qe2_1, qe2_2, qe2_3, qe2_4, qe2_5, qe2_6
# main problems: qa3.3 qa3.15 qa3.16
# qa6a_4, qa6a_12
# qa8_4, qa8_8, qa8_9
# Country: isocntry

summary(euro$qa3.3)
summary(euro$qa3.15)
summary(euro$qa3.16)

summary(euro$qa8_4)
summary(euro$qa8_8)
summary(euro$qa8_9)
