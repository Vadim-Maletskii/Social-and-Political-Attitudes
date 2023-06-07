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
