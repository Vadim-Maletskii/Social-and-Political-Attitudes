library(readr)
library(haven)
library(dplyr)
# install.packages('psych')
library(psych)
library(ggplot2)

# Data cleaning ---- 
euro2018<- read_sav('Data_preproc/euro2018.sav') #Eurobarometer 90.2 (2018)

euro = read_sav('Data_preproc/euro.sav')

euro2018 <- subset(euro2018, !grepl("5", qc5_6))

# Recode variable qc5_6 in dataset euro2018
euro2018 <- euro2018 %>%
  mutate(qc5_6 = case_when(
    qc5_6 == 1 ~ 4,
    qc5_6 == 2 ~ 3,
    qc5_6 == 3 ~ 2,
    qc5_6 == 4 ~ 1,
    TRUE ~ qc5_6
  ))

# Independent + dependent variables
all_variables <- euro %>% select(isocntry, qa3.3, qa3.15, qa3.16, qa6a_4, qa6a_12, qa8_4, qa8_8, qa8_9,
                                 qe2_1, qe2_2, qe2_3, qe2_4, qe2_5, qe2_6)
nona_variables <- na.omit(all_variables)
nona_variables <- filter(nona_variables, !(qa6a_4 %in% c(3,9)), qa6a_12 != 3, !(qa8_4 %in% c(5,9)),
                         qa8_8 != 5, qa8_9 != 5)

# Find unique values of isocntry in the dataset euro
unique_countries <- unique(nona_variables$isocntry)

# Filter dataset euro2018 to keep only the rows with isocntry values in unique_countries
euro2018 <- subset(euro2018, isocntry %in% unique_countries)


summary(euro2018$qc5_6)
hist(euro2018$qc5_6)
euro2018$isocntry

# Preparation to plot creation ---- 
means <- aggregate(qc5_6 ~ isocntry, data = euro2018, FUN = mean)

print(means)
unique(euro$country)

# Sort the means in ascending order
means <- means[order(means$qc5_6), ]

# Boxplot colored ---- 
# Create a new variable for the groups
means$region <- "Other"

means$region[means$isocntry %in% c("AT", "BE", "DE-W", "DE-E", "DK", "FR", "IE", "LU", "NL")] <- "Western Europe"

means$region[means$isocntry %in% c("CY", "ES", "GR", "HR", "IT", "PT", "MT")] <- "Southern Europe"

means$region[means$isocntry %in% c("BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SI", "SK")] <- "Eastern Europe"

means$region[means$isocntry %in% c("FI", "SE")] <- "Northern Europe"

# Define colors for each region
colors <- c("red", "blue", "green", "orange")

# Create ordered boxplot with colors
mean_values_boxplot <- aggregate(qc5_6 ~ region, data = means, FUN = mean)
ordered_regions <- mean_values_boxplot$region[order(mean_values_boxplot$qc5_6)]
means$region <- factor(means$region, levels = ordered_regions)
boxplot(qc5_6 ~ region, data = means, col = colors,
        xlab = "Region", ylab = "qc5_6",
        main = "Boxplot of qc5_6 by Region")

# Barplot colored ---- 

# Create a data frame with the means data
bar_col <- data.frame(isocntry = means$isocntry, qc5_6 = means$qc5_6, region = means$region)

# Define the colors based on the unique values of means$region
colors <- c("red", "blue", "green", "orange")

# Create the barplot using ggplot2 with ordered bars
ggplot(bar_col, aes(x = reorder(isocntry, qc5_6), y = qc5_6, fill = as.factor(region))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(x = "isocntry", y = "Mean qc5_6", title = "Means of qc5_6 across isocntry") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# barplot without colour ---- 
# Create a bar plot of the means in ascending order

barplot(means$qc5_6, names.arg = means$isocntry,
        xlab = "isocntry", ylab = "Mean qc5_6",
        main = "Means of qc5_6 across isocntry ", las = 2)

# Dep and Ind VARIABLES ---- 
# depended: qe2_1, qe2_2, qe2_3, qe2_4, qe2_5, qe2_6
# main problems: qa3.3 (rising prices), qa3.15 (energy), qa3.16 (international situation)
# qa6a_4 (trust in army), qa6a_12 (trust in NATO)
# qa8_4 (EU efficient), qa8_8 (EU reacting fast in crises), qa8_9 (EU united)
# Country: isocntry
# qd3a Most positive EU result
# qe1_2 Satisfaction with EU response
table(euro$qd3t.11)

summary(euro$qa3.3)
summary(euro$qa3.15)
summary(euro$qa3.16)

range(euro$qa8_4)

summary(euro$qa8_4)
summary(euro$qa8_8)
summary(euro$qa8_9)


# Only dependent variables
dep_vars <- nona_variables %>% select(qe2_1, qe2_2, qe2_3, qe2_4, qe2_5, qe2_6)

# Factor analysis ---- 
dep_cor <- cor(dep_vars)
dep_cor
scree(dep_cor)
principal(dep_cor, nfactors = 1, rotate='varimax')

# If we group the dependent variables together and make a new variable (sum of them) ----
nona_variables$dep_sum <- nona_variables$qe2_1 + nona_variables$qe2_2 + nona_variables$qe2_3 +
  nona_variables$qe2_4 + nona_variables$qe2_5 + nona_variables$qe2_6
table(nona_variables$dep_sum)
means_dep_vars <- aggregate(dep_sum ~ isocntry, data = nona_variables, FUN = mean)
means_dep_vars <- means_dep_vars[order(means_dep_vars$dep_sum), ]

# Barplot colored ----
means_dep_vars$region <- "Other"

means_dep_vars$region[means_dep_vars$isocntry %in% c("AT", "BE", "DE-W", "DE-E", "DK", "FR", "IE", "LU", "NL")] <- "Western Europe"

means_dep_vars$region[means_dep_vars$isocntry %in% c("CY", "ES", "GR", "HR", "IT", "PT", "MT")] <- "Southern Europe"

means_dep_vars$region[means_dep_vars$isocntry %in% c("BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SI", "SK")] <- "Eastern Europe"

means_dep_vars$region[means_dep_vars$isocntry %in% c("FI", "SE")] <- "Northern Europe"


bar_col1 <- data.frame(isocntry = means_dep_vars$isocntry, means = means_dep_vars$dep_sum, region = means_dep_vars$region)

# Define the colors based on the unique values of means$region
colors <- c("red", "blue", "green", "orange")

# Create the barplot using ggplot2 with ordered bars
ggplot(bar_col1, aes(x = reorder(isocntry, means), y = means, fill = as.factor(region))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(x = "isocntry", y = "Mean", title = "Means across isocntry") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# barplot without colors
barplot(means_dep_vars$dep_sum, names.arg = means_dep_vars$isocntry,
        xlab = "isocntry", ylab = "Sum of dep vars",
        main = "Means of dep vars across isocntry ", las = 2)

mean(nona_variables$dep_sum)
abline(h=mean(nona_variables$dep_sum), lty=2)
nona_variables %>% group_by(qa3.3) %>% summarise(mean_dep_sum = mean(dep_sum))
nona_variables %>% group_by(qa3.15) %>% summarise(mean_dep_sum = mean(dep_sum))
sum(is.na(euro$qa8_5))
inter0 <- nona_variables %>% filter(qa3.16 == 0) %>% select(dep_sum)
inter1 <- nona_variables %>% filter(qa3.16 == 1) %>% select(dep_sum)
t.test(inter0, inter1)
boxplot()
sum(is.na(euro$qe2_1))

test <- euro %>% select(isocntry, qe2_1)
test <- test[is.na(test$qe2_1), ]
table(euro$qa3.3)
table(euro$isocntry)

# "AL" Albania
# "BA" Bosnia and Herzegovina
# "GB" Great Britain
# "ME" Montenegro
# "MK" North Macedonia
# "RS" Kosovo
# "CY-TCC" Cyprus TCC
# "TR" Turkey
# "IS" Iceland
# "CH" Switzerland
# "NO" Norway

# qe1_2 graph (satisfaction in) ----
all_response <- euro %>% select(isocntry, qa3.3, qa3.15, qa3.16, qa6a_4, qa6a_12, qa8_8, qa8_9,
                                 qe1_2)
means_response <- aggregate(qe1_2 ~ isocntry, data = all_response, FUN = mean)
means_response <- means_response[order(means_response$qe1_2), ]
barplot(means_response$qe1_2, names.arg = means_response$isocntry,
        xlab = "isocntry", ylab = "Sum of dep vars",
        main = "Means of dep vars across isocntry ", las = 2)


summary(lm(dep_sum ~ qa3.3 + qa3.15 + qa3.16, data=nona_variables))

# Army to factor
nona_variables$qa6a_4 <- factor(nona_variables$qa6a_4)
levels(nona_variables$qa6a_4) <- c("1", "0")
boxplot(dep_sum ~ qa6a_4, data=nona_variables)
range(nona_variables$qa6a_4)
summary(nona_variables$qa6a_4)
army <- aggregate(dep_sum ~ qa6a_4, data = nona_variables, FUN = mean)

# NATO to factor
nona_variables$qa6a_12 <- factor(nona_variables$qa6a_12)
levels(nona_variables$qa6a_12) <- c("1", "0")
boxplot(dep_sum ~ qa6a_12, data=nona_variables)
nato <- aggregate(dep_sum ~ qa6a_12, data = nona_variables, FUN = mean)