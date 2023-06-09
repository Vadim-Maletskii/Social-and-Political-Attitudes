library(readr)
library(haven)
library(dplyr)
library(psych)
library(ggplot2)

# Data cleaning ---- 
euro2018<- read_sav('Data_preproc/euro2018.sav') #Eurobarometer 90.2 (2018)

euro = read_sav('Data_preproc/euro.sav')

euro2018 <- subset(euro2018, !grepl("5", qc5_6))


# Independent + dependent variables
all_variables <- euro %>% select(isocntry, qa3.3, qa3.15, qa3.16, qa6a_4, qa6a_12, qa8_4, qa8_8, qa8_9,
                                 qd6.3, qd6.6, qd6.8,
                                 qe2_1, qe2_2, qe2_3, qe2_4, qe2_5, qe2_6)
nona_variables <- na.omit(all_variables)
nona_variables <- filter(nona_variables, !(qa6a_4 %in% c(3,9)), qa6a_12 != 3, !(qa8_4 %in% c(5,9)),
                         qa8_8 != 5, qa8_9 != 5)
nona_variables <- subset(nona_variables, !grepl("5", qe2_1))
nona_variables <- subset(nona_variables, !grepl("5", qe2_2))
nona_variables <- subset(nona_variables, !grepl("5", qe2_3))
nona_variables <- subset(nona_variables, !grepl("5", qe2_4))
nona_variables <- subset(nona_variables, !grepl("5", qe2_5))
nona_variables <- subset(nona_variables, !grepl("5", qe2_6))

nona_variables <- nona_variables %>%
  mutate(qe2_1 = case_when(
    qe2_1 == 1 ~ 4,
    qe2_1 == 2 ~ 3,
    qe2_1 == 3 ~ 2,
    qe2_1 == 4 ~ 1,
    TRUE ~ qe2_1
  ),
  qe2_2 = case_when(
    qe2_2 == 1 ~ 4,
    qe2_2 == 2 ~ 3,
    qe2_2 == 3 ~ 2,
    qe2_2 == 4 ~ 1,
    TRUE ~ qe2_2
  ),
  qe2_3 = case_when(
    qe2_3 == 1 ~ 4,
    qe2_3 == 2 ~ 3,
    qe2_3 == 3 ~ 2,
    qe2_3 == 4 ~ 1,
    TRUE ~ qe2_3
  ),
  qe2_4 = case_when(
    qe2_4 == 1 ~ 4,
    qe2_4 == 2 ~ 3,
    qe2_4 == 3 ~ 2,
    qe2_4 == 4 ~ 1,
    TRUE ~ qe2_4
  ),
  qe2_5 = case_when(
    qe2_5 == 1 ~ 4,
    qe2_5 == 2 ~ 3,
    qe2_5 == 3 ~ 2,
    qe2_5 == 4 ~ 1,
    TRUE ~ qe2_5
  ),
  qe2_6 = case_when(
    qe2_6 == 1 ~ 4,
    qe2_6 == 2 ~ 3,
    qe2_6 == 3 ~ 2,
    qe2_6 == 4 ~ 1,
    TRUE ~ qe2_6
  ),
  qa8_4 = case_when(
    qa8_4 == 1 ~ 4,
    qa8_4 == 2 ~ 3,
    qa8_4 == 3 ~ 2,
    qa8_4 == 4 ~ 1,
    TRUE ~ qa8_4
  ),
  qa8_8 = case_when(
    qa8_8 == 1 ~ 4,
    qa8_8 == 2 ~ 3,
    qa8_8 == 3 ~ 2,
    qa8_8 == 4 ~ 1,
    TRUE ~ qa8_8
  ),
  qa8_9 = case_when(
    qa8_9 == 1 ~ 4,
    qa8_9 == 2 ~ 3,
    qa8_9 == 3 ~ 2,
    qa8_9 == 4 ~ 1,
    TRUE ~ qa8_9
  ))
table(nona_variables$qe2_1)
table(nona_variables$qe2_2)
table(nona_variables$qe2_3)
table(nona_variables$qe2_4)
table(nona_variables$qe2_5)
table(nona_variables$qe2_6)
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
colors <- c("orange", "green", "red", "blue")

# Define colors with transparency
alpha <- 0.5  # Adjust the transparency value as needed
colors_transparent <- adjustcolor(colors, alpha = alpha)

# Create ordered boxplot with colors
mean_values_boxplot <- aggregate(qc5_6 ~ region, data = means, FUN = mean)
ordered_regions <- mean_values_boxplot$region[order(mean_values_boxplot$qc5_6)]
means$region <- factor(means$region, levels = ordered_regions)
boxplot(qc5_6 ~ region, data = means, col = colors_transparent,
        xlab = "Region", ylab = "View on Russia",
        main = "Boxplot of view on Russia by Region",  cex.lab = 1.5)



# Barplot colored ---- 

# Create a data frame with the means data
bar_col <- data.frame(isocntry = means$isocntry, qc5_6 = means$qc5_6, region = means$region)

# Define the colors based on the unique values of means$region
colors <- c("orange", "green", "red", "blue")

# Define colors with transparency
alpha <- 0.5  # Adjust the transparency value as needed
colors_transparent <- adjustcolor(colors, alpha = alpha)

# Create the barplot using ggplot2 with ordered bars
ggplot(bar_col, aes(x = reorder(isocntry, qc5_6), y = qc5_6, fill = as.factor(region))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_transparent) +
  labs(x = "Country", y = "Mean score", title = "View on Russia across countries (the less the value, the more positive the view)", fill = 'Region') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),       
        axis.text.y = element_text(size = 16, hjust = 0),                               
        plot.title = element_blank(),
        legend.text = element_text(size = 18),                              
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 8)),
        axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 10)),
        legend.key.height = unit(2, "line"))+
  scale_y_continuous(expand = expansion(add = c(0, 0.05)))
  
  


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

eigenvalues <- eigen(dep_cor)$values

# Create a data frame for the scree plot
scree_data <- data.frame(Components = 1:length(eigenvalues), Eigenvalues = eigenvalues)

# Plot the scree plot
ggplot(scree_data, aes(x = Components, y = Eigenvalues)) +
  geom_point(size = 3, color = "blue") +
  geom_line(color = "blue") +
  scale_x_continuous(breaks = seq(1, length(eigenvalues), by = 1)) +
  scale_y_continuous(limits = c(0, max(eigenvalues) * 1.1)) +
  labs(x = "Components", y = "Eigenvalues", title = "Scree plot of a dependent variable") +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 10)))
        
# SUMMING THE DEPENDENT ----
nona_variables$dep_sum <- nona_variables$qe2_1 + nona_variables$qe2_2 + nona_variables$qe2_3 +
  nona_variables$qe2_4 + nona_variables$qe2_5 + nona_variables$qe2_6
nona_variables$dep_sum <- nona_variables$dep_sum - 6
range(nona_variables$dep_sum)
hist(nona_variables$dep_sum)
plot(density(nona_variables$dep_sum))

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
colors <- c("orange", "blue", "green", "red")

alpha <- 0.5  
colors_transparent <- adjustcolor(colors, alpha = alpha)

# Create the barplot using ggplot2 with ordered bars
ggplot(bar_col1, aes(x = reorder(isocntry, means), y = means, fill = as.factor(region))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_transparent) +
  labs(x = "Country", y = "Mean score", title = "Attitude to the EU's response (summed) across countries", fill='Region') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),       
        axis.text.y = element_text(size = 16, hjust = 0),                               
        plot.title = element_blank(),
        legend.text = element_text(size = 18),                              
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 10)),
        legend.key.height = unit(2, "line"))+
  scale_y_continuous(expand = expansion(add = c(0, 0.05)))

# barplot without colors
barplot(means_dep_vars$dep_sum, names.arg = means_dep_vars$isocntry,
        xlab = "isocntry", ylab = "Sum of dep vars",
        main = "Means of dep vars across isocntry ", las = 2)

mean(nona_variables$dep_sum)
abline(h=mean(nona_variables$dep_sum), lty=2)


# Main problems: t-tests & boxplot ----
nona_variables %>% group_by(qa3.3) %>% summarise(mean_dep_sum = mean(dep_sum))
nona_variables %>% group_by(qa3.15) %>% summarise(mean_dep_sum = mean(dep_sum))
prices0 <- nona_variables %>% filter(qa3.3 == 0) %>% select(dep_sum)
prices1 <- nona_variables %>% filter(qa3.3 == 1) %>% select(dep_sum)
energy0 <- nona_variables %>% filter(qa3.15 == 0) %>% select(dep_sum)
energy1 <- nona_variables %>% filter(qa3.15 == 1) %>% select(dep_sum)
inter0 <- nona_variables %>% filter(qa3.16 == 0) %>% select(dep_sum)
inter1 <- nona_variables %>% filter(qa3.16 == 1) %>% select(dep_sum)
t.test(prices0, prices1)
t.test(inter0, inter1)
t.test(energy0, energy1)
t.test(energy1, inter1)
t.test(prices1, energy1)

# Boxplot: main problems
problems_summed <- nona_variables %>% select(qa3.15, qa3.16, dep_sum)
problems_summed$indep_sum <- problems_summed$qa3.15 + problems_summed$qa3.16
problems_summed <- problems_summed %>% select(indep_sum, dep_sum)
problems_summed %>% 
  ggplot() +
  geom_boxplot(aes(x = indep_sum, y = dep_sum, group = indep_sum), 
               fill = adjustcolor("blue"),  
               color = "black",                           
               alpha = 0.3) +                             
  theme_bw() +                                          
  labs(x = "Number of national problems noted", y = "Attitude to the EU's response (summed)") +                
  ggtitle("Relationship between the number of national problems noted\nand the attitude to the EU's response (summed)")+
  theme_bw() +
  theme(axis.text.x = element_text (size = 14),       
        axis.text.y = element_text(size = 14),                               
        plot.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 10)),
        legend.key.height = unit(2, "line"))

# Boxplots: main problems OLD
# problems_inter <- inter1$dep_sum
# problems_energy <- energy1$dep_sum
# problems_prices <- prices1$dep_sum
# combined_problems <- list(problems_inter = problems_inter, problems_energy = problems_energy, problems_prices = problems_prices)
# names(combined_problems) <- c('International situation', 'Energy supply', 'Rising prices')
# boxplot(combined_problems, col = "skyblue", main = "Relationship between the person's main national problem\nand their attitude to EU's response to the invasion",
#         ylab='Mean of combined dependent variables', xlab='Main national problem (according to the respondent)')

# Personal values ----
pers_values <- nona_variables %>% select(qd6.3, qd6.6, qd6.8, dep_sum)
pers_values$indep_sum <- pers_values$qd6.3 + pers_values$qd6.6 + pers_values$qd6.8
pers_values <- pers_values %>% select(indep_sum, dep_sum)
pers_values %>% group_by(indep_sum) %>% summarise(mean = mean(dep_sum))
pers_values %>% ggplot() + geom_boxplot(aes(x= indep_sum, y=dep_sum, group=indep_sum))

pers_values %>% 
  ggplot() +
  geom_boxplot(aes(x = indep_sum, y = dep_sum, group = indep_sum),
               fill = adjustcolor("blue"),
               color = "black",
               alpha = 0.3) +
  theme_bw() +
  labs(x = "Number of personal values related to care about human life noted", y = "Attitude to the EU's response (summed)") +
  ggtitle("Relationship between personal values related to care about human life noted\nand the attitude to the EU's response (summed)")+
  theme_bw() +
  theme(axis.text.x = element_text (size = 14),       
        axis.text.y = element_text(size = 14),                               
        plot.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 10)),
        legend.key.height = unit(2, "line"))
# EU variables and dep_sum ----
eu_dep <- nona_variables %>% select(qa8_4, qa8_8, qa8_9, dep_sum)
eu_dep$indep_sum <- eu_dep$qa8_4 + eu_dep$qa8_8 + eu_dep$qa8_9
eu_dep$indep_sum <- eu_dep$indep_sum - 3

# Regression: sum of dependent ~ sum of independent
summary(lm(eu_dep$dep_sum ~ eu_dep$indep_sum))

# Boxplot DRAFT
eu_dep_box <- eu_dep %>% select(indep_sum, dep_sum) %>% filter(indep_sum %in% c(0,1,2, 3,4,5, 6,7,8, 9))
eu_dep_box %>% 
  ggplot() +
  geom_boxplot(aes(x = indep_sum, y = dep_sum, group = indep_sum),
               fill = adjustcolor("blue"),
               color = "black",
               alpha = 0.3) +
  theme_bw() +
  labs(x = "Attitude to the EU", y = "Attitude to the EU's response (summed)") +
  ggtitle("Relationship between one's attitude to the EU\nand their attitude to the EU's response (summed)")+
  theme_bw() +
  theme(axis.text.x = element_text (size = 14),       
        axis.text.y = element_text(size = 14),                               
        plot.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 10)),
        legend.key.height = unit(2, "line"))

table(eu_dep$indep_sum)
eu_dep_box %>% group_by(indep_sum) %>% summarise(mean = mean(dep_sum))
table(nona_variables$dep_sum)


lm_data <- data.frame(pers_values=pers_values$indep_sum, problems=problems_summed$indep_sum, eu=eu_dep$indep_sum,
                      dep=eu_dep$dep_sum)
lm_all <- lm(dep ~ pers_values + problems + eu, data=lm_data)
summary(lm_all)

# qe1_2 graph (satisfaction in) ----
all_response <- euro %>% select(isocntry, qa3.3, qa3.15, qa3.16, qa6a_4, qa6a_12, qa8_8, qa8_9,
                                 qe1_2)
means_response <- aggregate(qe1_2 ~ isocntry, data = all_response, FUN = mean)
means_response <- means_response[order(means_response$qe1_2), ]
barplot(means_response$qe1_2, names.arg = means_response$isocntry,
        xlab = "isocntry", ylab = "Sum of dep vars",
        main = "Means of dep vars across isocntry ", las = 2)
new_data <- data.frame(pers_values = 3, problems = 2, eu = 9)
predict(lm_all, newdata = new_data)
range(eu_dep$indep_sum)
range(nona_variables$dep_sum)
table(nona_variables$dep_sum)
# NOT NEEDED ----
# Army and NATO
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
# Countries with NAs ----
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

