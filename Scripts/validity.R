library(haven)

euro = read_sav('Data_preproc/euro.sav')

# Independent + dependent variables
all_variables <- euro %>% select(isocntry, qa3.3, qa3.15, qa3.16, qa6a_4, qa6a_12, qa8_4, qa8_8, qa8_9,
                                 qd6.3, qd6.6, qd6.8, qd7.3, qe5_2, qa11_2, qe4_1,
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

# VALIDITY TESTS ----
validity_vars <- data.frame(qd7.3=nona_variables$qd7.3, qe5_2=nona_variables$qe5_2, qa11_2=nona_variables$qa11_2, qe4_1=nona_variables$qe4_1,
                            pers_values=pers_values$indep_sum, problems_summed=problems_summed$indep_sum, eu_indep=eu_dep$indep_sum, dep_sum=nona_variables$dep_sum)
validity_vars <- subset(validity_vars, !grepl("5", qd7.3))
validity_vars <- subset(validity_vars, !grepl("5", qe5_2))
validity_vars <- subset(validity_vars, !grepl("5", qa11_2))
validity_vars <- subset(validity_vars, !grepl("5", qe4_1))
validity_vars <- validity_vars %>%
  mutate(qe4_1 = case_when(
    qe4_1 == 1 ~ 4,
    qe4_1 == 2 ~ 3,
    qe4_1 == 3 ~ 2,
    qe4_1 == 4 ~ 1,
    TRUE ~ qe4_1
  ),
  qe5_2 = case_when(
    qe5_2 == 1 ~ 4,
    qe5_2 == 2 ~ 3,
    qe5_2 == 3 ~ 2,
    qe5_2 == 4 ~ 1,
    TRUE ~ qe5_2
  ),
  qa11_2 = case_when(
    qa11_2 == 1 ~ 4,
    qa11_2 == 2 ~ 3,
    qa11_2 == 3 ~ 2,
    qa11_2 == 4 ~ 1,
    TRUE ~ qa11_2
  ))

cor(validity_vars$dep_sum, validity_vars$qe4_1)
cor(validity_vars$problems_summed, validity_vars$qe5_2)
cor(validity_vars$eu_indep, validity_vars$qa11_2)