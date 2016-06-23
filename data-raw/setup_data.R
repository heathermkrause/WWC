# this script requires the use of devtools

# countyFIPS data set -----------------------------------------------

countyFIPS <- read.csv("data-raw/national_county.txt", header = FALSE, 
                       stringsAsFactors = FALSE)
colnames(countyFIPS) <- c("state", "stateFIPS", "countyFIPS",
                          "countyname", "classfp")
devtools::use_data(countyFIPS, overwrite = TRUE)


# texassurvey data set ----------------------------------------------

library(WWC)
texas <- geo.make(state = "TX")
prob_sex <- c(0.55, 0.45)
prob_raceethnicity <- c(0.75, 0.15, 0.03, 0.02, 0.05)
prob_age <- c(0, 0, 0, 0, 0.05, 0.09, 0.09, 0.1, 0.18, 0.16, 0.14, 0.09, 0.06, 0.04)
prob_education <- c(0.18, 0.25, 0.29, 0.28)
weight_sex <- c(0.5, 2)
weight_raceethnicity <- c(0.2, 2, 1, 1, 1)
weight_age <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
weight_education <- c(1, 1, 1, 1)
texassurvey <- simulate_survey(prob_sex, weight_sex,
                               prob_raceethnicity, weight_raceethnicity,
                               prob_age, weight_age,
                               prob_education, weight_education,
                               n = 1000)
devtools::use_data(texassurvey, overwrite = TRUE)


