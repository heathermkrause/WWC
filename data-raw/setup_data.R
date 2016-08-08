# this script requires the use of devtools

# countyFIPS data set -----------------------------------------------

countyFIPS <- read.csv("data-raw/national_county.txt", header = FALSE, 
                       stringsAsFactors = FALSE)
colnames(countyFIPS) <- c("state", "stateFIPS", "countyFIPS",
                          "countyname", "classfp")
devtools::use_data(countyFIPS, overwrite = TRUE)


# texassurvey data set ----------------------------------------------

library(WWC)
library(purrr)
prop_sex <- c(0.55, 0.45)
prop_raceethnicity <- c(0.75, 0.15, 0.03, 0.02, 0.05)
prop_age <- c(0, 0, 0, 0, 0.05, 0.09, 0.09, 0.1, 0.18, 0.16, 0.14, 0.09, 0.06, 0.04)
prop_education <- c(0.18, 0.25, 0.29, 0.28)
prop_geography <- c(1, 0, 0)
odds_sex <- c(0.5, 2)
odds_raceethnicity <- c(0.2, 2, 1, 1, 1)
odds_age <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
odds_education <- c(1, 1, 1, 1)
odds_geography <- c(1, 1, 1)
texassurvey <- simulate_survey(prop_sex, odds_sex,
                               prop_raceethnicity, odds_raceethnicity,
                               prop_age, odds_age,
                               prop_education, odds_education,
                               prop_geography, odds_geography,
                               n = 1000)
devtools::use_data(texassurvey, overwrite = TRUE)
surveymissing <- map_df(texassurvey, 
                        function(x) {x[sample(c(TRUE, NA), 
                                              prob = c(0.8, 0.2), 
                                              size = length(x), 
                                              replace = TRUE)]})
#sapply(surveymissing, function(y) sum(length(which(is.na(y)))))
devtools::use_data(surveymissing, overwrite = TRUE)

# tinysurvey data set -----------------------------------------------

library(WWC)
prop_sex <- c(0.5, 0.5)
prop_raceethnicity <- c(0.2, 0.2, 0.2, 0.2, 0.2)
prop_age <- c(0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
prop_education <- c(0.25, 0.25, 0.25, 0.25)
prop_geography <- c(0, 0, 1)
lambda_sex <- c(10, 20)
lambda_raceethnicity <- c(50, 10, 10, 10, 10)
lambda_age <- rep(NA, 14)
lambda_education <- rep(NA, 4)
lambda_geography <- rep(NA, 3)
tinysurvey <- simulate_survey_continuous(prop_sex, lambda_sex,
                                         prop_raceethnicity, lambda_raceethnicity,
                                         prop_age, lambda_age,
                                         prop_education, lambda_education,
                                         prop_geography, lambda_geography,
                                         n = 20)
devtools::use_data(tinysurvey, overwrite = TRUE)

# twostatessurvey data set -------------------------------------------

library(WWC)
prop_sex <- c(0.6, 0.4)
prop_raceethnicity <- c(0.7, 0.2, 0.03, 0.02, 0.05)
prop_age <- c(0, 0, 0, 0, 0.05, 0.09, 0.09, 0.1, 0.18, 0.16, 0.14, 0.09, 0.06, 0.04)
prop_education <- c(0.15, 0.25, 0.3, 0.3)
prop_geography <- c(0.6, 0.4, 0)
lambda_sex <- c(20, 70)
lambda_raceethnicity <- c(50, 10, 10, 10, 10)
lambda_age <- rep(NA, 14)
lambda_education <- rep(NA, 4)
lambda_geography <- rep(NA, 3)
twostatessurvey <- simulate_survey_continuous(prop_sex, lambda_sex,
                                              prop_raceethnicity, lambda_raceethnicity,
                                              prop_age, lambda_age,
                                              prop_education, lambda_education,
                                              prop_geography, lambda_geography,
                                              n = 1000)
devtools::use_data(twostatessurvey, overwrite = TRUE)


