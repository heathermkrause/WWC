context("Simulating a categorical survey")

prop_sex <- c(0.5, 0.5)
prop_raceethnicity <- c(0.55, 0.25, 0.1, 0.05, 0.05)
prop_age <- c(0, 0, 0, 0.04, 0.1, 0.1, 0.12, 0.13, 0.12, 0.11, 0.11, 0.09, 0.06, 0.02)
prop_education <- c(0.1, 0.3, 0.4, 0.2)
prop_geography <- c(0.5, 0.2, 0.3)
odds_sex <- c(0.5, 2)
odds_raceethnicity <- c(0.2, 2, 2.5, 1, 1)
odds_age <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
odds_education <- c(0.4, 0.5, 2, 2.5)
odds_geography <- c(2, 1, 0.5)
n <- 10

su <- simulate_survey(prop_sex, odds_sex,
                      prop_raceethnicity, odds_raceethnicity,
                      prop_age, odds_age,
                      prop_education, odds_education,
                      prop_geography, odds_geography,
                      n)

test_that("can simulate a categorical survey question", {
        expect_is(su, "tbl_df")
        expect_true(all(dim(su) == c(10, 6)))
        expect_true(all(names(su) == c("sex",
                                       "raceethnicity",
                                       "age",
                                       "education",
                                       "geography",
                                       "response")))
        expect_is(su$sex, "character")
        expect_is(su$response, "character")
})

context("Simulating a continuous survey")

prop_sex <- c(0.5, 0.5)
prop_raceethnicity <- c(0.55, 0.25, 0.1, 0.05, 0.05)
prop_age <- c(0, 0, 0, 0.04, 0.1, 0.1, 0.12, 0.13, 0.12, 0.11, 0.11, 0.09, 0.06, 0.02)
prop_education <- c(0.1, 0.3, 0.4, 0.2)
lambda_sex <- c(25, 75)
lambda_raceethnicity <- c(90, 10, 50, 50, 50)
lambda_age <- rep(NA, 14)
lambda_education <- rep(NA, 4)
n <- 10

su <- simulate_survey_continuous(prop_sex, lambda_sex,
                                 prop_raceethnicity, lambda_raceethnicity,
                                 prop_age, lambda_age,
                                 prop_education, lambda_education,
                                 n)

test_that("can simulate a continuous survey question", {
        expect_is(su, "tbl_df")
        expect_true(all(dim(su) == c(10, 5)))
        expect_true(all(names(su) == c("sex",
                                       "raceethnicity",
                                       "age",
                                       "education",
                                       "response")))
        expect_is(su$sex, "character")
        expect_is(su$response, "integer")
})

