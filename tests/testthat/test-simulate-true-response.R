context("Simulating the true response for sex by age data")

suppressPackageStartupMessages(library(acs))
texas <- geo.make(state = "TX")
data(texassurvey)

test_that("error handling for sex odds is working", {
        expect_error(simulate_true_response_age(texas, 
                                                rep(1, 3), 
                                                rep(1, 5), 
                                                rep(1, 14)),
                     "weight_sex must be a vector of length 2")
})

test_that("error handling for race/ethnicity odds is working", {
        expect_error(simulate_true_response_age(texas, 
                                                rep(1, 2), 
                                                rep(1, 7), 
                                                rep(1, 14)),
                     "weight_raceethnicity must be a vector of length 5")
})

test_that("error handling for age odds is working", {
        expect_error(simulate_true_response_age(texas, 
                                                rep(1, 2), 
                                                rep(1, 5), 
                                                rep(1, 10)),
                     "weight_age must be a vector of length 14")
})

context("Simulating the true response for educational attainment data")

test_that("error handling for sex odds is working", {
        expect_error(simulate_true_response_education(texas, 
                                                      rep(1, 3), 
                                                      rep(1, 5), 
                                                      rep(1, 4)),
                     "weight_sex must be a vector of length 2")
})

test_that("error handling for race/ethnicity odds is working", {
        expect_error(simulate_true_response_education(texas,
                                                      rep(1, 2),
                                                      rep(1, 7),
                                                      rep(1, 4)),
                     "weight_raceethnicity must be a vector of length 5")
})

test_that("error handling for education odds is working", {
        expect_error(simulate_true_response_education(texas, 
                                                      rep(1, 2),
                                                      rep(1, 5),
                                                      rep(1, 10)),
                     "weight_education must be a vector of length 4")
})