context("Simulating the true response in continuous survey for sex by age data")

geovector <- c("TX", "TN")
lambda_geography <- c(20, 10)
lambda_sex <- c(40, 20)
lambda_raceethnicity <- c(90, 10, 50, 50, 50)
lambda_age <- c(50, 50, 50, 50, 50, 50, 50, 55, 55, 60, 65, 70, 75, 80)
resultDF <- simulate_response_age_continuous(geovector,
                                             lambda_geography, 
                                             lambda_sex, 
                                             lambda_raceethnicity,
                                             lambda_age)

test_that("error handling for geography odds is working", {
        expect_error(simulate_response_age_continuous(geovector, 
                                                      rep(1, 4), 
                                                      lambda_sex, 
                                                      lambda_raceethnicity,
                                                      lambda_age),
                     "geovector and lambda_geography must have the same length")
})

test_that("error handling for sex odds is working", {
        expect_error(simulate_response_age_continuous(geovector, 
                                                      lambda_geography, 
                                                      rep(1, 3), 
                                                      lambda_raceethnicity,
                                                      lambda_age),
                     "lambda_sex must be a vector of length 2")
})

test_that("error handling for race/ethnicity odds is working", {
        expect_error(simulate_response_age_continuous(geovector, 
                                                      lambda_geography, 
                                                      lambda_sex, 
                                                      rep(1, 4),
                                                      lambda_age),
                     "lambda_raceethnicity must be a vector of length 5")
})

test_that("error handling for age odds is working", {
        expect_error(simulate_response_age_continuous(geovector, 
                                                      lambda_geography, 
                                                      lambda_sex, 
                                                      lambda_raceethnicity,
                                                      rep(1, 10)),
                     "lambda_age must be a vector of length 14")
})

test_that("can simulate the true response", {
        expect_is(resultDF, "data.frame")
        expect_equal(length(resultDF), 3)
        expect_true(all(colnames(resultDF) == c("value", "answer", "result")))
        expect_is(resultDF$value, "numeric")
})

