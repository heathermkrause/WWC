context("Simulating the true response in categorical survey for sex by age data")

geovector <- c("NY", "NC")
odds_geography <- c(2, 0.2)
odds_sex <- c(0.5, 2)
odds_raceethnicity <- c(0.2, 2, 2.5, 1, 1)
odds_age <- c(1, 0.8, 2, 2.5, 3)
resultDF <- simulate_true_response_age(geovector,
                                     odds_geography, 
                                     odds_sex, 
                                     odds_raceethnicity,
                                     odds_age)

test_that("error handling for geography odds is working", {
        expect_error(simulate_true_response_age(geovector, 
                                                rep(1, 4), 
                                                odds_sex, 
                                                odds_raceethnicity,
                                                odds_age),
                     "geovector and odds_geography must have the same length")
})

test_that("error handling for sex odds is working", {
        expect_error(simulate_true_response_age(geovector, 
                                                odds_geography, 
                                                rep(1, 3), 
                                                odds_raceethnicity,
                                                odds_age),
                     "odds_sex must be a vector of length 2")
})

test_that("error handling for race/ethnicity odds is working", {
        expect_error(simulate_true_response_age(geovector, 
                                                odds_geography, 
                                                odds_sex, 
                                                rep(1, 3),
                                                odds_age),
                     "odds_raceethnicity must be a vector of length 5")
})

test_that("error handling for age odds is working", {
        expect_error(simulate_true_response_age(geovector, 
                                                odds_geography, 
                                                odds_sex, 
                                                odds_raceethnicity,
                                                rep(1, 10)),
                     "odds_age must be a vector of length 5")
})

test_that("can simulate the true response", {
        expect_is(resultDF, "data.frame")
        expect_equal(length(resultDF), 3)
        expect_true(all(colnames(resultDF) == c("value", "answer", "result")))
        expect_is(resultDF$value, "numeric")
})


context("Simulating the true response in categorical survey for educational attainment data")

geovector <- c("NY", "NC")
odds_geography <- c(2, 0.2)
odds_sex <- c(0.5, 2)
odds_raceethnicity <- c(0.2, 2, 2.5, 1, 1)
odds_education <- c(0.4, 0.5, 2, 2.5)
resultDF <- simulate_true_response_education(geovector,
                                             odds_geography, 
                                             odds_sex, 
                                             odds_raceethnicity,
                                             odds_education)

test_that("error handling for geography odds is working", {
        expect_error(simulate_true_response_education(geovector, 
                                                      rep(1, 4), 
                                                      odds_sex, 
                                                      odds_raceethnicity,
                                                      odds_education),
                     "geovector and odds_geography must have the same length")
})

test_that("error handling for sex odds is working", {
        expect_error(simulate_true_response_education(geovector, 
                                                      odds_geography, 
                                                      rep(1, 3), 
                                                      odds_raceethnicity,
                                                      odds_education),
                     "odds_sex must be a vector of length 2")
})

test_that("error handling for race/ethnicity odds is working", {
        expect_error(simulate_true_response_education(geovector, 
                                                      odds_geography, 
                                                      odds_sex, 
                                                      rep(1, 4),
                                                      odds_education),
                     "odds_raceethnicity must be a vector of length 5")
})

test_that("error handling for education odds is working", {
        expect_error(simulate_true_response_education(geovector, 
                                                      odds_geography, 
                                                      odds_sex, 
                                                      odds_raceethnicity,
                                                      rep(1, 5)),
                     "odds_education must be a vector of length 4")
})

test_that("can simulate the true response", {
        expect_is(resultDF, "data.frame")
        expect_equal(length(resultDF), 3)
        expect_true(all(colnames(resultDF) == c("value", "answer", "result")))
        expect_is(resultDF$value, "numeric")
})
