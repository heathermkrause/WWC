context("Weighting by more than one indicator")

suppressPackageStartupMessages(library(acs))
texas <- geo.make(state = "TX")
data(texassurvey)

test_that("error handling for weighting indicator names is working", {
        expect_error(weight_wwc(texassurvey, texas, howabouthemapples),
                     "indicators must be one or more of sex, raceethnicity, age, and education")
})

test_that("error handling for education + age is working", {
        expect_error(weight_wwc(texassurvey, texas, age, education),
                     "indicators cannot include both age and education")
})