context("Weighting by age")

suppressPackageStartupMessages(library(acs))
texas <- geo.make(state = "TX")
data(texassurvey)

test_that("error handling for weighting indicator is working", {
        expect_error(weight_age(texassurvey, texas, 
                                response, howabouthemapples),
                     "indicator must be one of sex, raceethnicity, or age")
})