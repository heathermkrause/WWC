context("Weighting by one or more indicators")

data(tinysurvey)

test_that("error handling for weighting indicator names is working", {
        expect_error(weight_wwc(tinysurvey, TX, howabouthemapples),
                     "indicators must be one or more of sex, raceethnicity, age, and education")
})

test_that("error handling for education + age is working", {
        expect_error(weight_wwc(tinysurvey, TX, age, education),
                     "indicators cannot include both age and education")
})

test_that("can weight a survey", {
        expect_warning(resultDF <- weight_wwc(tinysurvey, TX, 
                                              sex, raceethnicity),
                       "Some strata absent from sample: ignored")
        expect_is(resultDF, "tbl_df")
        expect_equal(length(tinysurvey) + 1, length(resultDF))
        expect_true(all(tinysurvey$sex == resultDF$sex))
        expect_is(resultDF$weight, "numeric")
})

test_that("can weight a survey with missing data", {
        resultDF <- weight_wwc(surveymissing, TX, sex, raceethnicity)
        expect_is(resultDF, "tbl_df")
        expect_equal(length(surveymissing) + 1, length(resultDF))
        expect_equal(dim(resultDF)[1], 620)
        expect_is(resultDF$weight, "numeric")
})
