context("Weighting by one or more indicators")


test_that("error handling for weighting indicator names is working", {
        expect_error(weight_wwc(tinysurvey, howabouthemapples),
                     "indicators must be one or more of sex, raceethnicity, age, and education")
})

test_that("error handling for education + age is working", {
        expect_error(weight_wwc(tinysurvey, age, education),
                     "indicators cannot include both age and education")
})

test_that("error handling for force_edu + age is working", {
        expect_error(weight_wwc(tinysurvey, age, force_edu = TRUE),
                     "force_edu cannot be TRUE if age is one of the indicators")
})

test_that("can weight a survey", {
        expect_warning(resultDF <- weight_wwc(tinysurvey, 
                                              sex, raceethnicity),
                       "Some strata absent from sample: ignored")
        expect_is(resultDF, "tbl_df")
        expect_equal(length(tinysurvey) + 1, length(resultDF))
        expect_true(all(tinysurvey$sex == resultDF$sex))
        expect_is(resultDF$weight, "numeric")
})

test_that("can weight a survey with missing data", {
        resultDF <- weight_wwc(surveymissing, sex, raceethnicity)
        expect_is(resultDF, "tbl_df")
        expect_equal(length(surveymissing) + 1, length(resultDF))
        expect_equal(dim(resultDF)[1], 521)
        expect_is(resultDF$weight, "numeric")
})

test_that("can weight a survey with more than one geography", {
        resultDF <- weight_wwc(twostatessurvey, raceethnicity)
        expect_is(resultDF, "tbl_df")
        expect_equal(length(surveymissing) + 1, length(resultDF))
        expect_true(all(twostatessurvey %>% 
                                group_by(geography) %>% 
                                summarise(n = n()) ==
                                resultDF %>% 
                                group_by(geography) %>% 
                                summarise(n = n())))
        expect_is(resultDF$weight, "numeric")
})
