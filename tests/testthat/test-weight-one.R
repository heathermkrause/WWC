context("Weighting using age ACS tables")

data(tinysurvey)

test_that("error handling for weighting indicator is working", {
        expect_error(weight_age(tinysurvey, UT, howabouthemapples),
                     "indicator must be one of sex, raceethnicity, or age")
})

test_that("can weight a survey using age ACS tables", {
        expect_warning(resultDF <- weight_age(tinysurvey, UT, 
                                              sex, raceethnicity),
                       "Some strata absent from sample: ignored")
        expect_is(resultDF, "tbl_df")
        expect_equal(length(tinysurvey) + 1, length(resultDF))
        expect_true(all(tinysurvey$sex == resultDF$sex))
        expect_is(resultDF$weight, "numeric")
})


context("Weighting using education ACS tables")

data(tinysurvey)

test_that("error handling for weighting indicator is working", {
        expect_error(weight_education(tinysurvey, UT, howabouthemapples),
                     "indicator must be one of sex, raceethnicity, or education")
})

test_that("can weight a survey using education ACS tables", {
        expect_warning(resultDF <- weight_education(tinysurvey, UT, 
                                                    sex, raceethnicity),
                       "Some strata absent from sample: ignored")
        expect_is(resultDF, "tbl_df")
        expect_equal(length(tinysurvey) + 1, length(resultDF))
        expect_true(all(tinysurvey$sex == resultDF$sex))
        expect_is(resultDF$weight, "numeric")
})
