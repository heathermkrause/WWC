context("Summarizing a weighted survey")

suppressPackageStartupMessages(library(dplyr))
data(tinysurvey)

test_that("can summarize a survey", {
        expect_warning(sm <- weight_wwc(tinysurvey, OK, 
                                        sex, raceethnicity) %>% 
                               summarize_survey(response),
                       "Some strata absent from sample: ignored")
        expect_is(sm, "tbl_df")
        expect_equal(length(sm), 3)
        expect_true(all(sm$result == c("Raw", "Weighted")))
        expect_is(sm$value, "numeric")
        expect_true(sm$value[1] > sm$value[2])
})
