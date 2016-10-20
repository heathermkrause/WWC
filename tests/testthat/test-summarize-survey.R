context("Summarizing a weighted survey")

suppressPackageStartupMessages(library(dplyr))
data(tinysurvey)

test_that("can summarize a survey", {
        sm <- weight_wwc(tinysurvey, 
                         sex, raceethnicity) %>% 
                summarize_survey(response)
        expect_is(sm, "tbl_df")
        expect_equal(length(sm), 4)
        expect_true(all(sm$result == c("Raw", "Weighted")))
        expect_is(sm$mean, "numeric")
        expect_is(sm$se, "numeric")
        expect_true(sm$mean[1] < sm$mean[2])
})
