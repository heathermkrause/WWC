context("Calculating margin of error estimates")

test_that("can find the error estimates", {
        error_list <- margin_of_error(system.file("extdata/weightedsurvey.csv", 
                                                  package = "WWC"),
                                      response)
        
        expect_is(error_list, "list")
        expect_equal(length(error_list), 4)
        expect_true(all(names(error_list) == c("raw_standard_error",
                                               "raw_bias",
                                               "raw_total_error",
                                               "weighted_standard_error")))
        expect_is(error_list[[1]], "numeric")
})
