context("Choosing best weighting indicators via bootstrap resampling")


test_that("error handling for weighting indicator names is working", {
        tmp <- tempfile()
        expect_error(choose_best_weighting(system.file("extdata/examplesurvey.csv", 
                                                       package = "WWC"), 
                                           tmp, 3, response, howaboutthemapples),
                     "indicators must be one or more of sex, raceethnicity, age, and education")
})

test_that("error handling for education + age is working", {
        expect_error(choose_best_weighting(system.file("extdata/examplesurvey.csv", 
                                                       package = "WWC"), 
                                           tmp, 3, response, age, education),
                     "indicators cannot include both age and education")
})



test_that("can find a set of best weighting indicators", {
        tmp <- tempfile()
        result <- choose_best_weighting(system.file("extdata/tinysurvey.csv", package = "WWC"), 
                                            tmp, 3, response, sex, raceethnicity)
        
        expect_is(result, "list")
        expect_gt(length(result), 0)
        expect_is(result[[1]], "character")
})
