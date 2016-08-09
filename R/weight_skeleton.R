weight_skeleton_ <- function(mysurvey, acsDF, dots) {

        # what are the population frequencies for post-stratification?
        popDF <- group_by_(acsDF, .dots = dots) %>%
                summarise(Freq = sum(nrow(mysurvey)*population/geototal))
        #print(popDF)

        # exclude rows/observations/respondents who have not answered all 
        # the demographic questions, i.e. have NAs in the weighting indicator
        # columns
        mysurvey <- mysurvey[complete.cases(mysurvey[,(colnames(mysurvey) %in% dots)]),]

        # what is the raw result on the survey question in the population?
        rawSurvey <- survey::svydesign(ids = ~0, data = mysurvey, weights = NULL)

        # now do the post-stratification
        dots <- unlist(dots)
        vars <- paste(dots, collapse="+")
        indicatorform <- as.formula(paste("~", vars))
        psSurvey <- survey::postStratify(rawSurvey, indicatorform, 
                                         population = popDF,
                                         partial = TRUE)
        psSurvey <- survey::as.svrepdesign(psSurvey)
        mysurvey %>% mutate(weight = psSurvey$pweights)
}