weight_skeleton_ <- function(mysurvey, acsDF, dots) {

        # what are the population frequencies for post-stratification?
        
        ###############   added by GM to trace missing education problem ------
        # try to fix dot list problem here
        
        dots <- unique(unlist(dots))
        
        ############### end of code added by GM -------------------------------
        popDF <- group_by_(acsDF, .dots = dots) %>%
                summarise(Freq = sum(nrow(mysurvey)*population/geototal))
        #print(popDF)

        # exclude rows/observations/respondents who have not answered all 
        # the demographic questions, i.e. have NAs in the weighting indicator
        # columns
        mysurvey <- mysurvey[complete.cases(mysurvey[,(colnames(mysurvey) %in% dots)]),]

        # remove bins from popDF that have zero respondents in survey to avoid
        # bias in weighting algorithm from too many empty strata
        popDF <- popDF %>% 
                left_join(group_by_(mysurvey, .dots = dots) %>%
                                  summarise(n = n())) %>%
                filter(n != 0) %>%
                select(-n)
        
        # what is the raw result on the survey question in the population?
        
        #############  added by GM to deal with single PSU problem:
        if(nrow(mysurvey) == 1) {
                mysurvey$weight <- 1
                return(mysurvey)
        }
        ############ end of code added by GM
        
        rawSurvey <- survey::svydesign(ids = ~0, data = mysurvey, weights = NULL)

        # now do the post-stratification
        dots <- unlist(dots)
        vars <- paste(dots, collapse="+")
        indicatorform <- as.formula(paste("~", vars))
        psSurvey <- survey::postStratify(rawSurvey, indicatorform, 
                                         population = popDF,
                                         partial = TRUE)
        psSurvey <- survey::as.svrepdesign(psSurvey)

        # trim weights above 3
        mysurvey %>% mutate(weight = if_else(psSurvey$pweights < 5, 
                                             psSurvey$pweights, 5))
}