#' Weight a survey using post-stratification for one weighting indicator
#' 
#' @param mysurvey A survey data frame, such as that created by 
#' \code{simulate_survey}
#' @param response A column in \code{mysurvey} that contains the quantity to be
#' weighted, such as the response to a yes/no question as in 
#' \code{simulate_survey}
#' @param indicator A weighting indicator to be used for post-stratification.
#' One of \code{sex}, \code{raceethnicity}, \code{age}
#' @param geographyfetch A geography created with the \code{geo.make()} function
#' of the acs package. The geography must be the entire U.S. or a single state
#' or a single county.
#' 
#' @return A data frame with some columns that have some stuff in them.
#' 
#' @import dplyr
#' @importFrom reshape2 melt
#' 
#' @name weight_age_one
#' 
#' @export

weight_age_one <- function(mysurvey, response, indicator, geographyfetch) {

        acsageDF <- process_acs_age(geographyfetch)
        
        indicator_col <- col_name(substitute(indicator))
        popDF <- group_by_(acsageDF, indicator_col) %>%
                summarise(Freq = sum(population))
        
        mySurvey <- survey::svydesign(ids = ~0, data = mysurvey)
        rawresult <- survey::svymean(~response, mysurvey)        
        
        psSurvey <- survey::postStratify(mysurvey, ~indicator, population = popDF)
        psresult <- survey::svymean(~response, psSurvey)       
        results <- bind_rows(data_frame(answer = rownames(melt(rawresult))) %>% 
                                     mutate(value = melt(rawresult)$value) %>%
                                     mutate(result = "Raw"),
                             data_frame(answer = rownames(melt(psresult))) %>% 
                                     mutate(value = melt(psresult)$value) %>%
                                     mutate(result = "Weighted"))
        results
}