#' Weight a survey using post-stratification for weighting indicator(s) from 
#' ACS tables with age by sex data
#' 
#' @param mysurvey A survey data frame, such as that created by 
#' \code{simulate_survey}
#' @param ... Weighting indicator(s) to be used for post-stratification.
#' One or more of \code{sex}, \code{raceethnicity}, and \code{age}, which must 
#' be columns in the survey data frame.
#' @param dots List of weighting indicator(s) as string(s)
#' 
#' @return The original survey data frame with 1 column added, \code{weight},
#' the post-stratification weight for each row in the survey.
#' 
#' @details \code{weight_age} is given bare names while \code{weight_age_} is 
#' given strings and is therefore suitable for programming with. One column of
#' \code{mysurvey} must be \code{geography}, to indicate what ACS data to use
#' for post-stratification weighting.
#' 
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom stats as.formula
#' 
#' @name weight_age
#' 
#' @examples 
#' data(texassurvey)
#' weight_age(texassurvey, sex, raceethnicity)
#' 
#' @export
weight_age <- function(mysurvey, ...) {
        # NSE magic
        dots <- eval(substitute(alist(...)))
        dots <- purrr::map(dots, col_name)
        
        weight_age_(mysurvey, dots)
}

#' @rdname weight_age
#' @export
weight_age_ <- function(mysurvey, dots) {
        
        # error handling for weighting indicator
        if (any(purrr::map(dots, function(x) 
                {x[[1]] %in% c("sex", "raceethnicity", "age")}) == FALSE)) {
                stop("indicator must be one of sex, raceethnicity, or age") }

        # exclude rows/observations/respondents who have NA for geography
        mysurvey <- mysurvey[!is.na(mysurvey$geography),]
        
        # what is the population of the survey by geography?
        surveytotals <- mysurvey %>% group_by(geography) %>% 
                summarise(surveytotal = n())
        totalsurvey <- nrow(mysurvey)
                
        # download and process ACS data
        geovector <- mysurvey %>% distinct(geography)
        acsDF <- acsagetable %>% filter(region %in% geovector$geography)
        totalpop <- as.numeric(acsDF %>% distinct(geototal) %>% 
                                       summarise(sum = sum(geototal)))

        # find the relative weights for each geography in the survey
        totals <- acsDF %>% group_by(region) %>% distinct(geototal) %>% 
                ungroup %>% 
                left_join(surveytotals, by = c("region" = "geography")) %>%
                mutate(geototal = geototal/totalpop,
                       surveytotal = surveytotal/totalsurvey,
                       geoweight = geototal/surveytotal)
        
        # separate the survey into geographical groups, then find weights
        mysurvey <- mysurvey %>% group_by(geography) %>% tidyr::nest()
        acsDF <- acsDF %>% group_by(region) %>% tidyr::nest()
        nestedDF <- left_join(mysurvey, acsDF, 
                           by = c("geography" = "region")) %>% 
                rename(mysurvey = data.x, acsDF = data.y)

        ret <- purrr::map2_df(nestedDF$mysurvey, nestedDF$acsDF, weight_skeleton_, dots, 
                       .id = "geography") %>%
                mutate(geography = as.character(factor(geography, 
                                                       labels = nestedDF$geography))) %>%
                left_join(totals, by = c("geography" = "region")) %>% 
                mutate(weight = weight * geoweight) %>% # weight each respondent by geography
                select(-geototal, -surveytotal, -geoweight)
        ret
}