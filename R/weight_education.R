#' Weight a survey using post-stratification for weighting indicator(s) from 
#' ACS tables with educational attainment data
#' 
#' @param mysurvey A survey data frame, such as that created by 
#' \code{simulate_survey}
#' @param georegion A geographical region specified as a two letter abbreviation
#' or a 5-digit FIPS code. The geography must be the entire U.S. (\code{US}) 
#' or a single state (for example, \code{TX} or \code{CA}) or a single 
#' county (for example, \code{49035}).
#' @param georegion_ Geographical region as string.
#' @param ... Weighting indicator(s) to be used for post-stratification.
#' One or more of \code{sex}, \code{raceethnicity}, and \code{education}, which 
#' must be columns in the survey data frame.
#' @param dots List of weighting indicator(s) as string(s).
#' 
#' @return The original survey data frame with 1 column added, \code{weight},
#' the post-stratification weight for each row in the survey.
#' 
#' @details \code{weight_education} is given bare names while 
#' \code{weight_education_} is given strings and is therefore suitable for 
#' programming with.
#' 
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom stats as.formula
#' 
#' @name weight_education
#' 
#' @examples 
#' data(texassurvey)
#' weight_education(texassurvey, TX, sex, raceethnicity)
#' 
#' @export
weight_education <- function(mysurvey, georegion, ...) {
        # NSE magic
        georegion_ <- toupper(col_name(substitute(georegion)))
        dots <- eval(substitute(alist(...)))
        dots <- purrr::map(dots, col_name)
        
        weight_education_(mysurvey, georegion_, dots)
}

#' @rdname weight_education
#' @export
weight_education_ <- function(mysurvey, georegion_, dots) {
        
        # error handling for weighting indicator
        if (any(purrr::map(dots, function(x) 
        {x[[1]] %in% c("sex", "raceethnicity", "education")}) == FALSE)) {
                stop("indicator must be one of sex, raceethnicity, or education") }
        
        # download and process ACS data
        acseducationDF <- acsedutable %>% 
                filter(region == georegion_)
        
        # what are the population frequencies for post-stratification?
        popDF <- group_by_(acseducationDF, .dots = dots) %>%
                summarise(Freq = sum(nrow(mysurvey)*population/geototal)) %>%
                ungroup()
        print(popDF)
        
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