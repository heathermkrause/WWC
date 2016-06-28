#' Weight a survey using post-stratification for weighting indicator(s) from 
#' ACS tables with educational attainment data
#' 
#' @param mysurvey A survey data frame, such as that created by 
#' \code{simulate_survey}
#' @param geographyfetch A geography created with the \code{geo.make()} function
#' of the acs package. The geography must be the entire U.S. or a single state
#' or a single county.
#' @param ... Weighting indicator(s) to be used for post-stratification.
#' One or more of \code{sex}, \code{raceethnicity}, and \code{education}, which 
#' must be columns in the survey data frame.
#' @param dots List of weighting indicator(s) as string(s)
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
#' 
#' \dontrun{
#' library(acs)
#' # if you are new to using the acs package, you will need to get an API key
#' # and run api.key.install() one time to install your key on your system
#' texas <- geo.make(state = "TX")
#' data(texassurvey)
#' weight_education(texassurvey, texas, sex, raceethnicity)
#' }
#' 
#' @export
weight_education <- function(mysurvey, geographyfetch, ...) {
        # NSE magic
        dots <- eval(substitute(alist(...)))
        dots <- purrr::map(dots, col_name)
        
        weight_education_(mysurvey, geographyfetch, dots)
}

#' @rdname weight_education
#' @export
weight_education_ <- function(mysurvey, geographyfetch, dots) {
        
        # error handling for weighting indicator
        if (any(purrr::map(dots, function(x) 
        {x[[1]] %in% c("sex", "raceethnicity", "education")}) == FALSE)) {
                stop("indicator must be one of sex, raceethnicity, or education") }
        
        # download and process ACS data
        acseducationDF <- process_acs_education(geographyfetch)
        
        # what are the population frequencies for post-stratification?
        popDF <- group_by_(acseducationDF, .dots = dots) %>%
                summarise(Freq = sum(nrow(mysurvey)*population/geototal))
        #print(popDF)
        
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