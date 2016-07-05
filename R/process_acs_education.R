#' Fetch and process educational attainment and demographic data from ACS 
#' tables C15002 and C15002B/D/H/I and return a tidy data frame for a specified 
#' single geography
#' 
#' @param geographyfetch A geography created with the \code{geo.make()} function
#' of the acs package. The geography must be the entire U.S. or a single state
#' or a single county.
#' 
#' @details Uses ACS 1-year estimate for 2014
#' 
#' @return A data frame with 7 columns that tabulates the educational attainment
#' by sex for five racial/ethnic groups: black alone, white alone (not Hispanic 
#' or Latino), Hispanic or Latino, Asian alone, and other. The educational 
#' attainment binning in this data frame is the same as in ACS tables 
#' C15002B/D/H/I (the data from table C15002 is rebinned and used to find the 
#' "other" population).
#' 
#' @import dplyr
#' @importFrom stringr str_extract
#' 
#' 
#' @name process_acs_education
#' 
#' @examples 
#' 
#' 
#' \dontrun{
#' library(acs)
#' # if you are new to using the acs package, you will need to get an API key
#' # and run api.key.install() one time to install your key on your system
#' unitedstates <- geo.make(us = TRUE)
#' usDF <- process_acs_education(unitedstates)
#' 
#' texas <- geo.make(state = "TX")
#' txDF <- process_acs_education(texas)
#' 
#' cookcounty <- geo.make(state = "IL", county = 31)
#' cookDF <- process_acs_education(cookcounty)
#' }
#' 
#' @export

process_acs_education <- function(geographyfetch) {

        # get education attainment for male/female population, not broken
        # down by race/ethnicity
        totaleducationfetch <- acs::acs.fetch(geography = geographyfetch, endyear = 2014,
                                         span = 1, table.number = "C15002",
                                         col.names = "pretty")
        totaleducation <- reshape2::melt(acs::estimate(totaleducationfetch))
        totaleducation$Var2 <- str_extract(as.character(totaleducation$Var2), 
                                           "(Female:.+$|Female:|Male:.+$|Male:)")
        totaleducation$sex <- str_extract(totaleducation$Var2, "(Female|Male)")
        totaleducation$education <- str_extract(str_extract(totaleducation$Var2, ":.+$"), "\\b.+$")
        totaleducation$education[is.na(totaleducation$education)] <- "Total"
        totaleducation$education <- factor(totaleducation$education, levels = unique(totaleducation$education))
        totaleducation <- totaleducation %>% 
                select(sex, education, population = value) %>%
                filter(!is.na(sex))
        # this data frame above has sex by educational attainment for total population
        

        # get educational attainment for male/female population broken down
        # by race/ethnicity
        tablenames <- paste0("C15002", c("B","D","H", "I"))
        educationfetch <- purrr::map(tablenames, function(x) {
                acs::acs.fetch(geography = geographyfetch, endyear = 2014, span = 1,
                          table.number = x, col.names = "pretty")
        })
        
        process_fetch <- function(fetch) {
                education <- acs::estimate(fetch)
                education <- reshape2::melt(education)
                education$raceethnicity <- str_extract(str_extract(as.character(education$Var2), "\\(.+\\):{1}"),
                                                       "[a-zA-Z\\,\\s]+")
                education$Var2 <- str_extract(as.character(education$Var2), "(Female:.+$|Female:|Male:.+$|Male:)")
                education$sex <- str_extract(education$Var2, "(Female|Male)")
                education$education <- str_extract(str_extract(education$Var2, ":.+$"), "\\b.+$")
                education$education[is.na(education$education)] <- "Total"
                education$education <- factor(education$education, levels = unique(education$education))
                education <- education %>% select(sex, education, raceethnicity, population = value)
        }
        
        # this data frame has sex by educational attainment for four
        # racial/ethnic groups but NOT total or other
        education <- purrr::map_df(educationfetch, function(x) {process_fetch(x)})
        education <- education[!is.na(education$sex),] %>%
                mutate(education = as.character(education))
        
        # unfortunately, it is not the same education bins as the
        # total education data frame above
        
        totaleducation <- bind_rows(totaleducation %>% group_by(sex) %>% 
                                            filter(education == "Total") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "Total"),
                                    totaleducation %>% group_by(sex) %>% 
                                            filter(education == "Less than 9th grade" | 
                                                           education == "9th to 12th grade, no diploma") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "Less than high school diploma"),
                                    totaleducation %>% group_by(sex) %>% 
                                            filter(education == "High school graduate (includes equivalency)") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "High school graduate (includes equivalency)"),
                                    totaleducation %>% group_by(sex) %>% 
                                            filter(education == "Some college, no degree" | 
                                                           education == "Associate's degree") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "Some college or associate's degree"),
                                    totaleducation %>% group_by(sex) %>% 
                                            filter(education == "Bachelor's degree" | 
                                                           education == "Graduate or professional degree") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "Bachelor's degree or higher"))
        geototal <- totaleducation %>% filter(education == "Total") %>% 
                summarise(geototal = sum(sextotal))
        totaleducation <- totaleducation %>% mutate(geototal = geototal[[1]])
        
        # now both education and total education have the same bins
        
        # what about "other" as a racial/ethnic group?
        educationother <- education %>% group_by(sex, education) %>% 
                summarise(notother = sum(population)) %>% 
                left_join(totaleducation, by = c("sex", "education")) %>% 
                mutate(raceethnicity = "Other", 
                       population = sextotal - notother) %>% 
                select(sex, education, raceethnicity, population)
        education <- bind_rows(education, educationother)
        education <- left_join(education, totaleducation, 
                               by = c("sex", "education")) %>% 
                mutate(prob = population/geototal) %>%
                filter(education != "Total")
        education$raceethnicity <- toupper(education$raceethnicity)

        education        
}