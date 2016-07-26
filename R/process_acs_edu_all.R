#' Fetch and process educational attainment and demographic data from ACS 
#' tables B15002 and C15002B/D/H/I and return a tidy data frame for a specified
#' geography with FIPS codes
#' 
#' @param geographyfetch A geography created with the \code{geo.make()} function
#' of the acs package, such as all counties in the United States
#' @param yearspan The span in years of the desired ACS data (should be 1 or
#' 5). Default is 1. Not all county data is available as one-year estimates.
#' 
#' @details Uses ACS 1-year estimate for 2014 by default; uses ACS 5-year 
#' estimate for 2010-2014 with \code{yearspan = 5}. (Not all rural counties
#' have 1-year estimates.)
#' 
#' @return A data frame with 8 columns that tabulates the sex by age population
#' for five racial/ethnic groups: black alone, white alone (not Hispanic or 
#' Latino), Hispanic or Latino, Asian alone, and other. The educational 
#' attainment binning in this data frame is the same as in ACS tables 
#' C15002B/D/H/I (the data from table B15002 is rebinned and used to find the 
#' "other" population).
#' 
#' @import dplyr
#' @importFrom stringr str_extract
#' @importFrom stringr str_c
#' @importFrom stringr str_sub
#' 
#' @name process_acs_edu_all
#' 
#' @examples 
#' \dontrun{
#' library(acs)
#' # if you are new to using the acs package, you will need to get an API key
#' # and run api.key.install() one time to install your key on your system
#' allcounties <- geo.make(state = "*", county = "*")
#' countiesDF <- process_acs_edu_all(allcounties, yearspan = 5)
#' }
#' 
#' @export

process_acs_edu_all <- function(geographyfetch, yearspan = 1) {
        
        # get education attainment for male/female population, not broken
        # down by race/ethnicity
        totaleducationfetch <- acs::acs.fetch(geography = geographyfetch, 
                                              endyear = 2014,
                                              span = yearspan, 
                                              table.number = "B15002",
                                              col.names = "pretty")
        myfips <- acs::geography(totaleducationfetch) %>%  
                mutate(FIPS = str_c(str_sub(str_c("000", state),-2),
                                    str_sub(str_c("000", county),-3))) %>%
                select(FIPS)
        acs::geography(totaleducationfetch)=cbind(myfips, 
                                                  acs::geography(totaleducationfetch))
        totaleducation <- reshape2::melt(acs::estimate(totaleducationfetch))
        totaleducation$Var2 <- str_extract(as.character(totaleducation$Var2), 
                                           "(Female:.+$|Female:|Male:.+$|Male:)")
        totaleducation$sex <- str_extract(totaleducation$Var2, "(Female|Male)")
        totaleducation$education <- str_extract(str_extract(totaleducation$Var2, ":.+$"), "\\b.+$")
        totaleducation$education[is.na(totaleducation$education)] <- "Total"
        totaleducation$education <- factor(totaleducation$education, levels = unique(totaleducation$education))
        totaleducation$region <- str_sub(str_c("00", totaleducation$Var1), -5)
        totaleducation <- totaleducation %>% 
                select(sex, education, population = value, region) %>%
                filter(!is.na(sex))
        # this data frame above has sex by educational attainment for total population
        
        
        # get educational attainment for male/female population broken down
        # by race/ethnicity
        tablenames <- paste0("C15002", c("B","D","H", "I"))
        educationfetch <- purrr::map(tablenames, function(x) {
                acs::acs.fetch(geography = geographyfetch, 
                               endyear = 2014, 
                               span = yearspan,
                               table.number = x, 
                               col.names = "pretty")
        })
        
        process_fetch <- function(fetch) {
                myfips <- acs::geography(fetch) %>%  
                        mutate(FIPS = str_c(str_sub(str_c("000", state),-2),
                                            str_sub(str_c("000", county),-3))) %>%
                        select(FIPS)
                acs::geography(fetch)=cbind(myfips, acs::geography(fetch))
                education <- acs::estimate(fetch)
                education <- reshape2::melt(education)
                education$raceethnicity <- str_extract(str_extract(as.character(education$Var2), 
                                                                   "\\(.+\\):{1}"),
                                                       "[a-zA-Z\\,\\s]+")
                education$Var2 <- str_extract(as.character(education$Var2), 
                                              "(Female:.+$|Female:|Male:.+$|Male:)")
                education$sex <- str_extract(education$Var2, "(Female|Male)")
                education$education <- str_extract(str_extract(education$Var2, ":.+$"), "\\b.+$")
                education$education[is.na(education$education)] <- "Total"
                education$region <- str_sub(str_c("00", education$Var1), -5)
                education$education <- factor(education$education, levels = unique(education$education))
                education <- education %>% 
                        select(sex, education, raceethnicity, 
                               population = value, region)
        }
        
        # this data frame has sex by educational attainment for four
        # racial/ethnic groups but NOT total or other
        education <- purrr::map_df(educationfetch, function(x) {process_fetch(x)})
        education <- education[!is.na(education$sex),] %>%
                mutate(education = as.character(education))
        
        # unfortunately, it is not the same education bins as the
        # total education data frame above
        
        totaleducation <- bind_rows(totaleducation %>% group_by(sex, region) %>% 
                                            filter(education == "Total") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "Total"),
                                    totaleducation %>% group_by(sex, region) %>% 
                                            filter(education == "No schooling completed" | 
                                                           education == "Nursery to 4th grade" |                                                           education == "11th grade" |
                                                           education == "5th and 6th grade" |                                                           education == "11th grade" |
                                                           education == "7th and 8th grade" |                                                           education == "11th grade" |
                                                           education == "9th grade" |                                                           education == "11th grade" |
                                                           education == "10th grade" |                                                           education == "11th grade" |
                                                           education == "11th grade" |                                                           education == "11th grade" |
                                                           education == "12th grade, no diploma") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "Less than high school diploma"),
                                    totaleducation %>% group_by(sex, region) %>% 
                                            filter(education == "High school graduate (includes equivalency)") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "High school graduate (includes equivalency)"),
                                    totaleducation %>% group_by(sex, region) %>% 
                                            filter(education == "Some college, less than 1 year" | 
                                                           education == "Some college, 1 or more years, no degree" |                                                           education == "11th grade" |
                                                           education == "Associate's degree") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "Some college or associate's degree"),
                                    totaleducation %>% group_by(sex, region) %>% 
                                            filter(education == "Bachelor's degree" | 
                                                           education == "Master's degree" |                                                           education == "11th grade" |
                                                           education == "Professional school degree" |                                                           education == "11th grade" |
                                                           education == "Doctorate degree") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "Bachelor's degree or higher"))

        geototal <- totaleducation %>% filter(education == "Total") %>%
                group_by(region) %>%
                summarise(geototal = sum(sextotal, na.rm = TRUE)) %>%
                mutate(region = myfips$FIPS)
        totaleducation <- left_join(totaleducation, geototal, by = "region")
        
        # now both education and total education have the same bins
        
        # what about "other" as a racial/ethnic group?
        educationother <- education %>% group_by(sex, education, region) %>% 
                summarise(notother = sum(population, na.rm = TRUE)) %>%
                ungroup() %>%
                left_join(totaleducation, by = c("sex", "education", "region")) %>% 
                mutate(raceethnicity = "Other", 
                       population = sextotal - notother) %>% 
                select(sex, education, raceethnicity, population, region)
        education <- bind_rows(education, educationother)
        education <- left_join(education, totaleducation, 
                               by = c("sex", "education", "region")) %>% 
                mutate(prob = population/geototal) %>%
                filter(education != "Total")
        education$raceethnicity <- toupper(education$raceethnicity)
        education <- education %>% select(sex, education, raceethnicity, 
                                          population, sextotal,
                                          geototal, prob, region)
        
        education        
}