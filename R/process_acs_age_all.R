#' Fetch and process age and demographic data from ACS 
#' tables B01001 and B01001B/D/H/I and return a tidy data frame for a specified
#' geography with FIPS codes
#' 
#' @param geographyfetch A geography created with the \code{geo.make()} function
#' of the acs package, such as all counties in the United States
#' @param yearspan The span in years of the desired ACS data (should be 1 or
#' 5). Default is 1. Not all county data is available as one-year estimates.
#' 
#' @details Uses ACS 1-year estimate for 2014 by default; uses ACS 5-year 
#' estimate for 2010-2014 with \code{yearspan = 5}.
#' 
#' @return A data frame with 8 columns that tabulates the sex by age population
#' for five racial/ethnic groups: black alone, white alone (not Hispanic or 
#' Latino), Hispanic or Latino, Asian alone, and other. The age binning in this
#' data frame is the same as in ACS tables B01001B/D/H/I (the data from table
#' B01001 is rebinned and used to find the "other" population).
#' 
#' @import dplyr
#' @importFrom stringr str_extract
#' @importFrom stringr str_c
#' @importFrom stringr str_sub
#' 
#' @name process_acs_age_all
#' 
#' @examples 
#' \dontrun{
#' library(acs)
#' # if you are new to using the acs package, you will need to get an API key
#' # and run api.key.install() one time to install your key on your system
#' allcounties <- geo.make(state = "*", county = "*")
#' countiesDF <- process_acs_age_all(allcounties, yearspan = 5)
#' }
#' 
#' @export

process_acs_age_all <- function(geographyfetch, yearspan = 1) {
        
        # get age for male/female population, not broken
        # down by race/ethnicity
        totalagefetch <- acs::acs.fetch(geography = geographyfetch, 
                                        endyear = 2014,
                                        span = yearspan, 
                                        table.number = "B01001",
                                        col.names = "pretty")
        myfips <- acs::geography(totalagefetch) %>%  
                mutate(FIPS = str_c(str_sub(str_c("000", state),-2),
                                    str_sub(str_c("000", county),-3))) %>%
                select(FIPS)
        acs::geography(totalagefetch)=cbind(myfips, acs::geography(totalagefetch))
        totalage <- reshape2::melt(acs::estimate(totalagefetch))
        totalage$age <- str_extract(as.character(totalage$Var2), 
                                    "(\\d+ (to|and) \\d+ years|\\d+ years and over|\\d+ years|Under \\d+ years)")
        totalage$sex <- str_extract(totalage$Var2, "(Female|Male)")
        totalage$age[is.na(totalage$age)] <- "Total"
        totalage$age <- factor(totalage$age, levels = unique(totalage$age))
        totalage$region <- str_sub(str_c("00", totalage$Var1), -5)
        geototal <- data_frame(geototal = totalage$value[is.na(totalage$sex)],
                               region = myfips$FIPS)
        totalage <- totalage %>% 
                select(sex, age, population = value, region) %>%
                filter(!is.na(sex)) %>%
                mutate(age = as.character(age))
        # this data frame above has sex by age for total population
        
        
        # get age for male/female population broken down
        # by race/ethnicity
        tablenames <- paste0("B01001", c("B","D","H", "I"))
        agefetch <- purrr::map(tablenames, function(x) {
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
                sexbyage <- acs::estimate(fetch)
                sexbyage <- reshape2::melt(sexbyage)
                sexbyage$raceethnicity <- str_extract(str_extract(as.character(sexbyage$Var2), "\\(.+\\):{1}"),
                                                      "[a-zA-Z\\,\\s]+")
                sexbyage$age <- str_extract(as.character(sexbyage$Var2), 
                                            "(\\d+ (to|and) \\d+ years|\\d+ years and over|\\d+ years|Under \\d+ years)")
                sexbyage$sex <- str_extract(sexbyage$Var2, "(Female|Male)")
                sexbyage$age[is.na(sexbyage$age)] <- "Total"
                sexbyage$region <- str_sub(str_c("00", sexbyage$Var1), -5)
                sexbyage$age <- factor(sexbyage$age, levels = unique(sexbyage$age))
                sexbyage <- sexbyage %>% select(sex, age, raceethnicity, population = value, region)
        }
        
        # this data frame has sex by age for four
        # racial/ethnic groups but NOT total or other
        age <- purrr::map_df(agefetch, function(x) {process_fetch(x)})
        age <- age[!is.na(age$sex),] %>%
                mutate(age = as.character(age))
        
        # unfortunately, it is not the same age bins as the
        # total age data frame above
        
        totalage <- bind_rows(totalage %>% filter(age %in% age$age),
                              totalage %>% group_by(sex, region) %>% 
                                      filter(age == "20 years" |
                                                     age == "21 years" |
                                                     age == "22 to 24 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "20 to 24 years"),
                              totalage %>% group_by(sex, region) %>% 
                                      filter(age == "35 to 39 years" | 
                                                     age == "40 to 44 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "35 to 44 years"),
                              totalage %>% group_by(sex, region) %>%
                                      filter(age == "45 to 49 years" |
                                                     age == "50 to 54 years") %>%
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "45 to 54 years"),
                              totalage %>% group_by(sex, region) %>% 
                                      filter(age == "55 to 59 years" | 
                                                     age == "60 and 61 years" |
                                                     age == "62 to 64 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "55 to 64 years"),
                              totalage %>% group_by(sex, region) %>% 
                                      filter(age == "65 and 66 years" | 
                                                     age == "67 to 69 years" |
                                                     age == "70 to 74 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "65 to 74 years"),
                              totalage %>% group_by(sex, region) %>% 
                                      filter(age == "75 to 79 years" | 
                                                     age == "80 to 84 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "75 to 84 years")) %>%
                select(sex, age, sextotal = population, region) %>%
                left_join(geototal, by = "region")
        
        # now both age and totalage have the same bins
        
        # what about "other" as a racial/ethnic group?
        ageother <- age %>% group_by(sex, age, region) %>% 
                summarise(notother = sum(population, na.rm = TRUE)) %>% 
                ungroup() %>%
                mutate(age = as.character(age)) %>%
                left_join(totalage, by = c("sex", "age", "region")) %>% 
                mutate(raceethnicity = "Other", 
                       population = sextotal - notother) %>% 
                select(sex, age, raceethnicity, population, region)
        age <- bind_rows(age, ageother)
        age <- left_join(age, totalage, by = c("sex", "age", "region")) %>% 
                mutate(prob = population/geototal) %>%
                filter(age != "Total")
        age$raceethnicity <- toupper(age$raceethnicity)
        age <- age %>% select(sex, age, raceethnicity, population, sextotal,
                              geototal, prob, region)
        
        age        
}