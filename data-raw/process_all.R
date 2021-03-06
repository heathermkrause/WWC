# Some examples...
# allcounties <- geo.make(state = "*", county = "*")
# countiesDF <- process_acs_edu_all(allcounties, yearspan = 5)
# countiesDF <- process_acs_age_all(allcounties, yearspan = 5)


# Fetch and process educational tables B01001 and B01001B/D/H/I
# Binning for race/ethnicity in this data frame is the same as in ACS tables 
# B01001B/D/H/I; the data from table B01001 is rebinned and used to find the 
# "other" population

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
        
        # now we will rebin BOTH data frames to 5 age bins
        
        totalage <- bind_rows(totalage %>% group_by(sex, region) %>% 
                                      filter(age == "Under 5 years" |
                                                     age == "5 to 9 years" |
                                                     age == "10 to 14 years" |
                                                     age == "15 to 17 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "Under 18 years"),
                              totalage %>% group_by(sex, region) %>% 
                                      filter(age == "18 and 19 years" | 
                                                     age == "20 years" |
                                                     age == "21 years" |
                                                     age == "22 to 24 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "18 to 24 years"),
                              totalage %>% group_by(sex, region) %>%
                                      filter(age == "25 to 29 years" |
                                                     age == "30 to 34 years" |
                                                     age == "35 to 39 years" |
                                                     age == "40 to 44 years") %>%
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "25 to 44 years"),
                              totalage %>% group_by(sex, region) %>% 
                                      filter(age == "45 to 49 years" |
                                                     age == "50 to 54 years" |
                                                     age == "55 to 59 years" | 
                                                     age == "60 and 61 years" |
                                                     age == "62 to 64 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "45 to 64 years"),
                              totalage %>% group_by(sex, region) %>% 
                                      filter(age == "65 and 66 years" |
                                                     age == "67 to 69 years" |
                                                     age == "70 to 74 years" |
                                                     age == "75 to 79 years" |
                                                     age == "80 to 84 years" |
                                                     age == "85 years and over") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "65 years and over")) %>%
                ungroup %>%
                select(sex, age, sextotal = population, region) %>%
                left_join(geototal, by = "region")
        
        age <- bind_rows(age %>% group_by(sex, raceethnicity, region) %>% 
                                 filter(age == "Under 5 years" |
                                                age == "5 to 9 years" |
                                                age == "10 to 14 years" |
                                                age == "15 to 17 years") %>% 
                                 summarize(population = sum(population, na.rm = TRUE)) %>% 
                                 mutate(age = "Under 18 years"),
                         age %>% group_by(sex, raceethnicity, region) %>% 
                                 filter(age == "18 and 19 years" | 
                                                age == "20 to 24 years") %>% 
                                 summarize(population = sum(population, na.rm = TRUE)) %>% 
                                 mutate(age = "18 to 24 years"),
                         age %>% group_by(sex, raceethnicity, region) %>%
                                 filter(age == "25 to 29 years" |
                                                age == "30 to 34 years" |
                                                age == "35 to 44 years") %>%
                                 summarize(population = sum(population, na.rm = TRUE)) %>% 
                                 mutate(age = "25 to 44 years"),
                         age %>% group_by(sex, raceethnicity, region) %>% 
                                 filter(age == "45 to 54 years" | 
                                                age == "55 to 64 years") %>% 
                                 summarize(population = sum(population, na.rm = TRUE)) %>% 
                                 mutate(age = "45 to 64 years"),
                         age %>% group_by(sex, raceethnicity, region) %>% 
                                 filter(age == "65 to 74 years" |
                                                age == "75 to 84 years" |
                                                age == "85 years and over") %>% 
                                 summarize(population = sum(population, na.rm = TRUE)) %>% 
                                 mutate(age = "65 years and over")) %>%
                ungroup %>%
                select(sex, raceethnicity, age, population, region)
        
        # now both age and totalage have the same 5 bins
        
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

# Fetch and process educational tables B15002 and C15002B/D/H/I
# Binning in this data frame is the same as in ACS tables 
# C15002B/D/H/I; the data from table B15002 is rebinned and used to find the 
# "other" population

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
                                                           education == "Nursery to 4th grade" |
                                                           education == "5th and 6th grade" |
                                                           education == "7th and 8th grade" |
                                                           education == "9th grade" |
                                                           education == "10th grade" |
                                                           education == "11th grade" |
                                                           education == "12th grade, no diploma") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "Less than high school diploma"),
                                    totaleducation %>% group_by(sex, region) %>% 
                                            filter(education == "High school graduate (includes equivalency)") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "High school graduate (includes equivalency)"),
                                    totaleducation %>% group_by(sex, region) %>% 
                                            filter(education == "Some college, less than 1 year" | 
                                                           education == "Some college, 1 or more years, no degree" |
                                                           education == "Associate's degree") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(education = "Some college or associate's degree"),
                                    totaleducation %>% group_by(sex, region) %>% 
                                            filter(education == "Bachelor's degree" | 
                                                           education == "Master's degree" |
                                                           education == "Professional school degree" |
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
