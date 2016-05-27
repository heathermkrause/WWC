simulate_survey_ACS_education <- function(state) {
        statefetch <- geo.make(state = state)
        
        # get education attainment for male/female population, not broken
        # down by race/ethnicity
        totaleducationfetch <- acs.fetch(geography = statefetch, endyear = 2014,
                                         span = 1, table.number = "C15002",
                                         col.names = "pretty")
        totaleducation <- reshape2::melt(estimate(totaleducationfetch))
        totaleducation$Var2 <- str_extract(as.character(totaleducation$Var2), 
                                           "(Female:.+$|Female:|Male:.+$|Male:)")
        totaleducation$sex <- str_extract(totaleducation$Var2, "(Female|Male)")
        totaleducation$level <- str_extract(str_extract(totaleducation$Var2, ":.+$"), "\\b.+$")
        totaleducation$level[is.na(totaleducation$level)] <- "Total"
        totaleducation$level <- factor(totaleducation$level, levels = unique(totaleducation$level))
        totaleducation$sex <- as.factor(totaleducation$sex)
        totaleducation <- totaleducation %>% 
                select(sex, level, population = value) %>%
                filter(!is.na(sex))
        # this data frame above has sex by educational attainment for total population
        

        # get educational attainment for male/female population broken down
        # by race/ethnicity
        tablenames <- paste0("C15002", c("B","D","H", "I"))
        educationfetch <- map(tablenames, function(x) {
                acs.fetch(geography = statefetch, endyear = 2014, span = 1,
                          table.number = x, col.names = "pretty")
        })
        
        process_fetch <- function(fetch) {
                education <- estimate(fetch)
                education <- reshape2::melt(education)
                education$raceethnicity <- str_extract(str_extract(as.character(education$Var2), "\\(.+\\):{1}"),
                                                       "[a-zA-Z\\,\\s]+")
                education$Var2 <- str_extract(as.character(education$Var2), "(Female:.+$|Female:|Male:.+$|Male:)")
                education$sex <- str_extract(education$Var2, "(Female|Male)")
                education$level <- str_extract(str_extract(education$Var2, ":.+$"), "\\b.+$")
                education$level[is.na(education$level)] <- "Total"
                education$level <- factor(education$level, levels = unique(education$level))
                education$sex <- as.factor(education$sex)
                education <- education %>% select(sex, level, raceethnicity, population = value)
        }
        
        # this data frame has sex by educational attainment for four
        # racial/ethnic groups but NOT total or other
        education <- purrr::map_df(educationfetch, function(x) {process_fetch(x)})
        education <- education[!is.na(education$sex),]
        
        # unfortunately, it is not the same education bins as the
        # total education data frame above
        
        totaleducation <- bind_rows(totaleducation %>% group_by(sex) %>% 
                                            filter(level == "Total") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(level = "Total"),
                                    totaleducation %>% group_by(sex) %>% 
                                            filter(level == "Less than 9th grade" | 
                                                           level == "9th to 12th grade, no diploma") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(level = "Less than high school diploma"),
                                    totaleducation %>% group_by(sex) %>% 
                                            filter(level == "High school graduate (includes equivalency)") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(level = "High school graduate (includes equivalency)"),
                                    totaleducation %>% group_by(sex) %>% 
                                            filter(level == "Some college, no degree" | 
                                                           level == "Associate's degree") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(level = "Some college or associate's degree"),
                                    totaleducation %>% group_by(sex) %>% 
                                            filter(level == "Bachelor's degree" | 
                                                           level == "Graduate or professional degree") %>% 
                                            summarize(sextotal = sum(population, na.rm = TRUE)) %>% 
                                            mutate(level = "Bachelor's degree or higher"))
        statetotal <- totaleducation %>% filter(level == "Total") %>% 
                summarise(statetotal = sum(sextotal))
        totaleducation <- totaleducation %>% mutate(statetotal = statetotal[[1]])
        
        # now both education and total education have the same bins
        
        # what about "other" as a racial/ethnic group?
        educationother <- education %>% group_by(sex, level) %>% 
                summarise(notother = sum(population)) %>% 
                left_join(totaleducation) %>% 
                mutate(raceethnicity = "Other", 
                       population = sextotal - notother) %>% 
                select(sex, level, raceethnicity, population)
        education <- bind_rows(education, educationother)
        education <- left_join(education, totaleducation) %>% 
                mutate(prob = population/statetotal)
        education$raceethnicity <- toupper(education$raceethnicity)

        education        
}