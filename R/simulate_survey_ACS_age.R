simulate_survey_ACS_age <- function(state) {
        statefetch <- geo.make(state = state)
        
        # get age for male/female population, not broken
        # down by race/ethnicity
        totalagefetch <- acs.fetch(geography = statefetch, endyear = 2014,
                                         span = 1, table.number = "B01001",
                                         col.names = "pretty")
        totalage <- reshape2::melt(estimate(totalagefetch))
        totalage$age <- str_extract(as.character(totalage$Var2), 
                                           "(\\d+ (to|and) \\d+ years|\\d+ years and over|\\d+ years|Under \\d+ years)")
        totalage$sex <- str_extract(totalage$Var2, "(Female|Male)")
        totalage$age[is.na(totalage$age)] <- "Total"
        totalage$age <- factor(totalage$age, levels = unique(totalage$age))
        totalage$sex <- as.factor(totalage$sex)
        statetotal <- totalage$value[is.na(totalage$sex)]
        totalage <- totalage %>% 
                select(sex, age, population = value) %>%
                filter(!is.na(sex))
        # this data frame above has sex by age for total population
        
        
        # get age for male/female population broken down
        # by race/ethnicity
        tablenames <- paste0("B01001", c("B","D","H", "I"))
        agefetch <- map(tablenames, function(x) {
                acs.fetch(geography = statefetch, endyear = 2014, span = 1,
                          table.number = x, col.names = "pretty")
        })
        
        process_fetch <- function(fetch) {
                sexbyage <- estimate(fetch)
                sexbyage <- reshape2::melt(sexbyage)
                sexbyage$raceethnicity <- str_extract(str_extract(as.character(sexbyage$Var2), "\\(.+\\):{1}"),
                                                       "[a-zA-Z\\,\\s]+")
                sexbyage$age <- str_extract(as.character(sexbyage$Var2), 
                                            "(\\d+ (to|and) \\d+ years|\\d+ years and over|\\d+ years|Under \\d+ years)")
                sexbyage$sex <- str_extract(sexbyage$Var2, "(Female|Male)")
                sexbyage$age[is.na(sexbyage$age)] <- "Total"
                sexbyage$age <- factor(sexbyage$age, levels = unique(sexbyage$age))
                sexbyage$sex <- as.factor(sexbyage$sex)
                sexbyage <- sexbyage %>% select(sex, age, raceethnicity, population = value)
        }
        
        # this data frame has sex by age for four
        # racial/ethnic groups but NOT total or other
        age <- purrr::map_df(agefetch, function(x) {process_fetch(x)})
        age <- age[!is.na(age$sex),]
        
        # unfortunately, it is not the same age bins as the
        # total age data frame above
        
        totalage <- bind_rows(totalage %>% filter(age %in% levels(age$age)),
                              totalage %>% group_by(sex) %>% 
                                      filter(age == "20 years" |
                                                     age == "21 years" |
                                                     age == "22 to 24 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "20 to 24 years"),
                              totalage %>% group_by(sex) %>% 
                                      filter(age == "35 to 39 years" | 
                                                     age == "40 to 44 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "35 to 44 years"),
                              totalage %>% group_by(sex) %>%
                                      filter(age == "45 to 49 years" |
                                                     age == "50 to 54 years") %>%
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "45 to 54 years"),
                              totalage %>% group_by(sex) %>% 
                                      filter(age == "55 to 59 years" | 
                                                     age == "60 and 61 years" |
                                                     age == "62 to 64 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "55 to 64 years"),
                              totalage %>% group_by(sex) %>% 
                                      filter(age == "65 and 66 years" | 
                                                     age == "67 to 69 years" |
                                                     age == "70 to 74 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "65 to 74 years"),
                              totalage %>% group_by(sex) %>% 
                                      filter(age == "75 to 79 years" | 
                                                     age == "80 to 84 years") %>% 
                                      summarize(population = sum(population, na.rm = TRUE)) %>% 
                                      mutate(age = "75 to 84 years")) %>%
                select(sex, age, sextotal = population) %>%
                mutate(statetotal = statetotal)
        
        # now both age and totalage have the same bins
        
        # what about "other" as a racial/ethnic group?
        ageother <- age %>% group_by(sex, age) %>% 
                summarise(notother = sum(population)) %>% 
                left_join(totalage) %>% 
                mutate(raceethnicity = "Other", 
                       population = sextotal - notother) %>% 
                select(sex, age, raceethnicity, population)
        age <- bind_rows(age, ageother)
        age <- left_join(age, totalage) %>% 
                mutate(prob = population/statetotal)
        
        age        
}