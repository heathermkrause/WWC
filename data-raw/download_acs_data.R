library(acs)
library(dplyr)
library(purrr)
library(stringr)
library(WWC)
data(countyFIPS)

unitedstates <- geo.make(us = TRUE)
states <- countyFIPS %>% group_by(state) %>% distinct() %>% ungroup()
states <- states[1:51,]

counties <- countyFIPS %>% select(state, stateFIPS, countyFIPS) %>% 
        filter(state %in% unlist(states)) %>%
        mutate(FIPS = str_c(str_sub(str_c("000", stateFIPS),-2),
                            str_sub(str_c("000", countyFIPS),-3))) %>%
        select(-stateFIPS)


# countieschunk <- counties[1:1000,] 
counties <- split(counties, seq(nrow(counties)))


# download age tables from ACS ------------------------------------------------

usageDF <- process_acs_age(unitedstates, yearspan = 5) %>% mutate(region = "US")

statesageDF <- map_df(unlist(states), function(x) {
        process_acs_age(geo.make(state = x), yearspan = 5) %>% 
                mutate(region = x)
        })
#sapply(statesageDF, function(y) sum(length(which(is.na(y)))))

countiesageDF <- map_df(counties, function(x) {
         process_acs_age(geo.make(state = x$state, 
                                  county = x$countyFIPS), yearspan = 5) %>% 
                 mutate(region = x$FIPS)
         })

#sapply(countiesageDF, function(y) sum(length(which(is.na(y)))))

acsagetable <- bind_rows(usageDF, statesageDF, countiesageDF)
devtools::use_data(acsagetable, overwrite = TRUE)



# download education tables from ACS ------------------------------------------

useduDF <- process_acs_education(unitedstates) %>% mutate(region = "US")

stateseduDF <- map_df(unlist(states), function(x) {
        process_acs_education(geo.make(state = x)) %>% mutate(region = x)
        })

 countieseduDF <- map_df(counties, function(x) {
         process_acs_education(geo.make(state = x$state, 
                                        county = x$countyFIPS)) %>% 
                 mutate(region = x$FIPS)
 })

acsedutable <- bind_rows(useduDF, stateseduDF, countieseduDF)
devtools::use_data(acsedutable, overwrite = TRUE)

