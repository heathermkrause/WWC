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


counties <- split(counties, seq(nrow(counties)))


# where are the functions used in the next section?
# source("./data-raw/process_acs.R")
# source("./data-raw/process_all.R")

# download age tables from ACS ------------------------------------------------

usageDF <- process_acs_age(unitedstates, yearspan = 5) %>% mutate(region = "US")

statesageDF <- map_df(unlist(states), function(x) {
        process_acs_age(geo.make(state = x), yearspan = 5) %>% 
                mutate(region = x)
        })
#sapply(statesageDF, function(y) sum(length(which(is.na(y)))))

countiesageDF <- process_acs_age_all(geo.make(state = "*", county = "*"), 
                         yearspan = 5)


acsagetable <- bind_rows(usageDF, statesageDF, countiesageDF)
acsagetable[acsagetable < 0] <- 1

#sapply(acsagetable, function(x) sum(x < 0))
#sapply(countiesageDF, function(y) sum(length(which(is.na(y)))))


devtools::use_data(acsagetable, overwrite = TRUE)



# download education tables from ACS ------------------------------------------

useduDF <- process_acs_education(unitedstates, yearspan = 5) %>% mutate(region = "US")

stateseduDF <- map_df(unlist(states), function(x) {
        process_acs_education(geo.make(state = x), yearspan = 5) %>% 
                mutate(region = x)
        })

countieseduDF <- process_acs_edu_all(geo.make(state = "*", county = "*"), 
                                     yearspan = 5)

acsedutable <- bind_rows(useduDF, stateseduDF, countieseduDF)
acsedutable[acsedutable < 0] <- 1

#sapply(acsedutable, function(y) sum(length(which(is.na(y)))))
#sapply(acsedutable, function(x) sum(x < 0))

devtools::use_data(acsedutable, overwrite = TRUE)

