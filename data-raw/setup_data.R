# this script requires the use of devtools

# countyFIPS data set -----------------------------------------------

countyFIPS <- read.csv("data-raw/national_county.txt", header = FALSE, 
                       stringsAsFactors = FALSE)
colnames(countyFIPS) <- c("state", "stateFIPS", "countyFIPS",
                          "countyname", "classfp")
devtools::use_data(countyFIPS, overwrite = TRUE)
