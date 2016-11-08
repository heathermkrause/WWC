### script to write example csv to be read in weight_csv ----------------------

library(readr)
library(WWC)
write_csv(texassurvey, "inst/extdata/examplesurvey.csv")

write_csv(weight_wwc(twostatessurvey, sex, raceethnicity),
          "inst/extdata/weightedsurvey.csv")
