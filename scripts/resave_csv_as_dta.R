# Script for converting tidied doc to dta format

rm(list = ls())

require(readr)
require(foreign)

require(tidyr)
require(stringr)
require(dplyr)


dta <- read_csv("data/tidied/icd_8_to_10_8fold.csv")


write.dta(dta, "data/tidied/icd_8_to_10_8fold.dta")

write.table(dta, file = "data/tidied/icd_8_to_10_8fold.txt", row.names = F)
