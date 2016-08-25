# transfer the header to create table in postgresql 

library(readr)
library(dplyr)
DMEPOS <- read.csv(file = "DMEPOS/Referring_Primary_Provider_DMEPOS_CA2013_trial.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) 
header <- names(DMEPOS)

test <- sapply(DMEPOS,class)
test <- as.character(test)
str(test)
unique(test)

out <- paste(header, test, sep = " ")

test <- gsub(pattern = "character$", replace = "character (40)", out)
tmp <- paste(test, collapse = ",")
