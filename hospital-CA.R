# dig into the hospital data of CA
library(dplyr)
df.hospital.ca <- read.csv("/Users/batta/Dropbox/phd4/nursing-homes/read/Data/hospital-CA.csv")

system <- df.hospital.ca %>% 
	group_by(PARENT_NAME,PARENT_ZIP_9) %>% 
	tally

system$initial.name <- gsub("^([[:alpha:]]+)([[:blank:]]|[[:punct:]]).*", "\\1", system$PARENT_NAME)
system$PARENT_ZIP_5 <- gsub("^([[:digit:]]{5}).*", "\\1", system$PARENT_ZIP_9)
system$LOWER_PARENT_NAME <- tolower(system$PARENT_NAME)
system$name <- system$LOWER_PARENT_NAME
pattern <- "[[:punct:]]|inc|incorporated|llc|central|and"
system$name <- gsub(pattern, "", system$name)
system$name <- gsub("avanti hospitals", "avanti hospital", system$name)
system$name <- gsub("avanti hospital", "avanti hospitals", system$name)
system$name <- gsub("crestwod", "crestwood", system$name)
system$name <- gsub("daughters of charity health systems", "daughters of charity health system", system$name)
system$name <- gsub("daughters of charity healthcare systems", "daughters of charity health system", system$name)
system$name <- gsub("dignith health", "dignity health", system$name)
system$name <- gsub("dignity health (formerly catholic healthcare west)", "dignity health", system$name)
system$name <- gsub("hca holdings", "hca", system$name)
system$name <- gsub("interhealth corporation dba pih health", "interhealth corp dba pih health", system$name)
system$name <- gsub("kaier", "kaiser", system$name)
system$name <- gsub("kaiser foundation hospitals", "kaiser foundation hospital", system$name)
system$name <- gsub("kaiser foundation hospital", "kaiser foundation hospitals", system$name)
# to be finished, stops at kaiser permanente

##############
system$tmp <- gsub("Crestwod", "Crestwood", system$initial.name,fixed = TRUE)
system$tmp <- gsub("Dignith", "Dignity", system$initial.name,fixed = TRUE)
system$tmp <- gsub("Los", "LAC", system$initial.name,fixed = TRUE)
system$tmp <- gsub("none", "N/A", system$initial.name,fixed = TRUE)
# system$tmp <- gsub("//N", "N///A", system$initial.name,fixed = TRUE)
system$tmp <- gsub("NONE", "N/A", system$initial.name,fixed = TRUE)
system$tmp <- gsub("Non", "N/A", system$initial.name,fixed = TRUE)
#########
system$tmp <- gsub("North", "N/A", system$initial.name,fixed = TRUE)
system$tmp <- gsub("city", "N/A", system$initial.name,fixed = TRUE)
system$tmp <- gsub("county", "N/A", system$initial.name,fixed = TRUE)


