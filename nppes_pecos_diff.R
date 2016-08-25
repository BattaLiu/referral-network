# Author: Jiayi Liu
# Date: Aug 21, 2016
# In this Rscript,  I want to find out why some medicare provider code in physician compare data of CA 
# is not matched in nppes data of CA.
# The result is that some physicians have different practice address in different states, some
# physicians have slight difference in address input. Only one physician deactivated in the sample.


# install.packages("RPostgreSQL")
require("RPostgreSQL")
library(dplyr)
library("ggplot2")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
	"no323232"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "liubatta",
								 host = "192.168.0.10", port = 5432,
								 user = "liubatta", password = pw)

rm(pw) # removes the password

# check for the cartable
dbExistsTable(con, "tmp_test")

#
df.npi <- dbGetQuery(con, 
												" SELECT npi_id, entity_type_code,
												provider_organization_name_legal_business_name,
												provider_first_line_business_practice_location_address,
												provider_business_practice_location_address_state_name,
												provider_enumeration_date,
												last_update_date,provider_business_practice_location_address_postal_code
												from npi_20160710_select")
npi.deact <- read.csv("/Users/batta/Dropbox/phd4/nursing-homes/read/Data/NPPES_Deactivated_NPI_Report_20160809.csv", header = TRUE, sep = ",")
med.comp.ca <- read.csv("/Users/batta/Dropbox/phd4/nursing-homes/read/Data/Physician_Compare_National_Downloadable_File.csv", header = TRUE, sep = ",")

non.fac <- df.npi %>% 
	filter(entity_type_code==1)

non.fac$npi_id <- as.numeric(non.fac$npi_id)

tmp <- anti_join(med.comp.ca, non.fac, by = c("NPI" = "npi_id"))
# some npi in medicare physician compare file of CA doesn't not exist in NPPES file of CA, maybe they 
# have different address info in PECOS data.
# and only one of these NPIs are not in deactivtion file, others have different address info in NPPES
# and patient compare file (PECOS)
tmp2 <- inner_join(npi.deact, tmp, by = "NPI")

# find out their locations in full NPPES file
tmp3 <- inner_join(non.fac, test, by = c("npi_id" = "NPI"))

# usually, the difference is due to multiple practice address