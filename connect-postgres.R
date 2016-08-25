# Learn how to use postgresql

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
# TRUE

df_postgres <- dbGetQuery(con, 
													" SELECT npi_id, entity_type_code from npi_20160710_ca")

df_pc <- dbGetQuery(con,
										" SELECT npi_id, entity_type_code, 
										Provider_Business_Practice_Location_Address_Postal_Code 
										from npi_20160710_ca")
# take a look at the data type and change varchar to int
# in CA data, only 2 postal code are missing, no npi or entity type code is missing
# which(is.na(df_pc$provider_business_practice_location_address_postal_code))

df_dmepos <- dbGetQuery(con, " SELECT * from dmepos_refer_ca")
# df_primary_patient_pattern <- dbGetQuery(con,
#																				 " SELECT * from primary_patient_pattern_ca1")

df_refer_patient_pattern <- dbGetQuery(con," SELECT * from refer_patient_pattern_ca1")

df_patient_pattern_ca1 <- dbGetQuery(con,
																		" SELECT npi1,npi2, pair_count, bene_count, same_day_count from patient_pattern_ca1")
length(unique(df_patient_pattern_ca$npi1))

df_primary_npi_ca <- read.csv("primary/Medicare_family_practice_NPI_CA2013.csv",header = TRUE, sep = ",")

# Check whether family practices are individuals 
unique(df_dmepos$referring_provider_entity_code) # Individuals and organizations 14413:445
unique(df_primary_npi_ca$nppes_entity_code) # all are type I in CA

# check how many individual family practices in patient-pattern data
df_primary_patient_pattern <- semi_join(df_patient_pattern_ca, df_primary_npi_ca, by = c("npi1" = "npi"))
# save the data 
# primary_npi_ca2013 and dmepos_primary_order in postgresql are form CMS but dmepos only filter state(CA)
# while primary data filter provider type and state
dbWriteTable(con, "primary_npi_ca2013", value = df_primary_npi_ca, row.names = TRUE)
test <- df_primary_npi_ca %>% 
	select(nppes_provider_zip_code,npi, nppes_entity_code, 
				 nppes_provider_street_address_1, nppes_provider_street_address_2, 
				 nppes_provider_first_name, nppes_provider_last_name_organization_name) %>% 
	arrange(nppes_provider_zip_code)

# group the practice location by zip code
family.group.zip <- df_primary_npi_ca %>% 
	group_by(nppes_provider_zip_code) %>% 
	tally() %>% 
	filter(n>1) %>% 
	arrange(desc(n))
# group the practice location by zip code and street info
family.group.zip.street <- df_primary_npi_ca %>% 
	group_by(nppes_provider_zip_code, nppes_provider_street_address_1) %>% 
	tally() %>% 
	filter(n>1) %>% 
	arrange(desc(n))

# maybe there is something wrong with data download as provider type is only family practice,
# should include internal medicine
test <- semi_join(df_primary_npi_ca, family.group.zip.street, by =  "nppes_provider_zip_code" )
