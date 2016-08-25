# In this Rscript, nppes data and medicare provider compare data are combined using NPI.
# Used info are provider type, entity code, addresses, postal codes.

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

#  load the npi data of CA
df.npi.ca <- dbGetQuery(con, 
												" SELECT npi_id, entity_type_code,
												provider_organization_name_legal_business_name,
												provider_first_line_business_practice_location_address,
												provider_business_practice_location_address_postal_code
												from npi_20160710_ca")

df.npi.ca.2 <- dbGetQuery(con, 
												" SELECT npi_id, 
												provider_enumeration_date,last_update_date, npi_deactivation_reason_code,
												npi_deactivation_date, npi_reactivation_date, provider_gender_code,
												healthcare_provider_taxonomy_code_1 from npi_20160710_ca")

df.npi.ca.2$provider_enumeration_date <- as.Date(df.npi.ca.2$provider_enumeration_date, "%m/%d/%Y")
df.npi.ca.2$last_update_date <- as.Date(df.npi.ca.2$last_update_date, "%m/%d/%Y")

# group providers by address and names
test <- df.npi.ca

test$pc_head <- gsub("^([[:digit:]]{5}).*$","\\1", 
								test$provider_business_practice_location_address_postal_code)

test$pc_tail <- gsub("^([[:digit:]]{5})(.*)$","\\2", 
										 test$provider_business_practice_location_address_postal_code)

test <- test %>% 
	arrange(pc_head,pc_head,pc_tail)

facilities <- test %>% 
	filter(entity_type_code==2)

fac.locations <- facilities %>% 
	group_by(provider_first_line_business_practice_location_address, pc_head, pc_tail) %>% 
	tally

non.fac <- test %>% 
	filter(entity_type_code==1)

facilities.only1.npi <- facilities %>% 
	group_by(provider_business_practice_location_address_postal_code,
					 provider_first_line_business_practice_location_address) %>% 
	tally %>%
	filter(n==1)

non.fac.count <- non.fac %>% 
	group_by(provider_first_line_business_practice_location_address, 
					 pc_head,
					 pc_tail) %>% 
	tally

# want to match the individual NPIs with facility NPIs using their location, 
# however, some location has more than 1 facilities.
# about 1/2 of locations 
match <- inner_join(facilities, non.fac.count,
										by = c("provider_first_line_business_practice_location_address",
													 "pc_head", "pc_tail"))

tmp <- match %>% 
	group_by(provider_organization_name_legal_business_name, pc_head, pc_tail) %>% 
	tally()

##########################

# find the facilities with individual fellows and only one location

tmp <- match
tmp$n <- NULL # existence of two n's will disrupt the count
tmp <- tmp %>% 
	group_by(provider_first_line_business_practice_location_address, pc_head, pc_tail) %>% 
	tally 
max(tmp$n)
tmp2 <- tmp %>% 
	filter(n==1)
tmp2$n <- NULL
tmp3 <- semi_join(facilities,tmp2, by = c("provider_first_line_business_practice_location_address", 
												 "pc_head", "pc_tail"))
# match again, only 22160 facilities are matched to individual NPIS, about 2/7 locations.
fac.system <- inner_join(tmp3, non.fac.count,
												 by = c("provider_first_line_business_practice_location_address",
												 			 "pc_head", "pc_tail"))
fac.system <- fac.system %>% 
	arrange(desc(n))

# match the data with medicare providers info of 2014.
# med.prv.1 <- read.csv("primary/Medicare_Physician_and_Other_Supplier_NPI_ca2014.csv", header = TRUE, sep = ",")

med.prv <- read.csv("/Users/batta/Dropbox/phd4/nursing-homes/read/Data/Physician_Compare_National_Downloadable_File.csv", header = TRUE, sep = ",")


fac.system.med <- semi_join(fac.system, med.prv, by = c("npi_id" = "National.Provider.Identifier"))
fac.system.med <- fac.system.med %>% 
	arrange(pc_head)

facilities.med <- semi_join(facilities, med.prv, by = c("npi_id" = "National.Provider.Identifier"))

non.fac.med <- inner_join(non.fac, med.prv, by = c("npi_id" = "NPI"))

npi.med <- semi_join(test, med.prv, by = c("npi_id" = "National.Provider.Identifier"))

fac.system <- fac.system %>% 
	arrange(pc_head, pc_tail)

# add record date data

fac.system.med <- inner_join(fac.system.med, df.npi.ca.2, by = "npi_id")
facilities.med <- inner_join(facilities.med, df.npi.ca.2, by = "npi_id")
non.fac.med <- inner_join(non.fac.med, df.npi.ca.2, by = "npi_id")
npi.med <- inner_join(npi.med, df.npi.ca.2, by = "npi_id")

# pick physicians join npi in year 12, 13, 14

non.fac.med.12 <- non.fac.med %>% 
	filter(provider_enumeration_date>="2012-01-01" & provider_enumeration_date<"2013-01-01") %>% 
	filter(healthcare_provider_taxonomy_code_1!="390200000X") # exclude the new student providers 

family.doctor.12 <- non.fac.med.12 %>% 
	filter(Provider.Type.of.the.Provider== "Family Practice" | Provider.Type.of.the.Provider== "Internal Medicine")

tmp <- inner_join(family.doctor.12, fac.system.med, by = "pc_head")

# check 
test <- med.prv %>% 
	filter(Hospital.affiliation.CCN.1!="NA") 

test <- anti_join(med.prv, non.fac.med, by = c("NPI" = "npi_id"))
test <- anti_join(med.prv, df.npi.ca, by = c("NPI" = "npi_id"))

# 