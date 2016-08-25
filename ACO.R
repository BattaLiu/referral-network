


# take a look at the ACO data

library(readr)
library(dplyr)
aco.part <- read.csv(file = "aco/aco-participant.csv", header = TRUE, sep = ",") 
aco <- read.csv(file = "aco/aco-list.csv", header = TRUE, sep = ",") #397

part.cnt <- aco.part %>% 
	group_by(ACO.Legal.or.Name.Doing.Business.As, Public.Contact.Phone) %>% 
	tally()

# found that 2 ACOs don't have data of participants,check why ???????
# Accountable Care Partners, LLC
# Emerald Physicians
anti_join(aco, part.cnt, by = "ACO.Legal.or.Name.Doing.Business.As") #2
anti_join(part.cnt, aco, by = "ACO.Legal.or.Name.Doing.Business.As")
test <- inner_join(part.cnt, aco, by = "ACO.Legal.or.Name.Doing.Business.As") #395
# found that two ACOs are duplicate
# Baroma Health Partners
# Mercy ACO, LLC
str(unique(aco$ACO.Legal.or.Name.Doing.Business.As))
test <- table(aco$ACO.Legal.or.Name.Doing.Business.As)
which(test==2)
# which(aco$ACO.Legal.or.Name.Doing.Business.As=="Baroma Health Partners")
# which(aco$ACO.Legal.or.Name.Doing.Business.As=="Mercy ACO, LLC")
aco[aco$ACO.Legal.or.Name.Doing.Business.As=="Baroma Health Partners",] # same info but different ACO.Service.Area and public.contact.phone
aco[aco$ACO.Legal.or.Name.Doing.Business.As=="Mercy ACO, LLC",] # totally different info

# find ACOs serving a targeted state
CA.ACO <- as.character(aco$ACO.Legal.or.Name.Doing.Business.As[grep("CA", aco$ACO.Service.Area)])
CA.only.ACO <- as.character(aco$ACO.Legal.or.Name.Doing.Business.As[aco$ACO.Service.Area=="CA"])
part.cnt[part.cnt$ACO.Legal.or.Name.Doing.Business.As=="Baroma Health Partners",]
test <- data.frame(aco.part[aco.part$ACO.Legal.or.Name.Doing.Business.As=="Baroma Health Partners",])
# write_csv(test, path = "part_name.csv")

# associate ACO with shared patient network.

library(RPostgreSQL)
pw <- {
	"no323232"
}
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname = "liubatta",
								 host = "192.168.0.10", port = 5432,
								 user = "liubatta", password = pw)

rm(pw) # removes the password
dbExistsTable(con, "tmp_test") # TRUE means successful connection
df_ca <- dbGetQuery(con,
										" SELECT * FROM npi_20160710_ca_full")

aco$postal.code <- as.character(aco$Zip.Code)
# remove the GPS location information from the zip.code
aco$postal.code <- gsub("\n(.*)$","\\",aco$postal.code)

npi_aco_pc <- semi_join(df_ca, aco, by = c("provider_business_practice_location_address_postal_code" = "postal.code"))
unique(npi_aco_pc$provider_business_practice_location_address_city_name)
unique(npi_aco_pc$provider_business_practice_location_address_postal_code)
unique(npi_aco_pc$healthcare_provider_taxonomy_code_1)

npi_aco_part <- semi_join(df_ca, aco.part, by = c("provider_organization_name_legal_business_name" = "Participant.Legal.Business.Name"))

test <- npi_aco_pc %>% 
	.[which(.$entity_type_code==2),] %>% 
	group_by(healthcare_provider_taxonomy_code_1) %>% 
	tally()

test$pc <- gsub("^[[:digit:]{5}]*","\\l_", test$provider_business_practice_location_address_postal_code)


# remove the GPS 

