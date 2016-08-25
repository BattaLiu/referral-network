# Learn how to use postgresql

# install.packages("RPostgreSQL")
require("RPostgreSQL")

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
													" SELECT npi_id, entity_type_code, from npi_20160710_ca")

df_pc <- dbGetQuery(con,
										" SELECT npi_id, entity_type_code, 
										Provider_Business_Practice_Location_Address_Postal_Code 
										from npi_20160710_ca")
# take a look at the data type and change varchar to int
# in CA data, only 2 postal code are missing, no npi or entity type code is missing
# which(is.na(df_pc$provider_business_practice_location_address_postal_code))



