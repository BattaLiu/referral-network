# Author: Jiayi Liu (Batta)
# Date: Sep 12, 2016

# in this script, I use multicore to make queries to DSTK API. And use clusters > number of cores.

# install.packages("RPostgreSQL")require("RPostgreSQL")
require("RPostgreSQL")
library(plyr)
library(dplyr)
library("ggplot2")
library(ggmap)
library(RDSTK)
library(zipcode)
library(abind)
library(parallel)
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

df.npi.ca <- dbGetQuery(con, 
												" SELECT npi_id, entity_type_code,
												provider_organization_name_legal_business_name,
												provider_first_line_business_practice_location_address,
												provider_second_line_business_practice_location_address,
												provider_business_practice_location_address_city_name,
												provider_business_practice_location_address_state_name,
												provider_business_practice_location_address_postal_code,
												latitude, longitude
												from npi_20160710_ca_coord")

df.physician <- dbGetQuery(con, 
													 " SELECT *
													 from physician_compare_20160908")

df.npi.ca$practice_address <- paste(df.npi.ca$provider_first_line_business_practice_location_address,
																		df.npi.ca$provider_business_practice_location_address_city_name,
																		df.npi.ca$provider_business_practice_location_address_state_name,
																		sep = ",")
df.npi.ca$provider_business_practice_location_address_postal_code <- as.character(df.npi.ca$provider_business_practice_location_address_postal_code)
df.npi.ca$pc_head <- gsub("^([[:digit:]]{5}).*$","\\1", 
													df.npi.ca$provider_business_practice_location_address_postal_code)
df.npi.ca$practice_address <- paste(df.npi.ca$practice_address, 
																		df.npi.ca$pc_head,
																		sep = " ")


tmp <- street2coordinates(df.npi.ca$practice_address[[1]])
try <- tmp[0,]
try[1,] <- rep(NA, 13)

# functions
get.coord <- function(x){
	try[1,1] <- x
	tmp.df <- tryCatch(street2coordinates(x),
										 error = function(e){try})
	if (nrow(tmp.df)<1 | ncol(tmp.df)!=13) {
		tmp.df <- try
	}
	return(tmp.df)
}

# Usually, we calculate the number of cores and set the number of cluster to be less than cores of computer. 
# However, in this program, most of time are spent in waiting for the reply of API. Therefore, the more cores, the faster.
no_cores <- 30
# Initiate cluster，due to the limitation of memory, I need to run the below set in subsets
system.time({
	sub <- df.npi.ca[c(1:553780),] ######### change the range to maximum 100000
	cl <- makeCluster(no_cores,type = "FORK")
	#	clusterEvalQ(cl, library(RDSTK))
	#	clusterExport(cl, c("try","get.coord"))
	tmp <- parLapply(cl,sub$practice_address, get.coord)
	stopCluster(cl)
	tmp2 <- do.call(rbind,tmp)
})

pile$row.names <- NULL
pile <- rbind(pile,tmp2)

dbWriteTable(con, "coord_ca_55",pile)


# Then I cbind the coordinate information with original npi information
df.npi.ca <- dbGetQuery(con, " SELECT * from npi_20160710_ca")
coord <- dbGetQuery(con, " SELECT * from coord_ca_55")
coord$row.names <- NULL
new <- cbind(df.npi.ca, coord)
dbWriteTable(con,"npi_20160710_ca_coord",new)

