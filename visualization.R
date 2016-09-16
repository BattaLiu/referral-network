# Author: Jiayi Liu (Batta)
# Date: Sep 10, 2016

# in this script, I will mark the location of health care providers on a map and highlight the affiliations
# between them.
```{r}
# install.packages("RPostgreSQL")
require("RPostgreSQL")
library(plyr)
library(dplyr)
library("ggplot2")
library(ggmap)
library(RDSTK)
library(zipcode)
library(abind)
library(jsonlite)
```

```{r}
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

```
Load data
```{r}
df.npi.ca <- dbGetQuery(con, 
												" SELECT npi_id, entity_type_code,
												healthcare_provider_taxonomy_code_1,
												provider_organization_name_legal_business_name,
												provider_first_line_business_practice_location_address,
												provider_second_line_business_practice_location_address,
												provider_business_practice_location_address_city_name,
												provider_business_practice_location_address_state_name,
												provider_business_practice_location_address_postal_code,
												latitude, longitude, confidence
												from npi_20160710_ca_coord")

df.physician <- dbGetQuery(con, " SELECT * from physician_compare_20160908")

df.npi.geocode <- dbGetQuery(con,"SELECT * from npi_201507_geocode")
```



First attempt, according to a document (https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf) plot the locations of CA health providers.
```{r}
qmplot( longitude, latitude, data = df.npi.ca, color = I('red'), size = I(0.3),darken = .3)
state <- "CA"
state.info <- fromJSON(paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",state))
state.coord <- state.info$results$geometry$bounds

within.state <- df.npi.ca %>% 
	filter(longitude>=state.coord$southwest$lng & longitude<=state.coord$northeast$lng &
				 latitude>=state.coord$southwest$lat & latitude<=state.coord$northeast$lat )

```
some missing coordinates
```{r}
nas <- df.npi.ca[is.na(df.npi.ca$longitude),]
```
some coordinates incorrectly mapped out of CA
```{r}
outside.state <- df.npi.ca %>% 
	filter(longitude<=state.coord$southwest$lng | longitude>=state.coord$northeast$lng |
				 	latitude<=state.coord$southwest$lat | latitude>=state.coord$northeast$lat )

```
try fix the problem using geocode, but still some failed, the reason is that some addresses includes suite number
```{r}
test <- geocode(outside.state$practice_address)
outside.state$latitude <- test$lat
outside.state$longitude <- test$lon
test2 <- outside.state %>% 
	filter(longitude>=state.coord$southwest$lng & longitude<=state.coord$northeast$lng &
				 	latitude>=state.coord$southwest$lat & latitude<=state.coord$northeast$lat)

test3 <- outside.state %>% 
	filter(longitude<=state.coord$southwest$lng | longitude>=state.coord$northeast$lng |
				 	latitude<=state.coord$southwest$lat | latitude>=state.coord$northeast$lat)

qmplot( longitude, latitude, data = test2, color = I('red'), size = I(0.3),darken = .3)

tmp <- rbind(within.state,nas,outside.state)
tmp2 <- tmp[match(df.npi.ca$npi_id, tmp$npi_id),]
df.npi.ca <- tmp2
qmplot(longitude, latitude, data = df.npi.ca, color = I('red'), size = I(0.1),darken = .3)

```
however, I found that quite a few of locations are wrong
```{r}
df.npi.san <- df.npi.ca %>% 
	filter(provider_business_practice_location_address_city_name == "SAN DIEGO")
city <- "SAN%2DIEGO"
city.info <- fromJSON(paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",city))
city.coord <- city.info$results$geometry$bounds
within.city <- df.npi.san %>% 
	filter(longitude>=city.coord$southwest$lng & longitude<=city.coord$northeast$lng &
				 	latitude>=city.coord$southwest$lat & latitude<=city.coord$northeast$lat)
outside.city <- df.npi.san %>% 
	filter(longitude<=city.coord$southwest$lng | longitude>=city.coord$northeast$lng |
				 	latitude<=city.coord$southwest$lat | latitude>=city.coord$northeast$lat )
qmplot(longitude, latitude, data = within.city, color = I('red'), size = I(0.1),darken = .3)

```
then I check how many coordinates are correct, 5/8 is correct, I need to look for true locations of these NPIs
```{r}
test <- head(df.npi.san,100)
test2 <- do.call(rbind, lapply(test$practice_address,get.coord))
unique(test2$full.address)
test3 <- test2 %>% 
	filter(longitude<=city.coord$southwest$lng | longitude>=city.coord$northeast$lng |
				 	latitude<=city.coord$southwest$lat | latitude>=city.coord$northeast$lat )
qmplot(longitude, latitude, data = test3, color = I('red'), size = I(0.1),darken = .3)

```
Import geocode data and get CA-only part.
Geocode Table for National Provider Identifier (NPI) File. Dabo Brantley (CDC) and Robert Borchers (Wisconsin Cancer Reporting System) April 2014. Geocoding at CDC using Centrus Geocoder
```{r}
df.new <- left_join(df.npi.ca,df.npi.geocode, by = c("npi_id"="npi"))
```
Let's see how different the address from DSTK API and address from 2015 NPI geocode data from naaccr. I think it is precise enough. I may clean the data and compliment these geocodes.
```{r}
unique(df.new$st_abbr)
df.ca.both <- df.new[which(df.new$st_abbr=="CA"),]
qmplot( lon, lat, data = df.ca.both, color = I('red'), size = I(0.3),darken = .3)
df.new <- data.frame(df.new)
test <- df.new %>% 
	select(npi_id,lon,lat)
within.state <- df.ca.both %>% 
	filter(lon>=-124.4152 & lon<=-114.1314 &
				 lat>=32.53426 & lat<=42.00952 )

within.state.dstk <- df.ca.both %>% 
	filter(longitude>=-124.4152 & longitude<=-114.1314 &
				 latitude>=32.53426 & latitude<=42.00952 )

outside.state <- df.ca.both %>% 
	filter(lon<=-124.4152 | lon>=-114.1314 |
				 lat<=32.53426 | lat>=42.00952 )
qmplot( lon, lat, data = within.state, color = I('red'), size = I(0.3),darken = .3)

dbWriteTable(con,"npi_16ca_15geocode_visualization",df.ca.both)
```



