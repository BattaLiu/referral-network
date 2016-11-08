
```{r}
require("RPostgreSQL")
library(plyr)
library(dplyr)
library("ggplot2")
library(geosphere)
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

```{r}
# import the shared patient data
df.patient.share.14 <- dbGetQuery(con, 
 												" SELECT npi1, npi2, pair_count, bene_count,same_day_count,
 													entity_type_code,  
													provider_business_practice_location_address_state_name,
 													provider_business_practice_location_address_postal_code,
 													healthcare_provider_taxonomy_code_1
 													from patient_share_2014_180_ca1")


df.npi <- dbGetQuery(con, 
										 " SELECT npi_id,
										 entity_type_code, provider_business_practice_location_address_state_name,
										 provider_business_practice_location_address_postal_code,
										 healthcare_provider_taxonomy_code_1
										 from npi_20160710_ca")

```


find the physician network components
filter npi1 that also recorded as npi2 in CA, and match the NPPES data to npi2
```{r}
df.patient.share.14 <- df.patient.share.14 %>% 
	arrange(npi1,npi2)
only.npi2.14 <- df.patient.share.14 %>% 
	select(npi2)
only.npi1.14 <- df.patient.share.14 %>% 
	select(npi1)
info.npi1.14 <- df.patient.share.14 %>% 
	select(npi1, entity_type_code, provider_business_practice_location_address_state_name,
				 provider_business_practice_location_address_postal_code,
				 healthcare_provider_taxonomy_code_1)

```

Construct the information for npi2
```{r}
df.npi$npi_id <- as.numeric(df.npi$npi_id)

info.npi2.14 <- left_join(only.npi2.14, df.npi, by = c("npi2"="npi_id"))
sum(is.na(info.npi2.14$entity_type_code))
# info.npi1.14 <- info.npi1.14[!duplicated(info.npi1.14),]
# check why the duplicated command generates a different outcome.
# info.npi2.14 <- left_join(only.npi2.14, info.npi1.14, by = c("npi2" = "npi1"))
#tmp <- df.patient.share.14 %>% 
#	select(npi1)
#info.npi2.14 <- cbind(tmp,info.npi2.14)

info.npi12.14 <- cbind(info.npi1.14,info.npi2.14)
names(info.npi12.14) <- c("npi1", "npi1_entity_type_code", "npi1_provider_business_practice_location_address_state_name", "npi1_provider_business_practice_location_address_postal_code",
"npi1_healthcare_provider_taxonomy_code_1",               
"npi2",              
"npi2_entity_type_code",                                       
"npi2_provider_business_practice_location_address_state_name", 
"npi2_provider_business_practice_location_address_postal_code",
"npi2_healthcare_provider_taxonomy_code_1")

info.npi12.14$pair_count <- df.patient.share.14$pair_count
info.npi12.14$bene_count <- df.patient.share.14$bene_count
info.npi12.14$same_day_count <- df.patient.share.14$same_day_count

df.patient.share.14 <- info.npi12.14 %>% 
	arrange(npi1, npi2)

#tmp <- df.patient.share.14 %>% 
#	select(npi1,npi2_healthcare_provider_taxonomy_code_1, npi2_entity_type_code)

#tmp2 <- unique(tmp)

# share.type.cnt.14 shows the types of providers an NPI1 sharing patients with.
share.type.cnt.14 <- df.patient.share.14 %>% 
	group_by(npi1, npi2_healthcare_provider_taxonomy_code_1) %>% 
	tally

```
This block tracks the npi2s with the same taxonomy. But I don't think it helps my research.
```{r eval=FALSE}
tmp <- share.type.cnt.14 %>% 
	filter(n > 1 & !is.na(npi2_healthcare_provider_taxonomy_code_1))
tmp2 <- semi_join(df.patient.share.14, tmp, by = c("npi1","npi2_healthcare_provider_taxonomy_code_1"))
tmp2 <- tmp2 %>% 
	arrange(npi1, npi2)
length(unique(tmp2$npi2)) #92651 npi2 in CA has relation with other same taxonomy provider through a middle provider.
n2 <- data.frame(unique(tmp2$npi2))
names(n2) <- "npi2"
# find npi1 that have links with providers in the same taxonomy
tmp3 <- semi_join(df.patient.share.14, n2, by = c("npi1" = "npi2"))
```

```{r}
# tmp3 <- semi_join(df.patient.share.14, n2, by = c("npi1" = "npi2"))
tmp3 <- df.patient.share.14
tmp4 <- tmp3 %>% 
	filter(npi1_healthcare_provider_taxonomy_code_1 == npi2_healthcare_provider_taxonomy_code_1)
length(unique(tmp4$npi1_healthcare_provider_taxonomy_code_1))
tmp5 <- data.frame(unique(tmp4$npi1_healthcare_provider_taxonomy_code_1))
tmp6 <- as.character(tmp5$unique.tmp4.npi1_healthcare_provider_taxonomy_code_1.)
tmp7 <- data.frame(grep("^(20)",tmp6, value = TRUE)) # taxonomy number starting from 20 means they are Allopathic & Osteopathic Physicians (MD or OD)
names(tmp7) <- "taxonomy"
direct.compete.physician.14 <- semi_join(tmp4, tmp7, by = c("npi1_healthcare_provider_taxonomy_code_1" = "taxonomy"))

```
??????????? in last steps, I look at npi1 that are also npi2 with competitors sharing the same middle provider. However, I can also look at npi1s with same taxonomy npi2, ignoring the middle provider.

Analyze the npi2 taxonomies with more than one link. 
Check whether two providers sharing one provider have own link.
If they have own link, that link may go through the middle provider.


```{r}
# find the PCPs
tmp <- data.frame(grep("^((207Q)|(207R)|(208D00000X))",tmp6, value = TRUE))
names(tmp) <- "taxonomy"
pcp.14 <- semi_join(direct.compete.physician.14, tmp, by=c("npi1_healthcare_provider_taxonomy_code_1" = "taxonomy"))

# check their distance
df.npi.geocode <- dbGetQuery(con, 
																	" SELECT
                npi_id,lon,lat
								from npi_16ca_15geocode_visualization")
pcp.14 <- left_join(pcp.14,df.npi.geocode, by = c("npi1" = "npi_id") )

names(pcp.14)[names(pcp.14)=="lon"] <- "npi1_lon"
names(pcp.14)[names(pcp.14)=="lat"] <- "npi1_lat"
pcp.14 <- left_join(pcp.14,df.npi.geocode, by = c("npi2" = "npi_id") )
names(pcp.14)[names(pcp.14)=="lon"] <- "npi2_lon"
names(pcp.14)[names(pcp.14)=="lat"] <- "npi2_lat"

tmp <- as.matrix(cbind(pcp.14$npi1_lon,pcp.14$npi1_lat))
tmp2 <- as.matrix(cbind(pcp.14$npi2_lon, pcp.14$npi2_lat))
pcp.14$distance <- distHaversine(tmp,tmp2) 

#tmp <- as.matrix(head(cbind(pcp.14$npi1_lon,pcp.14$npi1_lat),20000))

```

I want to look at the physicians not located in the same place.

```{r}

pcp.different.location.14 <- pcp.14 %>% 
	filter(distance>3000)

tmp <- pcp.different.location.14 %>% 
	group_by(npi1,npi1_healthcare_provider_taxonomy_code_1) %>% 
	tally

# calculate average number of npi2 sharing the same taxonomy with npi1
ave.same.taxonomy.14 <- tmp %>% 
	group_by(npi1_healthcare_provider_taxonomy_code_1) %>% 
	summarise(ave.same.taxonomy = mean(n)) %>% 
	arrange(ave.same.taxonomy)

```

Similarly, I find out the npi2 with different taxonomy with npi1 and count their number.

```{r}

tmp4 <- df.patient.share.14 %>% 
	filter(npi1_healthcare_provider_taxonomy_code_1 != npi2_healthcare_provider_taxonomy_code_1)
tmp5 <- data.frame(unique(tmp4$npi1_healthcare_provider_taxonomy_code_1))
tmp6 <- as.character(tmp5$unique.tmp4.npi1_healthcare_provider_taxonomy_code_1.)
# find the PCPs
tmp <- data.frame(grep("^((207Q)|(207R)|(208D00000X))",tmp6, value = TRUE))
names(tmp) <- "taxonomy"
pcp.different.14 <- semi_join(tmp4, tmp, by=c("npi1_healthcare_provider_taxonomy_code_1" = "taxonomy"))

# check their distance
df.npi.geocode <- dbGetQuery(con, 
																	" SELECT
                npi_id,lon,lat
								from npi_16ca_15geocode_visualization")
pcp.different.14 <- left_join(pcp.different.14,df.npi.geocode, by = c("npi1" = "npi_id") )
names(pcp.different.14)[names(pcp.different.14)=="lon"] <- "npi1_lon"
names(pcp.different.14)[names(pcp.different.14)=="lat"] <- "npi1_lat"
pcp.different.14 <- left_join(pcp.different.14,df.npi.geocode, by = c("npi2" = "npi_id") )
names(pcp.different.14)[names(pcp.different.14)=="lon"] <- "npi2_lon"
names(pcp.different.14)[names(pcp.different.14)=="lat"] <- "npi2_lat"

tmp <- as.matrix(cbind(pcp.different.14$npi1_lon,pcp.different.14$npi1_lat))
tmp2 <- as.matrix(cbind(pcp.different.14$npi2_lon, pcp.different.14$npi2_lat))
pcp.different.14$distance <- distHaversine(tmp,tmp2) 

pcp.different.location.14 <- pcp.different.14 %>% 
	filter(distance>3000)

tmp <- pcp.different.location.14 %>% 
	group_by(npi1,npi1_healthcare_provider_taxonomy_code_1) %>% 
	tally

# calculate average number of npi2 sharing different taxonomy with npi1
ave.different.taxonomy.14 <- tmp %>% 
	group_by(npi1_healthcare_provider_taxonomy_code_1) %>% 
	summarise(ave.different.taxonomy = mean(n)) %>% 
	arrange(ave.different.taxonomy)

```

I want to look at the relation between physicians and organizations.





