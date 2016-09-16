
```{r}
require("RPostgreSQL")
library(dplyr)
library("ggplot2")
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
										 from npi_20160710")

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
info.npi1.14 <- unique(info.npi1.14)
```
Construct the information for npi2
```{r}
df.npi$npi_id <- as.numeric(df.npi$npi_id)

info.npi2.14 <- left_join(only.npi2.14, df.npi, by = c("npi2"="npi_id"))
# info.npi1.14 <- info.npi1.14[!duplicated(info.npi1.14),]
# check why the duplicated command generates a different outcome.
# info.npi2.14 <- left_join(only.npi2.14, info.npi1.14, by = c("npi2" = "npi1"))
info.npi2.14$npi1 <- df.patient.share.14$npi1
info.npi2.14$pair_count <- df.patient.share.14$pair_count
info.npi2.14$bene_count <- df.patient.share.14$bene_count
info.npi2.14$same_day_count <- df.patient.share.14$same_day_count

df.patient.share.info2.14 <- info.npi2.14 %>% 
	arrange(npi1, npi2)

tmp <- df.patient.share.info2.14 %>% 
	select(npi1,healthcare_provider_taxonomy_code_1, entity_type_code)

tmp2 <- unique(tmp)

# share.type.cnt.14 shows the types of providers an NPI1 sharing patients with.
share.type.cnt.14 <- tmp2 %>% 
	group_by(npi1) %>% 
	tally

tmp <- share.type.cnt.14 %>% 
	filter(n == 1)
tmp2 <- semi_join(info.npi2.14, tmp, by = "npi1")
tmp2 <- tmp2 %>% 
	arrange(npi1, npi2)

```

analyze the NPI1s with only one type of NPI2
```{r}
taxonomy <- read.csv("/Users/batta/Dropbox/phd4/nursing-homes/read/Data/nucc_taxonomy_161.csv")
only1typeNPI2.14 <- left_join(tmp2, taxonomy, by = c("healthcare_provider_taxonomy_code_1" = "Code"))
tmp <- only1typeNPI2.14 %>% 
	group_by(npi1) %>% 
	tally %>% 
	arrange(desc(n))
tmp2 <- only1typeNPI2.14 %>% 
	filter(entity_type_code==1)
tmp3 <- tmp2 %>% 
	group_by(npi1) %>% 
	tally %>% 
	arrange(desc(n))


```

# for 2013

```{r}
df.patient.share.13 <- dbGetQuery(con, 
																	" SELECT npi1, npi2, pair_count, bene_count,same_day_count,
																	entity_type_code, provider_business_practice_location_address_state_name,
																	provider_business_practice_location_address_postal_code,
																	healthcare_provider_taxonomy_code_1
																	from patient_share_2013_180_ca1")
df.patient.share.13$npi1 <- as.numeric(df.patient.share.13$npi1)
df.patient.share.13$npi2 <- as.numeric(df.patient.share.13$npi2)

df.patient.share.13 <- df.patient.share.13 %>% 
	arrange(npi1,npi2)

only.npi2.13 <- df.patient.share.13 %>% 
	select(npi2)

only.npi1.13 <- df.patient.share.13 %>% 
	select(npi1)
```
filter npi1 that also recorded as npi2 in CA, and match the NPPES data to npi2
```{r}
info.npi1.13 <- df.patient.share.13 %>% 
	select(npi1, entity_type_code, provider_business_practice_location_address_state_name,
				 provider_business_practice_location_address_postal_code,
				 healthcare_provider_taxonomy_code_1)
info.npi1.13 <- unique(info.npi1.13)
df.npi$npi_id <- as.numeric(df.npi$npi_id)
info.npi2.13 <- left_join(only.npi2.13, df.npi, by = c("npi2"="npi_id"))
# info.npi1.13 <- info.npi1.13[!duplicated(info.npi1.13),]
# check why the duplicated command generates a different outcome.
# info.npi2.13 <- left_join(only.npi2.13, info.npi1.13, by = c("npi2" = "npi1"))
info.npi2.13$npi1 <- df.patient.share.13$npi1
info.npi2.13$pair_count <- df.patient.share.13$pair_count
info.npi2.13$bene_count <- df.patient.share.13$bene_count
info.npi2.13$same_day_count <- df.patient.share.13$same_day_count

df.patient.share.info2.13 <- info.npi2.13 %>% 
	arrange(npi1, npi2)

tmp <- df.patient.share.info2.13 %>% 
	select(npi1,healthcare_provider_taxonomy_code_1, entity_type_code)

tmp2 <- unique(tmp)
# share.type.cnt.13 shows the types of providers an NPI1 sharing patients with.
 
share.type.cnt.13 <- tmp2 %>% 
	group_by(npi1) %>% 
	tally


tmp <- share.type.cnt.13 %>% 
	filter(n == 1)
tmp2 <- semi_join(info.npi2.13, tmp, by = "npi1")
tmp2 <- tmp2 %>% 
	arrange(npi1, npi2)

# analyze the NPI1s with only one type of NPI2
only1typeNPI2.13 <- left_join(tmp2, taxonomy, by = c("healthcare_provider_taxonomy_code_1" = "Code"))
tmp <- only1typeNPI2.13 %>% 
	group_by(npi1) %>% 
	tally %>% 
	arrange(desc(n))
tmp2 <- only1typeNPI2.13 %>% 
	filter(entity_type_code==1)
tmp3 <- tmp2 %>% 
	group_by(npi1) %>% 
	tally %>% 
	arrange(desc(n))
```

 compare 13 and 14 data
```{r}
ave.type.cnt.13 <- share.type.cnt.13 %>% # 25.10747
	summarise(ave.n = mean(n))

ave.type.cnt.14 <- share.type.cnt.14 %>% # 24.74353
	summarise(ave.n = mean(n))

com.13.14 <- inner_join(share.type.cnt.13, share.type.cnt.14, by = "npi1")
ave.type.cnt.13.14 <- com.13.14 %>% 
	summarise(ave.n.13 = mean(n.x), ave.n.14 = mean(n.y)) 
# 26.54967 in 13, 26.44592 in 14

com.13.14 <- com.13.14 %>% 
	mutate(diff = n.y-n.x, diff.rel = diff/n.x)

com.13.14 %>% 
	filter(diff<0) %>% 
	nrow()

com.13.14 %>% 
	filter(diff>0) %>% 
	nrow()

tmp <- df.patient.share.info2.13 %>% 
	group_by(npi1) %>% 
	tally
	
tmp2 <- df.patient.share.info2.14 %>% 
	group_by(npi1) %>% 
	tally

tmp3 <- inner_join(tmp, tmp2, by = "npi1")

com.13.14$cnt.13 <- tmp3$n.x
com.13.14$cnt.14 <- tmp3$n.y
com.13.14 <- com.13.14 %>% 
	mutate(cnt.diff = cnt.14-cnt.13, cnt.diff.rel = cnt.diff/cnt.13)
ave.cnt.13.14 <- com.13.14 %>% 
	summarise(ave.cnt.13 = mean(cnt.13), ave.cnt.14 = mean(cnt.14)) 
com.13.14 %>% 
	filter(cnt.diff<0) %>% 
	nrow()
com.13.14 %>% 
	filter(cnt.diff>0) %>% 
	nrow()
tmp <- com.13.14 %>% 
	filter(cnt.diff<0)

library(ggplot2)
ggplot(tmp, aes(cnt.diff.rel)) + geom_histogram()

```



