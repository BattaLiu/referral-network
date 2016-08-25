# This rscript just play a little bit around the true data, but I need to 
# learn how to use postgresql to get a full picture of the true data.
# load the NPI data from json file
library(readr)
library(dplyr)
library(jsonlite)
city.NPI.1 <- fromJSON("CA-San.txt",flatten = TRUE)
first200 <- city.NPI.1$results$number
city.NPI.2 <- fromJSON("CA-San2.txt",flatten = TRUE)
second200 <- city.NPI.2$results$number
CA.NPI <- c(first200,second200)
CA.list <- data.frame(CA.NPI)
names(CA.list) <- "from.NPI"
CA.list$to.NPI <- CA.list$from.NPI

# load the referral data from txt file
df <- read_delim(file = "../physician-shared-patient-patterns-2009-days30.txt", 
								 col_names = c("from.NPI","to.NPI","pair.count","bene.count","same.day.count"),
								 delim = ",")
df$from.NPI <- as.integer(df$from.NPI)

from.df <- semi_join(df,CA.list,by = "from.NPI") # from.NPI is in CA San
to.df <- semi_join(df,CA.list,by = "to.NPI") # to.NPI is in CA San

from.list <- data.frame(unique(from.df$from.NPI))
names(from.list) <- "from.NPI"
to.list <- data.frame(unique(to.df$to.NPI))
names(to.list) <- "to.NPI"
full.list <- full_join(from.list,to.list, by = c("from.NPI" = "to.NPI"))
diff.list <- anti_join(from.list,to.list, by = c("from.NPI" = "to.NPI"))
common.list <- semi_join(from.list, to.list, by = c("from.NPI" = "to.NPI"))

intermediate.df <- semi_join(to.df, common.list, by = "from.NPI")

intermediate.df <- intermediate.df %>% 
	arrange(from.NPI, to.NPI)

from.count <- intermediate.df %>% 
	group_by(from.NPI) %>% 
	tally() %>% 
	arrange(desc(n))

to.count <- intermediate.df %>% 
	group_by(to.NPI) %>% 
	tally() %>% 
	arrange(desc(n))

# the NPIs with the greatest number of referrals are radiology physicians in CA San 
semi_join(head.from, head.to, by = c("from.NPI" = "to.NPI")) 

pair.sum <- intermediate.df %>% 
	group_by(to.NPI) %>% 
	summarise(pair.sum = sum(pair.count)) %>% 
	arrange(desc(pair.sum))

bene.sum <- intermediate.df %>% 
	group_by(to.NPI) %>% 
	summarise(bene.sum = sum(bene.count)) %>% 
	arrange(desc(bene.sum))

same.day.sum <- intermediate.df %>% 
	group_by(to.NPI) %>% 
	summarise(same.day.sum = sum(same.day.count)) %>% 
	arrange(desc(same.day.sum))

# take a look how correlated the most popular NPIs are, and find that they are highly correlated.
# one quarter of all possible relations are built in real life.
test <- semi_join(intermediate.df, head.from, by = "from.NPI")
test2 <- semi_join(test, head.to, by = "to.NPI")
test2 <- test2 %>% 
	arrange(desc(from.NPI))


