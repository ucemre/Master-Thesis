setwd("/Users/cemre/Desktop/Thesis/Crunchbase")
install.packages("tidyr")
install.packages("caret")
install.packages("lattice", .Library)
install.packages("countrycode")
library(countrycode)
library(dplyr)
library(tidyr)
library(lattice)
library(ggplot2)

library(caret)

source("helperFunctions.R")
test <- read.csv("20181031_crunchbase_export_firms.csv", sep = ";", na.strings=c("","NA"))
data <- read.csv("20181031_crunchbase_export_firms.csv", sep = ";", na.strings=c("","NA"))
data_bu <- data
#data <- data_bu
summary(data)
# Deleting the columns with no useful information
data <- data[ , -which(names(data) %in% c("phone","email", "closed_on", "uuid", "state_code", "region"))]


lapply(data, class)
# Adjusting vartiable types
data$funding_rounds <- as.numeric(data$funding_rounds)
data$funding_total_usd <- as.numeric(data$funding_total_usd)
# Filtering out the companis with no funding information
# initial number of obs. -> 215729
data <- data[is.na(data$funding_total_usd) == FALSE, ] # 119942 firms have no funding amount information / no funding rounds
data <- data[is.na(data$funding_rounds) == FALSE, ]
# num obs. -> 95787

# Transforming year founded and funding information to continous variables and adjusting variable type
data <- separate(data, founded_on, sep="-", into = c("year_founded"))
data <- separate(data, first_funding_on, sep="-", into = c("first_funding"))
data <- separate(data, last_funding_on, sep="-", into = c("last_funding"))


data$year_founded <- as.numeric(data$year_founded)
data$first_funding <- as.numeric(data$first_funding)
data$last_funding <- as.numeric(data$last_funding)

# We assume that companies <10 years of age are considered as start-up
data <- data[data$year_founded >2009,]
# 61431 firms are founded after 2009, 8147 of which does not have founded year information
data <- data[!is.na(data$year_founded), ]

data <- data[data$first_funding > 2010, ] #1000


data <- data[!is.na(data$company_name), ] # all firms have non-NA company names
data <- data[!is.na(data$domain), ] # 1681 firms did not provide domain information
# At this poinnt number of obs. 51163
table(is.na(data$company_name))

data$company_age <- 2019 - data$year_founded
barplot(table(data$company_age), main = 'Overview of company age', xlab = 'Age of company', ylab= 'Count', ylim = c(0, 10000))

data$first_funding_lag <- data$first_funding - data$year_founded
barplot(table(data$first_funding_lag), main = 'Overview of first funding', xlab = 'Years since establishment', ylab= 'Count', ylim = c(0, 20000))
data$last_funding_lag <- data$last_funding - data$first_funding 

barplot(table(data$last_funding_lag), main = 'Overview of last funding', xlab = 'Years since first funding', ylab= 'Count', ylim =c(0, 30000))

data$lastFundingtoDate <- 2019 - data$last_funding

barplot(table(data$last_funding), main = 'Last funding', xlab = 'Year of last funding', ylab= 'Count', ylim =c(0, 12000))

names(data)

df <- data
#data <- df

# Merging social media existance
data$twitter_url <- as.character(data$twitter_url)
data$facebook_url <- as.character(data$facebook_url)

social <- has_social(data)
data <- cbind(data, social)

table(data$social)
barplot(table(data$social), main = 'Social media existance', xlab = 'Platforms', ylab= 'Count', ylim =c(0, 35000))
names(data)

data <- data[!is.na(data$category_group_list), ] # 49541 companies states business category / industry
cats <- separate(data, category_group_list, sep="\\|", into = c("cat1", 'cat2', 'cat3', 'cat4', 'cat5', 'cat6', 
                                                                'cat7', 'cat8', 'cat9', 'cat10', 'cat11', "cat12", "cat13", "cat14", 'cat15', "cat16"), remove = FALSE)

cats <- cats[, which(names(cats) %in% c("cat1", 'cat2', 'cat3', 'cat4', 'cat5', 'cat6', 
                                                                'cat7', 'cat8', 'cat9', 'cat10', 'cat11', "cat12", "cat13", "cat14", 'cat15', "cat16"))]


cats_count <- sapply(cats, function(x) sum(!is.na(x)))
barplot(cats_count, main = 'Category Overview', xlab = 'Category number', ylab= 'Count', ylim =c(0, 50000))
length(unique(cats$cat1))

cat1_counts <- sort(table(cats$cat1), decreasing = TRUE)
cat1_counts <- as.data.frame(cat1_counts)

top_cat1 <- top_n(cat1_counts, 10)
top10_cat1 <- top_cat1[, "Freq"]
names(top10_cat1) <- top_cat1$Var1
par(mar=c(5, 11, 5, 1), bg = NA)
barplot(top10_cat1, main = 'Top 10 Primary category', xlab = 'Count', xlim =c(0, 6000), horiz = TRUE, las =2, bg = "transparent")

data <- cbind(data, cats$cat1)

#data <- separate(data, category_group_list, sep="\\|", into = c("category"), remove = FALSE)
#data <- separate(data, category_list, sep="\\|", into = c("sector"), remove = FALSE)

#table(is.na(data$category))
#table(data$sector)

#length(unique(data$category))
#length(unique(data$sector))


data <- data[ , -which(names(data) %in% c("employee_count","cb_url", "twitter_url", "facebook_url", "category_list", 
                                          "category_group_list", "short_description"))]
# Checking for duplicates and cleaning them
data$unique <- paste(as.character(data$domain), as.character(data$sector),as.character(data$category), sep = "_")
length(unique(data$unique))
names(data)
table(duplicated(data$unique))
summary(data[duplicated(data$unique) == TRUE, c(1:3, 8, 10, 17)])
data$dub <- duplicated(data$domain)
table(data$dub)
data <- data[data$dub == FALSE, ]
table(duplicated(data$unique))
df <- data
data <- data[!is.na(data$country_code),]

lapply(data, class)

length(unique(data$country_code))

df2 <-  data
#k <- BU
data$country_code <- as.character(data$country_code)

data$country_code[data$country_code == "ROM"] <-  "ROU"
data$country_code[data$country_code == "BAH"] <-  "BHS"
data$country_code[data$country_code == "TAN"] <-  "TZA"

data$country <- countrycode(data$country_code, "iso3c", "country.name")
data$continent <- countrycode(data$country_code, "iso3c", "continent")
unique(data$continent)
length(unique(data$country))
table(data$continent)

str(data)
data$success <- 1
data$success[as.character(data$status) == "closed"] <- 0

table(data$status, data$success)
table(data$status)
table(data$success)

cdf <- data[ , -which(names(data) %in% c("company_name","domain", "country_code","city", "sector", "category", "employee_count", 
                                         "unique", "country", "dub", "year_founded", "first_funding", "last_funding", "status"))]

cdf %>% group_by(success) %>% summarise(avg = mean(company_age))
names(cdf)
mean(cdf$company_age)
mean(cdf$funding_total_usd)
mean(cdf$first_funding_lag)
mean(cdf$funding_rounds)
mean(cdf$lastFundingtoDate)
length(unique(cats$cat1))
