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
data <- data[is.na(data$funding_total_usd) == FALSE, ]
data <- data[is.na(data$funding_rounds) == FALSE, ]
table(is.na(data$funding_rounds))

# Transforming year founded and funding information to continous variables and adjusting variable type
data <- separate(data, founded_on, sep="-", into = c("year_founded"))
data <- separate(data, first_funding_on, sep="-", into = c("first_funding"))
data <- separate(data, last_funding_on, sep="-", into = c("last_funding"))

data$year_founded <- as.numeric(data$year_founded)
data$first_funding <- as.numeric(data$first_funding)
data$last_funding <- as.numeric(data$last_funding)

# We assume that companies <10 years of age are considered as start-up
data <- data[data$year_founded >2009,]
data <- data[!is.na(data$year_founded), ]

data <- data[!is.na(data$company_name), ]
data <- data[!is.na(data$domain), ]
table(is.na(data$company_name))

data$company_age <- 2019 - data$year_founded
data$first_funding_lag <- data$first_funding - data$year_founded
data$last_funding_lag <- data$last_funding - data$first_funding 
data$lastFundingtoDate <- 2019 - data$last_funding

names(data)

df <- data
#data <- df

# Merging social media existance
data$twitter_url <- as.character(data$twitter_url)
data$facebook_url <- as.character(data$facebook_url)

social <- has_social(data)
data <- cbind(data, social)

table(data$social)
names(data)

data <- separate(data, category_group_list, sep="\\|", into = c("category"), remove = FALSE)
data <- separate(data, category_list, sep="\\|", into = c("sector"), remove = FALSE)

table(is.na(data$category))
table(data$sector)

length(unique(data$category))
length(unique(data$sector))


data <- data[ , -which(names(data) %in% c("founded_on","first_funding_on", "last_funding_on","cb_url", "twitter_url", "facebook_url", "category_list", 
                                 "category_group_list", "short_description"))]
# Checking for duplicates and cleaning them
data$unique <- paste(as.character(data$domain), as.character(data$sector),as.character(data$category), sep = "_")
length(unique(data$unique))
names(data)
table(duplicated(data$unique))
summary(data[duplicated(data$unique) == TRUE, c(1:3, 8, 10, 20)])
data$dub <- duplicated(data$domain)
table(data$dub)
data <- data[data$dub == FALSE, ]
table(duplicated(data$unique))
data <- data[!is.na(data$country_code),]
data <- data[!is.na(data$category),]

lapply(data, class)

unique(data$category)
table(is.na(data$category))

table(is.na(data$sector))

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

str(data)
data$success <- 1
data$success[as.character(data$status) == "closed"] <- 0

table(data$status, data$success)

cdf <- data[ , -which(names(data) %in% c("company_name","domain", "country_code","city", "sector", "category", "employee_count", 
                                   "unique", "country", "dub", "year_founded", "first_funding", "last_funding", "status", "cats$cat1" ))]

names(cdf)
dummies <- dummyVars(success ~ ., data = cdf )
dummy_frame <- data.frame(predict(dummies, newdata = cdf))
success_dummies <- predict(dummies, newdata = cdf)
head(dummy_frame)

# We want to find the weather the dummies have zero varince or not. Cut frequency - most frequently encountered value over most frequently encountered value
# We need 10 different appearances
near_zero <- nearZeroVar(success_dummies, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE)
near_zero[near_zero$zeroVar == TRUE | (near_zero$nzv == TRUE), ] # top 4 are status.closed, status.ipo, continent.africa and continent oceania

cdf <- filter(cdf, continent != "Oceania" & continent != "Africa")
table(cdf$continent)
dummies <- dummyVars(success ~ ., data = cdf )
success_dummies <- predict(dummies, newdata = cdf)
head(success_dummies)
names(data.frame(success_dummies))
lapply(success_dummies, colMeans)
dim(success_dummies)
class(success_dummies)
#success_dummies <- as.matrix(apply(success_dummies, 2,  as.numeric))
#success_ddf <- as.data.frame(success_dummies)

set.seed(9999)
success_factor <- as.factor(cdf[, "success"])
#################### WIP ##################
recallSummary <-  function (data, lev = NULL, model = NULL){
  out = recall(data$obs, data$pred)
  names(out) <- "recall"
}

fitControl <-  trainControl(method = "cv", number = 10, summaryFunction = recallSummary)

trGrid <-  expand.grid(alpha = c(0.2, 0.5, 0.75, 1.0), lambda = c(0.5, 0.1, 0.2, 0.3))

set.seed(9999)

cv_mod <-  train(x = success_dummies, y= success_factor, metric = "recall", method = "glmnet", trControl = fitControl, tuneGrid = trGrid)

warnings()
cv_mod

############### END WIP ################

ncdf <- as.data.frame(cbind(success = cdf$success, success_dummies))
head(ncdf)
train_inv <- sample(seq_len(nrow(ncdf)), size = 0.7*nrow(ncdf))
cor(ncdf)
plot(cor(ncdf))
library(corrplot)
corrplot(cor(ncdf), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

train <- ncdf[train_inv,]
test <- ncdf[-train_inv,]

dim(train)
dim(test)
names(train)
num_cols <- c("funding_rounds", "company_age", "first_funding_lag", "lastFundingtoDate", "funding_total_usd", "last_funding_lag")
preProcValues <- preProcess(train[,num_cols], method = c("center", "scale")) # Z-score normalization
train[,num_cols] <- predict(preProcValues, train[, num_cols]) 
test[, num_cols] <-  predict(preProcValues, test[, num_cols])
names(train)
head(train[, num_cols])
str(train)
lr <- glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate ,data = train, family = "binomial")
summary(lr)
summary(lr)$coefficients
summary(lr)$r.squared
test$probs <- predict(lr, newdata = test, type = "response")
test[1:20, c("success", "probs")]
summary(test$probs)


hist_resids(test, test$probs, label = "success")

resids_qq(test, test$probs, label = "success")

summary(test$success)
fin <- test
fin$resids <- test[, 'success'] - test$probs
summary(fin$resids)
summary(fin[fin[, "resids"] < -0.75, ])
table(test$status)

test <- score_model(test, 0.7)
test[1:20, c("success", "score")]

table(test$success)
table(test$success, test$score)

logistic.eval(test)
ROC_AUC(test)

long_col <- c("funding_rounds", "company_age", "first_funding_lag", "lastFundingtoDate", "status.acquired", "status.operating", 
              "funding_total_usd", "last_funding_lag", "social.Both", "social.Facebook", "social.Twitter", "social.None", "continentAmericas", 
              "continentAsia", "continentEurope")
preProcValues <- preProcess(train[,long_col], method = c("center", "scale")) # Z-score normalization
train[,long_col] <- predict(preProcValues, train[, long_col]) 
test[, long_col] <-  predict(preProcValues, test[, long_col])
names(train)
head(train[, long_col])
str(train)

lr_all <- glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + funding_total_usd + 
              last_funding_lag + social.Both + social.Facebook + social.Twitter + social.None + continentAmericas +  
              continentAsia + continentEurope ,data = train, family = "binomial")
summary(lr_all)
summary(lr_all)$coefficients


test$probs <- predict(lr_all, newdata = test, type = "response")
test[1:20, c("success", "probs")]
summary(test$probs)

print_metrics(lr_all, test, test$probs, label = "success")
round(summary(lr_all)$r.squared)

hist_resids(test, test$probs, label = "success")

resids_qq(test, test$probs, label = "success")

summary(test$success)
fin <- test
fin$resids <- test[, 'success'] - test$probs
summary(fin$resids)
summary(fin[fin[, "resids"] < -0.75, ])
table(test$status)


test <- score_model(test, 0.7)
test[1:20, c("success", "score")]

table(test$success)
table(test$success, test$score)

logistic.eval(test)

install.packages("ROCR")
library(ROCR)

ROC_AUC(test)
table(k$success)
table(train$success)
table(test$success)
names(train)

sum(train$succes) / length(train$success)

weights <- ifelse(train$success == 0 , 0.95, 0.05)

log_mod <- glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + funding_total_usd + 
                last_funding_lag + social.Both + social.Facebook + social.Twitter + social.None + continentAmericas +  
                continentAsia + continentEurope, data = train, family = "quasibinomial", weights = weights)
log_test <- test
log_test$probs <- predict(log_mod, newdata = test, type = "response")
summary(log_test$probs)
log_test <- score_model(log_test, 0.5)

ggplot(log_test, aes(probs)) +
  geom_histogram(binwidth = 0,5, aes(y =..density..), alpha =0.5) +
  geom_density(aes(y=..density..), color ="blue") 

log_test %>% 
  filter(success == 0) %>%
  summarise(avg = mean(probs))
  
log_test %>% 
  filter(success == 1) %>%
  summarise(avg = mean(probs))

log_test[1:20, c("success", "score", "probs")]
logistic.eval(log_test)
ROC_AUC(log_test)

install.packages("glmnet")
library(glmnet)
glm_mod_l2 <- glmnet(x = success_dummies, y = train$success, nlambda = 20, alpha = 0, family =  "gaussian") # alpha = 0 pure l2 regularization and at 1 pure l1 regularization
