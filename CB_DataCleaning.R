#install.packages("tidyr")
#install.packages("caret")
#install.packages("lattice", .Library)
#install.packages("countrycode")
#install.package("ROCR")
library(countrycode)
library(dplyr)
library(tidyr)
library(lattice)
library(ggplot2)
library(corrplot)
library(caret)
library(ROCR)
library(rpart)
library(party)
library(imbalance)

source("helperFunctions.R")

data <- read.csv("20181031_crunchbase_export_firms.csv", sep = ";", na.strings=c("","NA"))
# n=215729, 23 variables 
data_bu <- data
table(data_bu$status)
#acquired    closed       ipo operating 
#76496      8522     13412    117299 

#data <- data_bu
summary(data)
# Deleting the columns with no useful information
data <- data[ , -which(names(data) %in% c("phone","email", "state_code", "region"))]

# Filtering out the companis with no funding information
data <- data[!is.na(data$funding_total_usd), ]
# 215729 - 95787 = 119942 firms do not have funding amount information
data <- data[!is.na(data$funding_rounds), ]
table(is.na(data$funding_total_usd))
table(is.na(data$funding_rounds))
# Adjusting vartiable types
data$funding_rounds <- as.numeric(data$funding_rounds)
data$funding_total_usd <- as.numeric(data$funding_total_usd)

# Transforming year founded and funding information to continous variables and adjusting variable type
data <- separate(data, founded_on, sep="-", into = c("year_founded"))
data <- separate(data, first_funding_on, sep="-", into = c("first_funding"))
data <- separate(data, last_funding_on, sep="-", into = c("last_funding"))
data <- separate(data, closed_on, sep="-", into = c("closing_date"))

data$year_founded <- as.numeric(data$year_founded)
data$first_funding <- as.numeric(data$first_funding)
data$last_funding <- as.numeric(data$last_funding)
data$closing_date <- as.numeric(data$closing_date)

table(data$closing_date)

# We assume that companies <10 years of age are considered as start-up
data <- data[data$year_founded >2009,] # only 61430 firms are founded after 2009

data <- data[!is.na(data$year_founded), ] # 53287 firms have year founded information

data <- data[!is.na(data$company_name), ] # 53287 firms have company name information
data <- data[!is.na(data$domain), ] # 51606 of the companies stated domains
table(is.na(data$company_name))

data$company_age <- 2019 - data$year_founded
data$first_funding_lag <- data$first_funding - data$year_founded
data$last_funding_lag <- data$last_funding - data$first_funding 
data$lastFundingtoDate <- 2019 - data$last_funding
data$closing_age <- data$closing_date - data$year_founded + 1 # will be deleted after descriptives

table(is.na(data$closing_age), data$status)
data_bu <- data
#data <- data_bu

# closed companies with missing closing date information are assumed to go out of business within the same year of founding. Therefore  
data$closing_age <- ifelse(data$status == "closed" & is.na(data$closing_age), 1, data$closing_age)
# companies which are acquired, ipod or still operating are assigned closing age of 0
data$closing_age <- ifelse(data$status != "closed" , 0, data$closing_age)

table(data$closing_age, data$status)
table(is.na(data$closing_age))
names(data)

df <- data
#data <- df

# Merging social media existance
data$twitter_url <- as.character(data$twitter_url)
data$facebook_url <- as.character(data$facebook_url)

social <- has_social(data)
data <- cbind(data, social)

table(data$social)

df <- data
#data <- df

# Processing different business industries

table(is.na(data$category_group_list)) # 628 of the firms did not state any category

data <- data[!is.na(data$category_group_list), ] # 50978 companies states business category / industry

cats <- separate(data, category_group_list, sep="\\|", into = c("cat1", 'cat2', 'cat3', 'cat4', 'cat5', 'cat6', 
                                                                'cat7', 'cat8', 'cat9', 'cat10', 'cat11', "cat12", "cat13", "cat14", 'cat15', "cat16"), remove = FALSE)

cats <- cats[, which(names(cats) %in% c("cat1", 'cat2', 'cat3', 'cat4', 'cat5', 'cat6', 
                                        'cat7', 'cat8', 'cat9', 'cat10', 'cat11', "cat12", "cat13", "cat14", 'cat15', "cat16"))]


cats_count <- sapply(cats, function(x) sum(!is.na(x)))
#cat1  cat2  cat3  cat4  cat5  cat6  cat7  cat8  cat9 cat10 cat11 cat12 cat13 cat14 cat15 cat16 
#50978 44789 32677 19396 10354  5227  2510  1147   475   199    68    30    14    10     3     1
barplot(cats_count, main = 'Category Overview', xlab = 'Category number', ylab= 'Count', ylim =c(0, 50000))
length(unique(cats$cat1)) # 43 unique categories
length(unique(cats$cat2))

cat1_counts <- sort(table(cats$cat1), decreasing = TRUE)
cat1_counts <- as.data.frame(cat1_counts)

top_cat1 <- top_n(cat1_counts, 10) # commerce and shopping (5431), apps (4576) and financial services ( 3687) are top 3 most freq industries
top10_cat1 <- top_cat1[, "Freq"]
names(top10_cat1) <- top_cat1$Var1
par(mar=c(5, 11, 5, 1))
barplot(top10_cat1, main = 'Top 10 Primary category', xlab = 'Count', xlim =c(0, 6000), horiz = TRUE, las =2, bg = "transparent")

data <- cbind(data, cats$cat1)
colnames(data)[colnames(data) == "cats$cat1"] <- "category"

levels(data$category)

# Mapping categories to S&P 500 industries
data$sector <- ifelse(data$category == "advertising" | data$category == "content and publishing" | # commercial services
                        data$category == "design" | data$category == "events" | 
                        data$category == "internet services" | data$category == "media and entertainment" |  
                        data$category == "messaging and telecommunications" | data$category == "mobile" | 
                        data$category == "professional services", "comm_serv", 
                ifelse(data$category == "administrative services" | data$category == "clothing and apparel" | #consumer discretionary
                       data$category == "commerce and shopping" | data$category == "consumer goods" | 
                       data$category == "gaming" | data$category == "travel and tourism" | data$category == "sales and marketing" , "cons_disc", 
                ifelse(data$category == "community and lifestyle" | data$category == "consumer electronics" | 
                       data$category == "education" | data$category == "food and beverage" , "cons_stap", # consumer staples
                ifelse(data$category == "energy" | data$category == "sustainability" , "energy",  # energy
                ifelse(data$category == "financial services" | data$category == "payments","financials", # financials
                ifelse(data$category == "agriculture and farming" | data$category == "biotechnology"|
                       data$category == "health care" | data$category == "sports","health", # health
                ifelse(data$category == "government and military" | data$category == "manufacturing"|
                      data$category == "science and engineering" | data$category == "transportation","industrials", # industrials
                ifelse(data$category == "apps" | data$category == "artificial intelligence"| data$category == "data and analytics"|
                       data$category == "hardware" | data$category == "information technology" |
                       data$category == "navigation and mapping" | data$category == "platforms" |data$category == "privacy and security" |
                       data$category == "software" ,"it", # IT
                ifelse(data$category == "real estate","realestate", # real estate
                ifelse(data$category == "natural resources","utilities", "")))))))))) # utilities

table(data$sector)
data$sector <- as.factor(data$sector)
table(data$sector, data$status)

df <- data
names(data)

data <- data[ , -which(names(data) %in% c("founded_on","first_funding_on", "last_funding_on","closing_date", "cb_url", "twitter_url", "facebook_url", "category_list", 
                                 "category_group_list", "short_description", "category"))]
df <- data

# Checking for duplicates and cleaning them
length(unique(data$domain)) # there are 56 repeating domains

table(duplicated(data$domain), data$sector)

dup_domains <- data[duplicated(data$domain) == TRUE, "domain"]

dubs <- data[data$domain %in% dup_domains, "sector" ]

data$unique <- paste(as.character(data$domain), as.character(data$sector), sep = "_")

length(unique(data$unique)) # 50962 -> 16 duplicates
table(duplicated(data$unique), data$sector)

data <- data[!duplicated(data$unique), ] # 16 duplicates are removed
table(duplicated(data$unique))
names(data)

df2 <-  data

#data <- df2
data$country_code <- as.character(data$country_code)
data <- data[!is.na(data$country_code), ] # 1436 companies do not have location information

data$country_code[data$country_code == "ROM"] <-  "ROU"
data$country_code[data$country_code == "BAH"] <-  "BHS"
data$country_code[data$country_code == "TAN"] <-  "TZA"

data$country <- countrycode(data$country_code, "iso3c", "country.name")
data$continent <- as.factor(countrycode(data$country_code, "iso3c", "continent"))
unique(data$continent)
length(unique(data$country))
table(is.na(data$country_code))
table(is.na(data$continent))

str(data)

df2 <- data

data$success <- 1
data$success[as.character(data$status) == "closed"] <- 0

table(data$status, data$success)
names(data)
cdf <- data[ , -which(names(data) %in% c("company_name","domain", "country_code","city", "employee_count", 
                                   "unique", "country", "dub", "year_founded", "first_funding", "last_funding", 
                                   "status", "cats$cat1", "uuid", "closed_on", "closing_date" ))]

names(cdf)
head(cdf)

dummies <- dummyVars(success ~ ., data = cdf )
dummy_frame <- data.frame(predict(dummies, newdata = cdf))
success_dummies <- predict(dummies, newdata = cdf)
head(success_dummies)

# We want to find the weather the dummies have zero varince or not. Cut frequency - most frequently encountered value over most frequently encountered value
# We need 10 different appearances
near_zero <- nearZeroVar(success_dummies, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE)
near_zero[near_zero$zeroVar == TRUE | (near_zero$nzv == TRUE), ] # top 3 are closing_age, continent.africa and continent oceania

cdf <- cdf[cdf$continent != "Oceania" & cdf$continent != "Africa", ]
cdf$continent <- droplevels(cdf$continent)
table(cdf$continent)
cdf <- cdf[cdf$sector != "energy" & cdf$sector != "industrials" & cdf$sector != "utilities" & cdf$sector != "realestate", ]
cdf$sector <- droplevels(cdf$sector)
table(cdf$sector)
names(cdf)
lapply(cdf, class)
dummies <- dummyVars(success ~ ., data = cdf )
success_dummies <- predict(dummies, newdata = cdf)
head(success_dummies)

success_dummies <- as.data.frame(success_dummies)

head(success_dummies)
str(success_dummies)
#success_dummies <- as.matrix(apply(success_dummies, 2,  as.numeric))
#success_ddf <- as.data.frame(success_dummies)


ncdf <- as.data.frame(cbind(success = cdf$success, success_dummies))
ncdf <- ncdf[, -which(names(ncdf) %in% c("closing_age"))]
names(cdf)
names(ncdf)

#Oversampling the unsuccessful companies to deal with imbalanced data
balanced.df <- oversample(ncdf, ratio=0.75, method = "ADASYN", filtering=FALSE, classAttr= "success", wrapper="KNN")
names(balanced.df)
dim(balanced.df)

write.csv(balanced.df, file = 'startup_balanced.csv')
write.csv(cdf, file = 'startup_cleaned.csv')



# Implementing logistic regression
cor(ncdf)
corrplot(cor(ncdf), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#with balanced data
train_inv <- sample(seq_len(nrow(balanced.df)), size = 0.7*nrow(balanced.df))
train <- balanced.df[train_inv,]
test <- balanced.df[-train_inv,]
dim(train)
dim(test)
names(train)
red_cols <- c("funding_rounds", "funding_total_usd","company_age", "first_funding_lag",  "last_funding_lag", "lastFundingtoDate")

preProcValues <- preProcess(train[,red_cols], method = c("center", "scale")) # Z-score normalization
train[,red_cols] <- predict(preProcValues, train[, red_cols]) 
test[, red_cols] <-  predict(preProcValues, test[, red_cols])
names(train)
head(train[, red_cols])
str(train)

lr <- glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate + 
          funding_total_usd ,data = train, family = "binomial")

summary(lr)
summary(lr)$coefficients

test$probs <- predict(lr, newdata = test, type = "response")

test[1:20, c("success", "probs")]

summary(test$probs)
boxplot(test$probs)
hist_resids(test, test$probs, label = "success")

resids_qq(test, test$probs, label = "success") # residual qq plot points to bimodal distribution

summary(test$success)
fin <- test
fin$resids <- test[, 'success'] - test$probs
plot(fin$resids)
summary(fin$resids)
summary(fin[fin[, "resids"] < -0.5, ])

test <- score_model(test, 0.6)
test[1:20, c("success", "score")]

table(test$success)
table(test$success, test$score)

logistic.eval(test)
par(bg="white")
ROC_AUC(test)
#par(mfrow = c(2, 2), bg='white')
#plot(lr)

names(test)

test$bins <- ifelse(test$probs < 0.1, "0- 0.1", 
                    ifelse(test$probs < 0.2 & test$probs > 0.1, "0.1-0.2",
                    ifelse(test$probs < 0.3 & test$probs > 0.2, "0.2-0.3",
                    ifelse(test$probs < 0.4 & test$probs > 0.3, "0.3-0.4",
                    ifelse(test$probs < 0.5 & test$probs > 0.4, "0.4-0.5",
                    ifelse(test$probs < 0.6 & test$probs > 0.5, "0.5-0.6",
                    ifelse(test$probs < 0.7 & test$probs > 0.6, "0.6-0.7",
                    ifelse(test$probs < 0.8 & test$probs > 0.7, "0.7-0.8",
                    ifelse(test$probs < 0.9 & test$probs > 0.8, "0.8-0.9","0.9-1")))))))))

table(test$success, test$bins)

# 0- 0.1 0.1-0.2 0.2-0.3 0.3-0.4 0.4-0.5 0.5-0.6 0.6-0.7 0.7-0.8 0.8-0.9 0.9-1
# 0    481    1842    1438    1388    1469    1128     930     640     252    60
# 1    128     565     486     803     886     984    1139    1861    3150  3192

### logit regression using caret package
ncdf$success_factor <- as.factor(ncdf$success)
train.n <- createDataPartition(ncdf$success_factor, p=0.70, list = FALSE)
training <- ncdf[train.n, ]
table(training$success)
testing <- ncdf[-train.n, ]
table(testing$success)

mod_fit <- train(success_factor ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate + 
                   funding_total_usd, data = training, method = "glm", family = "binomial")
summary(mod_fit)
exp(coef(mod_fit$finalModel))

testing$pred <- predict(mod_fit, newdata = testing)

table(testing$pred, testing$success_factor)

# Logistic regression with fullmodel
names(train)
long_col <- c("funding_rounds", "company_age", "first_funding_lag", "lastFundingtoDate", "funding_total_usd", "last_funding_lag", 
              "social.Both", "social.Facebook", "social.Twitter", "social.None", "continent.Americas", 
              "continent.Asia", "continent.Europe", "sector.comm_serv", "sector.cons_disc", "sector.cons_stap", "sector.financials", 
              "sector.health", "sector.it")

preProcValues <- preProcess(train[,long_col], method = c("center", "scale")) # Z-score normalization
train[,long_col] <- predict(preProcValues, train[, long_col]) 
test[, long_col] <-  predict(preProcValues, test[, long_col])
names(train)
names(test)
head(train[, long_col])
str(train)

lr_all <- glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + funding_total_usd + 
              last_funding_lag + social.Both + social.Facebook + social.Twitter + social.None + continent.Americas +  
              continent.Asia + continent.Europe + sector.comm_serv + sector.cons_disc + sector.cons_stap + sector.financials 
              + sector.health + sector.it  ,data = train, family = "binomial")

summary(lr_all)
summary(lr_all)$coefficients

test$probs <- predict(lr_all, newdata = test, type = "response")
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

test <- score_model(test, 0.6)
test[1:20, c("success", "score")]

table(test$success)
table(test$success, test$score)

logistic.eval(test)

#removing NA variables - indicates  linear dependency
lr_all_v2 <- glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + funding_total_usd + 
                social.Both + social.Facebook + social.Twitter + continent.Americas +  
                continent.Asia  + sector.comm_serv + sector.cons_disc + sector.cons_stap + sector.financials 
              + sector.health   ,data = train, family = "binomial")

summary(lr_all_v2)
#taking the significant indep variables from v2
lr_all_v3 <- glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + funding_total_usd + 
                   social.Both + social.Facebook + social.Twitter + continent.Americas +  
                   continent.Asia + sector.cons_disc + sector.cons_stap+ sector.health,
                   data = train, family = "binomial")

summary(lr_all_v3)

test$probs <- predict(lr_all_v3, newdata = test, type = "response")
test[1:20, c("success", "probs")]
summary(test$probs)

hist_resids(test, test$probs, label = "success")

resids_qq(test, test$probs, label = "success")

fin <- test
fin$resids <- test[, 'success'] - test$probs
summary(fin$resids)
summary(fin[fin[, "resids"] < -0.75, ])

test <- score_model(test, 0.603)
test[1:20, c("success", "score")]

table(test$success, test$score)
logistic.eval(test)
par(mfrow=c(1,1), bg="white")
ROC_AUC(test) # from 0.81 to 0.83 with balanced data
names(train)
names(test)
sum(train$succes) / length(train$success) # 95% of the firms are "successful"

test %>% 
  filter(success == 0) %>%
  summarise(avg = mean(probs))
#average predicted success probability for closed firms is 87.74%
  
test %>% 
  filter(success == 1) %>%
  summarise(avg = mean(probs))

#average predicted success probability for successful firms is 95.58%
#we do not need to use the normalized values for the decision trees
train.tree <- balanced.df[train_inv,]
test.tree <- balanced.df[-train_inv,]

sf.rp <- rpart(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + 
                        social.Both + social.Facebook + social.Twitter+ continent.Americas +continent.Asia+ 
                        sector.cons_disc + sector.cons_stap+sector.health , data = train.tree, 
                        control=rpart.control(minsplit=2, minbucket=2, cp=0.001))

sf.rp
par(mfrow = c(1, 1), bg='white')
plot(sf.rp, margin = 0.1, uniform = TRUE, branch = 0.6)
text(sf.rp, all=TRUE, use.n = TRUE)
pred.rp <- predict(sf.rp, test, type="vector")
summary(pred.rp)
pred.rp <- ifelse(pred.rp > 0.603, 1, 0)
table(test$success, pred.rp)
confusionMatrix(table(pred.rp, test$success))

# Pruning the recursice partioning tree - doesnt change the initial split
#Finding the min cross-validation error of the reg. tree model
min(sf.rp$cptable[, "xerror"]) # 0.2005301
# locating the record with the min cross-validation errors
which.min(sf.rp$cptable[, "xerror"]) # 46
# getting the cost complexity parameter
sf.cp <- sf.rp$cptable[46, "CP"]
sf.cp # 0.001

prune.tree <-  prune(sf.rp, cp= sf.cp)
plot(prune.tree, margin = 0.0001)
text(prune.tree, all = TRUE, use.n = TRUE)
pred.rpp <- predict(prune.tree, test, type= "vector")
pred.rpp <- ifelse(pred.rpp >0.7, 1, 0)
table(test$success, pred.rpp)
confusionMatrix(table(pred.rp, test$success))

# Building a conditional inference tree
new_train <- train
new_train$success <- ifelse(new_train$success == 1, "S", "B")
new_train$success <- as.factor(new_train$success)
names(new_train)
ctree.model <-  ctree(success ~ . , data = new_train)
ctree.model
#plot(ctree.model)
ctree.predict <- predict(ctree.model, test)
table(ctree.predict, test$success)


# Feature Selection
names(cdf)
cf1 <- cforest(success ~ . , data = balanced.df, control=cforest_unbiased(mtry=2, ntree = 50)) # fitting the random forest
varImport <- varimp(cf1) # getting the variable importance, based on mean decrease in accuracy
importance <- varImp(lr_all)
importance
#PCA decomposition
names(balanced.df)
pca.df <- balanced.df[, -which(names(ncdf) %in% c("success"))]
pca.df <- balanced.df[, which(names(ncdf) %in% c("funding_rounds",  "company_age", "first_funding_lag", "lastFundingtoDate",
                                                 "funding_total_usd","social.Both", "social.Facebook", "social.Twitter",
                                                  "continent.Americas", "continent.Asia", "sector.comm_serv", "sector.cons_disc",
                                                 "sector.cons_stap", "sector.health"))]

names(pca.df)

train_inv.pca <- sample(seq_len(nrow(pca.df)), size = 0.7*nrow(pca.df))
train.pca <- pca.df[train_inv.pca,]
test.pca <- pca.df[-train_inv.pca,]

preProcValues <- preProcess(pca.df, method = c("center", "scale")) # Z-score normalization

train.pca<- predict(preProcValues, train.pca) 
test.pca<-  predict(preProcValues, test.pca)
head(train.pca)

pca <- prcomp(train.pca, center = TRUE, scale = TRUE)
summary(pca)
names(pca)
pca$center
pca$scale
pca$rotation
dim(pca$x)
#biplot(pca, scale = 0)
std.dev <- pca$sdev
pr.var <- std.dev^2
pr.var
prop.var <- pr.var /sum(pr.var)
par(bg="white")
plot(prop.var, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
#cumulative scree plot
plot(cumsum(prop.var), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

rm(list = ls())
options(java.parameters = '-Xmx10000m')

train.data <- data.frame(success = train$success, pca$x)
train.data <- train.data[, 1:3]
head(train.data)
rpart.model <- rpart(success ~ ., data=train.data)

test.data <- predict(pca, newdata=test.pca)
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:3]
head(test.data)
rpart.pred <- predict(rpart.model, test.data)
table(rpart.pred)

#Scree test to determinet he number of principle components to use
screeplot(pca, type="line")
# the obvious change is slope occus at components 1 and component 2

fit.fact <- factanal(pca.df, 3, rotation="varimax", scores = "regression")
print(fit.fact, digits=2, cutoff=.3, sort=TRUE)
load <- fit.fact$loadings[, 1:2]
#plot(load, type="n")
text(load, labels=names(pca.df), cex=0.7)

library(nFactors)
ev <- eigen(cor(pca.df))
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpa)
plotnScree(nS) # non-graphical solution to scree test

#Kaiser method to determine the number of principle components -> selection criteria: retain eigenvalues greater than 1
pca$sdev # stddev from pca
pca$sdev^2 # variance from pca
which(pca$sdev^2 > 1)
screeplot(pca, type="line")
abline(h=1, col="red", lty=3) 
plot(pca$x[,1], pca$x[,2], xlim=c(-5,5))
text(pca$x[,1], pca$x[,2], rownames(pca$x), cex=0.7, pos=4, col="red")
#biplot(pca)

library(FactoMineR)
hcpc.df <- as.data.frame(pca$x)
res.hcpc <- HCPC(hcpc.df, nb.clust=0, iter.max = 10, min=5)
head(res.hcpc$data.clust, 10)

#Implementing MFA
mfa.df <- pca.df
library(factoextra)
res.mfa <- MFA(mfa.df, 
               group = c(4, 2, 3, 4, 2), 
               type = c("s", "s", "s", "s", "s"),
               name.group = c("funding_age","fundinglangs","social",
                              "sector", "continent"),
               graph = FALSE)
print(res.mfa)

eig.val <- get_eigenvalue(res.mfa)
head(eig.val)
fviz_screeplot(res.mfa)
par(bg="white")
fviz_mfa_var(res.mfa, "group")
quanti.var <- get_mfa_var(res.mfa, "quanti.var")
quanti.var 
fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")

fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")
fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")
fviz_mfa_ind(res.mfa, 
             habillage = "success", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE )# Avoid text overlapping

library("factoextra")
# Partition clustering
fviz_nbclust(pca$x[,1:2], kmeans, method = "gap_stat")
km.res <- kmeans(pca$x[,1:2], 2, nstart = 25)
fviz
fviz_cluster(km.res, data = pca$x[,1:2], 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())

# Hierarchical Clustering
res.dist <- dist(pca$x[,1:2], method = "euclidean")
res.hc <- hclust(d = res.dist, method = "ward.D2")

summary(res.dist)
