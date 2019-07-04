setwd("/Users/cemre/Desktop/Thesis/Crunchbase")
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
library(xgboost)
library(randomForest)

# Splitting train and test data sets
train_inv <- sample(seq_len(nrow(ncdf)), size = 0.7*nrow(ncdf))
train <- ncdf[train_inv,]
test <- ncdf[-train_inv,]

#Oversampling the unsuccessful companies to deal with imbalanced data in train and test datasets
train <- oversample(train, ratio=0.75, method = "ADASYN", filtering=FALSE, classAttr= "success", wrapper="KNN")
dim(train)
test <- oversample(test, ratio=0.75, method = "ADASYN", filtering=FALSE, classAttr= "success", wrapper="KNN")
dim(test)

# Implementing logistic regression
cor(ncdf)
par(mar=c(5, 5, 5, 5), mfrow=c(1,1))
corrplot(cor(ncdf), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#with balanced data
test_red <- test
train_red <- train
red_cols <- c("funding_rounds", "funding_total_usd","company_age", "first_funding_lag",  "last_funding_lag", "lastFundingtoDate")

preProcValues <- preProcess(train_red[,red_cols], method = c("center", "scale")) # Z-score normalization
train_red[,red_cols] <- predict(preProcValues, train_red[, red_cols]) 
test_red[, red_cols] <-  predict(preProcValues, test_red[, red_cols])
names(train_red)
head(train_red[, red_cols])
str(train_red)

lr <- glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate + 
            funding_total_usd ,data = train_red, family = "binomial")

summary(lr)
summary(lr)$coefficients

test_red$probs <- predict(lr, newdata = test_red, type = "response")

test_red[1:20, c("success", "probs")]

summary(test_red$probs)
boxplot(test_red$probs)
hist_resids(test_red, test_red$probs, label = "success")

resids_qq(test_red, test_red$probs, label = "success") # residual qq plot points to bimodal distribution

summary(test_red$success)
fin <- test_red
fin$resids <- test_red[, 'success'] - test_red$probs
plot(fin$resids)
summary(fin$resids)
summary(fin[fin[, "resids"] < -0.5, ])

test_red <- score_model(test_red, 0.6)
test_red[1:20, c("success", "score")]

logistic.eval(test_red)
par(bg="white")
ROC_AUC(test_red)
#par(mfrow = c(2, 2), bg='white')
#plot(lr)

names(test)

test_red$bins <- ifelse(test_red$probs < 0.1, "0- 0.1", 
                    ifelse(test_red$probs < 0.2 & test_red$probs > 0.1, "0.1-0.2",
                           ifelse(test_red$probs < 0.3 & test_red$probs > 0.2, "0.2-0.3",
                                  ifelse(test_red$probs < 0.4 & test_red$probs > 0.3, "0.3-0.4",
                                         ifelse(test_red$probs < 0.5 & test_red$probs > 0.4, "0.4-0.5",
                                                ifelse(test_red$probs < 0.6 & test_red$probs > 0.5, "0.5-0.6",
                                                       ifelse(test_red$probs < 0.7 & test_red$probs > 0.6, "0.6-0.7",
                                                              ifelse(test_red$probs < 0.8 & test_red$probs > 0.7, "0.7-0.8",
                                                                     ifelse(test_red$probs < 0.9 & test_red$probs > 0.8, "0.8-0.9","0.9-1")))))))))

table(test_red$success, test_red$bins)


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
train_long <- train
test_long <- test

names(train)
long_col <- c("funding_rounds", "company_age", "first_funding_lag", "lastFundingtoDate", "funding_total_usd", "last_funding_lag", 
              "social.Both", "social.Facebook", "social.Twitter", "social.None", "continent.Americas", 
              "continent.Asia", "continent.Europe", "sector.comm_serv", "sector.cons_disc", "sector.cons_stap", "sector.financials", 
              "sector.health", "sector.it")

preProcValues <- preProcess(train[,long_col], method = c("center", "scale")) # Z-score normalization
train_long[,long_col] <- predict(preProcValues, train_long[, long_col]) 
test_long[, long_col] <-  predict(preProcValues, test_long[, long_col])
head(train_long[, long_col])
str(train_long)

lr_all <- glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + funding_total_usd + 
                last_funding_lag + social.Both + social.Facebook + social.Twitter + social.None + continent.Americas +  
                continent.Asia + continent.Europe + sector.comm_serv + sector.cons_disc + sector.cons_stap + sector.financials 
              + sector.health + sector.it  ,data = train_long, family = "binomial")

summary(lr_all)
lr_all_coef <- summary(lr_all)$coefficients

write.csv(as.data.frame(lr_all_coef), file = "long_lr_coef.csv")

test_long$probs <- predict(lr_all, newdata = test_long, type = "response")
test_long[1:20, c("success", "probs")]
summary(test_long$probs)

hist_resids(test_long, test_long$probs, label = "success")

resids_qq(test_long, test_long$probs, label = "success")

test_long <- score_model(test_long, 0.6)
test_long[1:20, c("success", "score")]

logistic.eval(test_long)
ROC_AUC(test_long)
confusionMatrix(table(test_long$score, test_long$success))

table(test_long$success)
#removing NA variables - indicates  linear dependency
train_v2 <- train
test_v2 <- test
lr_all_v2 <- glm(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + funding_total_usd + 
                   social.Both + social.Facebook + social.Twitter + continent.Americas +  
                   continent.Asia  + sector.comm_serv + sector.cons_disc + sector.cons_stap + sector.financials 
                 + sector.health   ,data = train_v2, family = "binomial")

summary(lr_all_v2)
#taking the significant indep variables from v2
lr_all_v3 <- glm(success ~ funding_rounds + company_age +  lastFundingtoDate  + funding_total_usd + 
                   social.Both + social.Facebook + social.Twitter + continent.Americas +  
                   sector.comm_serv + sector.cons_disc  + sector.cons_stap + sector.health,
                 data = train_v2, family = "binomial")

summary(lr_all_v3)

lr_all_v3_coeff <- summary(lr_all_v3)$coefficients

write.csv(as.data.frame(lr_all_v3_coeff), file = "lr_all_v3_coeff.csv")

test_v2$probs <- predict(lr_all_v3, newdata = test_v2, type = "response")
test_v2[1:20, c("success", "probs")]
summary(test_v2$probs)

hist_resids(test_v2, test_v2$probs, label = "success")

hist(test_v2$probs)

resids_qq(test_v2, test_v2$probs, label = "success")

test_v2 <- score_model(test_v2, 0.603)
test_v2[1:20, c("success", "score")]

logistic.eval(test_v2)
par(mfrow=c(1,1), bg="white")
ROC_AUC(test_v2) # from 0.81 to 0.83 with balanced data
names(train)
names(test)
sum(train$succes) / length(train$success) # 95% of the firms are "successful"

test_v2 %>% 
  filter(success == 0) %>%
  summarise(avg = mean(probs))
#average predicted success probability for closed firms is 37.81%

test_v2 %>% 
  filter(success == 1) %>%
  summarise(avg = mean(probs))

#average predicted success probability for successful firms is 71.29%
#we do not need to use the normalized values for the decision trees
train.tree <- train
test.tree <- test

sf.rp <- rpart(success ~ funding_rounds + company_age + first_funding_lag + lastFundingtoDate  + 
                 social.Both + social.Facebook + social.Twitter+ continent.Americas +continent.Asia+ 
                 sector.cons_disc + sector.cons_stap+sector.health , data = train.tree, 
               control=rpart.control(minsplit=2, minbucket = 2, cp=0.001), method = "class")
sf.rp$method

sf.rp$variable.importance
summary(sf.rp)
sf.rp$splits
par(mfrow = c(1, 1), bg='white')
plot(sf.rp, margin = 0.1, uniform = TRUE, branch = 0.6)
text(sf.rp, all=TRUE, use.n = TRUE)
pred.rp <- predict(sf.rp, test.tree, type="class")
head(pred.rp)
summary(pred.rp)
#pred.rp <- ifelse(pred.rp > 0.603, 1, 0)
table(test.tree$success, pred.rp)
confusionMatrix(table(pred.rp, test.tree$success))

options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj = prediction(pred.rp, test.tree$success) 
perf_obj = performance(pred_obj, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC = performance(pred_obj, "auc")@y.values[[1]]
plot(perf_obj, main = "Rpart AUC")
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC,3))))

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(sf.rp, box.palette="RdBu", shadow.col="gray", nn=TRUE)

# Pruning the recursice partioning tree - doesnt change the initial split
#Finding the min cross-validation error of the reg. tree model
min(sf.rp$cptable[, "xerror"]) # 0.141085
# locating the record with the min cross-validation errors
which.min(sf.rp$cptable[, "xerror"]) # 25
# getting the cost complexity parameter
sf.cp <- sf.rp$cptable[25, "CP"]
sf.cp # 0.001

prune.tree <-  prune(sf.rp, cp= sf.cp)
plot(prune.tree, margin = 0.0001)
text(prune.tree, all = TRUE, use.n = TRUE)
pred.rpp <- predict(prune.tree, test.tree, type= "class")
pred.rpp <- ifelse(pred.rpp >0.7, 1, 0)
table(test.tree$success, pred.rpp)
confusionMatrix(table(pred.rpp, test$success))

# Building a conditional inference tree
new_train <- train
new_train$success <- ifelse(new_train$success == 1, "S", "B")
new_train$success <- as.factor(new_train$success)
names(new_train)
ctree.model <-  ctree(success ~ . , data = new_train)
ctree.model
new_test <- test

varimp(ctree.model)

#plot(ctree.model)
ctree.predict <- predict(ctree.model, new_test)
table(ctree.predict, new_test$success)
ctree.predict <- ifelse(ctree.predict == "S", 1, 0)
confusionMatrix(table(ctree.predict, new_test$success))

options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj = prediction(ctree.predict, test$success) 
perf_obj = performance(pred_obj, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC = performance(pred_obj, "auc")@y.values[[1]]
plot(perf_obj)
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC,3))))


# XGB Application

labels <- train$success
ts_label <- test$success

new_tr <- model.matrix(~.+0,data = train[,-1 ]) 
new_ts <- model.matrix(~.+0,data = test[,-1 ])

dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, 
               subsample=1, colsample_bytree=1)


# finding the optimal number of iteration rounds ( stops if the error doen not decrease in 20 consecutive iterations)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 200, nfold = 5, showsd = T, stratified = T, 
                 print_every_n = 10, early_stopping_rounds= 20, maximize = F)

# best iteration 67

min(xgbcv$evaluation_log$test_error_mean) # 0.030589
# therefore CV accuracy is 1-0.030589 = 0.9694

xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 98, watchlist = list(val=dtest,train=dtrain), 
                   print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "error")
#model prediction
xgbpred <- predict (xgb1,dtest)
summary(xgbpred)
xgbpred <- ifelse (xgbpred > 0.603,1,0)

xgbpred_df <-  as.data.frame(xgbpred)
ts_label_df <- as.data.frame(ts_label)

table(ts_label)
table(xgbpred)

confusionMatrix(table(xgbpred, ts_label))
length(ts_label)

# Printing area under the curve for XGB
options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj = prediction(xgbpred, ts_label) 
perf_obj = performance(pred_obj, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC = performance(pred_obj, "auc")@y.values[[1]]
plot(perf_obj)
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC,3))))

mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])

# Feature Selection
names(cdf)
cf1 <- cforest(success ~ . , data = ncdf, control=cforest_unbiased(mtry=2, ntree = 50)) # fitting the random forest
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

# Implementing random forest

train_rf <- train
train_rf$success <- ifelse(train_rf$success == 1, "S", "F")
train_rf$success <- as.factor(train_rf$success)
test_rf <- test
test_rf$success <- ifelse(test_rf$success == 1, "S", "F")
test_rf$success <- as.factor(test_rf$success)

head(train_rf)
model1 <- randomForest(success ~ ., data = train_rf, importance = TRUE)
model1

model2 <- randomForest(success ~ ., data = train_rf, ntree = 500, mtry = 6, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, train_rf, type = "class")

# Checking classification accuracy
table(predTrain, train_rf$success)
confusionMatrix(table(predTrain, train_rf$success))

# Predicting on test set
predTest <- predict(model2, test_rf, type = "class")

table(predTest, test_rf$success)
confusionMatrix(table(predTest, test_rf$success))

mean(predTest == test_rf$success)

# To check important variables in model2
importance(model2)        
varImpPlot(model2) 

model3 <- randomForest(success ~ ., data = train_rf, ntree = 500, mtry = 5, importance = TRUE)
model3

# Predicting on train set
predTest <- predict(model3, test_rf, type = "class")

table(predTest, test_rf$success)
confusionMatrix(table(predTest, test_rf$success))

importance(model3)        
varImpPlot(model3) 

model4 <- randomForest(success ~ ., data = train_rf, ntree = 500, mtry = 4, importance = TRUE)
model4

# Predicting on train set
predTest <- predict(model4, test_rf, type = "class")

table(predTest, test_rf$success)
confusionMatrix(table(predTest, test_rf$success))

pred_rf <- ifelse(predTest == "S", 1, 0)
rf_label <- ifelse(test_rf$success == "S", 1, 0)
# Printing area under the curve for RF
options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj = prediction(pred_rf, rf_label) 
perf_obj = performance(pred_obj, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC = performance(pred_obj, "auc")@y.values[[1]]
plot(perf_obj)
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC,3))))

