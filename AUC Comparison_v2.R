
#Logistic regression full
#ROC_AUC(test_long)
#mpar(5,5)
options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj_full = prediction(test_long$probs, test_long$success) 
perf_obj_full = performance(pred_obj, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC_full = performance(pred_obj_full, "auc")@y.values[[1]]

pdf("AUC_Comp_v2.pdf", width=5, height=5)
par(mar=c(5, 5, 5, 5), mfrow=c(1,1))

plot(perf_obj_full, main = "AUC Comparison between models")
par(new=T)
plot(perf_obj_reduced, col = 'blue')
par(new=T)
plot(perf_obj_rpart, col = "green")
par(new=T)
plot(perf_obj_inf, col = "blueviolet")
par(new=T)
plot(perf_obj_rf, col = "aquamarine4")
par(new=T)
plot(perf_obj_xgb,col = "chocolate1")
abline(a=0, b=1, col = "black")

dev.off()

#Logistic regression reduced

options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj_reduced = prediction(test_v2$probs, test_v2$success) 
perf_obj_reduced = performance(pred_obj_reduced, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC_reduced = performance(pred_obj_reduced, "auc")@y.values[[1]]
plot(perf_obj_reduced,main = "Logistic Regression (Reduced)")
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC_reduced,3))))

#rpart
options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj_rpart = prediction(as.numeric(pred.rp), as.numeric(test.tree$success))
perf_obj_rpart = performance(pred_obj, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC = performance(pred_obj_rpart, "auc")@y.values[[1]]
plot(perf_obj_rpart, main = "Rpart AUC")
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC,3))))

# Inference Tree
options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj_inf = prediction(ctree.predict, test$success) 
perf_obj_inf = performance(pred_obj_inf, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC = performance(pred_obj_inf, "auc")@y.values[[1]]
plot(perf_obj_inf, main = "Inference Tree")
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC,3))))

#Random Forest AUC
pred_rf <- ifelse(predTest == "S", 1, 0)
rf_label <- ifelse(test_rf$success == "S", 1, 0)
# Printing area under the curve for RF
options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj_rf = prediction(pred_rf, rf_label) 
perf_obj_rf = performance(pred_obj_rf, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC = performance(pred_obj_rf, "auc")@y.values[[1]]
plot(perf_obj_rf, main = "Random Forest")
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC,3))))

# Printing area under the curve for XGB
options(repr.plot.width =5, repr.plot.height =5) # setting initial plot area dimensions
pred_obj_xgb = prediction(xgbpred, ts_label) 
perf_obj_xgb = performance(pred_obj_xgb, measure = "tpr", x.measure = "fpr") # TPR: true positive rate, FPR; false positive rate
AUC_xgb = performance(pred_obj_xgb, "auc")@y.values[[1]]
plot(perf_obj_xgb, main = "Extreme Gradient Boosting")
abline(a=0, b=1, col = "red")
text(0.8, 0.2, paste("AUC =", as.character(round(AUC_xgb,3))))



