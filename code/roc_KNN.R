library(pROC)

roc_curve <- roc(as.numeric(test_data$position_group == "DB"), as.numeric(preds == "DB"))
auc_value <- auc(roc_curve)

roc_plot<-plot(roc_curve, main = "ROC Curve")
cat("AUC:", auc_value, "\n")
print(roc_plot)
