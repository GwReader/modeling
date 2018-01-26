
library(caret)
library(ROCR)

pred_out = function(pred, ref, yname, cutoff = .5, positive = NULL, print.prob = T) {
  
  c.yname = as.character(yname)
  tmp_pred = factor(ifelse(pred >= cutoff, "pos", "neg"))
  tmp_ctable = caret::confusionMatrix(tmp_pred, ref[, c.yname], positive = positive) #confusion table
  
  sense = tmp_ctable$byClass[1]
  spec = tmp_ctable$byClass[2]
  acc = tmp_ctable$overall["Accuracy"]
  
  tmp_predtn = prediction(pred, ref[, c.yname])
  auc = as.numeric(performance(tmp_predtn, "auc")@y.values)
  
  final_table = data.frame(Sensitivity = sense, Specificity = spec, Accuracy = acc, AUC = auc)
  rownames(final_table) = "Model"
  
  tmp_perf = performance(tmp_predtn, "tpr","fpr")
  
  plot(tmp_perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
  lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
  legend(0.5, 0.3, legend = c("Model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)
  lab = paste("AUC = ", round(auc, 3)) ; text(0.85, 0.2, lab)
  
  return(final_table)
  
}

## example


pred.lr = predict(mod.lr, newdata = tmp_test, type = "response") 

pred_out(pred.lr, tmp_test, "y", .5, "pos")
