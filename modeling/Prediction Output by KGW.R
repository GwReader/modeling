library(caret)
library(ROCR)

pred_out = function(pred, ref, yname, positive, cutoff = .5) {
  
  c.yname = as.character(yname)
  c.pos = as.character(positive)
  
  tmp_pred = factor(ifelse(pred >= cutoff, c.pos, setdiff(levels(ref[, c.yname]), c(c.pos))))
  tmp_ctable = caret::confusionMatrix(tmp_pred, ref[, c.yname], positive = c.pos) #confusion table

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


require(ISLR)
attach(Default)

str(Default)

idx_trn = createDataPartition(Default$default, p = .8, list = F)

train = Default[idx_trn, ] ; test = Default[-idx_trn, ]

model = glm(default ~ balance + income, data = train, family = binomial(link = "logit"))


pred_train = predict(model, newdata = train, type = "response")
pred_test = predict(model, newdata = test, type = "response") 

pred_out(pred_train, train, "default", "Yes", .5)
pred_out(pred_test, test, "default", "Yes", .5)
