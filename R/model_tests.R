library(dplyr)

confusion_matrix <- function(truth, pred) {
  data.frame(truth, pred) %>% count(truth, pred)
}

accuracy <- function(truth, pred) {
  cm <- confusion_matrix(truth, pred)
  tptn <- sum(cm$n[cm$truth == cm$pred])
  all <- sum(cm$n)
  return(tptn/all)
}

precision <- function(truth, pred) {
  cm <- confusion_matrix(truth, pred)
  tp <- cm$n[cm$truth == TRUE & cm$pred == TRUE]
  tpfp <- cm$n[cm$pred == TRUE]
  return(tp/tpfp)
}

recall <- function(truth, pred) {
  cm <- confusion_matrix(truth, pred)
  tp <- cm$n[cm$truth == TRUE & cm$pred == TRUE]
  tpfn <- sum(cm$n[cm$truth == TRUE])
  return(tp/tpfn)
}

specificity <- function(truth, pred) {
  cm <- confusion_matrix(truth, pred)
  tn <- cm$n[cm$truth == FALSE & cm$pred == FALSE]
  tnfp <- sum(cm$n[cm$truth == FALSE])
}

f1 <- function(truth, pred) {
  precision(truth, pred) * recall(truth, pred)
}
