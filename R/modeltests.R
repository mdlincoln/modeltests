library(dplyr)

#' Create a confusion matrix
#'
#' This function builds a confusion matrix from two logical vectors.
#' @param truth The true values
#' @param pred The values predicted by the model
#' @return A dataframe summing every combination
#' @export
#' @examples
#' t <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
#' p <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' confusion_matrix(t, p)

confusion_matrix <- function(truth, pred) {

  if(is.na(truth) | is.na(pred))
    stop("No NA vectors allowed")

  if(typeof(truth) != "logical" | typeof(pred) != "logical")
    stop("Both vectors must be of type 'logical'")

  if(length(truth) != length(pred))
    stop("Vectors must be of the same length")

  data.frame(truth, pred) %>% count(truth, pred)
}

#' Calculate accuracy
#'
#' This function calcuates the fraction of the time that the classifier
#' correctly identifies true and false values.
#' @param truth The true values
#' @param pred The values predicted by the model
#' @return numeric value
#' @export
#' @examples
#' t <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
#' p <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' accuracy(t, p)
accuracy <- function(truth, pred) {
  cm <- confusion_matrix(truth, pred)
  tptn <- sum(cm$n[cm$truth == cm$pred])
  all <- sum(cm$n)
  return(tptn/all)
}

#' Calculate precision
#'
#' This function calcuates the fraction of the items identified by the
#' classifier that actually \emph{are} in the class.
#' @param truth The true values
#' @param pred The values predicted by the model
#' @return numeric value
#' @export
#' @examples
#' t <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
#' p <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' precision(t, p)
precision <- function(truth, pred) {
  cm <- confusion_matrix(truth, pred)
  tp <- cm$n[cm$truth == TRUE & cm$pred == TRUE]
  tpfp <- cm$n[cm$pred == TRUE]
  return(tp/tpfp)
}

#' Calculate recall
#'
#' This function calcuates the fraction of the items \emph{within} the class that the
#' classifier correctly identifies.
#' @param truth The true values
#' @param pred The values predicted by the model
#' @return numeric value
#' @export
#' @examples
#' t <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
#' p <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' recall(t, p)
recall <- function(truth, pred) {
  cm <- confusion_matrix(truth, pred)
  tp <- cm$n[cm$truth == TRUE & cm$pred == TRUE]
  tpfn <- sum(cm$n[cm$truth == TRUE])
  return(tp/tpfn)
}

#' Calculate specificity
#'
#' This function calcuates the fraction of the items \emph{not} in the class that the
#' classifier correctly identifies
#' @param truth The true values
#' @param pred The values predicted by the model
#' @return numeric value
#' @export
#' @examples
#' t <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
#' p <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' specificity(t, p)
specificity <- function(truth, pred) {
  cm <- confusion_matrix(truth, pred)
  tn <- cm$n[cm$truth == FALSE & cm$pred == FALSE]
  tnfp <- sum(cm$n[cm$truth == FALSE])
}

#' Calculate F1 score
#'
#' This function calculates the product of the model's recall and precision.
#' Lower F1 scores may indicate that a model is gaining either precision or
#' recall at the expense of the complementary score.
#' @param truth The true values
#' @param pred The values predicted by the model
#' @return numeric value
#' @export
#' @examples
#' t <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
#' p <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' F1(t, p)
F1 <- function(truth, pred) {
  precision(truth, pred) * recall(truth, pred)
}
