library(dplyr)
library(tidyr)

#' Create a confusion matrix
#'
#' This function builds a confusion matrix from two logical vectors.
#' @param truth A logical vector of true values
#' @param pred A logical vector of values predicted by the model
#' @return A dataframe summing every combination
#' @export
#' @examples
#' t <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
#' p <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' confusion_matrix(t, p)

confusion_matrix <- function(truth, pred) {

  if(any(is.na(truth), is.na(pred)))
    stop("No NA vectors allowed")

  if(any(typeof(truth) != "logical", typeof(pred) != "logical"))
    stop("Both vectors must be of type 'logical'")

  if(length(truth) != length(pred))
    stop("Vectors must be of the same length")

  data.frame(truth, pred) %>% count(truth, pred)
}

#' Calculate accuracy
#'
#' This function calcuates the fraction of the time that the classifier
#' correctly identifies true and false values.
#' @param truth A logical vector of true values
#' @param pred A logical vector of values predicted by the model
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
#' @param truth A logical vector of true values
#' @param pred A logical vector of values predicted by the model
#' @return numeric value
#' @export
#' @examples
#' t <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
#' p <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' precision(t, p)
precision <- function(truth, pred) {
  cm <- confusion_matrix(truth, pred)
  tp <- cm$n[cm$truth == TRUE & cm$pred == TRUE]
  tpfp <- sum(cm$n[cm$pred == TRUE])
  return(tp/tpfp)
}

#' Calculate recall
#'
#' This function calcuates the fraction of the items \emph{within} the class that the
#' classifier correctly identifies.
#' @param truth A logical vector of true values
#' @param pred A logical vector of values predicted by the model
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
#' @param truth A logical vector of true values
#' @param pred A logical vector of values predicted by the model
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
  tn/tnfp
}

#' Calculate F1 score
#'
#' This function calculates the product of the model's recall and precision.
#' Lower F1 scores may indicate that a model is gaining either precision or
#' recall at the expense of the complementary score.
#' @param truth A logical vector of true values
#' @param pred A logical vector of values predicted by the model
#' @return numeric value
#' @export
#' @examples
#' t <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
#' p <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' F1(t, p)
F1 <- function(truth, pred) {
  precision(truth, pred) * recall(truth, pred)
}

#' Create a summary table of model performance measures
#'
#' This function returns a data frame of model accuracy, precision, recall,
#' specificity and F1 value.
#' @param truth A logical vector of true values
#' @param pred A logical vector of values predicted by the model
#' @param name An atomic character vector that will be appended to the output
#'   table, useful if you will be joining multipe summary tables
#' @param gathered A boolean value. If TRUE, returns a 'tidy' dataframe with a
#'   column of values, and one row per measure type. If FALSE, returns a one-row
#'   data frame with one column for each measure type.
#' @return A data frame with performance measure values
#' @export
#' @examples
#' t <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
#' p <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' summary_table(t, p, gathered = FALSE)
summary_table <- function(truth, pred, name = NULL, gathered = TRUE) {
  measures <- data.frame(
    accuracy = accuracy(truth, pred),
    precision = precision(truth, pred),
    recall = recall(truth, pred),
    specificity = specificity(truth, pred)
  ) %>%
    mutate(F1 = precision * recall)

  if(!is.null(name))
    measures <- measures %>% mutate(name = name)

  if(gathered)
    measures <- measures %>% gather(measure, value, accuracy:F1)

  return(measures)
}
