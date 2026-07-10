#' @title Compute classification metrics
#'
#' @description
#' Compute multi-class classification metrics from predicted and true labels,
#' including accuracy, macro-F1, purity, NMI, ARI, and rare-class recall.
#'
#' @md
#' @param predicted Character vector of predicted class labels.
#' @param truth Character vector of true class labels (same length as
#'   `predicted`).
#' @param rare_threshold Proportion threshold for rare-class recall.
#'   Classes with support proportion ≤ this value contribute to the
#'   `rare_recall` metric. Default is `0.05`.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{accuracy}{Overall accuracy (scalar).}
#'   \item{macro_f1}{Macro-averaged F1 score (scalar).}
#'   \item{purity}{Cluster purity (scalar).}
#'   \item{nmi}{Normalized Mutual Information (scalar).}
#'   \item{ari}{Adjusted Rand Index (scalar).}
#'   \item{rare_recall}{Mean recall on rare classes (scalar, or `NA` if no rare classes).}
#'   \item{class_table}{A data frame of per-class precision, recall, F1,
#'     and support.}
#' }
#'
#' @export
#'
#' @examples
#' predicted <- c("A", "A", "B", "B", "C")
#' truth <- c("A", "B", "B", "B", "C")
#' classification_metrics_compute(predicted, truth)
classification_metrics_compute <- function(
  predicted,
  truth,
  rare_threshold = 0.05
) {
  predicted <- as.character(predicted)
  truth <- as.character(truth)
  keep <- !is.na(predicted) & !is.na(truth)
  predicted <- predicted[keep]
  truth <- truth[keep]

  if (length(predicted) == 0L) {
    return(list(
      accuracy = NA_real_,
      macro_f1 = NA_real_,
      purity = NA_real_,
      nmi = NA_real_,
      ari = NA_real_,
      rare_recall = NA_real_,
      class_table = NULL
    ))
  }

  classes <- sort(unique(c(predicted, truth)))
  classification_metrics(
    predicted = predicted,
    truth = truth,
    classes = classes,
    rare_threshold = rare_threshold
  )
}
