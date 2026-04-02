#' Standardize and simplify data for nlmixr2 estimation
#'
#' This function is typically not needed by end users.
#'
#' The standardization keeps columns that rxode2 and nlmixr2 use along with the
#' covariates.  Column order is standardized (rxode2 then nlmixr2 then
#' alphabetically sorted covariates), and rxode2 and nlmixr2 column names are
#' converted to lower case.
#'
#' @inheritParams nlmixr2est::nlmixr
#' @param object an nlmixr_ui object (e.g. the output of running
#'   \code{nlmixr(object = model)}
#' @returns The data with the nlmixr2 column lower case and on the left and the
#'   covariate columns on the right and alphabetically sorted.
#' @family Simplifiers
#' @author William S. Denney with minor modifications from Matt Fidler
#' @export
nlmixrDataSimplify <- function(data, object, table = list()) {
  nlmixrCols <-
    c(
      # rxode2 columns
      c("id", "time", "amt", "rate", "dur", "evid", "cmt", "dvid", "ss", "ii", "addl"),
      # nlmixr2 columns
      c("dv", "mdv", "cens", "limit")
    )
  # nlmixr pays attention to the columns in a case-insensitive way for the
  # standard columns.  Verify that the data has case-insensitive column names
  # for these columns (for example not "ADDL" and "addl").
  maskNlmixrCols <- tolower(names(data)) %in% nlmixrCols
  nlmixrNames <- names(data)[maskNlmixrCols]
  maskDuplicated <- duplicated(tolower(nlmixrNames))
  if (any(maskDuplicated)) {
    stop(
      "the following column(s) are duplicated when lower case: ",
      paste0("'", nlmixrNames[maskDuplicated], "'", collapse = ", ")
    )
  }
  if (!is.null(object$ui)) {
    covVec <- object$ui$all.covs
  } else {
    covVec <- object$all.covs
  }
  covNames <- nlmixrDataSimplifyCols(data, cols = covVec, type = "covariate")
  keepNames <- nlmixrDataSimplifyCols(data, cols = table$keep, type = "keep")
  # Simplifying the nlmixrNames column names to always be lower case ensures
  # that upper/lower case column name changes will not affect the need to rerun.
  # Also, standardizing the column name order to always be the same will prevent
  # the need to rerun, so covNames is sorted.

  # Sorting so that they are in order, unique so that duplication between
  # covariates and keep do not try to duplicate columns in the output data.
  addColNames <- sort(unique(c(covNames, keepNames)))

  # Drop names from nlmixr_names from the added names
  addColNames <- setdiff(addColNames, nlmixrNames)

  stats::setNames(
    object = data[, c(nlmixrNames, addColNames), drop = FALSE],
    nm = c(tolower(nlmixrNames), addColNames)
  )
}

nlmixrDataSimplifyCols <- function(data, cols, type) {
  missingCol <- setdiff(cols, names(data))
  if (length(missingCol) > 0) {
    stop(
      "the following ", type, " column(s) are missing from the data: ",
      paste0("'", missingCol, "'", collapse = ", ")
    )
  }
  cols
}
