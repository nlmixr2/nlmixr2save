#' Standardize and simplify data for nlmixr2 estimation
#'
#' This function is typically not needed by end users.
#'
#' The standardization keeps columns that rxode2 and nlmixr2 use along with the
#' covariates.  Column order is standardized (rxode2 then nlmixr2 then
#' alphabetically sorted covariates), and rxode2 and nlmixr2 column names are
#' converted to lower case.
#'
#' Some estimation methods take their covariates from the data instead of from
#' the model; \code{est="vae"} searches the data for covariates to select.  For
#' those methods the covariate columns the search can pick from are kept as
#' well, since dropping them would change the fit.
#'
#' @inheritParams nlmixr2est::nlmixr
#' @param object an nlmixr_ui object (e.g. the output of running
#'   \code{nlmixr(object = model)}
#' @returns The data with the nlmixr2 column lower case and on the left and the
#'   covariate columns on the right and alphabetically sorted.
#' @family Simplifiers
#' @author William S. Denney with minor modifications from Matt Fidler
#' @export
nlmixrDataSimplify <- function(data, object, table = list(), est = NULL,
                               control = NULL) {
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
  # Covariates the estimation method finds in the data rather than in the model
  searchNames <- .nlmixrDataSimplifySearchCols(data, est = est, control = control)
  # Simplifying the nlmixrNames column names to always be lower case ensures
  # that upper/lower case column name changes will not affect the need to rerun.
  # Also, standardizing the column name order to always be the same will prevent
  # the need to rerun, so covNames is sorted.

  # Sorting so that they are in order, unique so that duplication between
  # covariates and keep do not try to duplicate columns in the output data.
  addColNames <- sort(unique(c(covNames, keepNames, searchNames)))

  # Drop names from nlmixr_names from the added names
  addColNames <- setdiff(addColNames, nlmixrNames)

  stats::setNames(
    object = data[, c(nlmixrNames, addColNames), drop = FALSE],
    nm = c(tolower(nlmixrNames), addColNames)
  )
}

#' Covariate columns an estimation method searches for in the data
#'
#' `est="vae"` selects its covariates from the data instead of taking them from
#' the model, so every column the search could pick has to survive the
#' simplification; dropping one would change the fit (and the data hash).
#' `nlmixr2est::vaeCovariates()` is what the search itself uses to decide which
#' columns are covariates, so it is asked here too.
#'
#' It is looked up dynamically rather than called as `nlmixr2est::vaeCovariates()`
#' because 'nlmixr2est' is a suggested package and older versions do not export
#' it -- a hard `::` reference would make `R CMD check` fail against those.  When
#' it is unavailable this returns nothing, which is the old behavior.
#'
#' @param data data that will be given to the estimation method
#' @param est estimation method (as in [nlmixr2est::nlmixr()])
#' @param control control for `est`; the covariate-search options are matched so
#'   the columns kept here are the columns the fit would consider
#' @return character vector of column names in `data`, possibly empty
#' @noRd
#' @author Matthew L. Fidler
.nlmixrDataSimplifySearchCols <- function(data, est = NULL, control = NULL) {
  if (!(is.character(est) && length(est) == 1L && est == "vae")) return(character(0))
  # covariateSelection=FALSE turns the search off, so nothing extra is needed
  if (isFALSE(control$covariateSelection)) return(character(0))
  if (!requireNamespace("nlmixr2est", quietly = TRUE)) return(character(0))
  .fun <- try(getExportedValue("nlmixr2est", "vaeCovariates"), silent = TRUE)
  if (inherits(.fun, "try-error") || !is.function(.fun)) return(character(0))
  # Match the search to the control so the kept columns are the considered
  # columns; ignore anything this version of vaeCovariates() does not take.
  .args <- list(data = data, warn = FALSE)
  for (.n in c("shapes", "covCenterType", "covCenter", "catCutoff")) {
    if (!is.null(control[[.n]])) .args[[.n]] <- control[[.n]]
  }
  .args <- .args[names(.args) %in% names(formals(.fun))]
  .cov <- try(suppressWarnings(do.call(.fun, .args)), silent = TRUE)
  if (inherits(.cov, "try-error") || is.null(.cov$raw)) return(character(0))
  # vaeCovariates() upper cases the data names, so map back to the names as they
  # are actually spelled in the data
  .w <- match(toupper(unique(as.character(.cov$raw))), toupper(names(data)))
  names(data)[.w[!is.na(.w)]]
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
