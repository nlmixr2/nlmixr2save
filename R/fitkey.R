## Stable keys for nlmixr2 fits.
##
## `:=` caches results built on a fit (`sim := rxSolve(fit, ev)`) under a hash
## of their arguments.  Serializing a fit is not stable: a fit reloaded from a
## zip differs byte-wise from the fresh one (loader fields, delayed bindings,
## tables that went through write.csv/read.csv).  These keys are built only
## from what identifies the fit and what downstream results depend on, read
## from the fit environment with get0() so nothing is forced or written.

#' Normalize a data frame so its digest survives a csv round trip
#' @noRd
.fitKeyNormDf <- function(df, digits) {
  if (is.null(df)) {
    return(NULL)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  .nm <- sort(names(df))
  .ret <- lapply(.nm, function(n) {
    .col <- df[[n]]
    if (is.numeric(.col)) signif(as.double(.col), digits) else as.character(.col)
  })
  names(.ret) <- .nm
  .ret
}

#' Round a numeric vector or matrix for hashing
#' @noRd
.fitKeySignif <- function(x, digits) {
  if (is.null(x)) {
    return(NULL)
  }
  .x <- unclass(x)
  if (!is.numeric(.x)) {
    return(.x)
  }
  .d <- dim(.x)
  .dn <- dimnames(.x)
  .n <- names(.x)
  .x <- signif(as.double(.x), digits)
  if (!is.null(.d)) {
    dim(.x) <- .d
    dimnames(.x) <- .dn
  } else {
    names(.x) <- .n
  }
  .x
}

#' Stable pieces of a model
#' @noRd
.fitKeyModel <- function(ui) {
  if (is.null(ui)) {
    return(NULL)
  }
  .norm <- tryCatch(unname(ui$mv0$model["normModel"]), error = function(e) NULL)
  .err <- tryCatch({
    .lst <- ui$lstExpr
    vapply(ui$predDf$line, function(l) deparse1(.lst[[l]]), character(1))
  }, error = function(e) NULL)
  .ini <- tryCatch(ui$iniDf, error = function(e) NULL)
  if (!is.null(.ini)) {
    .keep <- intersect(c("ntheta", "neta1", "neta2", "name", "lower", "est",
                         "upper", "fix", "err", "condition"), names(.ini))
    .ini <- as.list(.ini[, .keep, drop = FALSE])
    for (.c in intersect(c("lower", "est", "upper"), names(.ini))) {
      .ini[[.c]] <- signif(as.double(.ini[[.c]]), 12L)
    }
    .ini <- lapply(.ini, function(col) if (is.factor(col)) as.character(col) else col)
  }
  list(normModel = .norm, errLines = unname(.err), iniDf = .ini)
}

#' Identity and cache keys of an nlmixr2 fit
#'
#' `nlmixr2saveFitIdentity()` identifies *which* fit this is: the model, the
#' estimation method, the estimates and the data.  It is the same for a fit,
#' its `saveFit()`/`loadFit()` reload, and the fit after `addCwres()`.
#'
#' `nlmixr2saveFitKey()` additionally changes whenever something a downstream
#' result could depend on changes: added table columns (`addCwres()`,
#' `addNpde()`, `addTable()`), objective function rows (`setOfv()`), the
#' covariance (`preconditionFit()`) or the table values.  `:=` hashes fits on
#' its right-hand side with this key, so a result built on a fit reloaded in a
#' new session is still a cache hit.
#'
#' Neither key forces or writes anything in the fit.
#'
#' @param fit An nlmixr2 fit.
#' @return A list suitable for hashing.
#' @export
#' @author Matthew L. Fidler
nlmixr2saveFitIdentity <- function(fit) {
  .env <- fit$env
  list(
    modelKey = .fitKeyModel(get0("ui", envir = .env, inherits = FALSE)),
    est = get0("est", envir = .env, inherits = FALSE),
    fixef = .fitKeySignif(get0("fixef", envir = .env, inherits = FALSE), 12L),
    omega = .fitKeySignif(get0("omega", envir = .env, inherits = FALSE), 12L),
    dataKey = digest::digest(.fitKeyNormDf(get0("origData", envir = .env, inherits = FALSE), 12L),
                             algo = "sha256")
  )
}

#' @rdname nlmixr2saveFitIdentity
#' @param sha Include nlmixr2save's own `:=` fields (`nlmixr2save`,
#'   `nlmixr2saveOrig`)?
#' @export
nlmixr2saveFitKey <- function(fit, sha = TRUE) {
  .env <- fit$env
  .ob <- get0("objDf", envir = .env, inherits = FALSE)
  .tab <- tryCatch(as.data.frame(fit), error = function(e) NULL)
  list(
    identity = nlmixr2saveFitIdentity(fit),
    sha = if (isTRUE(sha)) {
      list(get0("nlmixr2save", envir = .env, inherits = FALSE),
           get0("nlmixr2saveOrig", envir = .env, inherits = FALSE))
    },
    names = names(fit),
    objRows = if (is.null(.ob)) NULL else rownames(.ob),
    objf = if (is.null(.ob)) NULL else signif(suppressWarnings(as.numeric(.ob$OBJF)), 10L),
    covMethod = get0("covMethod", envir = .env, inherits = FALSE),
    cov = .fitKeySignif(get0("cov", envir = .env, inherits = FALSE), 10L),
    table = digest::digest(.fitKeyNormDf(.tab, 10L), algo = "sha256")
  )
}
