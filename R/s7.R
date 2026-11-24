## S7 (>= 1.0.0) exports its own `:=`, the "bind" operator: `Foo :=
## new_class()` is `Foo <- new_class(name = "Foo")`.  Whichever of S7 and
## nlmixr2save is attached last masks the other's `:=`, so nlmixr2save's
## `:=` hands calls to S7's builders over to S7's `:=` (#18).

#' S7's `:=` when the right-hand side of `:=` is a call to an S7 builder
#'
#' A call to one of S7's exported builders with a `name` argument
#' (`new_class()`, `new_generic()`, `new_external_generic()`,
#' `deprecated_class()`, ...), called as `S7::new_class()` or by any name
#' bound to it, is bound with S7's `:=`, as when S7's `:=` comes first on the
#' search path.  Other functions with a `name` argument (including S7's
#' `prop()`) are cached by nlmixr2save's `:=` as before; use `` S7::`:=` ``
#' to bind those.
#'
#' @param value the unevaluated right-hand side of `:=`
#' @param envir the environment `:=` was called from
#' @return S7's exported `:=`, or `NULL` when `value` is not a call to an S7
#'   builder or the loaded S7 has no `:=` (S7 < 1.0.0)
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveS7Bind <- function(value, envir) {
  if (!is.call(value)) {
    return(NULL)
  }
  .fun <- value[[1]]
  .colons <- list(quote(`::`), quote(`:::`))
  if (is.call(.fun) && any(vapply(.colons, identical, logical(1), .fun[[1]]))) {
    # S7::new_class() -- resolve it without evaluating anything else; this
    # loads S7 just as evaluating `S7::new_class` would
    if (
      !identical(as.character(.fun[[2]]), "S7") ||
        !requireNamespace("S7", quietly = TRUE)
    ) {
      return(NULL)
    }
    .fun <- get0(
      as.character(.fun[[3]]),
      envir = asNamespace("S7"),
      inherits = FALSE
    )
  } else if (is.name(.fun)) {
    .fun <- get0(as.character(.fun), envir = envir, mode = "function")
  } else {
    return(NULL)
  }
  # a function bound to an S7 builder means S7 is loaded already
  if (!is.function(.fun) || !isNamespaceLoaded("S7")) {
    return(NULL)
  }
  .ns <- asNamespace("S7")
  .exports <- getNamespaceExports("S7")
  if (!(":=" %in% .exports)) {
    return(NULL)
  }
  # only the builders (new_class(), new_generic(), deprecated_class(), ...);
  # prop() also has a `name` argument but is not something one binds
  .builders <- grep("^(new|deprecated)_", .exports, value = TRUE)
  for (.b in .builders) {
    .f <- get0(.b, envir = .ns, inherits = FALSE)
    if (identical(.fun, .f) && "name" %in% names(formals(.f))) {
      return(getExportedValue("S7", ":="))
    }
  }
  NULL
}
