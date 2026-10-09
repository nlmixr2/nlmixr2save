## Events on the rxode2 event bus (see rxode2::rxEventListen()).
##
## `:=` emits `assignStart(name)` before it evaluates anything, then `assign`
## (on success) or `assignAbort` (on error) when it returns.  The evaluations
## `:=` does only to build its cache hash run inside an event scope, so a fit
## or simulation evaluated for hashing is never logged.  Everything is a
## no-op when rxode2 has no event bus.

#' @noRd
.nlmixr2saveEventBus <- function() {
  exists("rxEventEmit", envir=asNamespace("rxode2"), inherits=FALSE)
}

#' Evaluate `expr` silently on the event bus
#' @noRd
.nlmixr2saveScope <- function(expr) {
  if (!.nlmixr2saveEventBus()) {
    return(expr)
  }
  getExportedValue("rxode2", ".rxEventEnter")()
  on.exit(getExportedValue("rxode2", ".rxEventExit")(), add=TRUE)
  force(expr)
}

#' @noRd
.nlmixr2saveEmit <- function(event, ...) {
  if (.nlmixr2saveEventBus()) {
    getExportedValue("rxode2", "rxEventEmit")(event, ...)
  }
  invisible()
}

#' Number of events delivered so far (NULL without the bus)
#' @noRd
.nlmixr2saveSeq <- function() {
  if (.nlmixr2saveEventBus()) getExportedValue("rxode2", "rxEventSeq")() else NULL
}

#' Emit the end of a `:=`
#'
#' @param value The value `:=` returned, or `.saveFitEnv` (the sentinel used
#'   for "it errored").
#' @noRd
.nlmixr2saveAssignDone <- function(value, name, call, envir, sha, since) {
  if (identical(value, .saveFitEnv)) {
    .nlmixr2saveEmit("assignAbort", name=name)
    return(invisible())
  }
  .fun <- .saveFitEnv$fun
  .random <- isTRUE(.saveFitEnv$isRandom) ||
    (is.character(.fun) && length(.fun) == 1L && .fun %in% .saveFitEnv$random)
  .nlmixr2saveEmit("assign", name=name, value=value, call=call, envir=envir,
                   cached=isTRUE(.saveFitEnv$restore), sha=sha,
                   random=.random,
                   seed=if (.random) .saveFitEnv$old else NULL,
                   since=since)
  invisible()
}
