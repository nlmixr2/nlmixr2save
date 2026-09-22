#' Name of the search-path entry that holds nlmixr2save's `:=` ahead of
#' data.table's
#' @noRd
.nlmixr2saveAssignName <- "nlmixr2save:assign"

#' Put nlmixr2save's `:=` back in front of data.table's
#'
#' data.table exports a `:=` that only errors when called outside of
#' `DT[...]` ("Check that is.data.table(DT) == TRUE"), so attaching data.table
#' after nlmixr2save (e.g. `library(nlmixr2); library(data.table)`) turns every
#' `fit := nlmixr2(...)` into that error.  data.table itself never looks `:=`
#' up on the search path -- `[.data.table` handles it inside `j` -- so putting
#' nlmixr2save's `:=` first costs data.table nothing.
#'
#' This attaches a one-function environment holding nlmixr2save's `:=` at
#' position 2, which is ahead of data.table just after data.table's attach.
#' It does nothing when nlmixr2save is not attached, since then `:=` was never
#' going to reach nlmixr2save's anyway.
#'
#' @param pkgname,pkgpath passed by the package-event hook; ignored
#' @return invisible `TRUE` when the `:=` was re-attached, otherwise `FALSE`
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveReattachAssign <- function(pkgname, pkgpath) {
  if (!("package:nlmixr2save" %in% search())) {
    return(invisible(FALSE))
  }
  .nlmixr2saveDetachAssign()
  .env <- new.env(parent=emptyenv())
  assign(":=", get(":=", envir=asNamespace("nlmixr2save")), envir=.env)
  # base::attach() is what puts an environment on the search path; it is
  # looked up rather than called by name only so R CMD check does not flag a
  # package calling attach().  The entry is removed again when nlmixr2save is
  # unloaded.
  .attach <- get("attach", envir=baseenv())
  .attach(.env, pos=2L, name=.nlmixr2saveAssignName, warn.conflicts=FALSE)
  packageStartupMessage(
    "nlmixr2save: data.table's `:=` masked nlmixr2save's; re-attached ",
    "nlmixr2save's `:=` so `fit := nlmixr2(...)` keeps working ",
    "(data.table's `DT[, a := b]` is unaffected)")
  invisible(TRUE)
}

#' Remove the `:=` entry added by `.nlmixr2saveReattachAssign()`
#' @return invisible `TRUE` when an entry was removed
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveDetachAssign <- function() {
  .ret <- FALSE
  while (.nlmixr2saveAssignName %in% search()) {
    detach(pos=match(.nlmixr2saveAssignName, search()), character.only=TRUE)
    .ret <- TRUE
  }
  invisible(.ret)
}

.onLoad <- function(libname, pkgname) {
  .hook <- packageEvent("data.table", "attach")
  # a reloaded namespace (devtools::load_all(), unload + reload) would
  # otherwise stack a second copy of the hook
  .has <- vapply(getHook(.hook), function(f) {
    isTRUE(attr(f, "nlmixr2save"))
  }, logical(1))
  if (!any(.has)) {
    .fun <- function(pkgname, pkgpath) {
      # look the function up at call time so a reloaded namespace is used
      if (isNamespaceLoaded("nlmixr2save")) {
        asNamespace("nlmixr2save")$.nlmixr2saveReattachAssign(pkgname, pkgpath)
      }
    }
    attr(.fun, "nlmixr2save") <- TRUE
    setHook(.hook, .fun, "append")
  }
}

# No .onDetach(): detach() has already worked out which search position to
# drop when it runs that hook, so removing another entry there would shift the
# search path and make detach() drop the wrong package.  By the time
# .onUnload() runs, unloadNamespace() has finished detaching.
.onUnload <- function(libpath) {
  .nlmixr2saveDetachAssign()
}
