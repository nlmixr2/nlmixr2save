#' Re-attach nlmixr2save in front of data.table
#'
#' data.table exports a `:=` that only errors when called outside of
#' `DT[...]` ("Check that is.data.table(DT) == TRUE"), so attaching data.table
#' after nlmixr2save (e.g. `library(nlmixr2); library(data.table)`) turns every
#' `fit := nlmixr2(...)` into that error.  data.table itself never looks `:=`
#' up on the search path -- `[.data.table` handles it inside `j` -- so putting
#' nlmixr2save back in front costs data.table nothing.
#'
#' This detaches `package:nlmixr2save` and re-attaches it (as `library()`
#' does) directly in front of data.table.  It does nothing when nlmixr2save is
#' not attached, since then `:=` was never going to reach nlmixr2save's, or
#' when nlmixr2save is already in front of data.table.
#'
#' @param pkgname,pkgpath passed by the package-event hook; ignored
#' @return invisible `TRUE` when nlmixr2save was re-attached, otherwise
#'   `FALSE`
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveReattach <- function(pkgname, pkgpath) {
  .search <- search()
  .us <- match("package:nlmixr2save", .search)
  .dt <- match("package:data.table", .search)
  if (is.na(.us) || is.na(.dt) || .us < .dt ||
        .nlmixr2saveConflictsError()) {
    return(invisible(FALSE))
  }
  # library(nlmixr2save, include.only=/exclude=) attaches only some exports;
  # re-attach exactly those.  Without `:=` among them nothing needs moving.
  .attached <- ls(as.environment(.us), all.names=TRUE)
  if (!(":=" %in% .attached)) {
    return(invisible(FALSE))
  }
  # Anything besides exports means this was not attached by library() --
  # pkgload::load_all() attaches the internal functions too -- and
  # attachNamespace() cannot recreate that, so leave it alone.
  if (length(setdiff(.attached, getNamespaceExports("nlmixr2save")))) {
    return(invisible(FALSE))
  }
  # force: an attached package that Depends on nlmixr2save makes detach()
  # stop ("required by ... so will not be detached"), and the hook's try()
  # would swallow that, leaving `:=` broken.  nlmixr2save is back on the
  # search path immediately, so the "may no longer work correctly" warning
  # that force gives instead does not apply.
  suppressWarnings(detach(pos=.us, force=TRUE))
  # nlmixr2save sat below data.table, so detaching it left data.table's
  # position unchanged; attaching there puts nlmixr2save just in front of it.
  # Anything that sat between the two now sits below nlmixr2save too, which
  # is unavoidable if nlmixr2save is to come before data.table.
  tryCatch({
    attachNamespace("nlmixr2save", pos=.dt, include.only=.attached)
  }, error=function(e) {
    warning("could not re-attach nlmixr2save in front of data.table; use ",
            "nlmixr2save::`:=` (", conditionMessage(e), ")", call.=FALSE)
    # never leave the package detached, even if back behind data.table
    tryCatch(attachNamespace("nlmixr2save", pos=.us,
                             include.only=.attached), error=function(e2) {
      warning("could not re-attach nlmixr2save; call library(nlmixr2save) (",
              conditionMessage(e2), ")", call.=FALSE)
    })
  })
  if ("package:nlmixr2save" %in% search()[seq_len(.dt)]) {
    packageStartupMessage(
      "nlmixr2save: re-attached in front of data.table so ",
      "`fit := nlmixr2(...)` keeps working ",
      "(data.table's `DT[, a := b]` is unaffected)")
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

#' Whether `library()` turns masking conflicts into errors
#'
#' With `options(conflicts.policy=)` of `"strict"`, `"depends.ok"` or a list
#' with `error=TRUE` (the cases `library()` treats as errors),
#' `library(data.table)` checks conflicts after the attach hooks have run and,
#' on a conflict, detaches whatever sits at the position it attached
#' data.table to.  Once nlmixr2save has moved into that position, that would
#' be nlmixr2save, leaving data.table attached; so under this policy the
#' search path is left for `library()` to resolve.
#' @return boolean
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveConflictsError <- function() {
  .p <- getOption("conflicts.policy")
  if (is.character(.p)) return(length(.p) == 1L &&
                                .p %in% c("strict", "depends.ok"))
  is.list(.p) && isTRUE(.p$error)
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
        asNamespace("nlmixr2save")$.nlmixr2saveReattach(pkgname, pkgpath)
      }
    }
    attr(.fun, "nlmixr2save") <- TRUE
    setHook(.hook, .fun, "append")
  }
}
