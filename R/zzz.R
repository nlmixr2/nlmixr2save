## Packages whose attached `:=` masks nlmixr2save's when they are attached
## after it.  data.table's `:=` only errors outside `DT[...]` (#8); S7's
## (>= 1.0.0) names a class or generic after its variable, which nlmixr2save's
## `:=` hands on to S7's (see .nlmixr2saveS7Bind(), #18).
.nlmixr2saveMaskers <- c("data.table", "S7")

#' Re-attach nlmixr2save in front of a package that masks its `:=`
#'
#' data.table exports a `:=` that only errors when called outside of
#' `DT[...]` ("Check that is.data.table(DT) == TRUE"), so attaching data.table
#' after nlmixr2save (e.g. `library(nlmixr2); library(data.table)`) turns every
#' `fit := nlmixr2(...)` into that error.  data.table itself never looks `:=`
#' up on the search path -- `[.data.table` handles it inside `j` -- so putting
#' nlmixr2save back in front costs data.table nothing.
#'
#' S7 (>= 1.0.0) exports a `:=` that turns `x := f(...)` into
#' `x <- f(..., name = "x")`, so attaching S7 after nlmixr2save turns
#' `fit := nlmixr2(...)` into a call with an unused `name` argument.
#' nlmixr2save's `:=` hands calls to S7's builders (`Foo := new_class()`) to
#' S7's `:=`, so putting nlmixr2save in front keeps both working.
#'
#' This detaches `package:nlmixr2save` and re-attaches it (as `library()`
#' does) directly in front of the package just attached.  It does nothing
#' when nlmixr2save is not attached, since then `:=` was never going to reach
#' nlmixr2save's, when nlmixr2save is already in front, or when the package
#' was attached without a `:=` of its own (`library(data.table,
#' exclude=":=")`, or the `conflictRules()` S7 sets up for data.table).
#'
#' @param pkgname the package just attached (`"data.table"` or `"S7"`), as
#'   passed by the package-event hook
#' @param pkgpath passed by the package-event hook; ignored
#' @return invisible `TRUE` when nlmixr2save was re-attached, otherwise
#'   `FALSE`
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveReattach <- function(pkgname, pkgpath) {
  .search <- search()
  .us <- match("package:nlmixr2save", .search)
  .dt <- match(paste0("package:", pkgname), .search)
  if (is.na(.us) || is.na(.dt) || .us < .dt || .nlmixr2saveConflictsError()) {
    return(invisible(FALSE))
  }
  # attached without a `:=` (excluded, or an S7 too old to export one):
  # it masks nothing, so nothing needs moving
  if (!exists(":=", envir = as.environment(.dt), inherits = FALSE)) {
    return(invisible(FALSE))
  }
  # library(nlmixr2save, include.only=/exclude=) attaches only some exports;
  # re-attach exactly those.  Without `:=` among them nothing needs moving.
  .attached <- ls(as.environment(.us), all.names = TRUE)
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
  suppressWarnings(detach(pos = .us, force = TRUE))
  # nlmixr2save sat below the masking package, so detaching it left that
  # package's position unchanged; attaching there puts nlmixr2save just in
  # front of it.  Anything that sat between the two now sits below
  # nlmixr2save too, which is unavoidable if nlmixr2save is to come first.
  tryCatch(
    {
      attachNamespace("nlmixr2save", pos = .dt, include.only = .attached)
    },
    error = function(e) {
      warning(
        "could not re-attach nlmixr2save in front of ",
        pkgname,
        "; use nlmixr2save::`:=` (",
        conditionMessage(e),
        ")",
        call. = FALSE
      )
      # never leave the package detached, even if back behind data.table
      tryCatch(
        attachNamespace("nlmixr2save", pos = .us, include.only = .attached),
        error = function(e2) {
          warning(
            "could not re-attach nlmixr2save; call library(nlmixr2save) (",
            conditionMessage(e2),
            ")",
            call. = FALSE
          )
        }
      )
    }
  )
  if ("package:nlmixr2save" %in% search()[seq_len(.dt)]) {
    packageStartupMessage(
      "nlmixr2save: re-attached in front of ",
      pkgname,
      " so ",
      "`fit := nlmixr2(...)` keeps working ",
      if (identical(pkgname, "S7")) {
        "(S7's `Foo := new_class()` still binds S7 classes and generics)"
      } else {
        "(data.table's `DT[, a := b]` is unaffected)"
      }
    )
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
  if (is.character(.p)) {
    return(
      length(.p) == 1L &&
        .p %in% c("strict", "depends.ok")
    )
  }
  is.list(.p) && isTRUE(.p$error)
}

.onLoad <- function(libname, pkgname) {
  for (.pkg in .nlmixr2saveMaskers) {
    .nlmixr2saveSetHook(.pkg)
  }
}

#' Register the re-attach hook for one masking package
#'
#' @param pkg package whose attach should move nlmixr2save in front of it
#' @return nothing, called for its side effect
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveSetHook <- function(pkg) {
  .hook <- packageEvent(pkg, "attach")
  # a reloaded namespace (devtools::load_all(), unload + reload) would
  # otherwise stack a second copy of the hook
  .has <- vapply(
    getHook(.hook),
    function(f) {
      isTRUE(attr(f, "nlmixr2save"))
    },
    logical(1)
  )
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
  invisible()
}
