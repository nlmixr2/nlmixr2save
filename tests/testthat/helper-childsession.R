# The data.table and S7 attach hooks are exercised in a fresh R session
# (data.table or S7 may already be attached in this one), which loads the
# *installed* nlmixr2save.
# Under devtools::test() that can be an older build than the source being
# tested, so only run those tests when the loaded namespace is the installed
# one (always true under R CMD check).
# Run a command in a child R process that sees the same library paths as this
# one (a runtime .libPaths() change is not inherited), so it loads the same
# nlmixr2save build.  R_LIBS is set in this process rather than through
# system2(env=), which only works on Unix.
.withLibs <- function(cmd, args) {
  withr::with_envvar(
    c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE))
  )
}

# Run an R script in a fresh session
.runScript <- function(script) {
  .withLibs(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script))
  )
}

.skipIfNotInstalledBuild <- function() {
  .installed <- tryCatch(
    find.package("nlmixr2save", lib.loc = .libPaths()),
    error = function(e) ""
  )
  .loaded <- getNamespaceInfo("nlmixr2save", "path")
  skip_if_not(
    identical(
      normalizePath(.installed, mustWork = FALSE),
      normalizePath(.loaded, mustWork = FALSE)
    ),
    "the loaded nlmixr2save is not the installed build"
  )
}
