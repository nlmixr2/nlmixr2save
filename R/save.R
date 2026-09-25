.saveFitEnv <- new.env(parent = emptyenv())
.saveFitEnv$rowDF <- c("parFixedDf", "shrink", "time", "objDf", "parFixed", "iniDf0")
# items the loader builds only on first use (see .nlmixr2saveLoaderText): the
# model lists, which compile every model, and the ui, whose rebuild from the
# model function parses the whole model
.saveFitEnv$lazy <- c("foceiModel", "saemModel", "ui")
.saveFitEnv$DF <- c("ranef", "etaObf", "origData", "parHistData")
.saveFitEnv$parent <- NULL
.saveFitEnv$random <- c("rxSolve", "simulate", "sim", "mrgsim",
                        "predict", "vpcSim")
.saveFitEnv$isRandom <- FALSE
.saveFitEnv$fun <- ""
.saveFitEnv$restore <- FALSE

.minfo <- function (text, ..., .envir = parent.frame()) {
  .opt <- getOption("nlmixr2save.quiet", FALSE)
  if (checkmate::testLogical(.opt,
                             any.missing=FALSE, len=1) &&
        .opt) {
    return(invisible())
  }
  cli::cli_alert_info(gettext(text), ..., .envir = .envir)
}

#' `:=` cache location and behavior options
#'
#' The `:=` operator saves and reloads a fit/simulation to disk.  Three
#' `options()` control where and how it caches, mirroring
#' `getOption("nlmixr2save.quiet")`:
#'
#' - `nlmixr2save.dir` (default `"."`): the directory the cache files live in.
#' - `nlmixr2save.prefix` (default `""`): a string prepended to the assigned
#'   variable name to form the cache file base, e.g. a prefix of
#'   `"modelPiping-"` and `fit := nlmixr2(...)` caches to
#'   `modelPiping-fit.zip`.
#' - `nlmixr2save.check` (default `TRUE`): when `TRUE`, `:=` verifies that the
#'   cached fit matches the current model/data/arguments (the historical
#'   behavior) and refits when it does not.  When `FALSE`, `:=` simply loads the
#'   cache file if it exists and otherwise runs and saves it -- the cache is
#'   trusted and only regenerated when it is missing (see
#'   [nlmixr2saveInvalidate()]).  This keeps a committed cache stable across
#'   nlmixr2/rxode2 versions.
#'
#' @return the option value (`""`/`"."`/`TRUE` when unset)
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveDir <- function() {
  .d <- getOption("nlmixr2save.dir", ".")
  if (!checkmate::testString(.d)) "." else .d
}
.nlmixr2savePrefix <- function() {
  .p <- getOption("nlmixr2save.prefix", "")
  if (!checkmate::testString(.p, na.ok=FALSE)) "" else .p
}
.nlmixr2saveCheck <- function() {
  isTRUE(getOption("nlmixr2save.check", TRUE))
}
#' Whether to check the nlmixr2est/rxode2 version when a fit is loaded
#'
#' Controlled by `getOption("nlmixr2save.checkVersion", TRUE)`.  When `FALSE`,
#' `loadFit()` and `:=` load cached fits without comparing package versions.
#' @return boolean (default `TRUE`)
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveCheckVersion <- function() {
  isTRUE(getOption("nlmixr2save.checkVersion", TRUE))
}
#' Whether to store the original dataset when a fit is saved
#'
#' Controlled by `getOption("nlmixr2save.data", TRUE)`.  When `FALSE`,
#' `saveFit()` omits `origData` from the zip (see [nlmixr2saveShare()]).
#' @return boolean (default `TRUE`)
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveData <- function() {
  isTRUE(getOption("nlmixr2save.data", TRUE))
}

#' Packages whose version/sha is tracked in a saved fit
#'
#' A drift in any of these between save and load can change the fit, so it is
#' worth telling the user about: nlmixr2est estimates the model and rxode2
#' compiles/solves it.
#' @noRd
.nlmixr2savePkgs <- c("nlmixr2est", "rxode2")

#' Version/sha of an installed package
#'
#' Records the version and, when installed from a remote (e.g. GitHub via
#' `remotes`/`pak`), the commit sha so a saved fit knows exactly which build
#' produced it.
#' @param pkg package name
#' @return list with `version` and `sha` (both `NA_character_` when the package
#'   is not installed / not a remote build)
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2savePkgVer <- function(pkg) {
  if (!requireNamespace(pkg, quietly=TRUE)) {
    return(list(version=NA_character_, sha=NA_character_))
  }
  .d <- utils::packageDescription(pkg)
  .sha <- .d$RemoteSha
  if (is.null(.sha) || !nzchar(.sha)) .sha <- .d$GithubSHA1
  if (is.null(.sha) || !nzchar(.sha)) .sha <- NA_character_
  list(version=as.character(utils::packageVersion(pkg)),
       sha=.sha)
}

#' Restore-script lines that give `iniDf0`'s columns back their types
#'
#' `read.csv()` infers each column's type, so an all-`NA` column comes back
#' logical and a character column of numbers comes back numeric.  The types
#' are taken from the fit being saved, so a column rxode2 adds later (such as
#' the character `prior` column) keeps its type too; a fit without an
#' `iniDf0` data frame falls back to the columns rxode2 has always had.
#'
#' @param fit the fit being saved
#' @return a string of R code, one assignment per column
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveIniDf0Types <- function(fit) {
  .ini <- NULL
  if (!is.null(fit) && exists("iniDf0", envir=fit$env, inherits=FALSE)) {
    .lazy <- get0("..nlmixr2saveLazy..", envir=fit$env, inherits=FALSE)
    # a loaded fit's iniDf0 not yet used is a promise whose repair builds the
    # ui; do not force it to read its types -- it is saved as read, and the
    # loader repairs it again
    if (!(is.list(.lazy) && !is.null(.lazy[["iniDf0"]]) &&
            .nlmixr2saveIsPromise("iniDf0", fit$env))) {
      .ini <- get("iniDf0", envir=fit$env)
    } else if (is.character(.lazy[["typesOfIniDf0"]])) {
      # the exact coercion the loaded cache used
      return(.lazy[["typesOfIniDf0"]])
    }
  }
  .types <- if (is.data.frame(.ini)) {
    vapply(.ini, function(x) {
      if (is.integer(x)) "integer"
      else if (is.double(x)) "double"
      else if (is.logical(x)) "logical"
      else if (is.character(x)) "character"
      else NA_character_
    }, character(1))
  } else {
    c(ntheta="integer", neta1="double", neta2="double", name="character",
      lower="double", upper="double", est="double", fix="logical",
      label="character", backTransform="character", condition="character",
      err="character")
  }
  .types <- .types[!is.na(.types)]
  .col <- paste0("env$iniDf0[[", vapply(names(.types), deparse1, character(1)),
                 "]]")
  .ret <- paste0(.col, " <- as.", .types, "(", .col, ")\n", collapse="")
  if (!is.data.frame(.ini)) {
    # newer rxode2 only; an all-NA column reads back from the csv as logical
    .ret <- paste0(.ret, "if (!is.null(env$iniDf0$prior)) ",
                   "env$iniDf0$prior <- as.character(env$iniDf0$prior)\n")
  }
  .ret
}

#' Metadata stored alongside a saved fit
#'
#' Deterministic (no timestamps) so a committed cache stays byte-stable.
#' @return named list of the tracked packages' version/sha, plus the
#'   nlmixr2save version
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveMeta <- function() {
  .ret <- stats::setNames(lapply(.nlmixr2savePkgs, .nlmixr2savePkgVer),
                          .nlmixr2savePkgs)
  .ret$nlmixr2save <- as.character(utils::packageVersion("nlmixr2save"))
  .ret
}

#' Normalize a stored per-package entry to a `list(version, sha)`
#'
#' Tolerates a bare version string (an earlier metadata shape) as well as a
#' missing entry.
#' @param x stored entry (list, character, or `NULL`)
#' @return `list(version, sha)`
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveNormVer <- function(x) {
  if (is.null(x)) return(list(version=NA_character_, sha=NA_character_))
  if (is.character(x)) return(list(version=x[1], sha=NA_character_))
  list(version=if (is.null(x$version)) NA_character_ else x$version,
       sha=if (is.null(x$sha)) NA_character_ else x$sha)
}

#' Human-readable version label for one tracked package
#' @param meta metadata list (or `NULL`)
#' @param pkg package name
#' @return a string like `"6.2.0"`, `"6.2.0 (abcdef1234)"`, or `"(unknown)"`
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2savePkgLabel <- function(meta, pkg) {
  .m <- .nlmixr2saveNormVer(if (is.null(meta)) NULL else meta[[pkg]])
  if (is.na(.m$version)) return("(unknown)")
  if (!is.na(.m$sha) && nzchar(.m$sha)) {
    paste0(.m$version, " (", substr(.m$sha, 1, 10), ")")
  } else {
    .m$version
  }
}

#' Does one tracked package's stored version/sha differ from what is installed?
#' @param stored,current normalized `list(version, sha)` entries
#' @return boolean; `FALSE` when either side is unknown (nothing to compare)
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2savePkgDiffers <- function(stored, current) {
  .s <- .nlmixr2saveNormVer(stored)
  .c <- .nlmixr2saveNormVer(current)
  if (is.na(.s$version) || is.na(.c$version)) return(FALSE)
  if (!identical(.s$version, .c$version)) return(TRUE)
  # same version string but a different remote sha still means a different build
  if (!is.na(.s$sha) && nzchar(.s$sha) && !is.na(.c$sha) && nzchar(.c$sha)) {
    return(!identical(.s$sha, .c$sha))
  }
  FALSE
}

#' Tracked packages whose version/sha changed between save and now
#' @param stored metadata recorded when the fit was saved (or `NULL`)
#' @param current metadata from `.nlmixr2saveMeta()` (defaults to now)
#' @return character vector of package names (empty when nothing to report)
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveChanged <- function(stored, current=.nlmixr2saveMeta()) {
  if (is.null(stored)) return(character(0))
  .nlmixr2savePkgs[vapply(.nlmixr2savePkgs, function(p) {
    .nlmixr2savePkgDiffers(stored[[p]], current[[p]])
  }, logical(1))]
}

#' Does any tracked package differ between the stored metadata and now?
#'
#' Returns `FALSE` (no complaint) when there is nothing to compare: no stored
#' metadata (fit saved by an older nlmixr2save), or the packages are not
#' installed.
#' @param stored metadata recorded when the fit was saved (or `NULL`)
#' @param current metadata from `.nlmixr2saveMeta()` (defaults to now)
#' @return boolean
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveMetaDiffers <- function(stored, current=.nlmixr2saveMeta()) {
  length(.nlmixr2saveChanged(stored, current)) > 0L
}

#' Describe the tracked packages that changed, e.g.
#' `"nlmixr2est 6.2.0 (installed 9.9.9)"`
#' @param stored,current metadata lists
#' @return a single string (may be empty when nothing changed)
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveVersionMsg <- function(stored, current=.nlmixr2saveMeta()) {
  .changed <- .nlmixr2saveChanged(stored, current)
  paste(vapply(.changed, function(p) {
    paste0(p, " ", .nlmixr2savePkgLabel(stored, p),
           " (installed ", .nlmixr2savePkgLabel(current, p), ")")
  }, character(1)), collapse="; ")
}

#' Extract the stored save metadata from a loaded fit
#' @param fit a loaded `nlmixr2FitData`/`nlmixr2FitCore`
#' @return the metadata list, or `NULL` when absent
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveGetMeta <- function(fit) {
  .env <- if (is.environment(attr(class(fit), ".foceiEnv"))) {
    attr(class(fit), ".foceiEnv")
  } else if (is.environment(fit)) {
    fit
  } else {
    NULL
  }
  if (is.null(.env) || !exists(".nlmixr2saveMeta", envir=.env, inherits=FALSE)) {
    return(NULL)
  }
  get(".nlmixr2saveMeta", envir=.env)
}

#' Decide what to do when a cached fit's tracked-package versions differ from now
#'
#' When the versions match (or there is nothing to compare) nothing happens and
#' `FALSE` is returned so the caller uses the cached fit.  When nlmixr2est or
#' rxode2 differs: interactively the user is asked whether to rerun the fit with
#' the currently installed packages (`TRUE` -> caller should refit);
#' non-interactively the cached fit is kept and a warning is emitted.
#' @param fit the loaded cached fit
#' @return `TRUE` if the caller should rerun the fit, otherwise `FALSE`
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveVersionRerun <- function(fit) {
  .stored <- .nlmixr2saveGetMeta(fit)
  if (!.nlmixr2saveMetaDiffers(.stored)) return(FALSE)
  .msg <- .nlmixr2saveVersionMsg(.stored)
  if (interactive()) {
    .ans <- utils::menu(c("Reload the cached fit as-is",
                          "Rerun the fit with the installed packages"),
                        title=paste0("The cached fit was run with ", .msg, "."))
    return(.ans == 2L)
  }
  warning("the cached fit was run with ", .msg, "; loading the cached fit",
          call.=FALSE)
  FALSE
}

#' Warn (non-interactively) when a loaded fit's tracked-package versions differ
#'
#' Used by [loadFit()], which cannot rerun the fit (it has no original call), so
#' it only informs the user of the version skew (nlmixr2est and/or rxode2).
#' @param fit the loaded fit
#' @return `fit`, invisibly
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveWarnVersion <- function(fit) {
  .stored <- .nlmixr2saveGetMeta(fit)
  if (.nlmixr2saveMetaDiffers(.stored)) {
    warning("this fit was run with ", .nlmixr2saveVersionMsg(.stored),
            call.=FALSE)
  }
  invisible(fit)
}

#' Base cache file name (prefix + variable name), without extension
#' @param x variable name being assigned by `:=`
#' @return `paste0(prefix, x)`
#' @noRd
.nlmixr2saveBase <- function(x) {
  paste0(.nlmixr2savePrefix(), x)
}

#' Evaluate `code` with the working directory set to the cache directory
#'
#' `saveFit()`/`loadFit()` write and read component files by plain (relative)
#' name, so running them inside the cache directory keeps the file argument a
#' plain `<prefix><name>` and avoids embedding a subdirectory in it.  A no-op
#' when the cache directory is `"."`.
#' @param code expression to evaluate in the cache directory
#' @return the value of `code`
#' @noRd
.nlmixr2saveWithDir <- function(code) {
  .dir <- .nlmixr2saveDir()
  if (identical(.dir, ".") || identical(.dir, "")) {
    return(force(code))
  }
  if (!dir.exists(.dir)) {
    dir.create(.dir, recursive=TRUE, showWarnings=FALSE)
  }
  .owd <- setwd(.dir)
  on.exit(setwd(.owd), add=TRUE)
  force(code)
}

#' Save a fitted model item to a file
#'
#' This is a generic function to save a fitted model item to a file.
#'
#' @param item Item to be saved
#' @param name Name of the item
#' @param file Baseline file name to save the item to.
#' @return boolean to determine if the item was saved; if it wasn't it
#'   will be saved into the general list of items.
#' @export
#' @keywords internal
#' @author Matthew L. Fidler
saveFitItem <- function(item, name, file) {
  UseMethod("saveFitItem")
}

#' @rdname saveFitItem
#' @export
saveFitItem.rxUi <- function(item, name, file) {
  v <- NULL
  v <- try(writeLines(paste0(name, " <- ", paste(deparse(as.function(item)), collapse="\n"),
                             "\n",
                             paste0(name, " <- rxode2::rxode2(", name, ")\n"),
                             paste0(name, " <- rxode2::rxUiDecompress(", name, ")\n"),
                             paste0("assign(\"modelName\", ", deparse1(item$modelName),
                                    ", envir=, ", name, ")\n"),
                             paste0("rm(\"model\", envir=", name, ")\n"),
                             paste0(name, " <- rxode2::rxUiCompress(", name, ")\n")),
                      con = paste0(file,"-", name, ".R")))
  if (inherits(v, "try-error")) {
    return(FALSE) # nocov
  }
  TRUE
}

#' @rdname saveFitItem
#' @export
saveFitItem.data.frame <- function(item, name, file) {
  if (name %in% .saveFitEnv$rowDF) {
    v <- try(utils::write.csv(item, paste0(file,"-", name, ".csv"), row.names=TRUE))
  } else if (name %in% .saveFitEnv$DF) {
    v <- try(utils::write.csv(item, paste0(file,"-", name, ".csv"), row.names=FALSE))
  } else {
    v <- try(saveRDS(item, paste0(file,"-", name, ".rds")))
  }
  if (inherits(v, "try-error")) {
    return(FALSE) # nocov
  }
  TRUE
}

#' @rdname saveFitItem
#' @export
saveFitItem.nlmixr2estSessionInfo <- function(item, name, file) {
  saveRDS(item, paste0(file,"-", name, ".rds"))
  TRUE
}

#' @rdname saveFitItem
#' @export
saveFitItem.default <- function(item, name, file) {
  FALSE
}

#' @rdname saveFitItem
#' @export
saveFitItem.saemFit <- function(item, name, file) {
  saveRDS(item, paste0(file,"-", name, ".rds"))
  TRUE
}

#' @rdname saveFitItem
#' @export
saveFitItem.foceiModelList <- function(item, name, file) {
  .r <- c(paste0(name, " <- list()\n"),
             vapply(seq_along(item),
                    function(i) {
                      if (inherits(item[[i]], "rxode2")) {
                        paste0(name, "[[", deparse1(names(item)[i]), "]] <- ",
                               "rxode2::rxode2(",
                               deparse1(rxode2::rxNorm(item[[i]])),
                               ")\n")
                      } else {
                        paste0(name, "[[", deparse1(names(item)[i]), "]] <- ",
                               paste(deparse(item[[i]]), collapse="\n"), "\n")
                      }
                    },
                    character(1), USE.NAMES=FALSE),
          paste0("class(", name, ") <- ", deparse1(class(item)), "\n"))
  writeLines(.r, con = paste0(file,"-", name, ".R"))
  TRUE
}

#' @export
saveFitItem.saemModelList <- saveFitItem.foceiModelList

#' Save the deparsed object
#'
#' @param obj object
#' @param name name that the object should be assigned to
#' @return R expression of name `<-` R expression
#' @noRd
#' @author Matthew L. Fidler
.saveDeparse <- function(obj, name) {
  .expr <- try(rxode2::rxUiDeparse(obj, name), silent=TRUE)
  if (inherits(.expr, "try-error") ||
        is.null(.expr)) {
    .expr <- try(str2lang(paste0(name, "<-", deparse1(obj))), silent=TRUE) # nocov
  }
  if (inherits(.expr, "try-error")) {
    return(NULL) # nocov
  } else if (is.null(.expr)) { # nocov
    return(NULL) # nocov
  } else {
    return(as.list(.expr))
  }
}

#' Save a fitted model object to a series of files
#'
#' @param fit the fitted model object
#' @param file the base name of the files to save the fit to.  It may include
#'   a directory, e.g. `"path/to/fit"`: the files are written there (the
#'   directory is created if needed) and the archive `path/to/fit.zip` holds
#'   them under the bare name `fit`, so it can be moved and loaded from
#'   anywhere.
#' @param zip Boolean indicating if the files should be zipped.
#' @param data Boolean indicating whether the original dataset (`origData`) is
#'   stored in the saved fit.  When `FALSE` it is omitted, producing a fit that
#'   can be shared without the subject-level data (see [nlmixr2saveShare()]).
#'   Defaults to `getOption("nlmixr2save.data", TRUE)`.
#' @return nothing, called for side effects
#' @export
#' @author Matthew L. Fidler
#' @examples
#' \donttest{
#'   if (requireNamespace("nlmixr2est", quietly=TRUE) &&
#'         requireNamespace("nlmixr2data", quietly=TRUE) &&
#'         requireNamespace("withr")) {
#'     library(nlmixr2est)
#'     library(nlmixr2data)
#'     withr::with_tempdir({
#'       one.cmt <- function() {
#'         ini({
#'           tka <- 0.45
#'           tcl <- log(c(0, 2.7, 100))
#'           tv <- 3.45
#'           eta.ka ~ 0.6
#'           eta.cl ~ 0.3
#'           eta.v ~ 0.1
#'           add.sd <- 0.7
#'         })
#'         model({
#'           ka <- exp(tka + eta.ka)
#'           cl <- exp(tcl + eta.cl)
#'           v  <- exp(tv + eta.v)
#'           linCmt() ~ add(add.sd)
#'         })
#'       }
#'
#'       fit <- nlmixr2(one.cmt, theo_sd, est="focei")
#'
#'       saveFit(fit) # saved to fit.zip
#'       fit2 <- loadFit(fit) # load fit.zip
#'
#'       if (file.exists("fit.zip")) {
#'          unlink("fit.zip")
#'       }
#'
#'       print(fit2)
#'     })
#'   }
#' }
saveFit <- function(fit, file, zip=TRUE, data=.nlmixr2saveData()) {
  UseMethod("saveFit")
}

#' Is `name` in `env` bound to a promise (forced or not)?
#'
#' `substitute()` returns a promise's expression rather than its value, so a
#' binding the loader made with `delayedAssign()` -- whose expressions all
#' refer to `..nlmixr2saveLazy..` -- still yields that expression until a
#' value is assigned over it.
#' @param name item name
#' @param env environment
#' @return boolean
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveIsPromise <- function(name, env) {
  if (!exists(name, envir=env, inherits=FALSE)) return(FALSE)
  .e <- eval(call("substitute", as.name(name), env))
  # the loader's promises all refer to ..nlmixr2saveLazy..; a call a user
  # assigned (e.g. a quoted expression) does not
  is.call(.e) &&
    any(grepl("..nlmixr2saveLazy..", deparse(.e), fixed=TRUE))
}

#' Text of the loader script `<file>.R` that restores a saved fit
#'
#' `saveFit()` writes it next to the component files; `loadFit()` also
#' regenerates it from the files in a cache whose own loader cannot be used
#' as-is (see `.nlmixr2saveSourceLoader()`).
#' @param file base name of the fit (the loader's object and file prefix)
#' @param files the fit's component files, named `<file>-<item>.<ext>` (plus
#'   `<file>.csv` / `<file>-env.R`, which are handled separately)
#' @param parFixedDfNamed whether `parFixedDf`'s Estimate/SE are named vectors
#' @param iniDf0Types restore code giving `iniDf0`'s columns their types, from
#'   [.nlmixr2saveIniDf0Types()]; without the fit (a regenerated loader), the
#'   columns rxode2 has always had
#' @return the script, as one string
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveLoaderText <- function(file, files, parFixedDfNamed=TRUE,
                                   iniDf0Types=.nlmixr2saveIniDf0Types(NULL)) {
  .files <- files
  .parFixedDfNamed <- parFixedDfNamed
  .r <- do.call(`c`,
          lapply(.files,
                 function(f) {
                   if (f == paste0(file, "-env.R") ||
                         f == paste0(file, ".R") ||
                           f == paste0(file, ".csv")) {
                     return(NULL)
                   }
                   if (grepl(".R$", f)) {
                     val <- substr(f, nchar(file)+2, nchar(f)-2)
                     if (val %in% .saveFitEnv$lazy) {
                       # built with rxode2::rxode2(): a compiled model list
                       # (a long C compilation per model, for a large model)
                       # or the ui (a parse of the whole model).  Read the
                       # script now (the files are gone by the time it is
                       # used), build it on first use, and keep the text so
                       # saveFit() can write it back without building it.
                       return(paste0("local({\n",
                                     "  .txt <- readLines('", f, "', warn=FALSE)\n",
                                     "  .lazy <- env$`..nlmixr2saveLazy..`\n",
                                     "  .lazy$`", val, "` <- .txt\n",
                                     "  env$`..nlmixr2saveLazy..` <- .lazy\n",
                                     "  delayedAssign('", val, "', local({\n",
                                     "    eval(parse(text=.txt, keep.source=FALSE))\n",
                                     # once built, the object itself is saved
                                     # (with any changes made to it), not
                                     # the script it was built from
                                     "    .lazy <- env$`..nlmixr2saveLazy..`\n",
                                     "    .lazy$`", val, "` <- NULL\n",
                                     "    env$`..nlmixr2saveLazy..` <- .lazy\n",
                                     "    `", val, "`\n",
                                     "  }), assign.env=env)\n",
                                     "})\n"))
                     }
                     return(paste0("source('", f, "', local=TRUE)\n",
                                   "env$`", val, "` <- ", val, "\n"))
                   }
                   if (grepl(".csv$", f)) {
                     val <- substr(f, nchar(file)+2, nchar(f)-4)
                     if (val == "parFixed") {
                       ret <- paste0("env$`", val, "` <- read.csv('", f, "',check.names=FALSE, row.names=1, colClasses=\"character\")\nclass(env$`", val, "`) <- c('nlmixr2ParFixed', 'data.frame')\n")
                     } else if (val == "objDf") {
                       ret <- paste0("env$objDf <- read.csv('", f, "',check.names=FALSE, row.names=1)\n",
                                     "env$objDf$OBJF <- as.double(env$objDf$OBJF)\n",
                                     "env$objDf$AIC <- as.double(env$objDf$AIC)\n",
                                     "env$objDf$BIC <- as.double(env$objDf$BIC)\n",
                                     "env$objDf$`Log-likelihood` <- as.double(env$objDf$`Log-likelihood`)\n"
                                     )
                     } else if (val %in% .saveFitEnv$rowDF) {
                       ret <- paste0("env$`", val, "` <- read.csv('", f, "',check.names=FALSE, row.names=1)\n")
                       if (val == "parFixedDf") {
                         ret <- paste0(ret,
                                       "env$`parFixedDf` <- nlmixr2save::nlmixr2saveParFixedDf(env$`parFixedDf`, named=",
                                       deparse1(.parFixedDfNamed), ")\n")
                       } else if (val == "iniDf0") {
                         ret <- paste0(ret, iniDf0Types)
                       }
                     } else {
                       ret <- paste0("env$`", val, "` <- read.csv('", f, "', check.names=FALSE)\n")
                     }
                     return(ret)
                   }
                   if (grepl(".rds$", f)) {
                     val <- substr(f, nchar(file)+2, nchar(f)-4)
                     return(paste0("env$`", val, "` <- readRDS('", f, "')\n"))
                   }
                   NULL # nocov
                 }))
  .r <- paste0(.r, collapse="\n")
  # the loader script assigns to a variable named after `file`; a
  # nlmixr2save.prefix can make that a non-syntactic name (e.g. "modelPiping-fit"),
  # so quote it with backticks.
  .fq <- paste0("`", file, "`")
  paste0(.fq, " <- function() {\n",
                    "source('", paste0(file,"-env.R"), "', local=TRUE)\n",
                    ".class <- env$`..class..`\n",
                    ".id.level <- env$`..id.level..`\n",
                    ".parHistType.level <- env$`..parHistType.level..`\n",
                    "rm('..class..', envir=env)\n",
                    "rm('..id.level..', envir=env)\n",
                    "if (exists('..parHistType.level..', env)) rm('..parHistType.level..', envir=env)\n",
                    .r,
                    # derived from the ui, so built with it, when first used
                    "delayedAssign('model', rxode2::model(env$ui), assign.env=env)\n",
                    "if (!is.null(.id.level)) {\n",
                    "  if (!is.null(env$ranef$ID)) {\n",
                    "    env$ranef$ID <- factor(env$ranef$ID, levels=.id.level)\n",
                    "  }\n",
                    "  if (!is.null(env$etaObf$ID)) {\n",
                    "    env$etaObf$ID <- factor(env$etaObf$ID, levels=.id.level)\n",
                    "  }\n",
                    "}\n",
                    "if (!is.null(env$parHistData)) {\n",
                    # use the levels recorded from the fit; fall back to the
                    # known level set for fits saved before they were recorded
                    "  .phLevels <- .parHistType.level\n",
                    "  if (is.null(.phLevels)) {\n",
                    "    .phLevels <- c(\"Gill83 Gradient\", \"Mixed Gradient\", \"Forward Difference\", \"Central Difference\", \"Scaled\", \"Unscaled\", \"Back-Transformed\", \"Forward Sensitivity\", \"Analytic Gradient\")\n",
                    # a fit saved before the levels were recorded can still use a
                    # type this list predates; append it rather than drop it to NA
                    "    .phLevels <- c(.phLevels, setdiff(unique(as.character(env$parHistData$type)), .phLevels))\n",
                    "  }\n",
                    "  env$parHistData$type <- factor(env$parHistData$type, levels=.phLevels)\n",
                    "  env$parHistData$iter <- as.integer(env$parHistData$iter)\n",
                    "}\n",
                    "if (exists('saemControl', env) && is.numeric(env$saemControl$mcmc$niter[1])) {\n",
                    "    .parHistData <- env$parHistData\n",
                    "    .cls <- class(.parHistData)\n",
                    "    attr(.cls, 'niter') <- env$saemControl$mcmc$niter[1]\n",
                    "    class(.parHistData) <- .cls\n",
                    "    env$parHistData <- .parHistData\n",
                    "}\n",
                    "if (any(.class == 'nlmixr2FitData')) {\n",
                    "  ret <- read.csv('", paste0(file,".csv"), "')\n",
                    "  class(env) <- 'nlmixr2FitCoreSilent'\n",
                    "  attr(.class, '.foceiEnv') <- env\n",
                    "  class(ret) <- .class\n",
                    "  return(ret)\n",
                    "} else {\n",
                    "  ret <- env\n",
                    "  class(ret) <- .class\n",
                    "  return(ret)\n",
                    "}\n",
                    "}\n",
                    .fq, " <- ", .fq, "()\n")
}

#' Split a `saveFit()` target into the directory to save in and a base name
#'
#' Every component file, and every reference the loader script makes to one,
#' is named from the `file` argument.  Given `path_model/fit`, those names
#' carried the directory, so the archive stored a `path_model/` folder and
#' the loader only worked from the directory it was saved from; unzipping it
#' recreated `path_model/` wherever it was unzipped.  Saving from inside the
#' directory under the bare name keeps the archive flat and relocatable.
#' A trailing `.zip` is kept: `fit.zip` is a valid variable name, and the
#' `:=` cache saves under the variable name and expects `<name>.zip` back.
#' @param file the `file` argument given to `saveFit()`
#' @return list with `dir` (the directory to save in, created if needed) and
#'   `file` (the bare base name)
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveSaveTarget <- function(file) {
  .dir <- dirname(file)
  if (!dir.exists(.dir)) {
    dir.create(.dir, recursive = TRUE, showWarnings = FALSE)
  }
  # a file of that name is not a directory to save in (nor to copy over)
  if (!dir.exists(.dir)) {
    stop(
      "cannot save the fit in '",
      .dir,
      "': it is not a directory",
      call. = FALSE
    )
  }
  list(dir = .dir, file = basename(file))
}

#' @rdname saveFit
#' @export
saveFit.nlmixr2FitCore <- function(
  fit,
  file,
  zip = TRUE,
  data = .nlmixr2saveData()
) {
  if (missing(file)) {
    file <- as.character(substitute(fit))
  }
  .nlmixr2saveFitStaged(fit, file, zip = zip, data = data, table = FALSE)
}

#' Write a fit's files in a private directory, then zip or copy them out
#'
#' Every file is written into a fresh temporary directory, and only what is
#' there goes into the loader, the archive and the target directory.  Picking
#' a fit's files out of the target directory by name instead cannot tell them
#' apart from files already there: `-` is legal in a base name, so a fit saved
#' as `fit` claimed (zipped, then deleted) every file of one saved as
#' `fit-alt`, and an item this fit lacks was read from the file an earlier
#' `zip=FALSE` save of the same name left behind.
#' @param fit the fit
#' @param file the `file` argument given to `saveFit()` (not missing)
#' @param zip,data as in `saveFit()`
#' @param table whether to write the fit table as `<file>.csv`
#' @return nothing
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveFitStaged <- function(fit, file, zip, data, table) {
  .target <- .nlmixr2saveSaveTarget(file)
  file <- .target$file
  .outdir <- normalizePath(.target$dir, mustWork = TRUE)
  .stage <- tempfile("nlmixr2save-")
  dir.create(.stage)
  on.exit(unlink(.stage, recursive = TRUE, force = TRUE), add = TRUE)
  .owd <- setwd(.stage)
  on.exit(setwd(.owd), add = TRUE, after = FALSE)
  if (isTRUE(table)) {
    utils::write.csv(fit, paste0(file, ".csv"), row.names = FALSE)
  }
  .item <- ls(envir = fit$env, all.names = TRUE)
  .str <- character(0)
  # a loaded fit keeps its compiled model lists as unforced promises, with
  # their script text; write that back rather than force (compile) them
  .item <- setdiff(.item, "..nlmixr2saveLazy..")
  for (.i in .item) {
    # re-read each time: saving an earlier item can build a lazy one, which
    # drops its kept text
    .lazy <- get0("..nlmixr2saveLazy..", envir = fit$env, inherits = FALSE)
    # only while the binding is still the loader's promise and unbuilt (a
    # promise drops its kept text when it is built): a value assigned since
    # loading, or one changed in place after building, must be saved
    if (
      is.list(.lazy) &&
        !is.null(.lazy[[.i]]) &&
        .nlmixr2saveIsPromise(.i, fit$env)
    ) {
      .minfo(paste0("saving fit item: ", .i))
      if (is.data.frame(.lazy[[.i]])) {
        # iniDf0 as read; loading repairs it again, without building the ui now
        saveFitItem(.lazy[[.i]], .i, file)
      } else {
        writeLines(.lazy[[.i]], con = paste0(file, "-", .i, ".R"))
      }
      next
    }
    # .nlmixr2saveMeta is written once, below, from the preserved-or-fresh value
    if (.i == ".nlmixr2saveMeta") {
      next
    }
    # when data=FALSE the original dataset is left out of the zip entirely
    if (!isTRUE(data) && .i == "origData") {
      next
    }
    # `model` is always regenerated from `ui` by the loader
    # (env$model <- rxode2::model(env$ui)); saving it is redundant and, for a
    # reloaded fit (where it is a `call`), triggers a spurious "could not
    # determine how to save" warning.
    if (.i == "model") {
      next
    }
    .minfo(paste0("saving fit item: ", .i))
    .obj <- get(.i, envir = fit$env)
    if (is.raw(.obj)) {
      # decompresses object; a call, not parsed text, so any item name works
      .obj <- eval(call("$", quote(fit), as.name(.i)))
    }
    if (!saveFitItem(.obj, .i, file)) {
      if (.i %in% c("phiC", "phiH")) {
        .lines <- deparse(as.call(c(
          quote(`list`),
          lapply(seq_along(.obj), function(i) {
            .ret <- .saveDeparse(.obj[[i]], "x")
            if (!is.null(.ret)) {
              return(.ret[[3]])
            }
            NULL # nocov
          })
        )))
        .lines[1] <- paste0(.i, " <- ", .lines[1])
        if (!is.null(names(.obj))) {
          .lines <- c(
            .lines,
            paste0("names(", .i, ") <- ", deparse1(names(.obj)))
          )
        }
        writeLines(.lines, con = paste0(file, "-", .i, ".R"))
      } else {
        .expr <- .saveDeparse(.obj, .i)
        if (!is.null(.expr)) {
          .expr[[1]] <- quote(`=`)
          .expr <- as.call(.expr)
          .str <- c(.str, paste(deparse(.expr), collapse = "\n"))
        } else {
          warning(
            "could not determine how to save object of class ",
            paste(class(.obj), collapse = ", "),
            " for item ",
            .i,
            "; as a text-file, reverting to .rds format",
            call. = FALSE
          )
          saveRDS(.obj, paste0(file, "-", .i, ".rds"))
        }
      }
    }
  }
  # Version/sha metadata the fit was produced under (nlmixr2est and rxode2, plus
  # the nlmixr2save version); preserve it across a load -> save round-trip (it
  # records the run version, not the save version), otherwise stamp the
  # currently installed packages.
  .meta <- if (exists(".nlmixr2saveMeta", envir = fit$env, inherits = FALSE)) {
    get(".nlmixr2saveMeta", envir = fit$env)
  } else {
    .nlmixr2saveMeta()
  }
  # Capture the parHistData$type factor levels from the fit itself, so the
  # restored factor matches whatever nlmixr2est produced it (the level set has
  # grown over nlmixr2est versions, e.g. "Analytic Gradient").  A hardcoded
  # fallback in the loader still covers fits saved before this was recorded.
  .parHistTypeLevel <- NULL
  if (exists("parHistData", envir = fit$env, inherits = FALSE)) {
    .phd <- get("parHistData", envir = fit$env)
    if (is.raw(.phd)) {
      # nlmixr2est stores parHistData compressed; `$` decompresses it
      .phd <- fit$parHistData
    }
    if (is.data.frame(.phd) && is.factor(.phd$type)) {
      .parHistTypeLevel <- levels(.phd$type)
    }
  }
  .cls <- as.character(class(fit))
  attr(.cls, ".foceiEnv") <- NULL
  .str <- c(
    .str,
    paste0("..class.. = ", paste(deparse(.cls), collapse = "\n")),
    paste0("..id.level.. = ", paste(deparse(levels(fit$ID)), collapse = "\n")),
    paste0(
      "..parHistType.level.. = ",
      paste(deparse(.parHistTypeLevel), collapse = "\n")
    ),
    paste0(".nlmixr2saveMeta = ", paste(deparse(.meta), collapse = "\n"))
  )
  .str <- .str[.str != "NULL = NULL"]
  .str <- paste0(
    "env <- list(",
    paste(.str, collapse = ",\n"),
    ")\nenv <- list2env(env)\n"
  )
  writeLines(.str, con = paste0(file, "-env.R"))
  # the stage holds only what this save wrote
  .files <- list.files(".", all.files = TRUE, no.. = TRUE)
  # nlmixr2est <= 6.0 stores parFixedDf with named "Estimate"/"SE" columns;
  # the $parFixed refactor (nlmixr2est#645) stores them unnamed.  Record
  # which structure this fit uses so the restore script rebuilds it exactly.
  .parFixedDfNamed <- TRUE
  if (exists("parFixedDf", envir = fit$env)) {
    .pfd <- get("parFixedDf", envir = fit$env)
    if (is.data.frame(.pfd) && !is.null(.pfd$Estimate)) {
      .parFixedDfNamed <- !is.null(names(.pfd$Estimate))
    }
  }
  writeLines(
    .nlmixr2saveLoaderText(
      file,
      .files,
      .parFixedDfNamed,
      .nlmixr2saveIniDf0Types(fit)
    ),
    con = paste0(file, ".R")
  )
  .files <- c(.files, paste0(file, ".R"))
  if (isTRUE(zip)) {
    .minfo("zipping fit files")
    zip::zip(zipfile = paste0(file, ".zip"), files = .files)
    .files <- paste0(file, ".zip")
    # the loader of an earlier zip=FALSE save under this name, which would
    # load the old fit were the archive moved; only if it is one, as a script
    # the user wrote under that name must be left alone
    .old <- file.path(.outdir, paste0(file, ".R"))
    if (
      file.exists(.old) &&
        !dir.exists(.old) &&
        .nlmixr2saveLoaderUsable(readLines(.old, warn = FALSE), file)
    ) {
      unlink(.old)
    }
  } else {
    # the loader is replaced last, and the one there now removed first: when
    # a copy out fails, no loader is left reading a mix of old and new files.
    unlink(file.path(.outdir, paste0(file, ".R")))
    # An archive from an earlier save under this name goes too, as loadFit()
    # prefers it to the new loose files; only if it is one (its loader and
    # `-env.R` inside), as an unrelated archive of that name is not the fit's
    .zip <- file.path(.outdir, paste0(file, ".zip"))
    if (file.exists(.zip) && !dir.exists(.zip)) {
      .in <- tryCatch(zip::zip_list(.zip)$filename, error = function(e) {
        character(0)
      })
      if (all(paste0(file, c(".R", "-env.R")) %in% basename(.in))) {
        unlink(.zip)
      }
    }
  }
  for (.f in .files) {
    .to <- file.path(.outdir, .f)
    # file.copy() onto a directory copies into it, and reports success
    if (dir.exists(.to) || !file.copy(.f, .to, overwrite = TRUE)) {
      stop("could not write '", .f, "' to '", .target$dir, "'", call. = FALSE)
    }
  }
  invisible()
}

#' @rdname saveFit
#' @export
saveFit.nlmixr2FitData <- function(
  fit,
  file,
  zip = TRUE,
  data = .nlmixr2saveData()
) {
  if (missing(file)) {
    file <- as.character(substitute(fit))
  }
  .nlmixr2saveFitStaged(fit, file, zip = zip, data = data, table = TRUE)
}

#' @rdname saveFit
#' @export
saveFit.default <- function(fit, file, zip=TRUE, data=.nlmixr2saveData()) {
  stop("saveFit not implemented for object of class ", paste(class(fit), collapse=", "), call.=FALSE)
}


#' Put the `ID` column of a restored fit back to a factor
#'
#' The fit table is written as a plain `.csv`, so `read.csv()` brings `ID` back
#' as an integer (or character) while a real fit carries a factor.  Anything that
#' joins the fit table against something derived from the fit then hits a type
#' mismatch -- `nlme::augPred()` keeps `id` a factor, so `ggPMX::pmx_nlmixr()`
#' dies in a data.table join with "Incompatible join types: x.ID (factor) and
#' i.ID (integer)".
#'
#' Done here rather than in the restore script written by [saveFit()] so that
#' caches saved by earlier versions are repaired on load too.
#'
#' Levels come from `ranef`/`etaObf`, which the restore script has already put
#' back as factors with the fit's own levels; failing that, from the order the
#' IDs appear (the order nlmixr2est itself uses).
#'
#' @param fit restored object
#' @return `fit`, with `ID` a factor when it is a fit table that has one
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveRestoreIdFactor <- function(fit) {
  if (!inherits(fit, "nlmixr2FitData")) return(fit)
  if (!is.data.frame(fit)) return(fit)
  if (is.null(fit[["ID"]]) || is.factor(fit[["ID"]])) return(fit)
  .env <- try(fit$env, silent=TRUE)
  .levels <- NULL
  if (is.environment(.env)) {
    for (.n in c("ranef", "etaObf")) {
      .df <- try(get(.n, envir=.env, inherits=FALSE), silent=TRUE)
      if (is.data.frame(.df) && is.factor(.df$ID)) {
        .levels <- levels(.df$ID)
        break
      }
    }
  }
  .id <- as.character(fit[["ID"]])
  if (is.null(.levels)) {
    .levels <- unique(.id)
  } else {
    # ranef should cover every subject in the table, but never turn an ID the
    # table does have into NA on the way to fixing its type
    .levels <- c(.levels, setdiff(unique(.id), .levels))
  }
  ## `class<-` last: the class attribute of a fit carries the `.foceiEnv`
  ## attribute that `$` dispatches through, and column assignment must not be
  ## allowed to drop it.
  .cls <- class(fit)
  fit[["ID"]] <- factor(.id, levels=.levels)
  class(fit) <- .cls
  fit
}

#' Keep the loader's `iniDf0` column-type lines for re-saving an unused fit
#'
#' While a loaded fit's `iniDf0` is still the unrepaired table (see
#' [.nlmixr2saveRestoreIniDf0()]), its types cannot be read off it without
#' repairing it, which builds the ui.  The loader already holds the exact
#' coercion `saveFit()` wrote from the original fit's column types, so keep
#' those lines for [.nlmixr2saveIniDf0Types()] to write back.
#' @param fit the loaded fit
#' @param lines the loader script's lines
#' @return `fit`, invisibly
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveKeepIniDf0Types <- function(fit, lines) {
  .env <- if (is.environment(fit)) fit else try(fit$env, silent=TRUE)
  if (!is.environment(.env)) return(invisible(fit))
  .lazy <- get0("..nlmixr2saveLazy..", envir=.env, inherits=FALSE)
  if (!is.list(.lazy) || is.null(.lazy[["iniDf0"]])) return(invisible(fit))
  # the coercions, not the read.csv() that reads it (written env$`iniDf0`)
  .l <- lines[grepl("env$iniDf0", lines, fixed=TRUE)]
  if (length(.l)) {
    .lazy[["typesOfIniDf0"]] <- paste0(.l, "\n", collapse="")
    assign("..nlmixr2saveLazy..", .lazy, envir=.env)
  }
  invisible(fit)
}

#' Bring a restored `iniDf0` in line with the installed rxode2's `iniDf`
#'
#' The restore script coerces a fixed list of `iniDf0` columns, so two things
#' slip through:
#'
#' * a column the cache predates.  rxode2 added `prior`; a cache written before
#'   that has no such column, while the installed rxode2 expects one.
#' * a column whose values are all `NA`.  `read.csv()` reads it back as
#'   logical, but rxode2 keeps `prior` (for one) as character.
#'
#' The fit's `ui` is rebuilt by the installed rxode2 when the fit is loaded, so
#' its `iniDf` is a template of exactly the columns and types this rxode2 uses
#' -- no version check needed.  Missing columns are added as typed `NA`s in
#' the template's order, and all-`NA` logical columns take the template's
#' type.  Columns the installed rxode2 does not know are kept, after the
#' others.  Without a `ui` to compare against, only `prior` is retyped.
#' @param fit restored object
#' @return `fit`, invisibly; `iniDf0` is repaired in the fit environment
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveRestoreIniDf0 <- function(fit) {
  .env <- if (is.environment(fit)) fit else try(fit$env, silent=TRUE)
  if (!is.environment(.env)) return(invisible(fit))
  if (!exists("iniDf0", envir=.env, inherits=FALSE)) return(invisible(fit))
  .ini <- get("iniDf0", envir=.env, inherits=FALSE)
  if (!is.data.frame(.ini)) return(invisible(fit))
  if (!exists("ui", envir=.env, inherits=FALSE)) {
    assign("iniDf0", .nlmixr2saveIniDf0Fix(.ini, NULL), envir=.env)
    return(invisible(fit))
  }
  # the template is the ui, which the loader builds only when first used;
  # repair iniDf0 when it is first used too, rather than build the ui now.
  # Keep it as read, so saveFit() can write it back without the ui.
  .lazy <- get0("..nlmixr2saveLazy..", envir=.env, inherits=FALSE)
  if (!is.list(.lazy)) .lazy <- list()
  .lazy$iniDf0 <- .ini
  assign("..nlmixr2saveLazy..", .lazy, envir=.env)
  rm("iniDf0", envir=.env)
  delayedAssign("iniDf0", {
    .fixed <- .nlmixr2saveIniDf0Fix(.ini, get("ui", envir=.env, inherits=FALSE))
    # once repaired, save the repaired table (with any changes made to it)
    .lazy <- get0("..nlmixr2saveLazy..", envir=.env, inherits=FALSE)
    .lazy$iniDf0 <- NULL
    assign("..nlmixr2saveLazy..", .lazy, envir=.env)
    .fixed
  }, assign.env=.env)
  invisible(fit)
}

#' @describeIn dot-nlmixr2saveRestoreIniDf0 the repair itself
#' @param ini the restored `iniDf0`
#' @param ui the fit's ui, or `NULL`
#' @return the repaired `iniDf0`
#' @noRd
.nlmixr2saveIniDf0Fix <- function(ini, ui) {
  .ini <- ini
  .tmpl <- NULL
  if (!is.null(ui)) {
    # `$` decompresses a compressed ui
    .tmpl <- try(ui$iniDf, silent=TRUE)
    if (!is.data.frame(.tmpl)) .tmpl <- NULL
  }
  if (is.null(.tmpl)) {
    if (is.logical(.ini$prior)) .ini$prior <- as.character(.ini$prior)
    return(.ini)
  }
  .na <- rep(NA_integer_, nrow(.ini))
  for (.c in names(.tmpl)) {
    # indexing a zero-length column by NA gives NAs of the column's type
    .proto <- .tmpl[[.c]][0]
    if (is.null(.ini[[.c]])) {
      .ini[[.c]] <- .proto[.na]
    } else if (is.logical(.ini[[.c]]) && !is.logical(.proto) &&
                 all(is.na(.ini[[.c]]))) {
      .ini[[.c]] <- .proto[.na]
    }
  }
  .ini[, c(names(.tmpl), setdiff(names(.ini), names(.tmpl))), drop=FALSE]
}

#' Repair `parHistData$type` levels a cache's own restore script dropped
#'
#' The factor levels for `parHistData$type` are applied by the restore script
#' stored *inside* the cache.  A script written before a given nlmixr2est
#' version knows nothing of the types that version added ("Analytic Gradient
#' (relaxed)" and friends), so it coerces them to `NA` -- and re-saving cannot
#' recover them, because by then the strings are already gone.
#'
#' The `-parHistData.csv` still holds the original strings and has not been
#' cleaned up yet at this point in [loadFit()], so read the column back from
#' there and append whatever the script's level list was missing.
#'
#' @param fit restored object
#' @param file base name the fit was loaded from
#' @return `fit`, invisibly; `parHistData` is repaired in the fit environment
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveRestoreParHistType <- function(fit, file) {
  # a fit table dispatches `$env` through its class attribute; a data-less fit
  # (`nlmixr2saveShare(noFit=TRUE)`) restores as the environment itself
  .env <- if (is.environment(fit)) fit else try(fit$env, silent=TRUE)
  if (!is.environment(.env)) return(invisible(fit))
  if (!exists("parHistData", envir=.env, inherits=FALSE)) return(invisible(fit))
  .phd <- try(get("parHistData", envir=.env, inherits=FALSE), silent=TRUE)
  # only NA types are worth repairing; a cache whose script knew the levels is
  # already correct
  if (!is.data.frame(.phd) || !is.factor(.phd$type) || !anyNA(.phd$type)) {
    return(invisible(fit))
  }
  .csv <- paste0(file, "-parHistData.csv")
  if (!file.exists(.csv)) return(invisible(fit))
  # colClasses="character": type is a level name, and letting read.csv infer
  # would turn a level like "01" into 1 or "T" into TRUE on the way back
  .raw <- try(utils::read.csv(.csv, check.names=FALSE, colClasses="character"),
              silent=TRUE)
  if (!is.data.frame(.raw) || is.null(.raw$type) || nrow(.raw) != nrow(.phd)) {
    return(invisible(fit))
  }
  .type <- as.character(.raw$type)
  .levels <- c(levels(.phd$type), setdiff(unique(.type[!is.na(.type)]),
                                          levels(.phd$type)))
  ## `class<-` last: for a saem fit the class attribute of parHistData carries
  ## the `niter` attribute, which column assignment must not drop.
  .cls <- class(.phd)
  .phd$type <- factor(.type, levels=.levels)
  class(.phd) <- .cls
  assign("parHistData", .phd, envir=.env)
  invisible(fit)
}

#' Can a cache's own loader script be sourced as it is?
#'
#' Only when it names its fit, and reads every component file, by the bare
#' base name.  Older versions of `saveFit()` wrote the `file` argument into
#' the loader verbatim: given `path/to/fit`, `/home/me/models/fit` or
#' `~/models/fit`, the loader named its object after that path and read its
#' files from it, so it only worked where the fit was saved -- another user
#' gets "Permission denied" or "cannot open file".  With `~` the loader was
#' also garbled beyond its paths: the item names were cut from the file names
#' by the length of `file`, which differs once `~` is expanded, so it
#' restored items under nonsense names and skipped their type conversions.
#' Such a loader is not patched; it is regenerated from the files instead.
#' @param lines the loader script's lines
#' @param base the loader's base name (its file name without `.R`)
#' @return boolean
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveLoaderUsable <- function(lines, base) {
  .ex <- tryCatch(parse(text=lines, keep.source=FALSE), error=function(e) NULL)
  if (length(.ex) == 0L) return(FALSE)
  # it ends `<base> <- <base>()`
  .last <- .ex[[length(.ex)]]
  if (!is.call(.last) || length(.last) != 3L ||
        !identical(.last[[1]], as.name("<-")) ||
        !identical(.last[[2]], as.name(base))) {
    return(FALSE)
  }
  # and no quoted file name carries a directory
  .q <- unlist(regmatches(lines, gregexpr("'[^']*'", lines)))
  !any(grepl("[/\\]", .q))
}

#' Find the loader script in an extracted fit archive
#'
#' A loader `<name>.R` always has a `<name>-env.R` beside it.  Its name can
#' differ from the archive's when the `.zip` was renamed after saving.
#' @param dir directory the archive was extracted (flat) to
#' @param base base name the user asked for
#' @return the loader's file name in `dir`, or `NULL`
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveFindLoader <- function(dir, base) {
  .all <- setdiff(list.files(dir, all.files=TRUE), c(".", ".."))
  # the `-env.R` companion alone tells a loader from its env script; do not
  # drop names ending in "-env.R", since a fit can itself be named `my-env`
  .r <- .all[endsWith(.all, ".R")]
  .r <- .r[paste0(substr(.r, 1L, nchar(.r) - 2L), "-env.R") %in% .all]
  .exact <- .r[basename(.r) == paste0(base, ".R")]
  if (length(.exact) == 1L) return(.exact)
  if (length(.r) == 1L) return(.r)
  NULL
}

#' Is `e` a plain number as `deparse()` writes one?
#'
#' A literal, `Inf`, or `-`/`+` applied to one.  Nothing else is allowed, so
#' evaluating an accepted expression runs no user code.  `NA` is left out on
#' purpose: lotri rejects it, and rxode2 never writes a matrix holding one as
#' a `lotri()` block, so accepting it would only read what lotri would not.
#' @param e expression
#' @return boolean
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveIsNum <- function(e) {
  if (is.numeric(e) && length(e) == 1L && !is.na(e)) return(TRUE)
  if (is.name(e)) return(identical(as.character(e), "Inf"))
  if (is.call(e) && length(e) == 2L && is.name(e[[1]]) &&
        as.character(e[[1]]) %in% c("-", "+")) {
    return(.nlmixr2saveIsNum(e[[2]]))
  }
  FALSE
}

#' Build a matrix from the row-per-statement `lotri({...})` blocks `saveFit()` writes
#'
#' Each statement declares one row and its lower triangle, as lotri reads it:
#' a single value (`name ~ value`, or `name ~ c(name = value)`) starts a new
#' block, and `name ~ c(v1, ..., vk)` extends the current block of `k - 1`
#' rows; the values may be named after the block's columns.  This is all the
#' syntax `rxode2::rxUiDeparse()` produces for a fit's matrices (`cov`,
#' `omega`, `R`, `phiC`, ...), and it is simple enough to read without lotri,
#' so a cache does not depend on which lotri is installed -- some development
#' versions of lotri reject the named rows outright.
#' @param e the argument of the `lotri()` call, unevaluated
#' @return the symmetric matrix with dimnames, or `NULL` when `e` is not
#'   exactly that form (the caller then hands it to lotri)
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveLotriRows <- function(e) {
  if (!is.call(e) || !identical(e[[1]], as.name("{")) || length(e) < 2L) {
    return(NULL)
  }
  .rows <- as.list(e)[-1]
  .n <- length(.rows)
  .names <- character(.n)
  .m <- matrix(0, .n, .n)
  .start <- 1L # first row of the current block
  for (.i in seq_len(.n)) {
    .r <- .rows[[.i]]
    if (!is.call(.r) || !identical(.r[[1]], as.name("~")) || length(.r) != 3L ||
          !is.name(.r[[2]])) {
      return(NULL)
    }
    .names[.i] <- as.character(.r[[2]])
    .rhs <- .r[[3]]
    if (.nlmixr2saveIsNum(.rhs)) {
      .vals <- list(.rhs)
    } else if (is.call(.rhs) && identical(.rhs[[1]], as.name("c"))) {
      .vals <- as.list(.rhs)[-1]
    } else {
      return(NULL)
    }
    if (length(.vals) == 1L) .start <- .i
    .cols <- seq.int(.start, .i)
    if (length(.vals) != length(.cols)) return(NULL)
    .vn <- names(.vals)
    if (!is.null(.vn) && !identical(.vn, .names[.cols])) return(NULL)
    for (.v in .vals) if (!.nlmixr2saveIsNum(.v)) return(NULL)
    .m[.i, .cols] <- vapply(.vals, function(v) as.double(eval(v, baseenv())),
                            double(1), USE.NAMES=FALSE)
  }
  if (anyDuplicated(.names)) return(NULL)
  .m[upper.tri(.m)] <- t(.m)[upper.tri(.m)]
  dimnames(.m) <- list(.names, .names)
  .m
}

#' `lotri()` as seen by a cache's scripts while `loadFit()` sources them
#'
#' Reads the row form `saveFit()` writes itself, and hands anything else to
#' lotri.
#' @param x,... as for `lotri::lotri()`
#' @return the matrix
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveLotri <- function(x, ...) {
  if (...length() == 0L) {
    .m <- .nlmixr2saveLotriRows(substitute(x))
    if (!is.null(.m)) return(.m)
  }
  .call <- sys.call()
  .call[[1]] <- quote(rxode2::lotri)
  eval(.call, parent.frame())
}

#' The component files a loader script reads
#'
#' Each is `<base>-<item>.<ext>` or `<base>.csv`, written in the loader as the
#' end of a single-quoted string: `'<base>-ui.R'`, or behind the path it was
#' saved under, `'/home/me/models/<base>-ui.R'`.  Found by that shape rather
#' than by pairing quotes, since the path can itself hold one -- `O'Brien/fit`
#' was written as `'O'Brien/fit-ui.R'`, which is not even valid R.
#' @param lines the loader script's lines
#' @param base the loader's base name
#' @return the file names, without any directory
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveLoaderRefs <- function(lines, base) {
  .b <- gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", base)
  # a file name holds no quote, separator or backtick; the backtick keeps a
  # garbled `~` loader's item name (`012730/fit-tab` <- read.csv('...'))
  # from being read as one
  .pat <- paste0("(?<=['/\\\\])", .b, "(?:-[^'/\\\\`]+|[.]csv)(?=')")
  unique(unlist(regmatches(lines, gregexpr(.pat, lines, perl=TRUE))))
}

#' Source a fit's loader script and return the fit
#'
#' Runs with the working directory set to the loader's directory, since the
#' loader reads its component files by name.  A loader that cannot be used as
#' it is (see `.nlmixr2saveLoaderUsable()`) is regenerated from the component
#' files beside it with the generator `saveFit()` uses, so a fit saved under
#' any path loads from wherever its files now are.  The user's files are
#' never modified: a regenerated loader is only evaluated.
#' @param r path to the loader script
#' @param checkVersion passed from [loadFit()]
#' @return the fit
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveSourceLoader <- function(r, checkVersion) {
  .base <- substr(basename(r), 1L, nchar(basename(r)) - 2L)
  .owd <- setwd(dirname(r))
  on.exit(setwd(.owd), add=TRUE)
  .env <- new.env(parent=environment())
  # the scripts call `lotri(...)` unqualified; read the blocks saveFit()
  # writes without depending on the installed lotri
  assign("lotri", .nlmixr2saveLotri, envir=.env)
  .r <- basename(r)
  .lines <- readLines(.r, warn=FALSE)
  if (file.exists(paste0(.base, "-env.R")) &&
        !.nlmixr2saveLoaderUsable(.lines, .base)) {
    .minfo("the fit's loader script refers to the path it was saved under; regenerating it")
    # only the files the original loader reads: a stray `<base>-*` file (or
    # another fit's, swept into the archive) must not be run, and a missing
    # one must not silently drop out of the fit
    .ref <- .nlmixr2saveLoaderRefs(.lines, .base)
    # every loader names `<base>.csv`, but reads it only for a fit with a data
    # table; the per-item files it always reads
    .missing <- .ref[!file.exists(.ref) & .ref != paste0(.base, ".csv")]
    if (length(.missing)) {
      stop("'", r, "' reads files that are missing: ",
           paste(.missing, collapse=", "), call.=FALSE)
    }
    .files <- .ref
    # the one thing not recoverable from the file names; it is written into
    # the loader, and FALSE only for fits from newer nlmixr2est
    .named <- !any(grepl("named=FALSE", .lines, fixed=TRUE))
    eval(parse(text=.nlmixr2saveLoaderText(.base, .files, .named),
               keep.source=FALSE), envir=.env)
  } else {
    source(.r, local=.env)
  }
  if (!exists(.base, envir=.env, inherits=FALSE)) {
    stop("'", r, "' is not a fit loader script: it does not define `", .base,
         "`", call.=FALSE)
  }
  ret <- get(.base, envir=.env, inherits=FALSE)
  ret <- .nlmixr2saveRestoreIdFactor(ret)
  .nlmixr2saveRestoreIniDf0(ret)
  .nlmixr2saveKeepIniDf0Types(ret, .lines)
  # must run while the component files still exist; it reads the csv
  .nlmixr2saveRestoreParHistType(ret, .base)
  if (isTRUE(checkVersion)) {
    .nlmixr2saveWarnVersion(ret)
  }
  ret
}

#' Load a fit from its `.zip` archive
#'
#' @param zip path of the archive
#' @param checkVersion passed on
#' @param base the fit's name inside the archive (its loader is `<base>.R`);
#'   by default the archive's own name
#' @return the fit
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveLoadZip <- function(
  zip,
  checkVersion,
  base = sub("[.]zip$", "", basename(zip), ignore.case = TRUE)
) {
  .zip <- zip
  .base <- base
  # extract to a private directory: extracting into the working directory
  # only found the loader when that was the zip's own directory, and it
  # overwrote (then deleted) same-named files that were already there
  .exdir <- tempfile("nlmixr2save-")
  dir.create(.exdir)
  on.exit(unlink(.exdir, recursive = TRUE, force = TRUE), add = TRUE)
  # flat: an archive written by an older saveFit() given a path stores its
  # files under that whole path (e.g. home/me/models/fit.R)
  .entries <- zip::zip_list(.zip)$filename
  .entries <- .entries[!endsWith(.entries, "/")]
  .dup <- unique(basename(.entries)[duplicated(basename(.entries))])
  if (length(.dup)) {
    stop(
      .zip,
      " holds more than one file named ",
      paste(.dup, collapse = ", "),
      " in different directories",
      call. = FALSE
    )
  }
  zip::unzip(.zip, exdir = .exdir, junkpaths = TRUE)
  .loader <- .nlmixr2saveFindLoader(.exdir, .base)
  if (is.null(.loader)) {
    stop("cannot find the fit loader script inside ", .zip, call. = FALSE)
  }
  .minfo(paste0("loading fit from ", .zip))
  .nlmixr2saveSourceLoader(file.path(.exdir, .loader), checkVersion)
}

#' Load a fitted model object from a file
#'
#' @param file the fit to load: the base name of the files it was saved to
#'   (`"fit"` loads `fit.zip`, or `fit.R` for a fit saved with `zip=FALSE`),
#'   or the path of that `.zip` or `.R` file itself.  It may include a
#'   directory, e.g. `"path/to/fit"` or `"path/to/fit.zip"`; the working
#'   directory is not changed and nothing is extracted into it.
#'
#' @param checkVersion when `TRUE`, warn if the fit was produced with a
#'   different nlmixr2est/rxode2 version (or remote sha) than the one currently
#'   installed.  Defaults to `getOption("nlmixr2save.checkVersion", TRUE)`.
#'
#' @return the fitted model object
#'
#' @export
loadFit <- function(file, checkVersion=.nlmixr2saveCheckVersion()) {

  .file <- as.character(substitute(file))
  .tmp <- try(force(file), silent=TRUE)
  # a try-error is itself a character string: an undefined bare symbol would
  # otherwise be taken for a file named after its error message
  if (!inherits(.tmp, "try-error") && is.character(.tmp) && length(.tmp) == 1) {
    file <- .tmp
  } else {
    file <- .file
  }
  # accept the archive or loader path itself as well as the base name; when
  # both readings name an existing file (fits saved as `my` and `my.zip`, so
  # my.zip and my.zip.zip), neither can be assumed
  if (grepl("[.](zip|R)$", file, ignore.case=TRUE) && file.exists(file) &&
        (file.exists(paste0(file, ".zip")) || file.exists(paste0(file, ".R")))) {
    .other <- c(paste0(file, ".zip"), paste0(file, ".R"))
    .other <- .other[file.exists(.other)]
    .own <- sub("[.](zip|R)$", "", file, ignore.case=TRUE)
    stop("'", file, "' names two saved fits: the file '", file, "' itself, and ",
         "the fit saved as '", file, "' ('", .other[1], "'); use loadFit(\"",
         .own, "\") for the first or loadFit(\"", .other[1],
         "\") for the second", call.=FALSE)
  }
  if (grepl("[.]zip$", file, ignore.case=TRUE) && file.exists(file)) {
    .zip <- file
    .r <- NA_character_
  } else if (grepl("[.]R$", file) && file.exists(file)) {
    .zip <- NA_character_
    .r <- file
  } else {
    .zip <- paste0(file, ".zip")
    .r <-  paste0(file, ".R")
  }
  .base <- sub("[.](zip|R)$", "", basename(if (is.na(.zip)) .r else .zip),
               ignore.case=TRUE)
  if (!is.na(.zip) && file.exists(.zip)) {
    return(.nlmixr2saveLoadZip(.zip, checkVersion))
  }
  if (!is.na(.r) && file.exists(.r)) {
    .minfo(paste0("loading fit from ", .r))
    return(.nlmixr2saveSourceLoader(.r, checkVersion))
  }
  stop("cannot find fit file ", file,
       if (!is.na(.zip)) paste0(" or ", .zip),
       if (!is.na(.r)) paste0(" or ", .r),
       call.=FALSE)
}

#' This returns or assigns the environment used in the `:=` operator
#'
#'
#' @param env environment to assign to; if `NULL` (the default), the current parent environment is returned.
#'
#' @return the environment used in the `:=` operator
#'
#' @export
#'
#' @author Matthew L. Fidler
#'
#' @keywords internal
#'
#' @examples
#' .assignParent()
.assignParent <- function(env=NULL) {
  if (is.null(env)) return(.saveFitEnv$parent)
  if (is.environment(env)) {
    .saveFitEnv$parent <- env
    return(env)
  } else {
    stop("env must be an environment", call.=FALSE)
  }
}

#' Manage functions treated as random by `:=`
#'
#' Functions registered here are saved with random-state metadata so cached
#' restores behave like the original call was run again.
#'
#' @param fun Function name(s) to add or remove. If `NULL`, the current
#'   registry is returned without modification.
#' @param remove Boolean indicating if `fun` should be removed from the
#'   registry instead of added.
#'
#' @return Character vector of registered function names.
#' @export
#'
#' @author Matthew L. Fidler
#'
#' @examples
#' saveFitRandom()
#' saveFitRandom("myRandomFun")
#' saveFitRandom("myRandomFun", remove=TRUE)
saveFitRandom <- function(fun = NULL, remove = FALSE) {
  if (!checkmate::testLogical(remove, any.missing = FALSE, len = 1L)) {
    stop("`remove` must be a single logical value", call.=FALSE)
  }
  if (is.null(fun)) {
    return(.saveFitEnv$random)
  }
  if (checkmate::testCharacter(fun, any.missing = FALSE, min.chars = 1L)) {
    .fun <- fun
  } else if (is.function(fun)) {
    .fun <- deparse1(substitute(fun))
    if (grepl("^function\\s*\\(", .fun)) {
      stop("`fun` must be a named function or character vector", call.=FALSE)
    }
  } else {
    stop("`fun` must be a character vector, function, or `NULL`", call.=FALSE)
  }
  .fun <- unique(gsub(".*::", "", .fun))
  .fun <- .fun[nzchar(.fun)]
  if (!length(.fun)) {
    stop("`fun` must contain at least one function name", call.=FALSE)
  }
  if (remove) {
    .saveFitEnv$random <- setdiff(.saveFitEnv$random, .fun)
  } else {
    .saveFitEnv$random <- unique(c(.saveFitEnv$random, .fun))
  }
  .saveFitEnv$random
}

#' Load a `:=` cache entry if the file exists (no hash check)
#'
#' Used when `getOption("nlmixr2save.check")` is `FALSE`.  Looks, inside the
#' cache directory, for `<base>.zip` (a saved fit) then `<base>.rds` (a saved
#' simulation/other value) and returns the stored object, restoring the random
#' seed when the rds carries one.  Returns the sentinel `.saveFitEnv` (an
#' environment that can never be a legitimate value) when neither exists.
#' @param base cache file base name (`<prefix><name>`, no extension)
#' @return the cached object, or `.saveFitEnv` when absent
#' @noRd
#' @author Matthew L. Fidler

#' Save/load a fit `.zip` under its bare name, with the prefix on the outer file
#'
#' A fit `.zip` is a self-contained bundle whose internal component and loader
#' names are the fit's variable name, so it must be written and read under that
#' bare name to stay a normal, interchangeable fit archive.  The
#' `nlmixr2save.prefix` therefore applies only to the *outer* file: `saveFit()`
#' writes `<x>.zip` (bare internals) in a private directory, and it is copied
#' out as `<prefix><x>.zip`; loading reads `<prefix><x>.zip` directly, looking
#' for the loader named `<x>` inside it.  Neither ever touches a `<x>.zip` in
#' the cache directory, which is some other fit's archive.  Both assume the
#' working directory is already the cache directory (the callers wrap them in
#' `.nlmixr2saveWithDir()` or set it).
#' @param value fit to save; `x` the bare variable/fit name; `data` whether the
#'   original dataset is stored (passed through to [saveFit()])
#' @return the fit (load), or `value` invisibly (save)
#' @noRd
.saveFitZipPlain <- function(value, x, data = .nlmixr2saveData()) {
  .base <- .nlmixr2saveBase(x)
  if (identical(x, .base)) {
    saveFit(value, x, zip = TRUE, data = data)
    return(invisible(value))
  }
  .stage <- tempfile("nlmixr2save-")
  dir.create(.stage)
  on.exit(unlink(.stage, recursive = TRUE, force = TRUE), add = TRUE)
  saveFit(value, file.path(.stage, x), zip = TRUE, data = data)
  # a prefix can name a directory, e.g. "run1/"
  if (!dir.exists(dirname(.base))) {
    dir.create(dirname(.base), recursive = TRUE)
  }
  # file.copy() onto a directory copies into it, and reports success
  if (
    dir.exists(paste0(.base, ".zip")) ||
      !file.copy(
        file.path(.stage, paste0(x, ".zip")),
        paste0(.base, ".zip"),
        overwrite = TRUE
      )
  ) {
    stop("could not write '", .base, ".zip'", call. = FALSE)
  }
  invisible(value)
}
#' @rdname dot-saveFitZipPlain
#' @noRd
.loadFitZipPlain <- function(x) {
  # the `:=` caller performs its own version check/rerun handling.  Load the
  # archive itself rather than resolving a name: a variable can be named
  # `fit.zip`, whose cache fit.zip.zip sits beside a `fit`'s fit.zip
  # saveFit() names the files inside by the bare base name
  .nlmixr2saveLoadZip(
    paste0(.nlmixr2saveBase(x), ".zip"),
    checkVersion = FALSE,
    base = basename(x)
  )
}

.nlmixr2saveLoadIfExists <- function(x) {
  .nlmixr2saveWithDir({
    .base <- .nlmixr2saveBase(x)
    .zip <- paste0(.base, ".zip")
    .rds <- paste0(.base, ".rds")
    if (file.exists(.zip)) {
      .minfo(paste0("loading fit from ", .zip))
      return(.loadFitZipPlain(x))
    } else if (file.exists(.rds)) {
      .rdsInfo <- readRDS(.rds)
      if (is.list(.rdsInfo) && "ret" %in% names(.rdsInfo)) {
        if (isTRUE(.rdsInfo$random) && !is.null(.rdsInfo$seed)) {
          rxode2::.rxSetSeed(.rdsInfo$seed)
          .minfo("restoring random seed to state after run")
        }
        .minfo(paste0("loading from ", .rds))
        return(.rdsInfo$ret)
      }
      # a bare object saved directly
      .minfo(paste0("loading from ", .rds))
      return(.rdsInfo)
    }
    .saveFitEnv
  })
}

#' Represent a `nlmixr2FitData` as its core fit for saving, without mutating it
#'
#' A `nlmixr2FitData` is a data.frame whose core fit environment is carried in
#' `attr(class(fit), ".foceiEnv")`.  To save the fit *without* the returned
#' prediction/residual table, we save that core environment (reclassed to the
#' non-data `nlmixr2FitCore` class) instead of the data.frame -- `saveFit()` then
#' dispatches to `saveFit.nlmixr2FitCore`, which never writes the `.csv` table.
#'
#' The core environment is shared by reference with the original `fit`, so its
#' class is set only for the duration of `code` (which does the saving) and then
#' restored, leaving the original object untouched.
#'
#' @param fit a `nlmixr2FitData` (or an already-core fit)
#' @param saver a one-argument function called with the core fit object; it must
#'   perform the save while it runs (the reclass is reverted once it returns)
#' @return the value of `saver`
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveAsCore <- function(fit, saver) {
  if (!inherits(fit, "nlmixr2FitData")) {
    # already a core fit (e.g. a calcTables=FALSE fit); nothing to strip
    return(saver(fit))
  }
  .core <- attr(class(fit), ".foceiEnv")
  if (!is.environment(.core)) {
    stop("cannot find the core fit environment to strip the output tables",
         call.=FALSE)
  }
  # core class = the fit's class minus the data.frame-ness (the FitData marker
  # and the data.frame/tibble classes), leaving the fit-core classes so the
  # environment is no longer treated as a data frame
  .coreClass <- setdiff(class(fit),
                        c("nlmixr2FitData", "tbl_df", "tbl", "data.frame"))
  .origClass <- class(.core)
  on.exit(class(.core) <- .origClass, add=TRUE)
  class(.core) <- .coreClass
  saver(.core)
}

#' Save a shareable copy of a fit without the original data
#'
#' Writes a portable `.zip` copy of a saved fit that omits the original dataset
#' (the subject-level data), so a fitted model can be shared without its data.
#' The input may be a live fit object or the base name of an existing saved fit
#' `.zip`; either way the original is left unchanged and new sibling zips are
#' written.  Names are resolved through `getOption("nlmixr2save.dir")` and
#' `getOption("nlmixr2save.prefix")` (as for the `:=` cache).
#'
#' - `nlmixr2saveShare("fit")` writes `fit-noData.zip` -- the full fit
#'   (predictions and tables intact) with the original data removed.
#' - `nlmixr2saveShare("fit", noFit=TRUE)` writes `fit-noData-noFit.zip` -- the
#'   core fit only (model, parameter estimates, objective, `omega`, `etaObf`,
#'   `parHistData`), with both the original data and the returned
#'   prediction/residual table removed.
#'
#' @param x a fitted model object, or the base name (no extension) of a saved
#'   fit `.zip` to read.
#' @param noFit Boolean; when `TRUE`, also strip the output tables (the returned
#'   prediction/residual data.frame), writing `<file>-noData-noFit.zip` instead
#'   of `<file>-noData.zip`.
#' @param file optional output base name; defaults to the name of `x`.
#' @return the path of the written `.zip`, invisibly.
#' @export
#' @seealso [saveFit()], [loadFit()]
#' @author Matthew L. Fidler
#' @examples
#' \donttest{
#'   if (requireNamespace("nlmixr2est", quietly=TRUE) &&
#'         requireNamespace("nlmixr2data", quietly=TRUE) &&
#'         requireNamespace("withr")) {
#'     library(nlmixr2est)
#'     library(nlmixr2data)
#'     withr::with_tempdir({
#'       one.cmt <- function() {
#'         ini({
#'           tka <- 0.45; tcl <- log(c(0, 2.7, 100)); tv <- 3.45
#'           eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
#'           add.sd <- 0.7
#'         })
#'         model({
#'           ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
#'           linCmt() ~ add(add.sd)
#'         })
#'       }
#'       fit <- nlmixr2(one.cmt, theo_sd, est="focei")
#'       saveFit(fit)                       # fit.zip (with data)
#'       nlmixr2saveShare("fit")            # fit-noData.zip
#'       nlmixr2saveShare("fit", noFit=TRUE) # fit-noData-noFit.zip
#'     })
#'   }
#' }
nlmixr2saveShare <- function(x, noFit=FALSE, file=NULL) {
  if (!checkmate::testLogical(noFit, any.missing=FALSE, len=1L)) {
    stop("`noFit` must be a single logical value", call.=FALSE)
  }
  # Normalize `x` like `loadFit()`: if forcing `x` yields a single string, use
  # that value as the base name; otherwise use the deparsed variable name (for a
  # fit object passed by name).  Explicit `file=` always wins.
  .sym <- as.character(substitute(x))
  .val <- try(force(x), silent=TRUE)
  .name <- if (!is.null(file)) {
    file
  } else if (!inherits(.val, "try-error") && is.character(.val) && length(.val) == 1L) {
    .val
  } else {
    .sym
  }
  if (!checkmate::testString(.name, min.chars=1L)) {
    stop("cannot determine an output name; pass `file=`", call.=FALSE)
  }
  .isFit <- !inherits(.val, "try-error") &&
    (inherits(.val, "nlmixr2FitData") || inherits(.val, "nlmixr2FitCore"))
  .nlmixr2saveWithDir({
    if (.isFit) {
      .fit <- .val
    } else {
      # treat `x`/`file` as the base name of a saved fit zip
      .readBase <- if (!is.null(file)) file else .name
      if (!file.exists(paste0(.nlmixr2saveBase(.readBase), ".zip"))) {
        stop("cannot find saved fit '", .nlmixr2saveBase(.readBase), ".zip'",
             call.=FALSE)
      }
      .fit <- .loadFitZipPlain(.readBase)
    }
    if (isTRUE(noFit)) {
      .out <- paste0(.name, "-noData-noFit")
      .minfo(paste0("writing shareable fit (no data, no output tables) to ",
                    .nlmixr2saveBase(.out), ".zip"))
      .nlmixr2saveAsCore(.fit, function(.core) .saveFitZipPlain(.core, .out, data=FALSE))
    } else {
      .out <- paste0(.name, "-noData")
      .minfo(paste0("writing shareable fit (no data) to ",
                    .nlmixr2saveBase(.out), ".zip"))
      .saveFitZipPlain(.fit, .out, data=FALSE)
    }
    invisible(paste0(.nlmixr2saveBase(.out), ".zip"))
  })
}

#' Save a `:=` value to disk by its result type (no hash check)
#'
#' A `nlmixr2` fit is saved as a portable `.zip` under its bare name, renamed to
#' `<prefix><name>.zip` (see `.saveFitZipPlain()`); a value produced by a
#' random/simulation function (`.saveFitEnv$random`, e.g. `rxSolve`/`vpcSim`) is
#' saved as a seeded `<prefix><name>.rds`; anything else is saved as a plain
#' `<prefix><name>.rds`.
#' @param value the forced result value
#' @param x the bare variable/fit name being assigned
#' @param sha1 content hash to record in the rds wrapper (may be `NULL`)
#' @return `value`, invisibly
#' @noRd
#' @author Matthew L. Fidler
.nlmixr2saveByType <- function(value, x, sha1 = NULL) {
  .nlmixr2saveWithDir({
    .base <- .nlmixr2saveBase(x)
    if (inherits(value, "nlmixr2FitData")) {
      if (!is.null(sha1) && is.environment(attr(class(value), ".foceiEnv"))) {
        assign("nlmixr2save", sha1, attr(class(value), ".foceiEnv"))
      }
      .saveFitZipPlain(value, x)
    } else if (inherits(value, "nlmixr2FitCore")) {
      if (!is.null(sha1) && is.environment(value)) assign("nlmixr2save", sha1, envir=value)
      .saveFitZipPlain(value, x)
    } else if (isTRUE(.saveFitEnv$isRandom) ||
                 .saveFitEnv$fun %in% .saveFitEnv$random) {
      .new <- rxode2::.rxGetSeed()
      saveRDS(list(ret=value, sha1=sha1,
                   random=!identical(.saveFitEnv$old, .new),
                   old=.saveFitEnv$old, seed=.new),
              paste0(.base, ".rds"))
    } else {
      saveRDS(list(ret=value, sha1=sha1), paste0(.base, ".rds"))
    }
  })
  invisible(value)
}

#' Delete every `:=` cache entry under the active prefix and directory
#'
#' Removes the cache files (`.zip`, `.rds`), the fit loader scripts (`.R`) and
#' fit component files (`-*.csv`, `-*.rds`, `-*.R`) whose names begin with the
#' current `getOption("nlmixr2save.prefix")` in the current
#' `getOption("nlmixr2save.dir")`.  Use it to force `:=` to re-run cached
#' fits/simulations on the next render when `nlmixr2save.check` is `FALSE`.
#'
#' @return invisibly, the character vector of files removed
#' @export
#' @author Matthew L. Fidler
#' @examples
#' \dontrun{
#'   options(nlmixr2save.dir = "cache", nlmixr2save.prefix = "modelPiping-")
#'   nlmixr2saveInvalidate() # clears cache/modelPiping-* entries
#' }
nlmixr2saveInvalidate <- function() {
  .dir <- .nlmixr2saveDir()
  .prefix <- .nlmixr2savePrefix()
  if (!dir.exists(.dir)) return(invisible(character(0)))
  # a prefix starting with "." names hidden files; otherwise leave them (with
  # an empty prefix, a directory's .gitignore is not a cache file)
  .all <- list.files(.dir, all.files=startsWith(.prefix, "."), no..=TRUE)
  # a literal prefix match (the prefix may contain regex metacharacters); an
  # empty prefix matches everything in the directory
  .keep <- if (nzchar(.prefix)) startsWith(.all, .prefix) else rep(TRUE, length(.all))
  .files <- file.path(.dir, .all[.keep])
  if (length(.files)) {
    .minfo(paste0("invalidating ", length(.files), " cache file(s) matching '",
                  .prefix, "*' in ", .dir))
    unlink(.files)
  }
  invisible(.files)
}

#' This assignment operator is meant to assign or load a nlmixr2 fit
#' (and other objects)
#'
#' By default it is equivalent to the standard assignment operator `<-`, but
#' it is a S3 generic so it can have other behaviors for specific classes.
#'
#' For example, when used with a nlmixr2 call, say:
#'
#' fit := nlmixr2(one.cmt, theo_sd, est="focei")
#'
#' the `:=` operator will assign the result of the `nlmixr2` call to
#' `fit`, but it will also save the fit to a file named "fit.zip" in
#' the current working directory.
#'
#' If the "fit.zip" file already exists, it will be loaded instead of
#' running the possibly expensive fitting process (as long as the sha1
#' hash of the arguments are the same).
#'
#' This allows for easy saving and loading of fitted models without
#' having to explicitly call a save function.
#'
#' This S3 generic can be extended to other classes as needed, allowing for
#' custom behaviors when assigning values to objects of those classes.
#'
#' When trying to save expensive evaluations like the output of a
#' `nlmixr2()` fit, the s3 dispach would be to `:=.assign_nlmixr2(x,
#' value)` or whatever function is used in the call.  This allows
#' checking the arguments to see if there can be a cache that will be
#' loaded.
#'
#' Otherwise, the default s3 method would be `:=.class` where `class`
#' instead. Unlike the un-evaluated function dispach there is no way
#' to check the arguments for a cache, so loading from cache is not possible.
#'
#' @section data.table:
#'
#' data.table also exports a `:=`, which only works inside `DT[...]` and
#' errors anywhere else.  When data.table is attached after nlmixr2save (for
#' example `library(nlmixr2)` followed by `library(data.table)`), nlmixr2save
#' re-attaches itself in front of data.table, so `fit := nlmixr2(...)` keeps
#' working.  data.table's own `DT[, a := b]` is unaffected, since data.table
#' handles `:=` inside `[.data.table` rather than looking it up.
#' `` nlmixr2save::`:=` `` works regardless of the search path.  Under
#' a `conflicts.policy` that makes conflicts errors (`"strict"`,
#' `"depends.ok"` or `list(error=TRUE)`, see [base::library()]) nlmixr2save
#' stays where it is and `library()` reports the conflict as usual.
#'
#' @param x the name of the object to assign the value to
#'
#' @param value the value to assign to the object, because R can use
#'   non-standard evaluation, this expression may not be evaluated
#'   when passed to the function. In the case of the `nlmixr2`
#'   function, the expression will be evaluated only if the fit needs
#'   to be refit (i.e. if the zip file does not exist or if the sha1
#'   hash of the arguments does not match).
#'
#' @return the value that was assigned to the object, invisibly. It
#'   also has the side effect of assigning the value to the parent environment.
#'
#' @seealso [saveFit()] for saving fitted model objects to files,
#'   [loadFit()] for loading fitted model objects from files, and
#'   [.assignParent()] for getting or setting the environment used in
#'   the `:=` operator.
#'
#'
#' @usage x := value
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' \donttest{
#'  if (requireNamespace("nlmixr2est", quietly=TRUE) &&
#'        requireNamespace("nlmixr2data", quietly=TRUE) &&
#'        requireNamespace("withr")) {
#'    library(nlmixr2est)
#'   library(nlmixr2data)
#'   withr::with_tempdir({
#'      one.cmt <- function() {
#'        ini({
#'          tka <- 0.45
#'          tcl <- log(c(0, 2.7, 100))
#'          tv <- 3.45
#'          eta.ka ~ 0.6
#'          eta.cl ~ 0.3
#'          eta.v ~ 0.1
#'          add.sd <- 0.7
#'        })
#'        model({
#'         ka <- exp(tka + eta.ka)
#'         cl <- exp(tcl + eta.cl)
#'         v  <- exp(tv + eta.v)
#'         linCmt() ~ add(add.sd)
#'        })
#'     }
#'     # First fit creates fit.zip
#'     fit := nlmixr2(one.cmt, theo_sd, est="focei")
#'
#'     # Second fit loads from fit.zip since it had the same options
#'     fit := nlmixr2(one.cmt, theo_sd, est="focei")
#'
#'     # Third fit refits since the options are different
#'     fit := nlmixr2(one.cmt, theo_sd, est="saem")
#'   })
#'  }
#' }
#' @export
`:=` <- function(x, value) {
  .assignParent(parent.frame())
  .saveFitEnv$isRandom <- FALSE
  .saveFitEnv$restore <- FALSE
  .subs <- substitute(value)
  .saveFitEnv$fun <- if (is.call(.subs)) gsub(".*::", "", deparse1(.subs[[1]])) else ""
  if (!.nlmixr2saveCheck()) {
    # Trusted-cache mode: load the file if it exists (no model/data/version
    # hash check), otherwise run and save it by result type.  The cache is only
    # regenerated when it is missing (see nlmixr2saveInvalidate()).
    .x <- as.character(substitute(x))
    .hit <- .nlmixr2saveLoadIfExists(.x)
    if (!identical(.hit, .saveFitEnv)) {
      assign(.x, .hit, envir=.assignParent())
      .saveFitEnv$restore <- TRUE
      return(invisible(.hit))
    }
    if (.saveFitEnv$fun %in% .saveFitEnv$random) {
      # capture the seed before running so a re-run restores it on load
      .saveFitEnv$old <- rxode2::.rxGetSeed()
    }
    .val <- force(value)
    .nlmixr2saveByType(.val, .x)
    assign(.x, .val, envir=.assignParent())
    return(invisible(.val))
  }
  if (is.call(.subs)) {
    .cls <- .saveFitEnv$fun
    if (.cls %in% c("nlmixr2", "nlmixr")) {
      .est <- .subs
      .est[[1]] <- str2lang("nlmixr2save::.nlmixr2saveProps")
      .tmp <- eval(.est, envir=.assignParent())
      .est <- .tmp$est
      if (.isRandomMethod(.est)) {
        # This is nlmixr2 but it is a random estimation method, so use
        # the default method, with random save information flagged.
        class(.subs) <- "assign_default"
        .saveFitEnv$isRandom <- TRUE
        # If this was a fit object, it is expired so remove it
        .x <- as.character(substitute(x))
        .nlmixr2saveWithDir({
          .expZip <- paste0(.nlmixr2saveBase(.x), ".zip")
          if (file.exists(.expZip)) {
            .minfo(paste0("removing expired fit file ", .expZip))
            unlink(.expZip)
          }
        })
        return(UseMethod(":=", .subs))
      }
      .x <- as.character(substitute(x))
    }
    class(.subs) <- c(paste0("assign_", .cls), "assign_default")
    return(UseMethod(":=", .subs))
  }
  UseMethod(":=", value)
}

#' @export
`:=.nlmixr2FitCore` <- function(x, value) {
  # This will be evaluated
  .x <- as.character(substitute(x))
  .nlmixr2saveWithDir(.saveFitZipPlain(value, .x))
  assign(.x, value, envir=.assignParent())
}

#' @export
`:=.assign_nlmixr2FitData` <- `:=.nlmixr2FitCore`
#' This gets the properties of the nlmixr2 call for saving and loading purposes
#'
#' This is used to:
#'  - Determine if this is a simulation method like `rxSolve`
#'
#'  - Determine the simplified core dataset that is used for estimation
#'    that allows caching on the minimum dataset.
#'
#' @inheritParams nlmixr2est::nlmixr2
#' @param ... additional arguments for nlmixr2 but ignored for this call.
#' @return A list containing
#'   - `object`: the rxode2 model to be estimated
#'   - `data`: the simplified dataset used for estimation
#'   - `est`: the estimation method
#'   - `control`: the control list used for estimation
#'   - `table`: the table used for estimation
#'   - `shaOrig`: the sha1 hash of the original data
#'   - `dataOrig`: the original data
#'   - `sha`: the sha1 hash of the list of properties used for estimation
#'
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.nlmixr2saveProps <- function(object, data, est = NULL, control=NULL, table=NULL, ...) {
  .nlmixr2data <- NULL
  if (inherits(object, "nlmixr2FitCore")) {
    .nlmixr2data <- object$origData
    object <- object$ui
  } else if (!inherits(object, "rxUi")) {
    .object <- try(rxode2::rxode2(object), silent=TRUE)
    if (inherits(.object, "try-error")) {
      stop(" in `nlmixr2(object,...)`, object must model or fit", call.=FALSE)
    }
    object <- .object
  }
  if (missing(data) && missing(est)) {
    if (!is.null(.nlmixr2data)) {
      data <- .nlmixr2data
    } else {
      data <- NULL
      est <- NULL
    }
  }
  # Change data
  if (is.character(data) && length(data) == 1 &&
        data %in% nlmixr2est::nlmixr2AllEst() &&
        is.null(est)) {
    est <- data
    if (!is.null(.nlmixr2data)) {
      data <- .nlmixr2data
    } else {
      data <- NULL
    }
  }
  if (!is.null(data) && is.data.frame(data)) {
    .dataSimplify <- try(nlmixrDataSimplify(data, object, table, est=est, control=control),
                         silent=TRUE)
    if (inherits(.dataSimplify, "try-error")) {
      .dataSimplify <- data
    }
  } else {
    .dataSimplify <- data
  }
  list(object=object,
       data=.dataSimplify,
       est=est,
       control=control,
       table=table,
       shaOrig=digest::sha1(data),
       dataOrig=data,
       sha=.digest(object$md5, .dataSimplify,
                   est, control, table, ...))
}

#' Convert environment to hash stable list
#'
#' @param env environment to convert
#' @param .visited list of already-visited environment objects for cycle detection
#' @return a list that is stable for hashing
#' @noRd
#' @author Matthew L. Fidler
.env2list <- function(env, .visited=list()) {
  if (inherits(env, "rxode2")) {
    return(rxode2::rxNorm(env))
  }
  if (any(vapply(.visited, identical, logical(1L), y=env))) {
    return(NULL)
  }
  .visited <- c(.visited, list(env))
  .names <- ls(env, all.names=TRUE)
  stats::setNames(lapply(.names,
                         function(x){
                           .ret <- get(x, envir=env)
                           if (inherits(.ret, "rxode2")) {
                             rxode2::rxNorm(.ret)
                           } else if (is.environment(.ret)) {
                             .env2list(.ret, .visited)
                           } else if (is.list(.ret)) {
                             .stableLst(.ret, .visited)
                           } else if (is.function(.ret)) {
                             deparse1(.ret)
                           } else {
                             .ret
                           }
                         }),
                  .names)
}
#' Return hash table stable list
#'
#' @param lst list to make hash stable
#' @param .visited list of already-visited environment objects for cycle detection
#' @return a list that is stable for hashing
#' @noRd
#' @author Matthew L. Fidler
.stableLst <- function(lst, .visited=list()) {
  lapply(seq_along(lst), function(i) {
    if (inherits(lst[[i]], "rxode2")) {
      rxode2::rxNorm(lst[[i]])
    } else if (is.environment(lst[[i]])) {
      .env2list(lst[[i]], .visited)
    } else if (is.function(lst[[i]])){
      deparse1(lst[[i]])
    } else if (inherits(lst[[i]], "data.table")) {
      as.data.frame(lst[[i]])
    } else if (is.list(lst[[i]])) {
      if (identical(class(lst[[i]]), "list")) {
        .stableLst(lst[[i]], .visited)
      } else {
        base::serialize(lst[[i]], NULL)
      }
    } else {
      return(lst[[i]])
    }
  })
}
#' Digest a list object
#'
#'
#' @param lst list object
#' @return a hash of the list object
#' @noRd
#' @author Matthew L. Fidler
.digest1 <- function(lst) {
  digest::sha1(.stableLst(lst))
}
#' Digest the arguments to a function call
#'
#'
#' @param ... arguments to hash
#' @return a hash of the arguments
#' @noRd
#' @author Matthew L. Fidler
.digest <- function(...) {
  .digest1(list(...))
}

#' @export
`:=.assign_nlmixr2` <- function(x, value) {
  # First see if the zip or rds file exists
  .x <- as.character(substitute(x))
  .base <- .nlmixr2saveBase(.x)
  .dir <- .nlmixr2saveDir()
  if (!identical(.dir, ".") && !identical(.dir, "")) {
    if (!dir.exists(.dir)) dir.create(.dir, recursive=TRUE, showWarnings=FALSE)
    .owd <- setwd(.dir)
    on.exit(setwd(.owd), add=TRUE)
  }
  .zip <- paste0(.base, ".zip")
  .rds <- paste0(.base, ".rds")
  .sha1 <- substitute(value)
  .sha1[[1]] <- str2lang("nlmixr2save::.nlmixr2saveProps")
  .prop <- eval(.sha1, envir=.assignParent())
  .est <- .prop$est
  .sha1 <- .prop$sha
  if (file.exists(.zip)) {
    .fit <- .loadFitZipPlain(.x)
    .isData <- inherits(.fit, "nlmixr2FitData") &&
      is.environment(attr(class(.fit), ".foceiEnv")) &&
      exists("nlmixr2save", envir=attr(class(.fit), ".foceiEnv"), inherits=FALSE) &&
      get("nlmixr2save", envir=attr(class(.fit), ".foceiEnv"), inherits=FALSE) == .sha1
    .isCore <- !inherits(.fit, "nlmixr2FitData") &&
      inherits(.fit, "nlmixr2FitCore") &&
      is.environment(.fit) &&
      exists("nlmixr2save", envir=.fit, inherits=FALSE) &&
      get("nlmixr2save", envir=.fit, inherits=FALSE) == .sha1
    if ((.isData || .isCore) && .nlmixr2saveCheckVersion() &&
          .nlmixr2saveVersionRerun(.fit)) {
      # the cache matches but was run with different package versions and the
      # user asked to rerun it with the installed versions
      .minfo(paste0("rerunning fit in ", .zip, " with the installed nlmixr2est"))
      unlink(.zip)
      .fit <- NULL
    } else if (.isData) {
      .env <- attr(class(.fit), ".foceiEnv")
      if (!exists("nlmixr2saveOrig", envir=.env, inherits=FALSE) ||
            get("nlmixr2saveOrig", envir=.env, inherits=FALSE) != .prop$shaOrig) {
        assign("origData", .prop$dataOrig, envir=.env)
        assign("nlmixr2saveOrig", .prop$shaOrig, envir=.env)
      }
      assign(as.character(substitute(x)), .fit,
             envir=.assignParent())
      .saveFitEnv$restore <- TRUE
      return(invisible(.fit))
    } else if (.isCore) {
      if (!exists("nlmixr2saveOrig", envir=.fit, inherits=FALSE) ||
            get("nlmixr2saveOrig", envir=.fit, inherits=FALSE) != .prop$shaOrig) {
        assign("origData", .prop$dataOrig, envir=.fit)
        assign("nlmixr2saveOrig", .prop$shaOrig, envir=.fit)
      }
      assign(as.character(substitute(x)), .fit,
             envir=.assignParent())
      .saveFitEnv$restore <- TRUE
      return(invisible(.fit))
    } else {
      .minfo(paste0("fit in ", .zip, " does not match current fit; removing and refitting"))
      unlink(.zip)
      .fit <- NULL
    }
  } else if (file.exists(.rds)) {
    .rdsInfo <- readRDS(.rds)
    if (is.list(.rdsInfo) &&
          length(.rdsInfo) == 2 &&
          all(c("ret", "sha1") %in% names(.rdsInfo))) {
      if (.rdsInfo$sha1 == .sha1) {
        assign(as.character(substitute(x)), .rdsInfo$ret,
               envir=.assignParent())
        .saveFitEnv$restore <- TRUE
        return(invisible(.rdsInfo$ret))
      } else {
        .minfo(paste0("fit in ", .rds, " does not match current fit; removing and refitting"))
        unlink(.rds)
      }
    } else if (is.list(.rdsInfo) && length(.rdsInfo) == 5 &&
                 all(c("ret", "sha1", "random", "old",  "seed") %in% names(.rdsInfo))) {
      if (.rdsInfo$random) {
        # Here a random number has changed in some way, need to adapt
        # the sha1 to account for the change in random seed
        if (.rdsInfo$sha1 != .sha1) {
          .minfo(paste0(.rds, " does not match prior arguments, removing and re-running"))
          unlink(.rds)
        } else if (!identical(.saveFitEnv$old, .rdsInfo$old)) {
          .minfo(paste0(.rds, " was not started with the same random state, removing and re-running"))
          unlink(.rds)
        }
      } else if (.rdsInfo$sha1 != .sha1) {
        .minfo(paste0(.rds, " does not match prior argument, removing and re-running"))
        unlink(.rds)
      }
      if (file.exists(.rds)) {
        .minfo(paste0("loading from ", .rds))
        assign(as.character(substitute(x)), .rdsInfo$ret,
               envir=.assignParent())
        if (.rdsInfo$random) {
          # Restore the seed to what it would have been if the command
          # had been run, so that the state of the random seed is the
          # same as if the command had been run, which is important
          # for reproducibility if the command changes the random seed
          # state.
          rxode2::.rxSetSeed(.rdsInfo$seed)
          .minfo("restoring random seed to state after run")
        }
        .saveFitEnv$restore <- TRUE
        return(invisible(.rdsInfo$ret))
      }
    } else {
      .minfo(paste0("fit in ", .rds, " is not in expected format; removing and refitting"))
      unlink(.rds)
    }
  }
  .fit <- force(value)
  if (inherits(.fit, "nlmixr2FitData")) {
    assign("nlmixr2save", .sha1, attr(class(.fit), ".foceiEnv"))
    assign("nlmixr2saveOrig", .prop$shaOrig, attr(class(.fit), ".foceiEnv"))
    .saveFitZipPlain(.fit, .x)
  } else if (inherits(.fit, "nlmixr2FitCore")) {
    assign("nlmixr2save", .sha1, envir=.fit)
    assign("nlmixr2saveOrig", .prop$shaOrig, envir=.fit)
    .saveFitZipPlain(.fit, .x)
  } else {
    .minfo(paste0("fit is not a nlmixr2 fit, saving to ", .base, ".rds"))
    return(.saveRds(.fit, .sha1, .x, .base))
  }
  assign(.x, value, envir=.assignParent())
  invisible(.fit)
}

#' @export
`:=.assign_nlmixr` <- `:=.assign_nlmixr2`

#' Save rds with seed information
#'
#' @param value forced value
#' @param sha1 hash of the arguments
#' @param x name of the object to assign to and save
#' @return the value that was assigned to the object, invisibly. It
#'   also has the side effect of assigning the value to the parent
#'   environment.
#' @noRd
#' @author Matthew L. Fidler
.saveSimRds <- function(value, sha1, x, base=x) {
  # The seed is saved so it will restore the state as if the command
  # had been run, which is important for reproducibility if the
  # command changes the random seed state.
  .new <- rxode2::.rxGetSeed()
  .rdsInfo <- list(ret=value, sha1=sha1, random=!identical(.saveFitEnv$old, .new),
                   old=.saveFitEnv$old, seed=.new)
  saveRDS(.rdsInfo, paste0(base, ".rds"))
  assign(x, value, envir=.assignParent())
  invisible(value)
}
#' This saves the fit info without seed information
#'
#' @param value forced value
#' @param sha1 sha1 hash of the arguments
#' @param x name of the object to assign to and save
#' @param base file base name to save to (default `x`); lets the on-disk name
#'   carry a `nlmixr2save.prefix` while the assignment keeps the bare name.
#' @return the value that was assigned to the object, invisibly. It
#'   also has the side effect of assigning the value to the parent
#'   environment.
#' @noRd
#' @author Matthew L. Fidler
.saveRds <- function(value, sha1, x, base=x) {
  .rdsInfo <- list(ret=value, sha1=sha1)
  saveRDS(.rdsInfo, paste0(base, ".rds"))
  assign(x, value, envir=.assignParent())
  invisible(value)
}

#' @export
`:=.assign_default` <- function(x, value){
  .x <- as.character(substitute(x))
  .base <- .nlmixr2saveBase(.x)
  .dir <- .nlmixr2saveDir()
  if (!identical(.dir, ".") && !identical(.dir, "")) {
    if (!dir.exists(.dir)) dir.create(.dir, recursive=TRUE, showWarnings=FALSE)
    .owd <- setwd(.dir)
    on.exit(setwd(.owd), add=TRUE)
  }
  if (checkmate::testCharacter(.saveFitEnv$fun,
                               any.missing=FALSE,
                               min.chars=1L) &&
        (.saveFitEnv$isRandom || .saveFitEnv$fun %in% .saveFitEnv$random)) {
    .saveFitEnv$old <- rxode2::.rxGetSeed()
    .rds <- paste0(.base, ".rds")
    .sha1 <- substitute(value)
    .sha1[[1]] <- quote(`list`)
    .sha1 <- .digest(eval(.sha1, envir=.assignParent()))
    .random <- FALSE
    if (file.exists(.rds)) {
      .rdsInfo <- readRDS(.rds)
      if (is.list(.rdsInfo) && length(.rdsInfo) == 5 &&
            all(c("ret", "sha1", "random", "old",  "seed") %in% names(.rdsInfo))) {
        if (.rdsInfo$random) {
          # Here a random number has changed in some way, need to adapt
          # the sha1 to account for the change in random seed
          if (.rdsInfo$sha1 != .sha1) {
            .minfo(paste0(.rds, " does not match prior arguments, removing and re-running"))
            unlink(.rds)
          } else if (!identical(.saveFitEnv$old, .rdsInfo$old)) {
            .minfo(paste0(.rds, " was not started with the same random state, removing and re-running"))
            unlink(.rds)
          }
        } else if (.rdsInfo$sha1 != .sha1) {
          .minfo(paste0(.rds, " does not match prior argument, removing and re-running"))
          unlink(.rds)
        }
        if (file.exists(.rds)) {
          .minfo(paste0("loading from ", .rds))
          assign(as.character(substitute(x)), .rdsInfo$ret,
                 envir=.assignParent())
          if (.rdsInfo$random) {
            # Restore the seed to what it would have been if the command
            # had been run, so that the state of the random seed is the
            # same as if the command had been run, which is important
            # for reproducibility if the command changes the random seed
            # state.
            rxode2::.rxSetSeed(.rdsInfo$seed)
            .minfo("restoring random seed to state after run")
          }
          .saveFitEnv$restore <- TRUE
          return(invisible(.rdsInfo$ret))
        }
      } else {
        .minfo(paste0(.rds, " does not match argument sha1, removing and re-running"))
        unlink(.rds)
      }
    }
    # Get random seed before evaluating value, so that if the value
    # changes the seed will be different and thus the sha1 needs to change
    .value <- force(value)
    .saveSimRds(.value, .sha1, .x, .base)
  } else {
    .rds <- paste0(.base, ".rds")
    .sha1 <- substitute(value)
    .sha1[[1]] <- quote(`list`)
    .sha1 <- .digest(eval(.sha1, envir=.assignParent()))
    if (file.exists(.rds)) {
      .rdsInfo <- readRDS(.rds)
      if (is.list(.rdsInfo) && length(.rdsInfo) == 2 &&
            all(c("ret", "sha1") %in% names(.rdsInfo))) {
        if (.rdsInfo$sha1 != .sha1) {
          .minfo(paste0("fit in ", .rds, " does not match prior argument, removing and re-running"))
          unlink(.rds)
        }
        if (file.exists(.rds)) {
          .minfo(paste0("loading from ", .rds))
          assign(as.character(substitute(x)), .rdsInfo$ret,
                 envir=.assignParent())
          .saveFitEnv$restore <- TRUE
          return(invisible(.rdsInfo$ret))
        }
      } else {
        .minfo(paste0(.rds, " does not match argument sha1, removing and re-running"))
        unlink(.rds)
      }
    }
    # Get random seed before evaluating value, so that if the value
    # changes the seed will be different and thus the sha1 needs to change
    .value <- force(value)
    .saveRds(.value, .sha1, .x, .base)
  }
}

#' @export
`:=.default` <- function(x, value) {
  .val <- as.character(substitute(x))
  assign(.val, value, envir=.assignParent())
}

#' Return if the last `:=` assignment was actually a restore from a
#' file
#'
#' @return `TRUE` if the last `:=` assignment was a restore from a file, `FALSE` otherwise
#' @export
#' @author Matthew L. Fidler
#' @examples
#' .assignRestore()
.assignRestore <- function() {
  .saveFitEnv$restore
}

#' Is the nlmixr2 estimation method a random method?
#'
#' @param est estimation routine
#' @param control control object.
#' @return boolean
#' @noRd
#' @author Matthew L. Fidler
.isRandomMethod <- function(est, control = NULL) {
  .v <- as.character(utils::methods("nlmixr2Est"))
  .method <- paste0("nlmixr2Est.", est)
  if (.method %in% .v) {
    .random <- attr(utils::getS3method("nlmixr2Est", est), "random")
    if (is.null(.random)) return(FALSE)
    if (is.function(.random)) return(isTRUE(.random(control)))
    return(isTRUE(.random))
  }
  FALSE
}
