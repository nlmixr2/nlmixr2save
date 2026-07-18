.saveFitEnv <- new.env(parent = emptyenv())
.saveFitEnv$rowDF <- c("parFixedDf", "shrink", "time", "objDf", "parFixed", "iniDf0")
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
#' @param current metadata from [.nlmixr2saveMeta()] (defaults to now)
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
#' @param current metadata from [.nlmixr2saveMeta()] (defaults to now)
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
#' @param file the base name of the files to save the fit to.
#' @param zip Boolean indicating if the files should be zipped.
#' @return nothing, called for side effects
#' @export
#' @author Matthew L. Fidler
#' @examples
#' \donttest{
#'   if (requireNamespace("nlmixr2est", quietly=TRUE) && requireNamespace("nlmixr2data", quietly=TRUE) && requireNamespace("withr")) {
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
saveFit <- function(fit, file, zip=TRUE) {
  UseMethod("saveFit")
}

#' @rdname saveFit
#' @export
saveFit.nlmixr2FitCore <- function(fit, file, zip=TRUE) {
  if (missing(file)) {
    file <- as.character(substitute(fit))
  }
  .item <- ls(envir=fit$env, all.names=TRUE)
  .str <- character(0)
  for (.i in .item) {
    # .nlmixr2saveMeta is written once, below, from the preserved-or-fresh value
    if (.i == ".nlmixr2saveMeta") next
    .minfo(paste0("saving fit item: ", .i))
    .obj <- get(.i, envir=fit$env)
    if (is.raw(.obj)) {
      .obj <- eval(str2lang(paste0("fit$", .i))) # decompresses object
    }
    if (!saveFitItem(.obj, .i, file)) {
      if (.i %in% c("phiC", "phiH")) {
        .lines <- deparse(as.call(c(quote(`list`), lapply(seq_along(.obj), function(i) {
          .ret <- .saveDeparse(.obj[[i]], "x")
          if (!is.null(.ret)) {
            return(.ret[[3]])
          }
          NULL # nocov
        }))))
        .lines[1] <- paste0(.i, " <- ", .lines[1])
        if (!is.null(names(.obj))) {
          .lines <- c(.lines,
                      paste0("names(", .i, ") <- ", deparse1(names(.obj))))
        }
        writeLines(.lines, con = paste0(file,"-", .i, ".R"))
      } else {
        .expr <- .saveDeparse(.obj, .i)
        if (!is.null(.expr)) {
          .expr[[1]] <- quote(`=`)
          .expr <- as.call(.expr)
          .str <- c(.str, paste(deparse(.expr), collapse="\n"))
        } else {
          warning("could not determine how to save object of class ", paste(class(.obj), collapse=", "),
                  " for item ", .i, "; as a text-file, reverting to .rds format", call.=FALSE)
          saveRDS(.obj, paste0(file, "-", .i, ".rds"))
        }
      }
    }
  }
  # nlmixr2est version/sha the fit was produced under; preserve it across a
  # load -> save round-trip (it records the run version, not the save version),
  # otherwise stamp the currently installed nlmixr2est.
  .meta <- if (exists(".nlmixr2saveMeta", envir=fit$env, inherits=FALSE)) {
    get(".nlmixr2saveMeta", envir=fit$env)
  } else {
    .nlmixr2saveMeta()
  }
  # Capture the parHistData$type factor levels from the fit itself, so the
  # restored factor matches whatever nlmixr2est produced it (the level set has
  # grown over nlmixr2est versions, e.g. "Analytic Gradient").  A hardcoded
  # fallback in the loader still covers fits saved before this was recorded.
  .parHistTypeLevel <- NULL
  if (exists("parHistData", envir=fit$env, inherits=FALSE)) {
    .phd <- get("parHistData", envir=fit$env)
    if (is.data.frame(.phd) && is.factor(.phd$type)) {
      .parHistTypeLevel <- levels(.phd$type)
    }
  }
  .cls <- as.character(class(fit))
  attr(.cls, ".foceiEnv") <- NULL
  .str <- c(.str, paste0("..class.. = ", paste(deparse(.cls), collapse="\n")),
            paste0("..id.level.. = ", paste(deparse(levels(fit$ID)), collapse="\n")),
            paste0("..parHistType.level.. = ",
                   paste(deparse(.parHistTypeLevel), collapse="\n")),
            paste0(".nlmixr2saveMeta = ", paste(deparse(.meta), collapse="\n")))
  .str <- .str[.str != "NULL = NULL"]
  .str <- paste0("env <- list(", paste(.str, collapse=",\n"), ")\nenv <- list2env(env)\n")
  writeLines(.str, con = paste0(file,"-env.R"))
  .files <- c(list.files(dirname(file), pattern=paste0(basename(file), "(-|[.]csv$|[.]R$)"),
                         full.names=TRUE))
  .files <- gsub("^[.]/", "", .files)
  # nlmixr2est <= 6.0 stores parFixedDf with named "Estimate"/"SE" columns;
  # the $parFixed refactor (nlmixr2est#645) stores them unnamed.  Record
  # which structure this fit uses so the restore script rebuilds it exactly.
  .parFixedDfNamed <- TRUE
  if (exists("parFixedDf", envir=fit$env)) {
    .pfd <- get("parFixedDf", envir=fit$env)
    if (is.data.frame(.pfd) && !is.null(.pfd$Estimate)) {
      .parFixedDfNamed <- !is.null(names(.pfd$Estimate))
    }
  }
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
                         ret <- paste0(ret,
                                       "env$iniDf0$ntheta <- as.integer(env$iniDf0$ntheta)\n",
                                       "env$iniDf0$neta1 <- as.double(env$iniDf0$neta1)\n",
                                       "env$iniDf0$neta2 <- as.double(env$iniDf0$neta2)\n",
                                       "env$iniDf0$name <- as.character(env$iniDf0$name)\n",
                                       "env$iniDf0$lower <- as.double(env$iniDf0$lower)\n",
                                       "env$iniDf0$upper <- as.double(env$iniDf0$upper)\n",
                                       "env$iniDf0$est <- as.double(env$iniDf0$est)\n",
                                       "env$iniDf0$fix <- as.logical(env$iniDf0$fix)\n",
                                       "env$iniDf0$label <- as.character(env$iniDf0$label)\n",
                                       "env$iniDf0$backTransform <- as.character(env$iniDf0$backTransform)\n",
                                       "env$iniDf0$condition <- as.character(env$iniDf0$condition)\n",
                                       "env$iniDf0$err <- as.character(env$iniDf0$err)\n")
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
  writeLines(paste0(.fq, " <- function() {\n",
                    "source('", paste0(file,"-env.R"), "', local=TRUE)\n",
                    ".class <- env$`..class..`\n",
                    ".id.level <- env$`..id.level..`\n",
                    ".parHistType.level <- env$`..parHistType.level..`\n",
                    "rm('..class..', envir=env)\n",
                    "rm('..id.level..', envir=env)\n",
                    "if (exists('..parHistType.level..', env)) rm('..parHistType.level..', envir=env)\n",
                    .r,
                    "env$model <- rxode2::model(env$ui)\n",
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
                    "  if (is.null(.phLevels)) .phLevels <- c(\"Gill83 Gradient\", \"Mixed Gradient\", \"Forward Difference\", \"Central Difference\", \"Scaled\", \"Unscaled\", \"Back-Transformed\", \"Forward Sensitivity\", \"Analytic Gradient\")\n",
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
                    .fq, " <- ", .fq, "()\n"),
             con = paste0(file,".R"))
  if (isTRUE(zip)) {
    .minfo("zipping fit files")
    .files <- c(list.files(dirname(file), pattern=paste0(basename(file), "(-|[.]csv$|[.]R$)"),
                           full.names=TRUE))
    .files <- gsub("^[.]/", "", .files)
    zip::zip(zipfile = paste0(file, ".zip"),
             files = .files)
    .minfo("removing unzipped fit files")
    lapply(.files, unlink)
  }
  invisible()
}

#' @rdname saveFit
#' @export
saveFit.nlmixr2FitData <- function(fit, file, zip=TRUE) {
  if (missing(file)) {
    file <- as.character(substitute(fit))
  }
  utils::write.csv(fit, paste0(file, ".csv"), row.names=FALSE)
  saveFit.nlmixr2FitCore(fit, file, zip=TRUE)
}

#' @rdname saveFit
#' @export
saveFit.default <- function(fit, file, zip=TRUE) {
  stop("saveFit not implemented for object of class ", paste(class(fit), collapse=", "), call.=FALSE)
}


#' Load a fitted model object from a file
#'
#' @param file the base name of the files to load the fit from.
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
  if (is.character(.tmp) && length(.tmp) == 1) {
    file <- .tmp
  } else {
    file <- .file
  }
  .zip <- paste0(file, ".zip")
  .r <-  paste0(file, ".R")
  .didUnzip <- FALSE
  if (file.exists(.zip)) {
    zip::unzip(.zip)
    .didUnzip <- TRUE
  }
  if (file.exists(.r)) {
    .minfo(paste0("loading fit from ", .r))
    source(.r, local=TRUE)
    ret <- get(file)
    if (isTRUE(checkVersion)) {
      .nlmixr2saveWarnVersion(ret)
    }
    if (.didUnzip) {
      .files <- list.files(dirname(file), pattern=paste0(basename(file), "(-|[.]csv$|[.]R$)"),
                           full.names=TRUE)
      .files <- gsub("^[.]/", "", .files)
      .minfo("removing unzipped fit files")
      lapply(.files, unlink)
    }
    return(ret)
  } else {
    stop("cannot find fit file ", file, " or ", .r, " or ", .zip, call.=FALSE) # nocov
  }
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
#' writes `<x>.zip` (bare internals) and it is then renamed to `<prefix><x>.zip`;
#' loading renames it back to `<x>.zip`, `loadFit()`s it, and restores the
#' prefixed name.  Both assume the working directory is already the cache
#' directory (the callers wrap them in [.nlmixr2saveWithDir()] or set it).
#' @param value fit to save; `x` the bare variable/fit name
#' @return the fit (load), or `value` invisibly (save)
#' @noRd
.saveFitZipPlain <- function(value, x) {
  .base <- .nlmixr2saveBase(x)
  saveFit(value, x, zip=TRUE)
  if (!identical(x, .base)) {
    if (file.exists(paste0(.base, ".zip"))) unlink(paste0(.base, ".zip"))
    file.rename(paste0(x, ".zip"), paste0(.base, ".zip"))
  }
  invisible(value)
}
#' @rdname dot-saveFitZipPlain
#' @noRd
.loadFitZipPlain <- function(x) {
  .base <- .nlmixr2saveBase(x)
  if (!identical(x, .base)) {
    file.rename(paste0(.base, ".zip"), paste0(x, ".zip"))
    on.exit(if (file.exists(paste0(x, ".zip")))
              file.rename(paste0(x, ".zip"), paste0(.base, ".zip")),
            add=TRUE)
  }
  # the `:=` caller performs its own version check/rerun handling
  loadFit(x, checkVersion=FALSE)
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

#' Save a `:=` value to disk by its result type (no hash check)
#'
#' A `nlmixr2` fit is saved as a portable `.zip` under its bare name, renamed to
#' `<prefix><name>.zip` (see [.saveFitZipPlain()]); a value produced by a
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
  .all <- list.files(.dir)
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
#' @usage NULL
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' \donttest{
#'  if (requireNamespace("nlmixr2est", quietly=TRUE) && requireNamespace("nlmixr2data", quietly=TRUE) && requireNamespace("withr")) {
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
    .dataSimplify <- try(nlmixrDataSimplify(data, object, table), silent=TRUE)
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
      exists("nlmixr2save", envir=attr(class(.fit), ".foceiEnv")) &&
      get("nlmixr2save", envir=attr(class(.fit), ".foceiEnv")) == .sha1
    .isCore <- !inherits(.fit, "nlmixr2FitData") &&
      inherits(.fit, "nlmixr2FitCore") &&
      is.environment(.fit) &&
      exists("nlmixr2save", .fit) &&
      get("nlmixr2save", .fit) == .sha1
    if ((.isData || .isCore) && .nlmixr2saveCheckVersion() &&
          .nlmixr2saveVersionRerun(.fit)) {
      # the cache matches but was run with different package versions and the
      # user asked to rerun it with the installed versions
      .minfo(paste0("rerunning fit in ", .zip, " with the installed nlmixr2est"))
      unlink(.zip)
      .fit <- NULL
    } else if (.isData) {
      .env <- attr(class(.fit), ".foceiEnv")
      if (!exists("nlmixr2saveOrig", envir=.env) ||
            get("nlmixr2saveOrig", envir=.env) != .prop$shaOrig) {
        assign("origData", .prop$dataOrig, envir=.env)
        assign("nlmixr2saveOrig", .prop$shaOrig, envir=.env)
      }
      assign(as.character(substitute(x)), .fit,
             envir=.assignParent())
      .saveFitEnv$restore <- TRUE
      return(invisible(.fit))
    } else if (.isCore) {
      if (!exists("nlmixr2saveOrig", envir=.fit) ||
            get("nlmixr2saveOrig", envir=.fit) != .prop$shaOrig) {
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
