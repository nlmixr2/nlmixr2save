.saveFitEnv <- new.env(parent = emptyenv())
.saveFitEnv$rowDF <- c("parFixedDf", "shrink", "time", "objDf", "parFixed")
.saveFitEnv$DF <- c("ranef", "etaObf", "origData", "parHistData", "iniDf0")

.minfo <- function (text, ..., .envir = parent.frame()) {
  cli::cli_alert_info(gettext(text), ..., .envir = .envir)
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
#'   if (requireNamespace("nlmixr2est", quietly=TRUE) && requireNamespace("withr")) {
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
    .minfo(paste0("saving fit item: ", .i))
    .obj <- get(.i, envir=fit$env)
    if (is.raw(.obj)) {
      .obj <- eval(str2lang(paste0("fit$", .i))) # decompresses object
    }
    if (!saveFitItem(.obj, .i, file)) {
      .expr <- .saveDeparse(.obj, .i)
      if (!is.null(.expr)) {
        .expr[[1]] <- quote(`=`)
        .expr <- as.call(.expr)
        .str <- c(.str, paste(deparse(.expr), collapse="\n"))
      } else if (.i %in% c("phiC", "phiH")) {
        .lines <- deparse(as.call(c(quote(`list`), lapply(seq_along(.obj), function(i) {
          .ret <- .saveDeparse(.obj[[i]], "x")
          if (!is.null(.ret)) {
            return(.ret[[3]])
          }
          NULL
        }))))
        .lines[1] <- paste0(.i, " <- ", .lines[1])
        if (!is.null(names(.obj))) {
          .lines <- c(.lines,
                      paste0("names(", .i, ") <- ", deparse1(names(.obj))))
        }
        writeLines(.lines, con = paste0(file,"-", .i, ".R"))
      } else {
        warning("could not determine how to save object of class ", paste(class(.obj), collapse=", "),
                " for item ", .i, "; as a text-file, reverting to .rds format", call.=FALSE)
        saveRDS(.obj, paste0(file, "-", .i, ".rds"))
      }
    }
  }
  .cls <- as.character(class(fit))
  attr(.cls, ".foceiEnv") <- NULL
  .str <- c(.str, paste0("..class.. = ", paste(deparse(.cls), collapse="\n")),
            paste0("..id.level.. = ", paste(deparse(levels(fit$ID)), collapse="\n")))
  .str <- .str[.str != "NULL = NULL"]
  .str <- paste0("env <- list(", paste(.str, collapse=",\n"), ")\nenv <- list2env(env)\n")
  writeLines(.str, con = paste0(file,"-env.R"))
  .files <- c(list.files(dirname(file), pattern=paste0(basename(file), "(-|[.]csv$|[.]R$)"),
                         full.names=TRUE))
  .files <- gsub("^[.]/", "", .files)
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
                                       "env$`parFixedDf` <- nlmixr2save::nlmixr2saveParFixedDf(env$`parFixedDf`)\n")
                       }
                     } else if (val == "iniDf0"){
                       ret <- paste0("env$iniDf0 <- read.csv('", f, "',check.names=FALSE)\n",
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
                     } else {
                       ret <- paste0("env$`", val, "` <- read.csv('", f, "', check.names=FALSE)\n")
                     }
                     return(ret)
                   }
                   if (grepl(".rds$", f)) {
                     val <- substr(f, nchar(file)+2, nchar(f)-4)
                     return(paste0("env$`", val, "` <- readRDS('", f, "')\n"))
                   }
                   NULL
                 }))
  .r <- paste0(.r, collapse="\n")
  writeLines(paste0(file, " <- function() {\n",
                    "source('", paste0(file,"-env.R"), "', local=TRUE)\n",
                    ".class <- env$`..class..`\n",
                    ".id.level <- env$`..id.level..`\n",
                    "rm('..class..', envir=env)\n",
                    "rm('..id.level..', envir=env)\n",
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
                    "  env$parHistData$type <- factor(env$parHistData$type, levels=c(\"Gill83 Gradient\", \"Mixed Gradient\", \"Forward Difference\", \"Central Difference\", \"Scaled\", \"Unscaled\", \"Back-Transformed\", \"Forward Sensitivity\"))\n",
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
                    file, " <- ", file, "()\n"),
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
#' @return the fitted model object
#'
#' @export
loadFit <- function(file) {

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

.assign <- new.env(parent=emptyenv())
.assign$parent <- globalenv()
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
  if (is.null(env)) return(.assign$parent)
  if (is.environment(env)) {
    .assign$parent <- env
    return(env)
  } else {
    stop("env must be an environment", call.=FALSE)
  }
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
#' running the possibly expensive fitting process (as long as the md5
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
#'   to be refit (i.e. if the zip file does not exist or if the md5
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
#'  if (requireNamespace("nlmixr2est", quietly=TRUE) && requireNamespace("withr")) {
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
  .subs <- substitute(value)
  if (is.call(.subs)) {
    .cls <- gsub(".*::", "", deparse1(.subs[[1]]))
    class(.subs) <- c(paste0("assign_", .cls), "assign_default")
    return(UseMethod(":=", .subs))
  }
  UseMethod(":=", value)
}

#' @export
`:=.assign_default` <- function(x, value) {
  assign(as.character(substitute(x)), value, envir=.assignParent())
}

#' @export
`:=.nlmixr2FitCore` <- function(x, value) {
  # This will be evaluated
  saveFit(value, as.character(substitute(x)), zip=TRUE)
  assign(as.character(substitute(x)), value, envir=.assignParent())
}

#' @export
`:=.assign_nlmixr2FitData` <- `:=.nlmixr2FitCore`


#' @export
`:=.assign_nlmixr2` <- function(x, value) {
  # First see if the zip file exists
  .x <- as.character(substitute(x))
  .zip <- paste0(.x, ".zip")
  .rds <- paste0(.x, ".rds")
  .md5 <- substitute(value)
  .md5[[1]] <- quote(`list`)
  .md5 <- digest::digest(.md5)
  if (file.exists(.zip)) {
    .fit <- loadFit(.x)
    if (inherits(.fit, "nlmixr2FitData") &&
          is.environment(attr(class(.fit), ".foceiEnv")) &&
          exists("nlmixr2save", envir=attr(class(.fit), ".foceiEnv")) &&
          get("nlmixr2save", envir=attr(class(.fit), ".foceiEnv")) == .md5) {
      assign(as.character(substitute(x)), .fit,
             envir=.assignParent())
      return(invisible(.fit))
    } else if (!inherits(.fit, "nlmixr2FitData") &&
                 inherits(.fit, "nlmixr2FitCore") &&
                 is.environment(.fit) &&
                 exists("nlmixr2save", .fit) &&
                 get("nlmixr2save", .fit) == .md5) {
      assign(as.character(substitute(x)), .fit,
             envir=.assignParent())
      return(invisible(.fit))
    }
    .minfo(paste0("fit in ", .zip, " does not match current fit; removing and refitting"))
    unlink(.zip)
    .fit <- NULL
  } else if (file.exists(.rds)) {
    .minfo(paste0("loading fit from ", .rds))
    .rdsInfo <- readRDS(.rds)
    if (is.list(.rdsInfo) && length(.rdsInfo) == 3 &&
          all(c("fit", "md5", "random") %in% names(.rdsInfo)) &&
          ((.rdsInfo$random && .rdsInfo$md5 == digest::digest(list(.md5, .Random.seed))) ||
             (!.rdsInfo$random && .rdsInfo$md5 == .md5))) {
      .minfo(paste0("loading from ", .rds))
      assign(as.character(substitute(x)), .rdsInfo$fit,
             envir=.assignParent())
      return(invisible(.rdsInfo$fit))
    } else {
      .minfo(paste0("fit in ", .rds, " does not match md5, removing and re-running nlmixr2"))
      unlink(.rds)
    }
  }
  .fit <- force(value)
  if (inherits(.fit, "nlmixr2FitData")) {
    assign("nlmixr2save", .md5, attr(class(.fit), ".foceiEnv"))
    saveFit(.fit, as.character(substitute(x)), zip=TRUE)
  } else if (inherits(.fit, "nlmixr2FitCore")) {
    assign("nlmixr2save", .md5, envir=.fit)
    saveFit(.fit, as.character(substitute(x)), zip=TRUE)
  } else {
    .rdsInfo <- list(fit=.fit, md5=.md5)
    saveRDS(.rdsInfo, paste0(.x, ".rds"))
    .minfo(paste0("fit is not a nlmixr2 fit, saving to ", .x, ".rds"))
  }
  assign(as.character(substitute(x)), value, envir=.assignParent())
  invisible(.fit)
}

#' @export
`:=.assign_nlmixr` <- `:=.assign_nlmixr2`

#' @export
`:=.assign_default` <- function(x, value) {
  .x <- as.character(substitute(x))
  .rds <- paste0(.x, ".rds")
  .md5 <- substitute(value)
  .md5[[1]] <- quote(`list`)
  .md5 <- digest::digest(.md5)
  .random <- FALSE
  .old <- rxode2::.rxGetSeed()
  if (file.exists(.rds)) {
    .rdsInfo <- readRDS(.rds)
    if (is.list(.rdsInfo) && length(.rdsInfo) == 4 &&
          all(c("ret", "md5", "random", "seed") %in% names(.rdsInfo))) {
      if (.rdsInfo$random) {
        # Here a random number has changed in some way, need to adapt
        # the md5 to account for the change in random seed
        .md5 <- digest::digest(list(.md5, .old))
        if (.rdsInfo$md5 != .md5) {
          .minfo(paste0(.rds, " does not match prior arguments or seed state, removing and re-running"))
          unlink(.rds)
        }
      } else if (.rdsInfo$md5 != .md5) {
        .minfo(paste0("fit in ", .rds, " does not match prior argument, removing and re-running"))
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
      }
    } else {
      .minfo(paste0(.rds, " does not match argument md5, removing and re-running"))
      unlink(.rds)
    }
  }
  # Get random seed before evaluating value, so that if the value
  # changes the seed will be different and thus the md5 needs to change
  .value <- force(value)
  .new <- rxode2::.rxGetSeed()
  if (!identical(.old, .new)) {
    .md5 <- digest::digest(list(.md5, .old))
    .random <- TRUE
  }
  # The seed is saved so it will restore the state as if the command
  # had been run, which is important for reproducibility if the
  # command changes the random seed state.
  .rdsInfo <- list(fit=.value, md5=.md5, random=.random, seed=.new)
  saveRDS(.rdsInfo, paste0(.x, ".rds"))
  assign(as.character(substitute(x)), value, envir=.assignParent())
  invisible(.value)
}

#' @export
`:=.default` <- function(x, value) {
  .val <- as.character(substitute(x))
  assign(.val, value, envir=.assignParent())
}
