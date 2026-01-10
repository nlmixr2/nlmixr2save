.saveFitEnv <- new.env(parent = emptyenv())
.saveFitEnv$rowDF <- c("parFixedDf", "shrink", "time", "objDf")
.saveFitEnv$DF <- c("ranef", "etaObf", "origData", "parHistData", "iniDf0")

.minfo <- function (text, ..., .envir = parent.frame()) {
  cli::cli_alert_info(gettext(text), ..., .envir = .envir)
}

#' This sees if you have rxode2 installed and if you want to use it
#'
#' When `nlmixr2.rxode2` option is set to `FALSE`, this function will return
#' `FALSE` even if `rxode2` is installed, allowing testing of non-rxode2 code paths.
#'
#' @return boolean
#' @keywords internal
#' @author Matthew L. Fidler
#' @noRd
.hasRxode2 <- function() {
  requireNamespace("rxode2", quietly = TRUE) &&
    isTRUE(getOption("nlmixr2.rxode2", TRUE))
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
  if (.hasRxode2()) {
    v <- try(writeLines(paste0(name, " <- ", paste(deparse(as.function(item)), collapse="\n"),
                      "\n",
                      paste0(name, " <- rxode2::rxode2(", name, ")")),
               con = paste0(file,"-", name, ".R")))
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
saveFitItem.nlmixr2ParFixed <- function(item, name, file) {
  saveRDS(item, paste0(file,"-", name, ".rds"))
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

.saveDeparse <- function(obj, name) {
  .expr <- rxode2::rxUiDeparse(obj, name)
  if (inherits(.expr, "try-error")) {
    return(NULL)
  } else if (is.null(.expr)) {
    return(NULL)
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
#'
saveFit <- function(fit, file, zip=TRUE) {
  UseMethod("saveFit")
}

#' @rdname saveFit
#' @export
saveFit.nlmixr2FitCore <- function(fit, file, zip=TRUE) {
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
      } else {
        warning("cannot save object of class ", paste(class(.obj), collapse=", "),
                " for item ", .i, "; skipping", call.=FALSE)
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
                     if (val %in% .saveFitEnv$rowDF) {
                       ret <- paste0("env$`", val, "` <- read.csv('", f, "',check.names=FALSE, row.names=1)\n")
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
                    "if (any(.class == 'nlmixr2FitCore')) {\n",
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
  .zip <- paste0(file, ".zip")
  .r <-  paste0(file, ".R")
  .didUnzip <- FALSE
  if (file.exists(.zip)) {
    zip::unzip(.zip)
    .didUnzip <- TRUE
  }
  if (file.exists(.r)) {
    message("loading fit from ", .r)
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
    stop("cannot find fit file ", file, " or ", .r, " or ", .zip, call.=FALSE)
  }
  if (grepl("[.]zip$", file)) {
    .td <- tempdir()
    zip::unzip(file, exdir = .td)
    .files <- list.files(.td, full.names=TRUE)
    .rfile <- .files[grepl("[.]R$", .files)]
    source(.rfile, local=parent.frame())
    unlink(.td, recursive=TRUE)
  } else {
    source(paste0(file,".R"), local=parent.frame())
  }
  invisible()
}
