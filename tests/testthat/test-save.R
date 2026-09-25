
oldOpt <- getOption("nlmixr2save.quiet", FALSE)


options("nlmixr2save.quiet" = TRUE)

test_that("saveFit errors on number", {
  expect_error(saveFit(1), "saveFit not implemented")
})

test_that(".assignParent errors on non-environment", {
  expect_error(.assignParent(1), "env must be an environment")
})

# A stand-in for saveFit()'s output, written with the real item writers and
# the real loader generator, so it has exactly the shape of a saved fit: a
# small ui, a data frame item, an env script, and the `<name>.R` loader.  The
# files are created as `<name>-...`; the loader is written as if saveFit() had
# been given `savedAs`, with its component files listed under `filesAs` --
# which is how older versions produced loaders tied to a path:
#   savedAs = "a/b/fit"               saveFit(fit, "a/b/fit")
#   savedAs = "/abs/a/b/fit"          saveFit(fit, "/abs/a/b/fit")
#   savedAs = "~/a/b/fit",            saveFit(fit, "~/a/b/fit"): the files
#     filesAs = "/home/me/a/b"        were listed with ~ expanded
.fakeUi <- local({
  .ui <- NULL
  function() {
    if (is.null(.ui)) {
      .f <- function() {
        ini({
          tka <- 0.45
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka)
          ka ~ add(add.sd)
        })
      }
      .ui <<- rxode2::rxode2(.f)
    }
    .ui
  }
})
.fakeSavedFit <- function(name, zip=TRUE, val=42, savedAs=name,
                          filesAs=dirname(savedAs)) {
  writeLines(paste0("env <- list(val = ", val,
                    ", ..class.. = \"nlmixr2saveFake\"",
                    ", ..id.level.. = NULL, ..parHistType.level.. = NULL)\n",
                    "env <- list2env(env)\n"),
             paste0(name, "-env.R"))
  utils::write.csv(data.frame(a=1:2), paste0(name, "-tab.csv"), row.names=FALSE)
  saveFitItem(.fakeUi(), "ui", name)
  .files <- basename(.nlmixr2saveFitFiles(name))
  if (filesAs != ".") .files <- file.path(filesAs, .files)
  writeLines(.nlmixr2saveLoaderText(savedAs, .files), paste0(name, ".R"))
  if (zip) {
    .files <- .nlmixr2saveFitFiles(name)
    zip::zip(paste0(name, ".zip"), files=.files)
    unlink(.files)
  }
}

.expectFakeFit <- function(ret, val=42) {
  expect_true(inherits(ret, "nlmixr2saveFake"))
  expect_equal(ret$val, val)
  expect_equal(ret$tab$a, 1:2)
  expect_true(inherits(ret$ui, "rxUi"))
  # restored under the item names, not names cut from a path
  expect_setequal(setdiff(ls(ret, all.names=TRUE), "..nlmixr2saveLazy.."),
                  c("val", "tab", "ui", "model"))
}

test_that("loadFit() loads a fit from another directory by path", {
  withr::with_tempdir({
    dir.create("sub")
    withr::with_dir("sub", .fakeSavedFit("fit"))
    # a same-named file in the working directory must survive the load
    writeLines("keep me", "fit.R")
    .before <- list.files(all.files=TRUE, recursive=TRUE)
    .wd <- getwd()

    .expectFakeFit(loadFit("sub/fit.zip", checkVersion=FALSE))
    .expectFakeFit(loadFit("sub/fit", checkVersion=FALSE))
    .expectFakeFit(loadFit(file.path(getwd(), "sub", "fit.zip"),
                           checkVersion=FALSE))

    expect_equal(getwd(), .wd)
    expect_equal(list.files(all.files=TRUE, recursive=TRUE), .before)
    expect_equal(readLines("fit.R"), "keep me")
  })
})

test_that("loadFit() loads a fit that was saved under a directory", {
  withr::with_tempdir({
    dir.create("a/b", recursive=TRUE)
    # saveFit(fit, "a/b/fit") names every file, and the loader's references to
    # them, "a/b/fit-..."; the archive holds them under a/b/
    .fakeSavedFit("a/b/fit")
    .expectFakeFit(loadFit("a/b/fit.zip", checkVersion=FALSE))
    # and it still loads once the archive is moved and renamed
    dir.create("moved")
    dir.create("elsewhere")
    file.rename("a/b/fit.zip", "moved/run1.zip")
    .expectFakeFit(loadFit("moved/run1.zip", checkVersion=FALSE))
    .expectFakeFit(loadFit("moved/run1", checkVersion=FALSE))

    # a hidden name (`.fit` is an ordinary R name) and regexp metacharacters,
    # zipped and not, flat and saved under a directory (the rewrite path)
    for (.nm in c(".hidden", "my+fit(1)", "a/b/.hidden", "a/b/my+fit(1)")) {
      .fakeSavedFit(.nm)
      .expectFakeFit(loadFit(paste0(.nm, ".zip"), checkVersion=FALSE))
      .expectFakeFit(loadFit(.nm, checkVersion=FALSE))
      file.rename(paste0(.nm, ".zip"), file.path("moved", basename(paste0(.nm, ".zip"))))
      .expectFakeFit(loadFit(file.path("moved", basename(.nm)), checkVersion=FALSE))
      .fakeSavedFit(.nm, zip=FALSE)
      withr::with_dir("moved", {
        .expectFakeFit(loadFit(file.path("..", paste0(.nm, ".R")), checkVersion=FALSE))
      })
    }

    # saved with the FULL path, e.g. saveFit(fit, "/home/me/proj/models/fit"):
    # every entry carries the whole path (zip drops the leading "/"), and the
    # loader names its object and reads its files by the absolute path, which
    # no longer exists once the archive has moved
    dir.create("proj/models", recursive=TRUE)
    .abs <- file.path(normalizePath("proj/models"), "fitAbs")
    suppressWarnings(.fakeSavedFit(.abs))
    .entries <- zip::zip_list(paste0(.abs, ".zip"))$filename
    expect_true(any(endsWith(.entries, "proj/models/fitAbs.R")))
    expect_true(all(grepl("/", .entries, fixed=TRUE)))
    file.rename(paste0(.abs, ".zip"), "moved/fitAbs.zip")
    unlink("proj", recursive=TRUE) # the original location is gone
    .before <- list.files(all.files=TRUE, recursive=TRUE)
    .expectFakeFit(loadFit("moved/fitAbs.zip", checkVersion=FALSE))
    withr::with_dir("elsewhere", {
      .expectFakeFit(loadFit("../moved/fitAbs", checkVersion=FALSE))
    })
    expect_equal(list.files(all.files=TRUE, recursive=TRUE), .before)

    # saved with ~ (the user's report): the loader reads some files as
    # '~/...' and lists the rest with ~ expanded, and its item names were cut
    # from those by the length of the ~ form, so they are garbage
    dir.create("tilde/Desktop/model", recursive=TRUE)
    withr::with_dir("tilde/Desktop/model", {
      .fakeSavedFit("fit", zip=FALSE, savedAs="~/Desktop/model/fit",
                    filesAs="/home/someoneelse/Desktop/model")
    })
    .ldr <- readLines("tilde/Desktop/model/fit.R")
    expect_true(any(grepl("source('~/Desktop/model/fit-env.R'", .ldr, fixed=TRUE)))
    expect_true(any(grepl("/home/someoneelse/Desktop/model/fit-tab.csv", .ldr,
                          fixed=TRUE)))
    expect_false(any(grepl("env$`tab`", .ldr, fixed=TRUE))) # garbled
    withr::with_dir("tilde", {
      zip::zip("fit.zip", files=list.files("Desktop", recursive=TRUE,
                                           full.names=TRUE))
    })
    .expectFakeFit(loadFit("tilde/fit.zip", checkVersion=FALSE))
    .expectFakeFit(loadFit("tilde/Desktop/model/fit", checkVersion=FALSE))

    # a loader tied to no path is used as it is, not regenerated
    .fakeSavedFit("own", zip=FALSE)
    .own <- readLines("own.R")
    .own <- append(.own, "env$marker <- TRUE",
                   after=grep("^delayedAssign\\('model'", .own))
    writeLines(.own, "own.R")
    expect_true(isTRUE(loadFit("own", checkVersion=FALSE)$marker))

    # a fit named like an env script: its loader is `my-env.R`
    .fakeSavedFit("my-env")
    .expectFakeFit(loadFit("my-env.zip", checkVersion=FALSE))

    # unzipped (saveFit(zip=FALSE)), loaded from another working directory
    .fakeSavedFit("a/b/plain", zip=FALSE)
    withr::with_dir("elsewhere", {
      .expectFakeFit(loadFit("../a/b/plain", checkVersion=FALSE))
      .expectFakeFit(loadFit("../a/b/plain.R", checkVersion=FALSE))
    })
    # the unzipped files are the user's; loading leaves them in place
    expect_true(all(file.exists(c("a/b/plain.R", "a/b/plain-env.R",
                                  "a/b/plain-tab.csv", "a/b/plain-ui.R"))))
    # and the loader there is not rewritten
    expect_true(any(grepl("a/b/plain-env.R", readLines("a/b/plain.R"), fixed=TRUE)))
  })
})

test_that(".nlmixr2saveLoaderUsable accepts only a loader tied to no path", {
  .ok <- c("`fit` <- function() {", "source('fit-env.R', local=TRUE)",
           "env$`tab` <- read.csv('fit-tab.csv', check.names=FALSE)",
           "env", "}", "`fit` <- `fit`()")
  expect_true(.nlmixr2saveLoaderUsable(.ok, "fit"))
  # the pre-backtick form of older versions
  .old <- sub("`fit`", "fit", .ok, fixed=TRUE)
  .old <- gsub("`fit`", "fit", .old, fixed=TRUE)
  expect_true(.nlmixr2saveLoaderUsable(.old, "fit"))
  # another name than the file's (renamed, or saved under a path)
  expect_false(.nlmixr2saveLoaderUsable(.ok, "run1"))
  .p <- gsub("fit", "/home/me/models/fit", .ok, fixed=TRUE)
  expect_false(.nlmixr2saveLoaderUsable(.p, "fit"))
  # the right name, but a file read from a path (relative, ~, or Windows)
  for (.d in c("models/", "~/models/", "/home/me/", "C:\\\\Users\\\\me\\\\")) {
    expect_false(.nlmixr2saveLoaderUsable(
      sub("'fit-env.R'", paste0("'", .d, "fit-env.R'"), .ok, fixed=TRUE), "fit"))
  }
  # not a loader at all
  expect_false(.nlmixr2saveLoaderUsable(character(0), "fit"))
  expect_false(.nlmixr2saveLoaderUsable("x <- 1", "fit"))
  expect_false(.nlmixr2saveLoaderUsable("x <- (", "fit"))
})

test_that("the := loader takes the archive of a variable named like a zip", {
  withr::with_tempdir({
    # `my.zip` is a valid variable name, so its cache is my.zip.zip; a fit
    # called `my` sits beside it as my.zip and must not be picked up instead
    .fakeSavedFit("my.zip", val=1)
    .fakeSavedFit("my", val=2)
    expect_true(all(file.exists(c("my.zip.zip", "my.zip"))))
    .expectFakeFit(.loadFitZipPlain("my.zip"), val=1)
    .expectFakeFit(.loadFitZipPlain("my"), val=2)
  })
})

test_that("the lotri blocks saveFit() writes are read without lotri", {
  # the block from the pkgdown site's failing `:=` example: a development
  # lotri rejected its named, wrapped rows
  .site <- quote({
    tka ~ c(tka = 0.0367813010669496)
    tcl ~ c(tka = -0.000782456725690811, tcl = 0.0069543545026029)
    tv ~ c(tka = 0.000952366834165392, tcl = -0.000489959598361193,
           tv = 0.00218330787936497)
    add.sd ~ c(tka = -6.41258631708858e-05, tcl = -5.67004131128066e-05,
               tv = 3.82936894597721e-05, add.sd = 0.00243929995816957)
    om.eta.ka ~ c(tka = 0.000146993075376898, tcl = 0.000126165231341427,
                  tv = -0.000127191366433412, add.sd = -0.000541130240379496,
                  om.eta.ka = 0.0353763038424305)
  })
  # unnamed rows (R, S, phiC, ...) and a diagonal-only block (omega)
  .unnamed <- quote({
    tka ~ 22.5656297333401
    tcl ~ c(-7.59115223994971, 159.478624702269)
    tv ~ c(-30.7802054115303, 26.7036854081949, 603.566773588088)
  })
  .diag <- quote({
    eta.ka ~ 0.39790253245538
    eta.cl ~ 0.0702730989050519
  })
  .one <- quote({
    eta.ka ~ -Inf
  })
  # a single value starts a new block, as lotri reads it
  .blocks <- quote({
    a ~ 1
    b ~ c(0.5, 2)
    c ~ c(c = 3)
    d ~ c(c = 0.25, d = 4)
  })
  for (.b in list(.site, .unnamed, .diag, .one, .blocks)) {
    .m <- .nlmixr2saveLotriRows(.b)
    expect_false(is.null(.m))
    expect_identical(.m, eval(bquote(rxode2::lotri(.(.b)))))
  }
  # and through the lotri() the scripts see
  expect_identical(.nlmixr2saveLotri({
    eta.ka ~ 0.39790253245538
    eta.cl ~ c(0.1, 0.0702730989050519)
  }), rxode2::lotri({
    eta.ka ~ 0.39790253245538
    eta.cl ~ c(0.1, 0.0702730989050519)
  }))

  # anything else goes to lotri: a joint block, fix()
  expect_null(.nlmixr2saveLotriRows(quote({a + b ~ c(1, 0.5, 1)})))
  expect_identical(.nlmixr2saveLotri({a + b ~ c(1, 0.5, 1)}),
                   rxode2::lotri({a + b ~ c(1, 0.5, 1)}))
  expect_null(.nlmixr2saveLotriRows(quote({a ~ fix(1)})))
  # and extra arguments always do
  expect_identical(.nlmixr2saveLotri({a ~ 1}, cov=TRUE),
                   rxode2::lotri({a ~ 1}, cov=TRUE))

  # names that disagree with the rows, or a wrong row length, are not the
  # row form either
  expect_null(.nlmixr2saveLotriRows(quote({a ~ 1; b ~ c(x = 0.1, b = 1)})))
  expect_null(.nlmixr2saveLotriRows(quote({a ~ 1; b ~ c(0.1, 1, 2)})))
  # a row as long as the whole matrix, but not its block, is not lotri's form
  expect_null(.nlmixr2saveLotriRows(quote({a ~ 1; b ~ 2; c ~ c(0.1, 0.2, 3)})))
  expect_null(.nlmixr2saveLotriRows(quote({a ~ 1; a ~ c(0.1, 1)})))
  # only numbers are ever evaluated
  expect_null(.nlmixr2saveLotriRows(quote({a ~ c(a = stop("evaluated"))})))
  expect_null(.nlmixr2saveLotriRows(quote({a ~ log(2)})))
  # lotri rejects NA, so the reader leaves it to lotri as well
  expect_null(.nlmixr2saveLotriRows(quote({a ~ NA})))
  expect_null(.nlmixr2saveLotriRows(quote({a ~ 1; b ~ c(NA_real_, 2)})))
  # while the forms it does read match lotri.  Compared with a tolerance:
  # lotri's own parse of an extreme value is inexact on some platforms (on
  # macOS arm64, -1e-300 comes back as -9.999999985e-301), where the reader
  # evaluates the literal exactly
  for (.b in list(quote({a ~ 1L}), quote({`a b` ~ 1; c ~ c(0.1, 2)}),
                  quote({a ~ 1; b ~ c(Inf, 2)}),
                  quote({a ~ -0; b ~ c(-1e-300, +2)}))) {
    expect_equal(.nlmixr2saveLotriRows(.b),
                 eval(bquote(rxode2::lotri(.(.b)))))
  }
  expect_identical(.nlmixr2saveLotriRows(quote({a ~ 1; b ~ c(-1e-300, 2)}))[2, 1],
                   -1e-300)
})

test_that("loadFit() refuses a name that means two saved fits", {
  withr::with_tempdir({
    # fits saved as `my` and `my.zip`: my.zip is one's archive and the other's
    # base name
    .fakeSavedFit("my", val=2)
    .fakeSavedFit("my.zip", val=1)
    expect_error(loadFit("my.zip", checkVersion=FALSE),
                 'names two saved fits.*loadFit\\("my"\\).*loadFit\\("my.zip.zip"\\)')
    .expectFakeFit(loadFit("my", checkVersion=FALSE), val=2)
    .expectFakeFit(loadFit("my.zip.zip", checkVersion=FALSE), val=1)
  })
})

test_that("a regenerated loader reads exactly the files the original one did", {
  withr::with_tempdir({
    dir.create("a/b", recursive=TRUE)
    .fakeSavedFit("a/b/x", zip=FALSE)
    # a stray file that only matches the name is never run
    writeLines('stop("a stray script was run")', "a/b/x-extra.R")
    .ret <- loadFit("a/b/x", checkVersion=FALSE)
    .expectFakeFit(.ret)
    # a file the loader reads, but that is gone, is an error -- not an item
    # silently missing from the fit
    unlink("a/b/x-tab.csv")
    expect_error(loadFit("a/b/x", checkVersion=FALSE),
                 "reads files that are missing: x-tab.csv")
  })
})

test_that(".nlmixr2saveLoaderRefs finds the files a loader reads, by their shape", {
  .l <- c("source('O'Brien/fit-ui.R', local=TRUE)", # an apostrophe in the path
          "env$`x` <- read.csv('/home/me/fit-tab.csv')",
          "ret <- read.csv('C:\\\\Users\\\\me\\\\fit.csv')",
          "source('fit-env.R', local=TRUE)",
          "source('fitX-ui.R', local=TRUE)",   # another fit
          "source('my.fit-ui.R', local=TRUE)", # another fit, `.` not a wildcard
          # a garbled `~` loader: its item name is not a file name
          "env$`012730/fit-tab` <- read.csv('/home/me/fit-tab.csv')")
  expect_setequal(.nlmixr2saveLoaderRefs(.l, "fit"),
                  c("fit-ui.R", "fit-tab.csv", "fit.csv", "fit-env.R"))
  expect_equal(.nlmixr2saveLoaderRefs(.l, "my.fit"), "my.fit-ui.R")
  expect_equal(.nlmixr2saveLoaderRefs("source('a/my+fit(1)-ui.R')", "my+fit(1)"),
               "my+fit(1)-ui.R")
})

test_that("a fit saved under a path with an apostrophe loads", {
  withr::with_tempdir({
    # the loader older versions wrote for saveFit(fit, "O'Brien/fit") is not
    # even valid R: 'O'Brien/fit-ui.R'
    dir.create("O'Brien")
    .fakeSavedFit("O'Brien/fit", zip=FALSE)
    expect_error(parse("O'Brien/fit.R"))
    .expectFakeFit(loadFit("O'Brien/fit", checkVersion=FALSE))
    withr::with_dir("O'Brien", {
      zip::zip("fit.zip", files=list.files(all.files=TRUE, no..=TRUE))
    })
    dir.create("moved")
    file.rename("O'Brien/fit.zip", "moved/fit.zip")
    .expectFakeFit(loadFit("moved/fit.zip", checkVersion=FALSE))
  })
})

test_that(".nlmixr2saveIsPromise tells a loader's promise from an assigned value", {
  .e <- new.env()
  .e$`..nlmixr2saveLazy..` <- list()
  # the loader's promises all refer to ..nlmixr2saveLazy..
  delayedAssign("p", {
    `..nlmixr2saveLazy..`
    stop("never forced here")
  }, eval.env = .e, assign.env = .e)
  expect_true(.nlmixr2saveIsPromise("p", .e))
  delayedAssign("q", {
    `..nlmixr2saveLazy..`
    1
  }, eval.env = .e, assign.env = .e)
  force(.e$q)
  expect_true(.nlmixr2saveIsPromise("q", .e)) # forced, but still the promise
  assign("q", 2, envir = .e)
  expect_false(.nlmixr2saveIsPromise("q", .e)) # replaced by a value
  assign("q", quote(f(x)), envir = .e)
  expect_false(.nlmixr2saveIsPromise("q", .e)) # replaced by a call
  delayedAssign("r", identity(1), assign.env = .e)
  expect_false(.nlmixr2saveIsPromise("r", .e)) # someone else's promise
  expect_false(.nlmixr2saveIsPromise("missing", .e))
})

test_that("a fit's compiled model lists are built only when first used", {
  withr::with_tempdir({
    # the model list's script is evaluated (compiled) only on access; here it
    # would fail loudly, so a load that touched it could not pass
    .fakeSavedFit("lz", zip=FALSE)
    writeLines('foceiModel <- stop("compiled while loading")', "lz-foceiModel.R")
    writeLines(.nlmixr2saveLoaderText("lz", setdiff(.nlmixr2saveFitFiles("lz"), "lz.R")),
               "lz.R")
    zip::zip("lz.zip", files=.nlmixr2saveFitFiles("lz"))
    unlink(.nlmixr2saveFitFiles("lz"))
    .ret <- loadFit("lz.zip", checkVersion=FALSE)
    expect_equal(.ret$val, 42)
    # its script was read while the files existed, and is kept for saveFit()
    expect_equal(.ret$`..nlmixr2saveLazy..`$foceiModel,
                 'foceiModel <- stop("compiled while loading")')
    # the extracted files are gone, yet first use still evaluates it
    expect_error(.ret$foceiModel, "compiled while loading")
  })
})

test_that("nlmixr2saveInvalidate() clears a hidden prefix, and only that", {
  withr::with_tempdir({
    dir.create("models")
    file.create(c("models/.pk-fit.zip", "models/.pk-sim.rds", "models/.gitignore",
                  "models/fit.zip"))
    withr::with_options(list(nlmixr2save.dir="models", nlmixr2save.prefix=".pk-",
                             nlmixr2save.quiet=TRUE), {
      nlmixr2saveInvalidate()
    })
    expect_equal(sort(list.files("models", all.files=TRUE, no..=TRUE)),
                 c(".gitignore", "fit.zip"))
    # an empty prefix clears the caches but not the directory's hidden files
    withr::with_options(list(nlmixr2save.dir="models", nlmixr2save.prefix="",
                             nlmixr2save.quiet=TRUE), {
      nlmixr2saveInvalidate()
    })
    expect_equal(list.files("models", all.files=TRUE, no..=TRUE), ".gitignore")
  })
})

test_that("loadFit() takes a bare symbol naming a saved fit", {
  withr::with_tempdir({
    .fakeSavedFit("myfit")
    expect_false(exists("myfit", inherits = FALSE))
    .expectFakeFit(loadFit(myfit, checkVersion=FALSE))
  })
})

test_that("loadFit() errors clearly on a missing fit or a foreign zip", {
  withr::with_tempdir({
    expect_error(loadFit("nope.zip", checkVersion=FALSE), "cannot find fit file")
    expect_error(loadFit("nope", checkVersion=FALSE), "cannot find fit file")
    writeLines("x", "readme.txt")
    zip::zip("other.zip", files="readme.txt")
    expect_error(loadFit("other.zip", checkVersion=FALSE),
                 "cannot find the fit loader script")
    # an empty or unrelated .R, e.g. from an interrupted save
    file.create("empty.R")
    expect_error(loadFit("empty.R", checkVersion=FALSE),
                 "is not a fit loader script")
    writeLines("x <- 1", "notfit.R")
    expect_error(loadFit("notfit", checkVersion=FALSE),
                 "is not a fit loader script")
  })
})

test_that(".nlmixr2saveRestoreIniDf0 matches iniDf0 to the installed rxode2", {
  # the loaded fit's ui is rebuilt by the installed rxode2, so its iniDf is the
  # template; a list stands in for it here, since `$` is all that is used
  .tmpl <- data.frame(name=character(0), est=double(0), prior=character(0),
                      err=character(0))
  .withIni <- function(ini, ui=list(iniDf=.tmpl)) {
    .env <- new.env(parent=emptyenv())
    assign("iniDf0", ini, envir=.env)
    if (!is.null(ui)) assign("ui", ui, envir=.env)
    .nlmixr2saveRestoreIniDf0(.env)
    .env$iniDf0
  }

  # a cache from before rxode2 had `prior` gains it, typed and in place
  .i <- .withIni(data.frame(name=c("a", "b"), est=c(1, 2), err=c(NA, "add")))
  expect_equal(names(.i), c("name", "est", "prior", "err"))
  expect_identical(.i$prior, c(NA_character_, NA_character_))
  expect_equal(.i$err, c(NA, "add"))

  # an all-NA prior read back from the csv as logical is made character
  .i <- .withIni(data.frame(name="a", est=1, prior=NA, err="add"))
  expect_identical(.i$prior, NA_character_)
  # and a real prior is kept as it is
  .i <- .withIni(data.frame(name="a", est=1, prior="dnorm(0, 1)", err="add"))
  expect_identical(.i$prior, "dnorm(0, 1)")

  # an older rxode2 without `prior` keeps the cache's column, after its own
  .i <- .withIni(data.frame(name="a", est=1, prior="dnorm(0, 1)", err="add"),
                 ui=list(iniDf=.tmpl[, c("name", "est", "err")]))
  expect_equal(names(.i), c("name", "est", "err", "prior"))

  # row names survive (iniDf0 is read with row.names=1)
  .i <- .withIni(data.frame(name="a", est=1, err="add", row.names="7"))
  expect_equal(row.names(.i), "7")

  # with no ui to compare against, only prior is retyped and nothing added
  .i <- .withIni(data.frame(name="a", prior=NA), ui=NULL)
  expect_identical(.i$prior, NA_character_)
  expect_equal(names(.i), c("name", "prior"))
  .i <- .withIni(data.frame(name="a"), ui=NULL)
  expect_equal(names(.i), "name")

  # a real ui, compressed as the loader leaves it, works as the template
  .f <- function() {
    ini({
      tka <- 0.45
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka)
      ka ~ add(add.sd)
    })
  }
  .ui <- rxode2::rxUiCompress(rxode2::rxode2(.f))
  .real <- rxode2::rxUiDecompress(.ui)$iniDf
  .old <- .real[, setdiff(names(.real), "prior"), drop=FALSE]
  .i <- .withIni(.old, ui=.ui)
  expect_equal(names(.i), names(.real))
  if (!is.null(.real$prior)) {
    expect_identical(.i$prior, rep(NA_character_, nrow(.real)))
  }

  # a fit without iniDf0 is left alone
  .noIni <- new.env()
  expect_identical(.nlmixr2saveRestoreIniDf0(.noIni), .noIni)
  expect_false(exists("iniDf0", envir=.noIni, inherits=FALSE))
})

test_that("saveFitRandom adds and removes registered random functions", {
  .old <- saveFitRandom()
  on.exit(saveFitRandom(.old), add = TRUE)

  saveFitRandom(c("fooRandom", "pkg::barRandom"))
  .random <- saveFitRandom()

  expect_true(all(c("fooRandom", "barRandom") %in% .random))
  expect_equal(sum(.random == "fooRandom"), 1)
  expect_equal(sum(.random == "barRandom"), 1)

  saveFitRandom("barRandom", remove = TRUE)
  .random <- saveFitRandom()

  expect_true("fooRandom" %in% .random)
  expect_false("barRandom" %in% .random)
})

test_that("tracked-package version metadata helpers", {
  .cur <- .nlmixr2saveMeta()
  expect_true(is.list(.cur))
  expect_true(all(c("nlmixr2est", "rxode2", "nlmixr2save") %in% names(.cur)))
  expect_true(all(c("version", "sha") %in% names(.cur$nlmixr2est)))
  expect_true(all(c("version", "sha") %in% names(.cur$rxode2)))
  # metadata must deparse/reparse (it is embedded as text in the loader script)
  expect_equal(eval(parse(text = paste(deparse(.cur), collapse = "\n"))), .cur)

  .same <- list(nlmixr2est = list(version = "1.2.3", sha = NA_character_),
                rxode2 = list(version = "5.0.0", sha = NA_character_))
  .estDiff <- list(nlmixr2est = list(version = "9.9.9", sha = NA_character_),
                   rxode2 = list(version = "5.0.0", sha = NA_character_))
  .rxDiff <- list(nlmixr2est = list(version = "1.2.3", sha = NA_character_),
                  rxode2 = list(version = "6.0.0", sha = NA_character_))
  expect_false(.nlmixr2saveMetaDiffers(.same, .same))
  # a difference in EITHER tracked package is flagged
  expect_true(.nlmixr2saveMetaDiffers(.same, .estDiff))
  expect_true(.nlmixr2saveMetaDiffers(.same, .rxDiff))
  expect_equal(.nlmixr2saveChanged(.same, .estDiff), "nlmixr2est")
  expect_equal(.nlmixr2saveChanged(.same, .rxDiff), "rxode2")
  # nothing to compare -> no complaint (older saves, or package absent)
  expect_false(.nlmixr2saveMetaDiffers(NULL, .same))
  expect_false(.nlmixr2saveMetaDiffers(
    list(nlmixr2est = list(version = NA_character_)), .estDiff))
  # same version, different remote sha still counts as different
  .s1 <- list(rxode2 = list(version = "5.0.0", sha = "aaaaaaaaaaaa"))
  .s2 <- list(rxode2 = list(version = "5.0.0", sha = "bbbbbbbbbbbb"))
  expect_true(.nlmixr2saveMetaDiffers(.s1, .s2))

  expect_equal(.nlmixr2savePkgLabel(.same, "nlmixr2est"), "1.2.3")
  expect_equal(.nlmixr2savePkgLabel(.s1, "rxode2"), "5.0.0 (aaaaaaaaaa)")
  expect_equal(.nlmixr2savePkgLabel(NULL, "rxode2"), "(unknown)")
  # a bare version string (older metadata shape) is tolerated
  expect_equal(.nlmixr2savePkgLabel(list(rxode2 = "5.0.0"), "rxode2"), "5.0.0")

  expect_match(.nlmixr2saveVersionMsg(.same, .estDiff),
               "nlmixr2est 1.2.3 \\(installed 9.9.9\\)")
  expect_match(.nlmixr2saveVersionMsg(.same, .rxDiff),
               "rxode2 5.0.0 \\(installed 6.0.0\\)")
})

test_that("version-mismatch warning/rerun decision on a stub fit", {
  .env <- new.env(parent = emptyenv())
  assign(".nlmixr2saveMeta",
         list(nlmixr2est = list(version = "0.0.0-old", sha = NA_character_),
              rxode2 = list(version = "0.0.0-old", sha = NA_character_),
              nlmixr2save = "1"),
         envir = .env)
  class(.env) <- c("nlmixr2FitCore", "environment")
  expect_equal(.nlmixr2saveGetMeta(.env)$nlmixr2est$version, "0.0.0-old")

  # a fit with no stored metadata -> nothing to compare, no warning
  .noMeta <- new.env(parent = emptyenv())
  class(.noMeta) <- c("nlmixr2FitCore", "environment")
  expect_null(.nlmixr2saveGetMeta(.noMeta))
  expect_warning(.nlmixr2saveWarnVersion(.noMeta), NA)

  # stub whose stored versions differ from the installed packages: the
  # non-interactive branch warns and does not request a rerun
  skip_if_not_installed("nlmixr2est")
  expect_warning(.nlmixr2saveWarnVersion(.env),
                 "run with nlmixr2est 0.0.0-old")
  if (!interactive()) {
    expect_false(suppressWarnings(.nlmixr2saveVersionRerun(.env)))
  }
})

test_that("nlmixr2save.checkVersion option gates the check", {
  skip_if_not_installed("withr")
  expect_true(.nlmixr2saveCheckVersion())
  withr::with_options(list(nlmixr2save.checkVersion = FALSE), {
    expect_false(.nlmixr2saveCheckVersion())
    # loadFit's checkVersion argument defaults to the option
    expect_false(eval(formals(loadFit)$checkVersion))
  })
  withr::with_options(list(nlmixr2save.checkVersion = TRUE), {
    expect_true(.nlmixr2saveCheckVersion())
  })
})

if (requireNamespace("withr", quietly = TRUE)) {

  test_that("saveFitRandom marks registered functions as random", {
    .old <- saveFitRandom()
    on.exit(saveFitRandom(.old), add = TRUE)

    randomFun <- function() 1
    saveFitRandom(randomFun)

    withr::with_tempdir({
      rxode2::rxSetSeed(42)
      res := randomFun()
      .r <- readRDS("res.rds")
      expect_named(.r, c("ret", "sha1", "random", "old", "seed"))
    })
  })

  test_that(":= with rxSolve requires seed to be set to restore", {
    skip_on_cran()
    withr::with_tempdir({

      suppressWarnings(rxode2::rxWithSeed(42, {

        library(rxode2)
        library(nlmixr2data)

        one.cmt <- function() {
          ini({
            ## You may label each parameter with a comment
            tka <- 0.45 # Log Ka
            tcl <- log(c(0, 2.7, 100)) # Log Cl
            ## This works with interactive models
            ## You may also label the preceding line with label("label text")
            tv <- 3.45; label("log V")
            ## the label("Label name") works with all models
            eta.ka ~ 0.6
            eta.cl ~ 0.3
            eta.v ~ 0.1
            add.sd <- 0.7
          })
          model({
            ka <- exp(tka + eta.ka)
            cl <- exp(tcl + eta.cl)
            v <- exp(tv + eta.v)
            linCmt() ~ add(add.sd)
          })
        }

        rxode2::rxSetSeed(42)
        set.seed(42)
        solve42 := rxSolve(one.cmt, theo_sd)
        expect_false(.assignRestore())
        .new <- rxode2::.rxGetSeed()

        skip_if(!file.exists("solve42.rds"))

        .r <- readRDS("solve42.rds")
        expect_true(.r$random)
        expect_equal(.new, .r$seed)

        rxode2::rxSetSeed(42)
        set.seed(42)
        solve42 := rxSolve(one.cmt, theo_sd)
        expect_true(.assignRestore())
        expect_equal(.new, rxode2::.rxGetSeed())

        if (requireNamespace("nlmixr2est", quietly = TRUE)) {

          library(nlmixr2est)

          rxode2::rxSetSeed(42)
          set.seed(42)
          solveEst := nlmixr2(one.cmt, theo_sd, est="rxSolve")
          .new <- rxode2::.rxGetSeed()

          expect_false(.assignRestore())

          skip_if(!file.exists("solveEst.rds"))
          .r <- readRDS("solveEst.rds")
          expect_true(.r$random)
          expect_equal(.new, .r$seed)

          rxode2::rxSetSeed(42)
          set.seed(42)
          solveEst := nlmixr2(one.cmt, theo_sd, est="rxSolve")
          expect_true(.assignRestore())
          expect_equal(.new, rxode2::.rxGetSeed())

          if (requireNamespace("babelmixr2", quietly = TRUE) &&
                requireNamespace("PopED", quietly = TRUE)) {

            library(babelmixr2)
            library(PopED)

            f <- function() {
              ini({
                tKA <- 0.25
                tCL <- 3.75
                tV <- 72.8
                Favail <- fix(0.9)
                eta.ka ~ 0.09
                eta.cl ~ 0.25 ^ 2
                eta.v ~ 0.09
                prop.sd <- sqrt(0.04)
                add.sd <- sqrt(0.0025)
              })
              model({
                ka <- tKA * exp(eta.ka)
                v <- tV * exp(eta.v)
                cl <- tCL * exp(eta.cl)
                d/dt(depot) <- -ka * depot
                d/dt(central) <- ka * depot - cl / v * central
                cp <- central / v
                f(depot) <- DOSE * Favail
                cp ~ add(add.sd) + prop(prop.sd)
              })
            }

            f <- f() # compile/check nlmixr2/rxode2 model

            e <- et(amt=1, ii=24, until=250) %>%
              et(time=c(1,2,8,240,245)) %>%
              as.data.frame() %>%
              dplyr::mutate(low=c(NA_real_, 0, 0, 0, 240, 240),
                            high=c(NA_real_, 10, 10, 10, 248, 248))

            # Create a PopED database for `nlmixr2`:
            poped := nlmixr(f, e, "poped",
                            popedControl(a=list(c(DOSE=20),
                                                c(DOSE=40)),
                                         maxa=c(DOSE=200),
                                         mina=c(DOSE=0)))
            expect_true(file.exists("poped.rds"))
            expect_false(.assignRestore())

            poped := nlmixr(f, e, "poped",
                            popedControl(a=list(c(DOSE=20),
                                                c(DOSE=40)),
                                         maxa=c(DOSE=200),
                                         mina=c(DOSE=0)))
            expect_true(.assignRestore())

          }
        }
      }))
    })
  })

  withr::with_tempdir({
    test_that("test rxUi item saving with rxode2", {

      library(rxode2)

      ui <- function() {
        ini({
          CL <- 1
          V <- 10
        })
        model({
          k = CL / V
          d/dt(centr) <- -k * centr
        })
      }

      saveFitItem(ui(), "rxUi", "testfit")
      expect_true(file.exists("testfit-rxUi.R"))
      rm("ui")
      source("testfit-rxUi.R", local=TRUE)
      expect_true(exists("rxUi"))
      expect_true(inherits(rxUi, "rxUi"))

    })
  })
}

withr::with_tempdir({
  test_that("saveFitItem handles data.frame as expected", {

    df1 <- data.frame(a = 1:3, row.names = c("sub1", "sub2", "sub3"))
    res1 <- saveFitItem(df1, "parFixedDf", "testfit")
    expect_true(res1)
    expect_true(file.exists("testfit-parFixedDf.csv"))
    df2 <- read.csv("testfit-parFixedDf.csv", row.names=1)
    expect_equal(df1, df2)

    df1 <- data.frame(b = 4:6, row.names = c("sub1", "sub2", "sub3"))
    res2 <- saveFitItem(df1, "ranef", "testfit")
    expect_true(res2)
    expect_true(file.exists("testfit-ranef.csv"))
    df2 <- read.csv("testfit-ranef.csv")
    expect_false(identical(df1, df2))
    row.names(df1) <- NULL
    expect_equal(df1, df2)

    # data.frame not in special lists falling back to rds
    obj <- data.frame(x = 1)
    res3 <- saveFitItem(obj, "fooObj", "testfit")
    expect_true(res3)
    expect_true(file.exists("testfit-fooObj.rds"))

    obj2 <- readRDS("testfit-fooObj.rds")
    expect_equal(obj, obj2)

  })
})

withr::with_tempdir({
  test_that("nlmixr2saveParFixedDf restores both parFixedDf structures", {

    # numeric parFixedDf as produced by nlmixr2est; "CI Lower"/"CI Upper"
    # and "BSV(SD)" are all NA so read.csv() turns them into logicals
    df <- data.frame(
      Estimate = c(0.5, 1.0, 3.4),
      SE = c(0.2, NA, 0.05),
      `%RSE` = c(40, NA, 1.5),
      `Back-transformed` = c(1.6, 2.7, 30),
      `CI Lower` = rep(NA_real_, 3),
      `CI Upper` = rep(NA_real_, 3),
      `BSV(SD)` = rep(NA_real_, 3),
      `Shrink(SD)%` = c(1.5, 4.2, NA),
      check.names = FALSE,
      row.names = c("tka", "tcl", "add.sd"))

    # old structure (nlmixr2est <= 6.0): named Estimate/SE columns
    # (built as a list since `$<-.data.frame` drops names on columns)
    dfNamed <- as.list(df)
    dfNamed$Estimate <- stats::setNames(dfNamed$Estimate, row.names(df))
    dfNamed$SE <- stats::setNames(dfNamed$SE, row.names(df))
    dfNamed <- structure(dfNamed, class="data.frame", row.names=row.names(df))

    utils::write.csv(df, "parFixedDf.csv", row.names=TRUE)
    dfCsv <- read.csv("parFixedDf.csv", check.names=FALSE, row.names=1)
    expect_true(is.logical(dfCsv$`CI Lower`))

    expect_equal(nlmixr2saveParFixedDf(dfCsv), dfNamed)
    expect_equal(nlmixr2saveParFixedDf(dfCsv, named=TRUE), dfNamed)
    # new structure (nlmixr2est $parFixed refactor): unnamed Estimate/SE
    expect_equal(nlmixr2saveParFixedDf(dfCsv, named=FALSE), df)

  })
})

if (requireNamespace("nlmixr2est", quietly = TRUE) &&
      requireNamespace("nlmixr2data", quietly = TRUE)) {

  # Consolidated helper: compares all ui and env items in two fits using two
  # test_that blocks (instead of one per item) to avoid exhausting R's node
  # protection stack when testthat processes hundreds of accumulated results.
  fitEquals <- function(fitF, fit2F) {
    fitName <- as.character(substitute(fitF))

    test_that(paste0(fitName, " ui items match after load"), {
      for (m in names(fitF$ui)) {
        if (m == "control") next
        if (m %in% c("mv0", "mvL")) {
          expect_equal(rxode2::rxNorm(fitF$ui[[m]]),
                       rxode2::rxNorm(fit2F$ui[[m]]),
                       label = paste0(fitName, "$env$ui$", m))
        } else if (length(fitF$ui[[m]]) == 0L && length(fit2F$ui[[m]]) == 0L) {
          # empty ui slots (e.g. .muGroupCovNames) round-trip as character(0)
          # vs NULL depending on the nlmixr2est version; both are length 0 and
          # therefore consistent between the two implementations
          expect_equal(length(fitF$ui[[m]]), length(fit2F$ui[[m]]),
                       label = paste0(fitName, "$env$ui$", m, " (both empty)"))
        } else {
          expect_equal(fitF$ui[[m]], fit2F$ui[[m]],
                       label = paste0(fitName, "$env$ui$", m))
        }
      }
    })

    test_that(paste0(fitName, " env items match after load"), {
      for (n in ls(fitF$env, all.names=TRUE)) {
        if (n == "ui") next
        if (n %in% c("foceiModel", "saemModel", "saem0")) next
        if (n == "omega") {
          .omega <- fitF$env[[n]]
          .dn <- dimnames(.omega)
          attr(.omega, ".match.hash") <- NULL
          attr(.dn, ".match.hash") <- NULL
          attr(.dn[[1]], ".match.hash") <- NULL
          attr(.dn[[2]], ".match.hash") <- NULL
          dimnames(.omega) <- .dn

          .omega2 <- fit2F$env[[n]]
          .dn <- dimnames(.omega2)
          attr(.omega2, ".match.hash") <- NULL
          attr(.dn, ".match.hash") <- NULL
          attr(.dn[[1]], ".match.hash") <- NULL
          attr(.dn[[2]], ".match.hash") <- NULL
          dimnames(.omega2) <- .dn

          expect_equal(.omega, .omega2,
                       label = paste0(fitName, " env item omega"))
          next
        }
        if (n %in% c("phiH", "phiC")) {
          .phiHF <- fitF$env[[n]]
          .n <- names(.phiHF)
          .phiHF <- lapply(seq_along(.phiHF), function(i) {
            if (is.matrix(.phiHF[[i]])) {
              .dn <- dimnames(.phiHF[[i]])
              attr(.phiHF[[i]], ".match.hash") <- NULL
              attr(.dn, ".match.hash") <- NULL
              attr(.dn[[1]], ".match.hash") <- NULL
              attr(.dn[[2]], ".match.hash") <- NULL
              dimnames(.phiHF[[i]]) <- .dn
            }
            .phiHF[[i]]
          })
          names(.phiHF) <- .n

          .phiH2F <- fit2F$env[[n]]
          .n <- names(.phiH2F)
          .phiH2F <- lapply(seq_along(.phiH2F), function(i) {
            if (is.matrix(.phiH2F[[i]])) {
              .dn <- dimnames(.phiH2F[[i]])
              attr(.phiH2F[[i]], ".match.hash") <- NULL
              attr(.dn, ".match.hash") <- NULL
              attr(.dn[[1]], ".match.hash") <- NULL
              attr(.dn[[2]], ".match.hash") <- NULL
              dimnames(.phiH2F[[i]]) <- .dn
            }
            .phiH2F[[i]]
          })
          names(.phiH2F) <- .n
          expect_equal(.phiHF, .phiH2F,
                       label = paste0(fitName, " env item ", n))
          next
        }
        if (any(grepl("Control$", class(fitF$env[[n]])))) {
          f1 <- rxode2::rxUiDeparse(fitF$env[[n]], "ctl")
          f2 <- rxode2::rxUiDeparse(fit2F$env[[n]], "ctl")
          expect_equal(f1, f2,
                       label = paste0(fitName, " env item Control ", n))
          next
        }
        if (n == "iniDf0") {
          # Row names of iniDf0 may differ in type (integer vs character) after
          # CSV round-trip with row.names=1; normalize both sides to character
          .i1 <- fitF$env[[n]]
          .i2 <- fit2F$env[[n]]
          row.names(.i1) <- as.character(row.names(.i1))
          row.names(.i2) <- as.character(row.names(.i2))
          expect_equal(.i1, .i2,
                       label = paste0(fitName, " env item iniDf0"))
        } else if (is.raw(fitF$env[[n]]) || is.raw(fit2F$env[[n]])) {
          # the saved fit is never compressed internally
          .fit1 <- eval(str2lang(paste0("fitF$", n)))
          .fit2 <- eval(str2lang(paste0("fit2F$", n)))
          expect_equal(.fit1, .fit2,
                       label = paste0(fitName, " env item ", n))
        } else {
          expect_equal(fitF$env[[n]], fit2F$env[[n]],
                       label = paste0(fitName, " env item ", n))
        }
      }
    })
  }
  if (identical(Sys.getenv("NOT_CRAN"), "true") &&
        !nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
    withr::with_tempdir({

      library(nlmixr2est)
      library(nlmixr2data)

      one.cmt <- function() {
        ini({
          ## You may label each parameter with a comment
          tka <- 0.45 # Log Ka
          tcl <- log(c(0, 2.7, 100)) # Log Cl
          ## This works with interactive models
          ## You may also label the preceding line with label("label text")
          tv <- 3.45; label("log V")
          ## the label("Label name") works with all models
          eta.ka ~ 0.6
          eta.cl ~ 0.3
          eta.v ~ 0.1
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          linCmt() ~ add(add.sd)
        })
      }

      fitF <- suppressMessages(nlmixr(one.cmt, theo_sd, est="focei",
                                      control=list(print=0, compress=FALSE)))

      fitS <- suppressMessages(nlmixr(one.cmt, theo_sd, est="saem",
                                      control=list(print=0, compress=FALSE)))

      # now try iov
      theo_iov <- nlmixr2data::theo_md
      theo_iov$occ <- 1
      theo_iov$occ[theo_iov$TIME >= 144] <- 2

      one.cmt.iov <- function() {
        ini({
          tka <- 0.45 # Log Ka
          tcl <- log(c(0, 2.7, 100)) # Log Cl
          tv <- 3.45; label("log V")
          eta.ka ~ 0.6
          eta.cl ~ 0.3
          eta.v ~ 0.1
          iov.cl ~ 0.1 | occ
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl + iov.cl)
          v <- exp(tv + eta.v)
          linCmt() ~ add(add.sd)
        })
      }

      fitIF <- suppressMessages(nlmixr(one.cmt.iov, theo_iov, est="focei",
                                       control=list(print=0)))

      fitIS <- suppressMessages(nlmixr(one.cmt.iov, theo_iov, est="saem",
                                       control=list(print=0)))

      test_that("saving fits do not generate errors", {
        expect_error(suppressMessages(saveFit(fitS)), NA)
        expect_true(file.exists("fitS.zip"))

        expect_error(suppressMessages(saveFit(fitF, "fitF")), NA)
        expect_true(file.exists("fitF.zip"))

        expect_error(suppressMessages(saveFit(fitIF)), NA)
        expect_true(file.exists("fitIF.zip"))

        expect_error(suppressMessages(saveFit(fitIS)), NA)
        expect_true(file.exists("fitIS.zip"))
      })

      test_that("saveFit() to a path writes a flat archive there", {
        # the files used to be named, and the loader to read them, as
        # "path_model/fitP-...", so the archive carried a path_model/ folder
        # and unzipping it recreated one wherever that happened
        .wd <- getwd()
        .before <- list.files(all.files=TRUE)
        suppressMessages(saveFit(fitF, "path_model/fitP"))
        expect_equal(getwd(), .wd)
        expect_true(file.exists("path_model/fitP.zip"))
        # nothing but the new directory appears here, and no loose files there
        expect_equal(setdiff(list.files(all.files=TRUE), .before), "path_model")
        expect_equal(list.files("path_model", all.files=TRUE, no..=TRUE),
                     "fitP.zip")
        .entries <- zip::zip_list("path_model/fitP.zip")$filename
        expect_true(all(c("fitP.R", "fitP-env.R", "fitP.csv") %in% .entries))
        expect_false(any(grepl("/", .entries, fixed=TRUE)))
        # and the loader reads its components by the bare name
        .exdir <- withr::local_tempdir()
        zip::unzip("path_model/fitP.zip", files="fitP.R", exdir=.exdir)
        .loader <- readLines(file.path(.exdir, "fitP.R"))
        expect_false(any(grepl("path_model", .loader, fixed=TRUE)))

        # the full path: the archive is still flat and the loader holds no path
        suppressMessages(saveFit(fitF, file.path(getwd(), "path_model", "fitA")))
        expect_equal(getwd(), .wd)
        .entries <- zip::zip_list("path_model/fitA.zip")$filename
        expect_true("fitA.R" %in% .entries)
        expect_false(any(grepl("/", .entries, fixed=TRUE)))
        zip::unzip("path_model/fitA.zip", files="fitA.R", exdir=.exdir)
        expect_false(any(grepl(getwd(), readLines(file.path(.exdir, "fitA.R")),
                               fixed=TRUE)))

        # zip=FALSE leaves the loose files in the directory, by the bare name
        suppressMessages(saveFit(fitF, "path_model/fitQ", zip=FALSE))
        expect_equal(getwd(), .wd)
        expect_true(all(file.exists(file.path("path_model",
                                              c("fitQ.R", "fitQ-env.R", "fitQ.csv")))))
        expect_false(file.exists("path_model/fitQ.zip"))
        expect_false(dir.exists("path_model/path_model"))
        expect_false(any(grepl("path_model", readLines("path_model/fitQ.R"),
                               fixed=TRUE)))
        unlink("path_model", recursive=TRUE)
      })

      fit2F <- suppressMessages(loadFit("fitF"))
      fit2S <- suppressMessages(loadFit(fitS))

      test_that("a loaded fit builds its ui and model lists only on first use", {
        # every model, and the ui, is built by rxode2::rxode2(); count calls
        .cnt <- new.env()
        .cnt$n <- 0L
        suppressMessages(trace("rxode2",
                               tracer = bquote(assign("n", get("n", envir = .(.cnt)) + 1L,
                                                      envir = .(.cnt))),
                               where = asNamespace("rxode2"), print = FALSE))
        on.exit(suppressMessages(untrace("rxode2", where = asNamespace("rxode2"))),
                add = TRUE)
        .built <- function(expr) {
          .n0 <- .cnt$n
          force(expr)
          .cnt$n - .n0
        }
        expect_equal(.built(.f <- suppressMessages(loadFit("fitF", checkVersion=FALSE))), 0L)
        expect_equal(.built(.s <- suppressMessages(loadFit("fitS", checkVersion=FALSE))), 0L)
        expect_equal(.built(list(.f$objf, .f$parFixed, .f$omega, head(as.data.frame(.f)))), 0L)
        # the model list is compiled on first use
        expect_gt(.built(.f$foceiModel), 0L)
        expect_gt(.built(.s$saemModel), 0L)
        # re-saving writes the kept scripts (and iniDf0 as read) back: nothing
        # is built
        .g <- suppressMessages(loadFit("fitF", checkVersion=FALSE))
        .d <- withr::local_tempdir()
        expect_equal(.built(suppressMessages(saveFit(.g, file.path(.d, "resaved")))), 0L)
        .r <- suppressMessages(loadFit(file.path(.d, "resaved.zip"), checkVersion=FALSE))
        expect_equal(.r$iniDf0, fitF$iniDf0, ignore_attr = TRUE)
        # with the original save's exact iniDf0 column types, not a fallback
        .ldr <- function(z) {
          .x <- withr::local_tempdir()
          zip::unzip(z, exdir = .x, junkpaths = TRUE)
          .l <- readLines(list.files(.x, pattern = "^[^-]*[.]R$", full.names = TRUE)[1])
          .l[grepl("env$iniDf0", .l, fixed = TRUE)]
        }
        expect_equal(.ldr(file.path(.d, "resaved.zip")), .ldr("fitF.zip"))
        # but a value assigned over a lazy item since loading is what is saved
        .h <- suppressMessages(loadFit("fitF", checkVersion=FALSE))
        assign("foceiModel", "replaced after loading", envir = .h$env)
        suppressMessages(saveFit(.h, file.path(.d, "changed")))
        .c <- suppressMessages(loadFit(file.path(.d, "changed.zip"), checkVersion=FALSE))
        expect_identical(get("foceiModel", envir = .c$env), "replaced after loading")
        # and once an item is built, the object itself is saved, so a change
        # made to it in place is kept
        .k <- suppressMessages(loadFit("fitF", checkVersion=FALSE))
        .m <- .k$foceiModel # built now
        expect_null(.k$env$`..nlmixr2saveLazy..`[["foceiModel"]])
        .u <- .k$ui
        expect_null(.k$env$`..nlmixr2saveLazy..`[["ui"]])
        invisible(.k$iniDf0)
        expect_null(.k$env$`..nlmixr2saveLazy..`[["iniDf0"]])
        # fitEquals() below compares every item, built, to the originals
      })

      fitEquals(fitF, fit2F)
      fitEquals(fitS, fit2S)

      test_that("a restored fit keeps ID a factor", {
        # the fit table round-trips through a plain .csv, so ID comes back as an
        # integer unless it is put back.  Anything joining the fit table to
        # something derived from the fit then hits a type mismatch --
        # nlme::augPred() keeps `id` a factor, and ggPMX::pmx_nlmixr() dies in a
        # data.table join on it.
        expect_true(is.factor(fit2F$ID))
        expect_equal(levels(fit2F$ID), levels(fitF$ID))
        expect_equal(as.character(fit2F$ID), as.character(fitF$ID))
        expect_true(is.factor(fit2S$ID))
        expect_equal(levels(fit2S$ID), levels(fitS$ID))
      })

      # The two repair functions are exercised on hand-built objects rather
      # than on another saved-and-reloaded fit.  Each extra round trip rebuilds
      # an rxode2 model, which is nearly free against a warm cache but costs
      # minutes against a cold one -- three such tests took over 18 minutes on
      # CI where all five fits together took 81 seconds.  Synthetic inputs also
      # pin the behavior down harder, since the levels can be made to disagree
      # with the row order in a way theo_sd's IDs never do.

      test_that(".nlmixr2saveRestoreIdFactor takes its levels from ranef", {
        .env <- new.env(parent=emptyenv())
        # levels deliberately in the opposite order to the rows, so following
        # ranef and following the order of appearance give different answers
        assign("ranef", data.frame(ID=factor(c("b", "a"), levels=c("b", "a"))),
               envir=.env)
        .cls <- c("nlmixr2FitData", "nlmixr2FitCore", "data.frame")
        .fit <- data.frame(ID=c("a", "b", "a"), DV=1:3)
        attr(.cls, ".foceiEnv") <- .env
        class(.fit) <- .cls

        .r <- .nlmixr2saveRestoreIdFactor(.fit)
        expect_true(is.factor(.r$ID))
        expect_equal(levels(.r$ID), c("b", "a"))
        # the labels still line up with the rows; only the coding changed
        expect_equal(as.character(.r$ID), c("a", "b", "a"))
        # the class attribute carries the env `$` dispatches through, and
        # column assignment must not drop it
        expect_true(is.environment(attr(class(.r), ".foceiEnv")))

        # with no usable ranef it falls back to the order the IDs appear, not
        # a sort -- a character sort would put "10" before "2"
        .env2 <- new.env(parent=emptyenv())
        assign("ranef", data.frame(ID=c(2L, 10L)), envir=.env2)
        .cls2 <- c("nlmixr2FitData", "nlmixr2FitCore", "data.frame")
        .fit2 <- data.frame(ID=c(2L, 10L, 2L), DV=1:3)
        attr(.cls2, ".foceiEnv") <- .env2
        class(.fit2) <- .cls2
        expect_equal(levels(.nlmixr2saveRestoreIdFactor(.fit2)$ID), c("2", "10"))

        # an ID the ranef levels do not cover must not become NA
        .env3 <- new.env(parent=emptyenv())
        assign("ranef", data.frame(ID=factor("a", levels="a")), envir=.env3)
        .cls3 <- c("nlmixr2FitData", "nlmixr2FitCore", "data.frame")
        .fit3 <- data.frame(ID=c("a", "z"), DV=1:2)
        attr(.cls3, ".foceiEnv") <- .env3
        class(.fit3) <- .cls3
        .r3 <- .nlmixr2saveRestoreIdFactor(.fit3)
        expect_false(anyNA(.r3$ID))
        expect_equal(levels(.r3$ID), c("a", "z"))

        # nothing to do for an object that is not a fit table with an ID
        expect_identical(.nlmixr2saveRestoreIdFactor(1L), 1L)
        expect_identical(.nlmixr2saveRestoreIdFactor(data.frame(a=1)),
                         data.frame(a=1))
      })

      test_that(".nlmixr2saveRestoreParHistType repairs a dropped type", {
        withr::with_tempdir({
          .e <- new.env(parent=emptyenv())
          .ph <- data.frame(iter=1:3,
                            type=c("Unscaled", "Unscaled", "Future Gradient"))
          utils::write.csv(.ph, "b-parHistData.csv", row.names=FALSE)
          # what the cache's own script leaves behind: a level list that
          # predates the type, so it came back NA
          .ph$type <- factor(.ph$type, levels="Unscaled")
          expect_true(anyNA(.ph$type))
          .cls <- class(.ph)
          attr(.cls, "niter") <- 42L   # saem hangs this off the class
          class(.ph) <- .cls
          assign("parHistData", .ph, envir=.e)

          .nlmixr2saveRestoreParHistType(.e, "b")
          .out <- get("parHistData", envir=.e)
          expect_false(anyNA(.out$type))
          expect_equal(levels(.out$type), c("Unscaled", "Future Gradient"))
          expect_equal(as.character(.out$type[3]), "Future Gradient")
          # every attribute nlmixr2est hangs off the class survives
          expect_equal(attr(class(.out), "niter"), 42L)

          # a type column with no NA is left exactly as it was
          .e2 <- new.env(parent=emptyenv())
          .ok <- data.frame(iter=1L, type=factor("Unscaled"))
          assign("parHistData", .ok, envir=.e2)
          .nlmixr2saveRestoreParHistType(.e2, "b")
          expect_identical(get("parHistData", envir=.e2), .ok)

          # a missing csv, or one whose rows do not line up, is left alone
          .e3 <- new.env(parent=emptyenv())
          assign("parHistData", .ph, envir=.e3)
          .nlmixr2saveRestoreParHistType(.e3, "nosuch")
          expect_true(anyNA(get("parHistData", envir=.e3)$type))

          utils::write.csv(.ph[1, ], "short-parHistData.csv", row.names=FALSE)
          .e4 <- new.env(parent=emptyenv())
          assign("parHistData", .ph, envir=.e4)
          .nlmixr2saveRestoreParHistType(.e4, "short")
          expect_true(anyNA(get("parHistData", envir=.e4)$type))

          # an env with no parHistData at all is fine
          expect_error(.nlmixr2saveRestoreParHistType(new.env(), "b"), NA)
        })
      })

      test_that("a cache saved before the levels were recorded still loads", {
        # A cache written by an earlier nlmixr2save has no `..id.level..` and no
        # `..parHistType.level..`.  Simulate one by blanking both out of the
        # env script (`env` is a plain environment when those lines run, so
        # assigning NULL leaves exactly what a missing entry looks like to the
        # restore script), and by injecting a parHistData type that postdates
        # the loader's hardcoded fallback list.
        suppressMessages(saveFit(fitS, "fitOld", zip=FALSE))
        # zip=FALSE has to be honored for a fit table too, not just for a core
        expect_false(file.exists("fitOld.zip"))
        expect_true(file.exists("fitOld-env.R"))
        expect_true(file.exists("fitOld.csv"))
        cat("env$`..id.level..` <- NULL\n",
            "env$`..parHistType.level..` <- NULL\n",
            file="fitOld-env.R", append=TRUE, sep="")

        .ph <- utils::read.csv("fitOld-parHistData.csv", check.names=FALSE)
        .ph$type[1] <- "Future Gradient"
        utils::write.csv(.ph, "fitOld-parHistData.csv", row.names=FALSE)

        .old <- suppressMessages(loadFit("fitOld", checkVersion=FALSE))

        # ID is repaired from the order the IDs appear, not from a sort --
        # a character sort would put "10" before "2".
        expect_true(is.factor(.old$ID))
        expect_equal(levels(.old$ID), unique(as.character(fitS$ID)))
        expect_equal(as.character(.old$ID), as.character(fitS$ID))

        # the unrecognized type is appended to the fallback list rather than
        # dropped to NA.  Source the restore script directly rather than going
        # through loadFit(): loadFit() also repairs an NA type from the csv,
        # which would mask a broken script.
        .se <- new.env()
        source("fitOld.R", local=.se)
        .script <- get("fitOld", envir=.se)
        expect_true(is.factor(.script$parHistData$type))
        expect_false(anyNA(.script$parHistData$type))
        expect_true("Future Gradient" %in% levels(.script$parHistData$type))
        expect_equal(as.character(.script$parHistData$type[1]), "Future Gradient")

        # and the same holds through loadFit()
        expect_false(anyNA(.old$parHistData$type))
        expect_equal(as.character(.old$parHistData$type[1]), "Future Gradient")
      })

      test_that("saveFit(data=FALSE) omits the original data", {
        suppressMessages(saveFit(fitF, "fitFnd", data=FALSE))
        expect_true(file.exists("fitFnd.zip"))
        .nd <- suppressMessages(loadFit("fitFnd", checkVersion=FALSE))
        expect_null(.nd$origData)
        # still a full FitData with its prediction columns
        expect_true(inherits(.nd, "nlmixr2FitData"))
        expect_true("IPRED" %in% names(.nd))
        # the nlmixr2save.data option drives the same behavior
        withr::with_options(list(nlmixr2save.data = FALSE),
                            suppressMessages(saveFit(fitF, "fitFndOpt")))
        .ndo <- suppressMessages(loadFit("fitFndOpt", checkVersion=FALSE))
        expect_null(.ndo$origData)
      })

      test_that("saveFit() leaves another fit's files alone (#10)", {
        .d <- withr::local_tempdir()
        # `fit-alt` extends `fit` with -<suffix>; its loose files used to be
        # zipped into fit.zip and then deleted
        suppressMessages(saveFit(fitF, file.path(.d, "fitX-alt"), zip = FALSE))
        .alt <- list.files(.d, all.files = TRUE, no.. = TRUE)
        .altMd5 <- tools::md5sum(file.path(.d, .alt))
        suppressMessages(saveFit(fitF, file.path(.d, "fitX")))
        expect_setequal(
          list.files(.d, all.files = TRUE, no.. = TRUE),
          c(.alt, "fitX.zip")
        )
        expect_equal(tools::md5sum(file.path(.d, .alt)), .altMd5)
        .entries <- zip::zip_list(file.path(.d, "fitX.zip"))$filename
        expect_false(any(startsWith(.entries, "fitX-alt")))
        # both still load
        expect_true(inherits(
          suppressMessages(
            loadFit(file.path(.d, "fitX"), checkVersion = FALSE)
          ),
          "nlmixr2FitData"
        ))
        expect_true(inherits(
          suppressMessages(
            loadFit(file.path(.d, "fitX-alt"), checkVersion = FALSE)
          ),
          "nlmixr2FitData"
        ))
        # a working directory that is not the target is untouched as well
        expect_false(file.exists("fitX.zip"))
      })

      test_that("a stale zip=FALSE save does not leak into a new save (#10)", {
        .d <- withr::local_tempdir()
        suppressMessages(saveFit(fitF, file.path(.d, "fitY"), zip = FALSE))
        expect_true(file.exists(file.path(.d, "fitY-origData.csv")))
        suppressMessages(saveFit(fitF, file.path(.d, "fitY"), data = FALSE))
        expect_false(
          "fitY-origData.csv" %in%
            zip::zip_list(file.path(.d, "fitY.zip"))$filename
        )
        # read, zipped and removed by no one
        expect_true(file.exists(file.path(.d, "fitY-origData.csv")))
        .y <- suppressMessages(loadFit(
          file.path(.d, "fitY.zip"),
          checkVersion = FALSE
        ))
        expect_null(.y$origData)
        # and loose files left beside it are still readable after a zip=FALSE
        # resave that lacks the item: the new loader does not read them
        suppressMessages(saveFit(fitF, file.path(.d, "fitZ"), zip = FALSE))
        suppressMessages(saveFit(
          fitF,
          file.path(.d, "fitZ"),
          zip = FALSE,
          data = FALSE
        ))
        expect_false(any(grepl(
          "origData",
          readLines(file.path(.d, "fitZ.R")),
          fixed = TRUE
        )))
        .z <- suppressMessages(
          loadFit(file.path(.d, "fitZ"), checkVersion = FALSE)
        )
        expect_null(.z$origData)
        # nor does an earlier zip=TRUE save's archive shadow a zip=FALSE one
        suppressMessages(saveFit(fitF, file.path(.d, "fitV")))
        suppressMessages(saveFit(
          fitF,
          file.path(.d, "fitV"),
          zip = FALSE,
          data = FALSE
        ))
        expect_false(file.exists(file.path(.d, "fitV.zip")))
        # an unrelated archive of that name is kept
        withr::with_dir(.d, {
          writeLines("data", "raw.csv")
          zip::zip("fitR.zip", "raw.csv")
        })
        suppressMessages(saveFit(fitF, file.path(.d, "fitR"), zip = FALSE))
        expect_equal(
          zip::zip_list(file.path(.d, "fitR.zip"))$filename,
          "raw.csv"
        )
        .v <- suppressMessages(
          loadFit(file.path(.d, "fitV"), checkVersion = FALSE)
        )
        expect_null(.v$origData)
        # and a zip=TRUE save retires the loader of a zip=FALSE one
        suppressMessages(saveFit(fitF, file.path(.d, "fitV")))
        expect_false(file.exists(file.path(.d, "fitV.R")))
        expect_true(file.exists(file.path(.d, "fitV.zip")))
        # but not a script of that name that is no fit loader, even with an
        # earlier zip=FALSE save's `-env.R` still beside it
        writeLines("x <- 1", file.path(.d, "fitV.R"))
        suppressMessages(saveFit(fitF, file.path(.d, "fitV")))
        expect_true(file.exists(file.path(.d, "fitV-env.R")))
        expect_equal(readLines(file.path(.d, "fitV.R")), "x <- 1")
      })

      test_that("saveFit() fails cleanly when it cannot write (#10)", {
        .d <- withr::local_tempdir()
        # a file where the directory should be is left alone, not copied over
        writeLines("keep", file.path(.d, "notadir"))
        expect_error(
          suppressMessages(saveFit(fitF, file.path(.d, "notadir", "fit"))),
          "not a directory"
        )
        expect_equal(readLines(file.path(.d, "notadir")), "keep")
        # a component that cannot be copied out: no loader is left behind to
        # read the old and new files mixed
        suppressMessages(saveFit(fitF, file.path(.d, "fitU"), zip = FALSE))
        unlink(file.path(.d, "fitU-env.R"))
        dir.create(file.path(.d, "fitU-env.R"))
        expect_error(
          suppressMessages(saveFit(fitF, file.path(.d, "fitU"), zip = FALSE)),
          "could not write"
        )
        expect_false(file.exists(file.path(.d, "fitU.R")))
      })

      test_that("a prefixed := cache never touches the bare-name archive", {
        .d <- withr::local_tempdir()
        withr::local_dir(.d)
        # someone else's fitW.zip, beside the cache of `fitW` under a prefix
        writeLines("not a fit", "fitW.zip")
        .md5 <- tools::md5sum("fitW.zip")
        withr::local_options(list(nlmixr2save.prefix = "run1-"))
        suppressMessages(.saveFitZipPlain(fitF, "fitW"))
        expect_true(file.exists("run1-fitW.zip"))
        expect_equal(tools::md5sum("fitW.zip"), .md5)
        expect_true("fitW.R" %in% zip::zip_list("run1-fitW.zip")$filename)
        .w <- suppressMessages(.loadFitZipPlain("fitW"))
        expect_true(inherits(.w, "nlmixr2FitData"))
        # loadFit() takes the prefixed archive too; its loader is `fitW.R`
        expect_true(inherits(
          suppressMessages(
            loadFit("run1-fitW.zip", checkVersion = FALSE)
          ),
          "nlmixr2FitData"
        ))
        expect_equal(tools::md5sum("fitW.zip"), .md5)
        expect_setequal(
          list.files(all.files = TRUE, no.. = TRUE),
          c("fitW.zip", "run1-fitW.zip")
        )
        # a directory where the cache goes is an error, not copied into
        dir.create("run1-fitD.zip")
        expect_error(
          suppressMessages(.saveFitZipPlain(fitF, "fitD")),
          "could not write"
        )
        expect_length(list.files("run1-fitD.zip"), 0)
        # a prefix naming a directory that does not exist yet
        withr::local_options(list(nlmixr2save.prefix = "run2/"))
        suppressMessages(.saveFitZipPlain(fitF, "fitW"))
        expect_true(file.exists("run2/fitW.zip"))
        expect_equal(tools::md5sum("fitW.zip"), .md5)
        expect_true(inherits(
          suppressMessages(.loadFitZipPlain("fitW")),
          "nlmixr2FitData"
        ))
      })

      test_that("nlmixr2saveShare writes shareable zips and leaves the fit alone", {
        .clsBefore <- class(fitF)
        .rowsBefore <- nrow(fitF$origData)

        # from a live object -> fitF-noData.zip
        .p1 <- suppressMessages(nlmixr2saveShare(fitF))
        expect_true(file.exists("fitF-noData.zip"))
        .s1 <- suppressMessages(loadFit("fitF-noData", checkVersion=FALSE))
        expect_null(.s1$origData)
        expect_true(inherits(.s1, "nlmixr2FitData"))

        # noFit=TRUE -> only fitF-noData-noFit.zip, loads as a core
        .p2 <- suppressMessages(nlmixr2saveShare(fitF, noFit = TRUE))
        expect_true(file.exists("fitF-noData-noFit.zip"))
        .s2 <- suppressMessages(loadFit("fitF-noData-noFit", checkVersion=FALSE))
        expect_true(inherits(.s2, "nlmixr2FitCore"))
        expect_false(inherits(.s2, "nlmixr2FitData"))
        expect_false(inherits(.s2, "data.frame"))
        expect_null(.s2$origData)
        # eta/parameter-history tables and estimates are kept
        expect_false(is.null(.s2$etaObf))
        expect_false(is.null(.s2$parHistData))
        expect_false(is.null(.s2$parFixed))

        # reading from an existing zip base name also works
        .p3 <- suppressMessages(nlmixr2saveShare("fitF"))
        expect_true(file.exists("fitF-noData.zip"))

        # the original fit object is unchanged (env is shared by reference)
        expect_identical(class(fitF), .clsBefore)
        expect_true(is.environment(attr(class(fitF), ".foceiEnv")))
        expect_identical(nrow(fitF$origData), .rowsBefore)
      })

      test_that("nlmixr2saveShare honors nlmixr2save.dir / prefix", {
        withr::with_options(list(nlmixr2save.dir = "shareCache",
                                 nlmixr2save.prefix = "sh-"), {
          .p <- suppressMessages(nlmixr2saveShare(fitF))
          expect_true(file.exists(file.path("shareCache", "sh-fitF-noData.zip")))
        })
      })

      fit2IF <- loadFit("fitIF")
      fitEquals(fitIF, fit2IF)

      fit2IS <- loadFit("fitIS")
      fitEquals(fitIS, fit2IS)

      test_that("a compressed fit still records its parHistData type levels", {
        # nlmixr2est stores parHistData compressed (a raw vector in the env)
        # unless compress=FALSE, so saveFit() has to decompress before it can
        # read the type levels off it.  Without that it fell through to the
        # loader's hardcoded level list, which nlmixr2est has since outgrown
        # ("Analytic Gradient (relaxed)" and friends), and those levels came
        # back as NA.
        expect_true(is.raw(get("parHistData", envir=fitIS$env)))
        expect_equal(levels(fit2IS$parHistData$type),
                     levels(fitIS$parHistData$type))
        expect_false(anyNA(fit2IS$parHistData$type))
        expect_equal(levels(fit2IF$parHistData$type),
                     levels(fitIF$parHistData$type))
        expect_false(anyNA(fit2IF$parHistData$type))
      })

      one.cmt.nlm <- function() {
        ini({
          tka <- 0.45 # Log Ka
          tcl <- log(c(0, 2.7, 100)) # Log Cl
          tv <- 3.45; label("log V")
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka)
          cl <- exp(tcl)
          v <- exp(tv)
          linCmt() ~ add(add.sd)
        })
      }

      fitNL <- suppressMessages(nlmixr(one.cmt.nlm, theo_sd, est="nlm",
                                       control=list(print=0, compress=FALSE)))

      test_that("saving fits do not generate errors", {
        expect_error(suppressMessages(saveFit(fitNL, "fitNL")), NA)
        expect_true(file.exists("fitNL.zip"))
      })

      fit2NL <- loadFit("fitNL")
      fitEquals(fitNL, fit2NL)

      fitNL2 <- suppressMessages(nlmixr(one.cmt.nlm, theo_sd, est="nlm",
                                        control=list(print=0, compress=FALSE,
                                                     calcTables=FALSE)))

      test_that("saving fits do not generate errors", {
        expect_error(suppressMessages(saveFit(fitNL2)), NA)
        expect_true(file.exists("fitNL2.zip"))
      })

      fit2NL2 <- loadFit("fitNL2")
      fitEquals(fitNL2, fit2NL2)
    })


    test_that("test assignment", {

      suppressMessages(withr::with_tempdir({

        library(nlmixr2est)
        library(nlmixr2data)

        one.cmt <- function() {
          ini({
            ## You may label each parameter with a comment
            tka <- 0.45 # Log Ka
            tcl <- log(c(0, 2.7, 100)) # Log Cl
            ## This works with interactive models
            ## You may also label the preceding line with label("label text")
            tv <- 3.45; label("log V")
            ## the label("Label name") works with all models
            eta.ka ~ 0.6
            eta.cl ~ 0.3
            eta.v ~ 0.1
            add.sd <- 0.7
          })
          model({
            ka <- exp(tka + eta.ka)
            cl <- exp(tcl + eta.cl)
            v <- exp(tv + eta.v)
            linCmt() ~ add(add.sd)
          })
        }

        fitF := nlmixr(one.cmt, theo_sd, est="focei",
                       control=list(print=0, compress=FALSE))

        fitF2 := fitF
        expect_true(file.exists("fitF2.zip"))
        hash0 <- tools::md5sum("fitF.zip")

        expect_true(file.exists("fitF.zip"))
        hash1 <- tools::md5sum("fitF.zip")

        fitF := nlmixr(one.cmt, theo_sd, est="focei",
                       control=list(print=0, compress=FALSE))
        expect_true(file.exists("fitF.zip"))
        hash2 <- tools::md5sum("fitF.zip")

        expect_equal(hash1, hash2)

        fitF := nlmixr(one.cmt, theo_sd, est="saem",
                       control=list(print=0, compress=FALSE))

        expect_true(file.exists("fitF.zip"))
        hash3 <- tools::md5sum("fitF.zip")
        expect_false(identical(hash1, hash3))

        # Without tables
        fitF := nlmixr(one.cmt, theo_sd, est="focei",
                       control=list(print=0, compress=FALSE, calcTables=FALSE))

        expect_true(file.exists("fitF.zip"))
        hash4 <- tools::md5sum("fitF.zip")

        expect_false(identical(hash1, hash4))

        fitF := nlmixr(one.cmt, theo_sd, est="focei",
                       control=list(print=0, compress=FALSE, calcTables=FALSE))

        expect_true(file.exists("fitF.zip"))
        hash5 <- tools::md5sum("fitF.zip")

        expect_equal(hash5, hash4)
        fitF := nlmixr(one.cmt, theo_sd, est="saem",
                       control=list(print=0, compress=FALSE))

        expect_true(file.exists("fitF.zip"))
        hash6 <- tools::md5sum("fitF.zip")
        expect_false(identical(hash4, hash6))


      }))
    })

    test_that("dataset changes: irrelevant column does not refit; DV change does", {

      suppressMessages(withr::with_tempdir({

        library(nlmixr2est)
        library(nlmixr2data)

        one.cmt <- function() {
          ini({
            tka <- 0.45
            tcl <- log(c(0, 2.7, 100))
            tv <- 3.45
            eta.ka ~ 0.6
            eta.cl ~ 0.3
            eta.v ~ 0.1
            add.sd <- 0.7
          })
          model({
            ka <- exp(tka + eta.ka)
            cl <- exp(tcl + eta.cl)
            v  <- exp(tv + eta.v)
            linCmt() ~ add(add.sd)
          })
        }

        # Baseline fit
        fitD := nlmixr(one.cmt, theo_sd, est="focei",
                       control=list(print=0, compress=FALSE))
        expect_false(.assignRestore())
        hash_base <- tools::md5sum("fitD.zip")

        # Adding an irrelevant column should restore from cache unchanged
        theo_sd_extra <- theo_sd
        theo_sd_extra$.ignored <- "noise"
        fitD := nlmixr(one.cmt, theo_sd_extra, est="focei",
                       control=list(print=0, compress=FALSE))
        expect_true(.assignRestore())
        hash_extra <- tools::md5sum("fitD.zip")
        expect_equal(hash_base, hash_extra)
        # origData in the restored fit reflects the new (extra-column) data
        expect_equal(fitD$origData, theo_sd_extra)

        # Changing DV values must trigger a refit
        theo_sd_dv <- theo_sd
        theo_sd_dv$DV <- theo_sd_dv$DV + 1
        fitD := nlmixr(one.cmt, theo_sd_dv, est="focei",
                       control=list(print=0, compress=FALSE))
        expect_false(.assignRestore())
        hash_dv <- tools::md5sum("fitD.zip")
        expect_false(identical(hash_base, hash_dv))

      }))
    })

  }
}

options("nlmixr2save.quiet" = oldOpt)
