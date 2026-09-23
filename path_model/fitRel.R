`path_model/fitRel` <- function() {
source('path_model/fitRel-env.R', local=TRUE)
.class <- env$`..class..`
.id.level <- env$`..id.level..`
.parHistType.level <- env$`..parHistType.level..`
rm('..class..', envir=env)
rm('..id.level..', envir=env)
if (exists('..parHistType.level..', env)) rm('..parHistType.level..', envir=env)
env$`etaObf` <- read.csv('path_model/fitRel-etaObf.csv', check.names=FALSE)

source('path_model/fitRel-foceiModel.R', local=TRUE)
env$`foceiModel` <- foceiModel

env$`iniDf0` <- read.csv('path_model/fitRel-iniDf0.csv',check.names=FALSE, row.names=1)
env$iniDf0$ntheta <- as.integer(env$iniDf0$ntheta)
env$iniDf0$neta1 <- as.double(env$iniDf0$neta1)
env$iniDf0$neta2 <- as.double(env$iniDf0$neta2)
env$iniDf0$name <- as.character(env$iniDf0$name)
env$iniDf0$lower <- as.double(env$iniDf0$lower)
env$iniDf0$upper <- as.double(env$iniDf0$upper)
env$iniDf0$est <- as.double(env$iniDf0$est)
env$iniDf0$fix <- as.logical(env$iniDf0$fix)
env$iniDf0$label <- as.character(env$iniDf0$label)
env$iniDf0$backTransform <- as.character(env$iniDf0$backTransform)
env$iniDf0$condition <- as.character(env$iniDf0$condition)
env$iniDf0$err <- as.character(env$iniDf0$err)

env$objDf <- read.csv('path_model/fitRel-objDf.csv',check.names=FALSE, row.names=1)
env$objDf$OBJF <- as.double(env$objDf$OBJF)
env$objDf$AIC <- as.double(env$objDf$AIC)
env$objDf$BIC <- as.double(env$objDf$BIC)
env$objDf$`Log-likelihood` <- as.double(env$objDf$`Log-likelihood`)

env$`origData` <- read.csv('path_model/fitRel-origData.csv', check.names=FALSE)

env$`parFixed` <- read.csv('path_model/fitRel-parFixed.csv',check.names=FALSE, row.names=1, colClasses="character")
class(env$`parFixed`) <- c('nlmixr2ParFixed', 'data.frame')

env$`parFixedDf` <- read.csv('path_model/fitRel-parFixedDf.csv',check.names=FALSE, row.names=1)
env$`parFixedDf` <- nlmixr2save::nlmixr2saveParFixedDf(env$`parFixedDf`, named=TRUE)

env$`parHistData` <- read.csv('path_model/fitRel-parHistData.csv', check.names=FALSE)

source('path_model/fitRel-phiC.R', local=TRUE)
env$`phiC` <- phiC

source('path_model/fitRel-phiH.R', local=TRUE)
env$`phiH` <- phiH

env$`ranef` <- read.csv('path_model/fitRel-ranef.csv', check.names=FALSE)

env$`scaleInfo` <- readRDS('path_model/fitRel-scaleInfo.rds')

env$`sessioninfo` <- readRDS('path_model/fitRel-sessioninfo.rds')

env$`shrink` <- read.csv('path_model/fitRel-shrink.csv',check.names=FALSE, row.names=1)

env$`time` <- read.csv('path_model/fitRel-time.csv',check.names=FALSE, row.names=1)

source('path_model/fitRel-ui.R', local=TRUE)
env$`ui` <- ui
env$model <- rxode2::model(env$ui)
if (!is.null(.id.level)) {
  if (!is.null(env$ranef$ID)) {
    env$ranef$ID <- factor(env$ranef$ID, levels=.id.level)
  }
  if (!is.null(env$etaObf$ID)) {
    env$etaObf$ID <- factor(env$etaObf$ID, levels=.id.level)
  }
}
if (!is.null(env$parHistData)) {
  .phLevels <- .parHistType.level
  if (is.null(.phLevels)) {
    .phLevels <- c("Gill83 Gradient", "Mixed Gradient", "Forward Difference", "Central Difference", "Scaled", "Unscaled", "Back-Transformed", "Forward Sensitivity", "Analytic Gradient")
    .phLevels <- c(.phLevels, setdiff(unique(as.character(env$parHistData$type)), .phLevels))
  }
  env$parHistData$type <- factor(env$parHistData$type, levels=.phLevels)
  env$parHistData$iter <- as.integer(env$parHistData$iter)
}
if (exists('saemControl', env) && is.numeric(env$saemControl$mcmc$niter[1])) {
    .parHistData <- env$parHistData
    .cls <- class(.parHistData)
    attr(.cls, 'niter') <- env$saemControl$mcmc$niter[1]
    class(.parHistData) <- .cls
    env$parHistData <- .parHistData
}
if (any(.class == 'nlmixr2FitData')) {
  ret <- read.csv('path_model/fitRel.csv')
  class(env) <- 'nlmixr2FitCoreSilent'
  attr(.class, '.foceiEnv') <- env
  class(ret) <- .class
  return(ret)
} else {
  ret <- env
  class(ret) <- .class
  return(ret)
}
}
`path_model/fitRel` <- `path_model/fitRel`()

