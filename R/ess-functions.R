#' @keywords internal
ess.pander.evals <- function(x) {
    require(pander)
    wd <- getwd()
    setwd(tempdir())
    cat("\n")
    pander(evals(list(strsplit(x, '\n')[[1]]))[[1]])
    setwd(wd)
    cat("\n")
}

#' @keywords internal
ess.evals <- function(x) {
    require(pander)
    wd <- getwd()
    setwd(tempdir())
    cat("\n")
    evals(list(strsplit(x, '\n')[[1]]))
    setwd(wd)
    cat("\n")
}
