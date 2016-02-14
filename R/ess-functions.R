#' @keywords internal
ess.pander.evals <- function(x, show.src = TRUE) {
    require(pander, quietly = T)
    wd <- getwd()
    setwd(tempdir())
    cat('\n')
    res <- evals(x)
    for (x in res) {
        if (show.src) {
            cat('\n```r\n', paste(x$src, collapse = '\n'), '\n```\n\n', sep = '')
        }
        cat(pander(x), sep = '\n')
    }
    setwd(wd)
    cat('\n')
}

#' @keywords internal
ess.evals <- function(x) {
    require(pander, quietly = T)
    wd <- getwd()
    setwd(tempdir())
    cat('\n')
    evals(x)
    setwd(wd)
    cat('\n')
}
