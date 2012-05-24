#' Generic Pandoc method
#'
#' Prints an R object in pandoc style markdown.
#' @param x an R object
#' @param ... optional parameters
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @references \itemize{
#' \item John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' \item David Hajage (2011): _ascii. Export R objects to several markup languages._ \url{http://CRAN.R-project.org/package=ascii}
#' }
#' @author Gergely Daróczi
#' @export
#' @aliases Pandoc.return
#' @examples
#'
#' ## Vectors
#' Pandoc(1:10)
#' Pandoc(letters)
#' Pandoc(mtcars$am)
#' Pandoc(factor(mtcars$am))
#'
#' ## Lists
#' Pandoc(list(1,2,3, c(1,2)))
#' Pandoc(list(a=1, b=2, c=table(mtcars$am)))
#' Pandoc(list(1,2,3, list(1,2)))
#' Pandoc(list('FOO', letters[1:3], list(1:5), table(mtcars$gear), list('FOOBAR', list('a', 'b'))))
#' Pandoc(list(a=1, b=2, c=table(mtcars$am), x=list(myname=1,2), 56))
#' Pandoc(unclass(chisq.test(table(mtcars$am, mtcars$gear))))
#'
#' ## Arrays
#' Pandoc(mtcars)
#' Pandoc(table(mtcars$am))
#' Pandoc(table(mtcars$am, mtcars$gear))
#'
#' ## Tests
#' Pandoc(ks.test(runif(50), runif(50)))
#' Pandoc(chisq.test(table(mtcars$am, mtcars$gear)))
#' Pandoc(t.test(extra ~ group, data = sleep))
#'
#' ## Models
#' ml <- with(lm(mpg ~ hp + wt), data = mtcars)
#' Pandoc(ml)
#' Pandoc(anova(ml))
#' Pandoc(aov(ml))
#' ## Dobson (1990) Page 93: Randomized Controlled Trial (examples from: ?glm)
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' m <- glm(counts ~ outcome + treatment, family=poisson())
#' Pandoc(m)
#' Pandoc(anova(m))
#' Pandoc(aov(m))
#'
#' ## Prcomp
#' Pandoc(prcomp(USArrests))
#'
#' ## Others
#' Pandoc(density(runif(10)))
#' Pandoc(density(mtcars$hp))
#'
#' ## default method
#' x <- chisq.test(table(mtcars$am, mtcars$gear))
#' class(x) <- 'I heave never heard of!'
#' Pandoc(x)
Pandoc <- function(x, ...)
    UseMethod('Pandoc', x)

#' @export
Pandoc.return <- function(...)
    capture.output(Pandoc(...))

#' @S3method Pandoc table
Pandoc.table <- function(x, ...)
    pandoc.table(x)

#' @S3method Pandoc data.frame
Pandoc.data.frame <- function(x, ...)
    pandoc.table(x)

#' @S3method Pandoc cast_df
Pandoc.cast_df<- function(x, ...)
    pandoc.table(as.data.frame(x))

#' @S3method Pandoc matrix
Pandoc.matrix <- function(x, ...)
    pandoc.table(x)

#' @S3method Pandoc numeric
Pandoc.numeric <- function(x, ...)
    cat(p(x))        #ROUND!

#' @S3method Pandoc character
Pandoc.character <- function(x, ...)
    cat(p(x))

#' @S3method Pandoc factor
Pandoc.factor <- function(x, ...)
    cat(p(as.character(x)))

#' @S3method Pandoc list
Pandoc.list <- function(x, ...)
    pandoc.list(x)

#' @S3method Pandoc lm
Pandoc.lm <- function(x, ...)
    pandoc.table(summary(x)$coeff, caption = sprintf('Fitting linear model: %s', deparse(x$call$formula)), justify = c('right', rep('centre', 4)))

#' @S3method Pandoc glm
Pandoc.glm <- function(x, ...)
    pandoc.table(summary(x)$coeff, caption = sprintf('Fitting generalized (%s) linear model: %s', paste(x$family$family, x$family$link, sep = '/'), deparse(x$call$formula)), justify = c('right', rep('centre', 4)))

#' @S3method Pandoc aov
Pandoc.aov <- function(x, ...)
    pandoc.table(unclass(summary(x))[[1]], caption = 'Analysis of Variance Model', justify = c('right', rep('centre', 4)))

#' @S3method Pandoc anova
Pandoc.anova <- function(x, ...)
    pandoc.table(x, caption = strsplit(attr(x, 'heading'), '\n')[[1]][1], justify = c('right', rep('centre', 4)))

#' @S3method Pandoc htest
Pandoc.htest <- function(x, ...) {

    res <- data.frame('Test statistic' = as.numeric(x$statistic), check.names = FALSE)
    if (!is.null(x$parameter))
        res[names(x$parameter)] = x$parameter
    if (!is.null(x$p.value))    # TODO: add significance stars
        res$'P value' = x$p.value
    if (!is.null(x$alternative))
        res['Alternative hypothesis'] = x$alternative

    pandoc.table(res, caption = x$method, justify = 'centre')

}

#' @S3method Pandoc prcomp
Pandoc.prcomp <- function(x, ...) {
    pandoc.table(x$rotation, caption = 'Principal Components Analysis')
    pandoc.table(summary(x)$importance)
}

#' @S3method Pandoc density
Pandoc.density <- function(x, ...) {

    res <- data.frame(Coordinates = as.numeric(summary(x$x)), 'Density values' = as.numeric(summary(x$y)), check.names = FALSE)
    rownames(res) <- names(summary(1))
    pandoc.table(res, caption = sprintf('Kernel density of *%s* (bandwidth: %s)', x$data.name, format(x$bw)), justify = c('right', 'centre', 'centre'))
}

#' @S3method Pandoc list
Pandoc.list <- function(l, indent = 0, ...) {

    ## grab elements name (if any)
    x.names       <- sapply(names(l), function(x) ifelse(x == '', '  *', sprintf('  * **%s**:', x)))
    if (length(x.names) == 0)
        x.names <- rep('', length(l))

    ## capture pandoc output of list element
    res <- paste(unlist(lapply(1:length(l), function(i) {
        res.i <- paste(capture.output(Pandoc(l[[i]], indent = indent + 1)), collapse = '\n')
        if (grepl('\n', res.i) & !grepl('\n *\\*', res.i)) {
            res.i <- sub('^\n', '\n\n', res.i)
            res.i <- pandoc.indent(res.i, 1)
        }
        paste(x.names[i], res.i)
    })), collapse = '\n')

    ## indent output
    res <- pandoc.indent(res, indent)

    pandoc.p(res)

}

#' @S3method Pandoc default
Pandoc.default <- function(x, ...) {

    warning(sprintf('No Pandoc method for "%s", reverting to default.', class(x)))
    class(x) <- 'list'
    Pandoc(x)

}
