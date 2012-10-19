#' Generic pander method
#'
#' Prints an R object in Pandoc's markdown.
#' @param x an R object
#' @param ... optional parameters
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @note This function can be called by \code{pander} and \code{pandoc} too.
#' @references \itemize{
#' \item John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' \item David Hajage (2011): _ascii. Export R objects to several markup languages._ \url{http://CRAN.R-project.org/package=ascii}
#' }
#' @export
#' @aliases pander pander.return pandoc pandoc.return
#' @usage
#' pander(x, ...)
#'
#' pandoc(x, ...)
#' @examples
#'
#' ## Vectors
#' pander(1:10)
#' pander(letters)
#' pander(mtcars$am)
#' pander(factor(mtcars$am))
#'
#' ## Lists
#' pander(list(1,2,3, c(1,2)))
#' pander(list(a=1, b=2, c=table(mtcars$am)))
#' pander(list(1,2,3, list(1,2)))
#' pander(list(a = 1,2,3, list(1,2)))
#' pander(list('FOO', letters[1:3], list(1:5), table(mtcars$gear), list('FOOBAR', list('a', 'b'))))
#' pander(list(a=1, b=2, c=table(mtcars$am), x=list(myname=1,2), 56))
#' pander(unclass(chisq.test(table(mtcars$am, mtcars$gear))))
#'
#' ## Arrays
#' pander(mtcars)
#' pander(table(mtcars$am))
#' pander(table(mtcars$am, mtcars$gear))
#'
#' ## Tests
#' pander(ks.test(runif(50), runif(50)))
#' pander(chisq.test(table(mtcars$am, mtcars$gear)))
#' pander(t.test(extra ~ group, data = sleep))
#'
#' ## Models
#' ml <- with(lm(mpg ~ hp + wt), data = mtcars)
#' pander(ml)
#' pander(anova(ml))
#' pander(aov(ml))
#' ## Dobson (1990) Page 93: Randomized Controlled Trial (examples from: ?glm)
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' m <- glm(counts ~ outcome + treatment, family=poisson())
#' pander(m)
#' pander(anova(m))
#' pander(aov(m))
#'
#' ## Prcomp
#' pander(prcomp(USArrests))
#'
#' ## Others
#' pander(density(runif(10)))
#' pander(density(mtcars$hp))
#'
#' ## default method
#' x <- chisq.test(table(mtcars$am, mtcars$gear))
#' class(x) <- 'I heave never heard of!'
#' pander(x)
pander <- function(x, ...)
    UseMethod('pander', x)
#' @export
pandoc <- pander

#' @export
pandoc.return <- function(...)
    capture.output(pander(...))
#' @export
pander.return <- pandoc.return

#' @S3method pander NULL
pander.NULL <- function(x, ...)
    return(invisible(NULL))

#' @S3method pander logical
pander.logical <- function(x, ...)
    return(as.character(x))

#' @S3method pander image
pander.image <- function(x, caption = attr(x, 'caption'), href = attr(x, 'href'), ...) {

    res <- pandoc.image.return(as.character(x), caption)

    if (is.null(href))
        cat(res)
    else
        pandoc.link(href, res)

}

#' @S3method pander table
pander.table <- function(x, caption = attr(x, 'caption'), justify = attr(x, 'alignment'), ...) {

    if (is.null(caption))
        if (!is.null(storage$caption)) {
            caption <- storage$caption
            storage$caption <- NULL
        }

    if (is.null(justify))
        justify <- 'left'

    pandoc.table(x, caption = caption, justify = justify)

}

#' @S3method pander data.frame
pander.data.frame <- function(x, caption = attr(x, 'caption'), justify = attr(x, 'alignment'), ...) {

    if (is.null(caption))
        if (!is.null(storage$caption)) {
            caption <- storage$caption
            storage$caption <- NULL
        }

    if (is.null(justify))
        justify <- 'left'

    pandoc.table(x, caption = caption, justify = justify)

}

#' @S3method pander matrix
pander.matrix <- function(x, caption = attr(x, 'caption'), justify = attr(x, 'alignment'),  ...) {

    if (is.null(caption))
        if (!is.null(storage$caption)) {
            caption <- storage$caption
            storage$caption <- NULL
        }

    if (is.null(justify))
        justify <- 'left'

    pandoc.table(x, caption = caption, justify = justify)

}

#' @S3method pander cast_df
pander.cast_df<- function(x, caption = attr(x, 'caption'), justify = attr(x, 'alignment'), ...)
    pandoc.table(as.data.frame(x), caption = caption, justify = justify)

#' @S3method pander numeric
pander.numeric <- function(x, ...)
    cat(p(x))

#' @S3method pander character
pander.character <- function(x, ...) {

    if (length(x) < 2)
        cat(x)
    else
        cat(p(x))

}

#' @S3method pander factor
pander.factor <- function(x, ...)
    cat(p(as.character(x)))

#' @S3method pander list
pander.list <- function(x, ...)
    pandoc.list(x)

#' @S3method pander lm
pander.lm <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- sprintf('Fitting linear model: %s', deparse(x$call$formula))
        else {
            caption <- storage$caption
            storage$caption <- NULL
        }
    }

    pandoc.table(summary(x)$coeff, caption = caption, justify = c('right', rep('centre', 4)))

}

#' @S3method pander glm
pander.glm <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- sprintf('Fitting generalized (%s) linear model: %s', paste(x$family$family, x$family$link, sep = '/'), deparse(x$call$formula))
        else {
            caption <- storage$caption
            storage$caption <- NULL
        }
    }

    pandoc.table(summary(x)$coeff, caption = caption, justify = c('right', rep('centre', 4)))

}

#' @S3method pander aov
pander.aov <- function(x, caption = attr(x, 'caption'), ...) {

    res <- unclass(summary(x))[[1]]

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- 'Analysis of Variance Model'
        else {
            caption <- storage$caption
            storage$caption <- NULL
        }
    }

    pandoc.table(res, caption = caption, justify = c('right', rep('centre', ncol(res))))
}

#' @S3method pander anova
pander.anova <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- strsplit(attr(x, 'heading'), '\n')[[1]][1]
        else {
            caption <- storage$caption
            storage$caption <- NULL
        }
    }

    pandoc.table(x, caption = caption, justify = c('right', rep('centre', ncol(x))))

}

#' @S3method pander htest
pander.htest <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- paste0(x$method, ': `', gsub('( and | by )', '`\\1`', x$data.name), '`')
        else {
            caption <- storage$caption
            storage$caption <- NULL
        }
    }

    ## we do not know which values are provided
    res <- data.frame(placeholder = 'FOO')

    ## add what we know
    if (!is.null(x$statistic))
        res$'Test statistic' = as.numeric(x$statistic)
    if (!is.null(x$parameter))
        res[names(x$parameter)] = x$parameter
    if (!is.null(x$p.value))
        res$'P value' = add.significance.stars(x$p.value)
    if (!is.null(x$alternative))
        res['Alternative hypothesis'] = x$alternative

    ## drop placeholder
    res$placeholder <- NULL

    ## return
    pandoc.table(res, caption = caption, justify = 'centre')

}

#' @S3method pander prcomp
pander.prcomp <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- 'Principal Components Analysis'
        else {
            caption <- storage$caption
            storage$caption <- NULL
        }
    }

    pandoc.table(x$rotation, caption = caption)
    pandoc.table(summary(x)$importance)
}

#' @S3method pander density
pander.density <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- sprintf('Kernel density of *%s* (bandwidth: %s)', x$data.name, format(x$bw))
        else {
            caption <- storage$caption
            storage$caption <- NULL
        }
    }

    res <- data.frame(Coordinates = as.numeric(summary(x$x)), 'Density values' = as.numeric(summary(x$y)), check.names = FALSE)
    rownames(res) <- names(summary(1))

    pandoc.table(res, caption = caption, justify = c('right', 'centre', 'centre'))
}

#' @S3method pander list
pander.list <- function(x, ...) {

    ## match call
    mc <- match.call()
    if (is.null(mc$indent))
        indent <- 0
    else
        indent <- eval(mc$indent, parent.frame(1))

    ## grab elements name (if any)
    x.names <- sapply(names(x), function(x) ifelse(x == '', '  *', sprintf('  * **%s**:', x)))
    if (length(x.names) == 0)
        x.names <- rep('  *', length(x))

    ## capture pandoc output of list element
    res <- paste(unlist(lapply(1:length(x), function(i) {

        res.i <- paste(capture.output(pander(x[[i]], indent = indent + 1)), collapse = '\n')
        if (grepl('\n', res.i) & !grepl('\n *\\* ', res.i)) {
            res.i <- sub('^\n', '\n\n', res.i)
            res.i <- pandoc.indent(res.i, 1)
        }

        paste(x.names[i], res.i)

    })), collapse = '\n')

    ## indent output
    res <- pandoc.indent(res, indent)

    ## add (comment): end of list (preventing conflicts with forthcoming pandoc blocks)
    if (indent == 0)
        res <- paste0(res, '\n\n<!-- end of list -->\n')

    pandoc.p(res)

}

#' @S3method pander default
pander.default <- function(x, ...) {

    warning(sprintf('No pander method for "%s", reverting to default.', class(x)))
    class(x) <- 'list'
    pander(x)

}

#' @S3method pander evals
pander.evals <- function(x, ...) {

    o <- pander(x$result)

    if(panderOptions('evals.messages')) {
        if (!is.null(x$msg$messages))
            o <- paste0(o, ' **MESSAGE**', pandoc.footnote.return(x$msg$messages))
        if (!is.null(x$msg$warnings))
            o <- paste0(o, ' **WARNING**', pandoc.footnote.return(x$msg$warnings))
        if (!is.null(x$msg$error))
            o <- paste0(o, ' **ERROR**', pandoc.footnote.return(x$msg$errors))
    }

    cat(o)

}

#' @S3method pander rapport
pander.rapport <- function(x, ...)
    print(x)

#' @S3method pander POSIXct
pander.POSIXct <- function(x, ...)
    cat(format(x, panderOptions('date')))

#' @S3method pander POSIXt
pander.POSIXt <- function(x, ...)
    cat(format(x, panderOptions('date')))
