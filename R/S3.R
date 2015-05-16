#' Generic pander method
#'
#' Prints an R object in Pandoc's markdown.
#' @param x an R object
#' @param ... optional parameters passed to special methods and/or raw \code{pandoc.*} functions
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @note This function can be called by \code{pander} and \code{pandoc} too.
#' @references \itemize{
#'   \item John MacFarlane (2013): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#'   \item David Hajage (2011): _ascii. Export R objects to several markup languages._ \url{http://CRAN.R-project.org/package=ascii}
#'   \item Hlavac, Marek (2013): _stargazer: LaTeX code for well-formatted regression and summary statistics tables._ \url{http://CRAN.R-project.org/package=stargazer}
#' }
#' @export
#' @examples
#'
#' ## Vectors
#' pander(1:10)
#' pander(letters)
#' pander(mtcars$am)
#' pander(factor(mtcars$am))
#'
#' ## Lists
#' pander(list(1, 2, 3, c(1, 2)))
#' pander(list(a = 1, b = 2, c = table(mtcars$am)))
#' pander(list(1, 2, 3, list(1, 2)))
#' pander(list(a = 1, 2, 3, list(1, 2)))
#' pander(list('FOO', letters[1:3], list(1:5), table(mtcars$gear), list('FOOBAR', list('a', 'b'))))
#' pander(list(a = 1, b = 2, c = table(mtcars$am), x = list(myname = 1, 2), 56))
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
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' m <- glm(counts ~ outcome + treatment, family = poisson())
#' pander(m)
#' pander(anova(m))
#' pander(aov(m))
#' ## overwriting labels
#' pander(lm(Sepal.Width ~ Species, data = iris), covariate.labels = c('Versicolor', 'Virginica'))
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
pander <- function(x = NULL, ...) {

    ## save current knitr.auto.asis option
    kaao <- panderOptions('knitr.auto.asis')

    if (isTRUE(panderOptions('knitr.auto.asis')) && isTRUE(getOption('knitr.in.progress')) && requireNamespace('knitr', quietly = TRUE)) {

        ## override knitr.auto.asis option for nested calls
        panderOptions('knitr.auto.asis', FALSE)

        ## grab stdout
        stdout <- vector('character')
        con    <- textConnection('stdout', 'wr', local = TRUE)
        sink(con)
        sink(con, type = 'message')

        ## close
        on.exit({

            ## restore knitr.auto.asis option
            if (panderOptions('knitr.auto.asis') != kaao)
                panderOptions('knitr.auto.asis', kaao)

            ## revert R output back to normal
            sink()
            sink(type = 'message')
            close(con)

            ## re-add the final line-break
            if (tail(stdout, 1) == '')
                stdout <- c(stdout, '')

            return(knitr::asis_output(paste(stdout, collapse = '\n')))
        })
    }

    UseMethod('pander', x)

}


#' Deprecated pander_return
#'
#' This is a wrapper function around \code{pander} but instead of printing to \code{stdout}, this function returns a character vector of the captured lines.
#' @param ... everything passed to \code{pander}
#' @export pander.return
#' @usage pander.return(...)
#' @seealso pander
pander.return <- function(...) {
    .Deprecated('pander_return')
    capture.output(pander(...))
}


#' Pander and capture output
#'
#' This is a wrapper function around \code{pander} but instead of printing to \code{stdout}, this function returns a character vector of the captured lines.
#' @param ... everything passed to \code{pander}
#' @export
#' @seealso pander
pander_return <- function(...)
    capture.output(pander(...))


#' Pander method for a NULL object
#'
#' Prints a NULL object in Pandoc's markdown.
#' @param x a NULL object
#' @param ... ignored parameters
#' @export
pander.NULL <- function(x, ...)
    return(invisible(NULL))


#' Helper function to deal with atomic vectors
#' @param x vector
#' @param ... ignored parameters
#' @keywords internal
pander.vector <- function(x, ...) {

    if (!is.null(names(x)))
        return(pandoc.table(x))

    cat(p(x))

}


#' Pander method for logical class
#'
#' Prints a logical object in Pandoc's markdown.
#' @param x a logical object
#' @param ... ignored parameters
#' @export
pander.logical <- function(x, ...)
    pander.vector(x, ...)


#' Pander method for numeric class
#'
#' Prints a numeric class in Pandoc's markdown.
#' @param x a numeric object
#' @param ... igroned parameter
#' @export
pander.numeric <- function(x, ...)
    pander.vector(x, ...)


#' Pander method for factor class
#'
#' Prints a factor object in Pandoc's markdown.
#' @param x a factor object
#' @param ... igroned parameters
#' @export
pander.factor <- function(x, ...)
    pander.vector(x, ...)


#' Pander method for character class
#'
#' Prints a character class in Pandoc's markdown.
#' @param x a character object
#' @param ... igroned parameters
#' @export
pander.character <- function(x, ...) {

    if (!is.null(names(x)))
        return(pandoc.table(x))

    if (length(x) < 2)
        cat(x)
    else
        cat(p(x))

}


#' Pander method for image class
#'
#' Prints a image object in Pandoc's markdown.
#' @param x a image object
#' @param caption caption (string) to be shown under the table
#' @param href link that image should be linked with
#' @param ... ignored parameters
#' @export
pander.image <- function(x, caption = attr(x, 'caption'), href = attr(x, 'href'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    res <- pandoc.image.return(as.character(x), caption)

    if (is.null(href))
        cat(res)
    else
        pandoc.link(href, res)

}


#' Pander method for table class
#'
#' Prints a table object in Pandoc's markdown.
#' @param x a table object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.table <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    pandoc.table(x, caption = caption, ...)

}


#' Pander method for data.frame class
#'
#' Prints a data.frame object in Pandoc's markdown.
#' @param x a data.frame object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.data.frame <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    pandoc.table(x, caption = caption, ...)

}


#' Pander method for matrix class
#'
#' Prints a matrix object in Pandoc's markdown.
#' @param x a matrix object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.matrix <- function(x, caption = attr(x, 'caption'),  ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    pandoc.table(x, caption = caption, ...)

}


#' Pander method for cast_df class
#'
#' Prints a cast_df object in Pandoc's markdown.
#' @param x a cast_df object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.cast_df<- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    pandoc.table(as.data.frame(x), caption = caption, ...)

}


#' Pander method for summary.lm class
#'
#' Prints a summary.lm object in Pandoc's markdown.
#' @param x an summary.lm object
#' @param caption caption (string) to be shown under the table
#' @param covariate.labels vector to replace covariate lables in the table
#' @param omit vector of variable to omit for priting in resulting table
#' @param summary (defaut:\code{TRUE}) if used for summary.lm or lm
#' @param add.significance.stars if significance stars should be shown for P value
#' @param ... optional parameters passed to special methods and/or raw \code{pandoc.*} functions
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
pander.summary.lm <- function(x, caption = attr(x, 'caption'), covariate.labels, omit, summary = TRUE, add.significance.stars = FALSE, ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- pandoc.formula.return(x$call$formula, text = 'Fitting linear model:')
        else
            caption <- get.caption()
    }

    res <- as.data.frame(x$coeff)

    if (nrow(res) > 1)
        res <- res[c(2:nrow(res), 1), ]

    if (!missing(omit))
        res <- res[!apply(sapply(omit, grepl, row.names(res)), 1, any), ]

    if (!missing(covariate.labels))
        row.names(res)[1:length(covariate.labels)] <- covariate.labels

    if (add.significance.stars)
        res[, 4] <- add.significance.stars(res[, 4])

    if (summary) {
        pandoc.table(res, ...)
        if (class(x) == 'summary.glm') {
            cat('\n(Dispersion parameter for ', x$family$family, ' family taken to be ',
                format(x$dispersion), ')\n\n')
            stats <- cbind(paste(format(c('Null', 'Residual'), justify = 'right'), 'deviance:'),
                           apply(cbind(
                               format(unlist(x[c('null.deviance', 'deviance')]), digits = panderOptions('digits')), ' on',
                               format(unlist(x[c('df.null', 'df.residual')])), ' degrees of freedom\n'),
                                 1L, paste, collapse = ' '))
            rownames(stats) <- NULL
            colnames(stats) <- NULL
            pandoc.table(stats, keep.trailing.zeros = TRUE, ...)
        } else {
            pandoc.table(data.frame(
                'Observations'        = length(x$residuals),
                'Residual Std. Error' = x$sigma,
                '$R^2$'               = x$r.squared,
                'Adjusted $R^2$'      = x$adj.r.squared,
                check.names = FALSE), keep.trailing.zeros = TRUE, caption = caption, digits = panderOptions('digits'))
        }
    } else {

        pandoc.table(res, caption = caption, ...)

    }

}


#' Pander method for summary.glm class
#'
#' Prints a summary.glm object in Pandoc's markdown.
#' @param x an summary.glm object
#' @param caption caption (string) to be shown under the table
#' @param covariate.labels vector to replace covariate lables in the table
#' @param omit vector of variable to omit for priting in resulting table
#' @param summary (defaut:\code{TRUE}) if used for summary.lm or lm
#' @param ... optional parameters passed to special methods and/or raw \code{pandoc.*} functions
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
pander.summary.glm <- function(x, caption = attr(x, 'caption'), covariate.labels, omit, summary = TRUE, ...)
    pander.summary.lm(x, caption = caption, summary = summary, covariate.labels = covariate.labels, omit = omit, ...)


#' Pander method for summary.glm class
#'
#' Prints a summary.glm object in Pandoc's markdown.
#' @param x a summary.glm object
#' @param caption caption (string) to be shown under the table
#' @param covariate.labels vector to replace covariate lables in the table
#' @param omit vector of variable to omit for priting in resulting table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.lm <- function(x, caption = attr(x, 'caption'), covariate.labels, omit, ...)
    pander.summary.lm(summary(x), caption = caption, summary = FALSE, covariate.labels = covariate.labels, omit = omit, ...)


#' Pander method for summary.glm class
#'
#' Prints a summary.glm object in Pandoc's markdown.
#' @param x a summary.glm object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.glm <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- sprintf('Fitting generalized (%s) linear model: %s',
                               paste(x$family$family, x$family$link, sep = '/'),
                               pandoc.formula.return(x$call$formula))
        else
            caption <- get.caption()
    }

    pander.summary.glm(summary(x), caption = caption, summary = FALSE, ...)

}


#' Pander method for summary.aov class
#'
#' Prints a summary.aov object in Pandoc's markdown.
#' @param x a summary.aov object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.summary.aov <- function(x, caption = attr(x, 'caption'), ...) {

    res <- unclass(x)[[1]]

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- 'Analysis of Variance Model'
        else
            caption <- get.caption()
    }

    pandoc.table(res, caption = caption, ...)
}


#' Pander method for aov class
#'
#' Prints an aov object in Pandoc's markdown.
#' @param x an aov object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.aov <- function(x, caption = attr(x, 'caption'), ...) {
    pander(summary(x), caption = caption, ...)
}


#' Pander method for anova class
#'
#' Prints an anova object in Pandoc's markdown.
#' @param x an anova object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.anova <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption))
        if (is.null(storage$caption))
            if (!is.null(attr(x, 'heading')))
                caption <- strsplit(attr(x, 'heading'), '\n')[[1]][1]
    if (is.null(caption))
        caption <- get.caption()

    pandoc.table(x, caption = caption, ...)

}


#' Pander method for summary.aovlist class
#'
#' Prints a summary.aovlist object in Pandoc's markdown.
#' @param x a summary.aovlist object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.summary.aovlist <- function(x, caption = attr(x, 'caption'), ...) {
    n <- length(x)
    if (n == 1)
        pandoc.table(unclass(x[[1]][[1]]), caption, ...)
    else {
        z <- x[[1]][[1]]
        for (i in 2:n) {
            z <- rbind(z, x[[i]][[1]])
        }
        pandoc.table(z, caption = caption, ...)
    }
}


#' Pander method for aovlist class
#'
#' Prints an aovlist object in Pandoc's markdown.
#' @param x an aovlist object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.aovlist <- function(x, caption = attr(x, 'caption'), ...) {
    pander(summary(x), caption = caption, ...)
}


#' Pander method for htest class
#'
#' Prints a htest object in Pandoc's markdown.
#' @param x a htest object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.htest <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- paste0(x$method, ': `', gsub('( and | by )', '`\\1`', x$data.name), '`')
        else
            caption <- get.caption()
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
    pandoc.table(res, caption = caption, ...)

}


#' Pander method for summary.prcomp class
#'
#' Prints a summary.prcomp object in Pandoc's markdown.
#' @param x a summary.prcomp object
#' @param caption caption (string) to be shown under the table
#' @param summary (default:\code{TRUE}) if extended summary should be printed
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.summary.prcomp <- function(x, caption = attr(x, 'caption'), summary = TRUE, ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- 'Principal Components Analysis'
        else
            caption <- get.caption()
    }

    pandoc.table(x$rotation, caption = caption, ...)
    if (summary)
        pandoc.table(x$importance, ...)
}


#' Pander method for prcomp class
#'
#' Prints a prcomp object in Pandoc's markdown.
#' @param x a prcomp object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.prcomp <- function(x, caption = attr(x, 'caption'),  ...) {
    pander(summary(x), caption = caption, summary = FALSE, ...)
}


#' Pander method for density class
#'
#' Prints a density object in Pandoc's markdown.
#' @param x a density object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.density <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- sprintf('Kernel density of *%s* (bandwidth: %s)', x$data.name, format(x$bw))
        else
            caption <- get.caption()
    }

    res <- data.frame(Coordinates = as.numeric(summary(x$x)), 'Density values' = as.numeric(summary(x$y)), check.names = FALSE)
    rownames(res) <- names(summary(1))

    pandoc.table(res, caption = caption, ...)
}


#' Pander method for list class
#'
#' Prints a list object in Pandoc's markdown.
#' @param x a list object
#' @param ... ignored parameters
#' @export
pander.list <- function(x, ...) {

    ## match call
    mc <- match.call()
    if (is.null(mc$indent))
        indent <- 0
    else
        indent <- eval(mc$indent, parent.frame(1))

    ## replace missing values
    w <- which(is.na(x))
    if (length(w) > 0)
        x[w] <- panderOptions('missing')

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


#' Default Pander method
#'
#' Method to be used, when no exact S3 method for given object is found. Tries to render object as a list
#' @param x an object
#' @param ... optional parameters passed to raw \code{pandoc.list} function
#' @export
pander.default <- function(x, ...) {
    warning(sprintf('No pander.method for "%s", reverting to default.', class(x)))
    class(x) <- 'list'
    pander(x, ...)
}


#' Pander method for evals class
#'
#' Prints a evals object in Pandoc's markdown.
#' @param x a evals object
#' @param ... ignored parameters
#' @export
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


#' Pander method for rapport class
#'
#' Prints a rapport object in Pandoc's markdown.
#' @param x a rapport object
#' @param ... ignored parameters
#' @export
pander.rapport <- function(x, ...)
    print(x)


#' Pander method for POSIXlt class
#'
#' Prints a POSIXlt object in Pandoc's markdown.
#' @param x a POSIXlt object
#' @param ... optional parameters passed to raw \code{pandoc.date} function
#' @export
pander.POSIXlt <- function(x, ...)
    pandoc.date(x, ...)


#' Pander method for POSIXct class
#'
#' Prints a POSIXct object in Pandoc's markdown.
#' @param x a POSIXct object
#' @param ... optional parameters passed to raw \code{pandoc.date} function
#' @export
pander.POSIXct <- function(x, ...)
    pandoc.date(x, ...)


#' Pander method for Date class
#'
#' Prints a Date object in Pandoc's markdown.
#' @param x a Date object
#' @param ... optional parameters passed to raw \code{pandoc.date} function
#' @export
pander.Date <- function(x, ...)
    pandoc.date(x, ...)


#' Pander method for ftable class
#'
#' Prints a ftable object in Pandoc's markdown.
#' @param x a ftable object
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.ftable <- function(x, ...)
    pandoc.table(x, ...)


#' Pander method for mtable class
#'
#' Prints a mtable object in Pandoc's markdown.
#' @param x a mtable object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.mtable <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    coefs <- ftable(as.table(x$coefficients), row.vars = rev(x$as.row), col.vars = rev(x$as.col))
    coefs <- as.data.frame(rbind(coefs, x$summaries))
    col.size <- length(colnames(coefs))
    row.size <- length(dimnames(x$coefficients)[[3]])
    zeros <- rep(0, (col.size) * (row.size))
    temp <- matrix(zeros, ncol = (col.size))
    temp <- as.table(temp)

    for (i in 1:row.size) {
        tmp.row <- vector()
        s <- as.vector(rbind(as.vector(as.matrix(coefs[2 *i - 1, ])), as.vector(as.matrix(coefs[2*i, ]))))
        for (j in 1:col.size)
            tmp.row <- c(tmp.row, paste(s[2*j - 1], s[2 * j], sep = '\\ \n'))
        temp[i, ] <- tmp.row
    }

    temp <- rbind(temp, x$summaries)
    rownames(temp) <- c(dimnames(x$coefficients)[[3]], rownames(x$summaries))
    colnames(temp) <- colnames(coefs)

    pandoc.table(temp, caption = caption, keep.line.breaks = TRUE, ...)

}


#' Pander method for CrossTable class
#'
#' Prints a CrossTable object in Pandoc's markdown.
#' @param x a CrossTable object
#' @param caption caption (string) to be shown under the table
#' @param digits number of digits of precision
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.CrossTable <- function(x, caption = attr(x, 'caption'), digits = panderOptions("digits"), ...) {
    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()
    nr <- dim(x$t)[1]
    nc <- dim(x$t)[2]
    if (!is.null(rownames(x$t)))
        nt <- cbind(rownames(x$t), x$t, x$rs)
    else
        nt <- cbind("&nbsp;", x$t, x$rs)
    hdd <- 100
    if (!is.na(x$expected) && x$expected == TRUE) {
        xex <- outer(x$rs, x$cs, "*")
        xex <- xex/x$gt
        if (is.null(digits))
            digits = 1
        xx <- format(round(xex, digits), ...)
        xx <- cbind(rep("", nr), xx, rep("", nr))
        nt <- rbind(nt, xx)
        idx <- integer()
        for (i in 1:nr) idx <- c(idx, i, i + nr)
            nt <- nt[idx, ]
    }
    appendlines <- function(nt, xx, hasttl = FALSE, haslbl = FALSE) {
        if (!hasttl)
            xx <- cbind(xx, rep("", nr))
        if (!haslbl)
            xx <- cbind(rep("", nr), xx)
        n <- dim(nt)[1]/nr
        nt <- rbind(nt, xx)
        idx <- integer()
        k <- 1
        l <- nr * n + 1
        for (i in 1:nr) {
            for (j in 1:n) {
                idx <- c(idx, k)
                k <- k + 1
            }
            idx <- c(idx, l)
            l <- l + 1
        }
        nt <- nt[idx, ]
        nt
    }
    if (x$prop.chisq) {
        xx <- ((x$CST$expected - x$t)^2)/x$CST$expected
        xx <- format(round(xx, digits), trim = TRUE, ...)
        xx <- cbind("Chi-square", xx)
        nt <- appendlines(nt, xx, haslbl = TRUE)
    }
    if (!is.na(x$prop.row[1])) {
        xx <- cbind(x$prop.row, x$rs/x$gt)
        xx <- format(round(xx * hdd, digits), trim = TRUE, ...)
        xx <- matrix(paste(xx, "%", sep = ""), nrow = nr,
                 ncol = nc + 1)
        xx <- cbind("Row(%)", xx)
        nt <- appendlines(nt, xx, TRUE, haslbl = TRUE)
    }
    if (!is.na(x$prop.col[1])) {
        xx <- format(round(x$prop.col * hdd, digits), trim = TRUE,
                 ...)
        xx <- matrix(paste(xx, "%", sep = ""), nrow = nr,
                 ncol = nc)
        xx <- cbind("Column(%)", xx)
        nt <- appendlines(nt, xx, haslbl = TRUE)
    }
    if (!is.na(x$prop.tbl[1])) {
        xx <- format(round(x$prop.tbl * hdd, digits), trim = TRUE,
                 ...)
        xx <- matrix(paste(xx, "%", sep = ""), nrow = nr,
                 ncol = nc)
        xx <- cbind("Total(%)", xx)
        nt <- appendlines(nt, xx, haslbl = TRUE)
    }
    if (!is.na(x$resid) && x$resid == TRUE && x$expected == TRUE) {
        xx <- x$t - xex
        xx <- format(round(xx, digits), trim = TRUE, ...)
        nt <- appendlines(nt, xx)
    }
    if (!is.na(x$sresid) && x$sresid == TRUE && x$expected ==
          TRUE) {
        xx <- x$CST$residual
        xx <- format(round(xx, digits), trim = TRUE, ...)
        nt <- appendlines(nt, xx)
    }
    if (!is.na(x$asr[1])) {
        xx <- format(round(x$asr, digits), trim = TRUE, ...)
        nt <- appendlines(nt, xx)
    }
    n <- dim(nt)[1]/nr
    idx <- seq(1, dim(nt)[1], n)
    nt <- rbind(nt, c(gettext("Total", domain = "R-descr"), x$cs,
                    x$gt))
    if (!is.na(x$prop.col[1]))
        nt <- rbind(nt, c("", sapply(round(hdd * x$cs/x$gt, digits), function(x) paste(x, "%", sep = "")), ""))
    len <- dim(nt)[1]
    rownames(nt) <- as.character(1:len)

    # merging and print
    ts <- ifelse(!is.na(x$prop.col[1]), 2, 1)
    or <- (nrow(nt) - ts) / nr
    nt.nr <- nrow(nt)
    nc <- ncol(nt)
    RowData <- x$RowData
    ColData <- x$ColData
    res <- NULL
    for (i in 1:nr) {
        res.r <- paste(pandoc.strong.return(nt[1+or*(i - 1), 1]), "N", paste(nt[((2+or*(i - 1)) : (i * or)),1], collapse = "\\ \n"),
                   sep = "\\ \n")
        for (j in 2:nc) {
            res.r <- cbind(res.r, paste("&nbsp;", paste(nt[((1+or*(i - 1)) : (i * or)),j], collapse = "\\  \n"), sep = "\\ \n"))
        }
        res <- rbind(res, res.r)
    }
    res.r <- NULL
    for (j in 1:nc) {
        res.r <- cbind(res.r, paste(nt[(nt.nr-ts + 1) : nt.nr, j], collapse = "\\ \n"))
    }
    res <- rbind(res, res.r)
    cln <- c(if (RowData != "") RowData else '&nbsp;', colnames(x$t), 'Total')
    if (ColData != "") {
        cln.t <- paste(c("&nbsp;", ColData, rep("&nbsp;", nc - 2)), rep("\\\n", nc - 1), sep="")
        cln <- paste(cln.t, cln, sep="")
    }
    colnames(res) <- cln
    pandoc.table(res, caption = caption, keep.line.breaks = TRUE,...)
}


#' Pander method for timeseries class
#'
#' Prints a timeseries object in Pandoc's markdown.
#' @param x a timeseries object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.ts <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    if (!is.null(ncol(x))) {
        tp.1 <- trunc(time(x))
        tp.2 <- trunc(cycle(x))
        day.abb <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri',
                     'Sat')
        row.names <- switch(frequency(x), tp.1, 'Arg2', 'Arg3',
                            paste(tp.1, c('Q1', 'Q2', 'Q3', 'Q4')[tp.2], sep = ' '),
                            'Arg5', 'Arg6', paste('Wk.', tp.1, ' ', day.abb[tp.2],
                                                  sep = ''), 'Arg8', 'Arg9', 'Arg10', 'Arg11',
                            paste(tp.1, month.abb[tp.2], sep = ' '))
        t <- data.frame(x, row.names = row.names)
    } else {
        col.names <- switch(frequency(x), 'Value', 'Arg2', 'Arg3',
                            c('Q1', 'Q2', 'Q3', 'Q4'), 'Arg5', 'Arg6', day.abb,
                            'Arg8', 'Arg9', 'Arg10', 'Arg11', month.abb)
        row.names <- seq(from = start(x)[1], to = end(x)[1])
        t <- data.frame(matrix(c(rep(NA, start(x)[2] - 1),
                                 x, rep(NA, frequency(x) - end(x)[2])), ncol = frequency(x),
                               byrow = TRUE), row.names = row.names)
        names(t) <- col.names
    }

    pandoc.table(t, caption = caption, ...)

}


#' Pander method for formula class
#'
#' Prints a formula object in Pandoc's markdown.
#' @param x a formula object
#' @param max.width maximum width in characters per line
#' @param caption caption (string) to be shown under the formula
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.formula <- function(x, max.width = 80, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    pandoc.formula(x, max.width = max.width, caption = caption)

}


#' Pander method for call class
#'
#' Prints a call object in Pandoc's markdown.
#' @param x a call object
#' @param ... optional parameters passed to raw \code{pandoc.formula} function
#' @export
pander.call <- function(x, ...)
    pander.formula(x, ...)


#' Pander method for coxph class
#'
#' Prints a coxph object in Pandoc's markdown.
#' @param x an coxph object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.coxph <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- pandoc.formula.return(x$call$formula, text = 'Fitting Proportional Hazards Regression Model:')
        else
            caption <- get.caption()
    }

    cox <- x
    beta <- cox$coef
    se <- sqrt(diag(cox$var))

    if (is.null(cox$naive.var)) {
        c.tab <- cbind(beta, exp(beta), se, beta / se, 1 - pchisq((beta / se)^2, 1))
        dimnames(c.tab) <- list(names(beta), c('coef', 'exp(coef)', 'se(coef)', 'z', 'p'))
    } else {
        c.tab <- cbind(beta, exp(beta), se, beta / se, signif(1 - pchisq((beta / se)^2, 1), 1))
        dimnames(c.tab) <- list(names(beta), c('coef', 'exp(coef)', 'robust se', 'z', 'p'))
    }

    pandoc.table(c.tab, caption = caption, ...)
    logtest <- -2 * (x$loglik[1] - x$loglik[2])

    if (is.null(x$df))
        df <- sum(!is.na(beta))
    else
        df <- round(sum(x$df), 2)

    cat('\n')
    cat('Likelihood ratio test=', format(round(logtest, 2)), '  on ', df, ' df,', ' p=', format(1 - pchisq(logtest, df)), sep = '')

    omit <- x$na.action
    cat('  n=', x$n)
    if (!is.null(x$nevent))
        cat(', number of events=', x$nevent, '\n')
    else
        cat('\n')

    if (length(omit))
        cat('   (', naprint(omit), ')\n', sep = '')

}


#' Pander method for clogit class
#'
#' Prints a clogit object in Pandoc's markdown.
#' @param x an clogit object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.clogit <- function (x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- pandoc.formula.return(x$userCall, text = 'Fitting Conditional logistic regression:')
        else
            caption <- get.caption()
    }

    pander.coxph(x, caption = caption, ...)

}


#' Pander method for zoo class
#'
#' Prints a zoo object in Pandoc's markdown.
#' @param x an zoo object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.zoo <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    c.tab <- as.data.frame(x)
    c.tab <- cbind(pandoc.date.return(trunc(time(x)), simplified = TRUE), c.tab)

    if (length(colnames(x)) != 0)
        colnames(c.tab) <- c('Period', colnames(x))
    else
        colnames(c.tab) <- c('Period', 'Value')

    rownames(c.tab) <- NULL
    pandoc.table(c.tab, caption = caption, ...)

}


#' Pander method for lme class
#'
#' Prints a lme object in Pandoc's markdown.
#' @param x a lme object
#' @param caption caption (string) to be shown under the table
#' @param summary (default:\code{FALSE}) if to print expender summary
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.lme <- function(x, caption = attr(x, 'caption'), summary = FALSE, ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- sprintf('Linear mixed-effects model fit by %s : %s',
                               paste(sub('^[ ]*', '', ifelse(x$method == 'REML', 'REML', 'maximum likelihood'))),
                               pandoc.formula.return(x$call$fixed), collapse = '')
        else
            caption <- get.caption()
    }

    xs  <- summary(x)
    res <- as.data.frame(xs$tTable)

    if (summary) {
        pandoc.table(res, caption = pandoc.formula.return(x$call$fixed, text = 'Fixed effects: '),split.tables = Inf, ...)
        pandoc.table(xs$residuals, caption = 'Standardized Within-Group Residuals')
        pandoc.table(data.frame(
            'Observations'        = x$dims[["N"]],
            'Groups'              = x$dims$ngrps[1:x$dims$Q],
            'Log-restricted-likelihood' = x$logLik,
            check.names = FALSE), keep.trailing.zeros = TRUE, caption = caption, digits = 4)
    } else {

        pandoc.table(res, caption = caption, ...)

    }

}


#' Pander method for describe class
#'
#' Prints a describe object in Pandoc's markdown.
#' @param x an describe object
#' @param caption caption (string) to be shown under the table
#' @param short (default:\code{TRUE}) if to use consise output
#' @param split.tables (default:\code{60}) split.tables param for pandoc.table function
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.describe <- function(x, caption = attr(x, 'caption'), short = TRUE, split.tables = 60, ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()
    describe.single <- function(x, caption, short, split.tables, ...) {
        if (length(x$units))
            des <- paste(des, ' [', x$units, ']', sep = '')
        if (length(x$format))
            des <- paste(des, '  Format:', x$format, sep = '')
        dim.counts <- dim(x$count)
        if (is.null(dim.counts)) {
            counts <- as.character(x$count)
        }
        else {
            counts <- matrix(as.character(x$count), dim.counts[1], dim.counts[2])
        }
        names(counts) <- names(x$count)
        counts <- pandoc.table.return(counts, caption = caption, split.tables = split.tables, ...)
        val <- x$values
        if (length(val)) {
            if (!is.matrix(val)) {
                if (length(val) == 10) {
                    if (short) {
                        low <- paste('lowest:', paste(val[1:5], collapse = ' '))
                        hi  <- paste('highest:', paste(val[6:10], collapse = ' '))
                        if (nchar(low) + nchar(hi) + 2 > 80)
                            val <- as.list(c(low, hi))
                        else
                            val <- as.list(paste(low, hi, sep = ', '))
                        val <- pandoc.list.return(val, add.end.of.list = FALSE, ...)
                    } else {
                        if (is.null(dim(val))) {
                            val <- as.character(val)
                        }else {
                            val <- matrix(as.character(val), dim(val)[1],
                                          dim(val)[2])
                        }
                        names(val) <- names(x$values)
                        val <- pandoc.table.return(val, split.tables = split.tables, caption = caption, ...)
                    }
                } else {
                    val <- paste(names(val),
                                 ifelse(val > 1, paste(' (', val, ')', sep = ''), ''), sep = '')
                    val <- strwrap(val, exdent = 4)
                    val <- as.list(sub('(^    )(.*)', '\t\\2', val))
                    val <- pandoc.table(val, ...)
                }
            } else {
                lev <- dimnames(val)[[2]]
                if (short && (mean(nchar(lev)) > 10 | length(lev) <
                              5)) {
                    z <- ""
                    len <- 0
                    for (i in 1:length(lev)) {
                        w <- paste(lev[i], ' (', val[1, i], ', ', val[2, i], '%)', sep = '')
                        if (i == 1)
                            z <- w
                        else z <- paste(z, w, sep = ', ')
                    }
                    val <- pander.return(z)
                } else {
                    dim.val <- dim(val)
                    if (is.null(dim.val)) {
                        val <- as.character(val)
                    }
                    else {
                        val <- matrix(as.character(val), dim.val[1], dim.val[2])
                    }
                    rownames(val) <- rownames(x$values)
                    colnames(val) <- colnames(x$values)
                    val <- pandoc.table.return(val, split.tables = split.tables, caption = caption, ...)
                }
            }
        }
        cat(counts)
        cat(val, '\n')
    }
    at <- attributes(x)
    if (length(at$dimensions)) {
        cat(at$descript, '\n\n', at$dimensions[2], ' Variables\t', at$dimensions[1], ' Observations\n')
        if (length(at$naprint))
            cat('\n', at$naprint, '\n')
        for (z in x) {
            if (length(z) == 0)
                next
            describe.single(z, caption = paste('Variable ', z$descript, sep = ''), short = short, split.tables = split.tables, ...)
        }
        if (length(at$missing.vars)) {
            cat('\nVariables with all observations missing:\n\n')
            pander(at$missing.vars, quote = FALSE)
        }
    } else
        describe.single(x, caption = caption, short = short, split.tables = split.tables, ...)

    invisible()

}


#' Pander method for survdiff class
#'
#' Prints an survdiff object in Pandoc's markdown.
#' @param x an survdiff object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.survdiff <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- pandoc.formula.return(x$call$formula, text = 'Call:')
        else
            caption <- get.caption()
    }

    if (length(x$n) == 1) {
        z <- sign(x$exp - x$obs) * sqrt(x$chisq)
        temp <- c(x$obs, x$exp, z, signif(1 - pchisq(x$chisq, 1), panderOptions('digits')))
        names(temp) <- c('Observed', 'Expected', 'Z', 'p')
        temp <- t(temp)
        include.rownames = FALSE
    } else {
        if (is.matrix(x$obs)) {
            otmp <- apply(x$obs, 1, sum)
            etmp <- apply(x$exp, 1, sum)
        } else {
            otmp <- x$obs
            etmp <- x$exp
        }
        df <- sum(1 * (etmp > 0)) - 1
        p <- 1 - pchisq(x$chisq, df[!is.na(df)])
        temp <- cbind(x$n, otmp, etmp, ((otmp - etmp)^2)/etmp,
                      ((otmp - etmp)^2)/diag(x$var))
        dimnames(temp) <- list(names(x$n), c('N', 'Observed', 'Expected', '(O-E)^2/E', '(O-E)^2/V'))
        caption <- paste(caption, sprintf('Chisq = %f \non %d degrees of freedom, p = %f', x$chisq, df, p), sep = ' ')
    }

    temp <- as.data.frame(temp, checknames = FALSE)
    pandoc.table(temp, caption = caption)

}


#' Pander method for survfit class
#'
#' Prints an survfit object in Pandoc's markdown.
#'
#' @param x the result of a call to the survfit function.
#' @param caption caption (string) to be shown under the table
#' @param scale	a numeric value to rescale the survival time, e.g., if the input data to survfit were in days, scale=365 would scale the printout to years.
#' @param print.rmean,rmean options for computation and display of the restricted mean
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.survfit <- function (x, caption = attr(x, 'caption'), scale = 1, print.rmean = getOption('survfit.print.rmean'), rmean = getOption('survfit.rmean'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    omit <- x$na.action
    na <- NULL

    if (length(omit))
        na <- pander(list(naprint(omit)), list.type = 'none')

    if (!missing(print.rmean) && is.logical(print.rmean) && missing(rmean)) {
        if (print.rmean)
            rmean <- 'common'
        else
            rmean <- 'none'
    }

    if (is.null(rmean)) {
        if (is.logical(print.rmean)) {
            if (print.rmean)
                rmean <- 'common'
            else
                rmean <- 'none'
        } else
            rmean <- 'none'
    }

    if (is.numeric(rmean)) {
        if (is.null(x$start.time)) {
            if (rmean < min(x$time))
                stop('Truncation point for the mean is < smallest survival')
        } else
            if (rmean < x$start.time)
                stop('Truncation point for the mean is < smallest survival')
    } else {
        rmean <- match.arg(rmean, c('none', 'common', 'individual'))
        if (length(rmean) == 0)
            stop('Invalid value for rmean option')
    }

    temp <- getFromNamespace('survmean', 'survival')(x, scale = scale, rmean)
    mat <- pandoc.table(temp$matrix, caption = ...)
    restrm <- NULL

    if (rmean != 'none') {
        if (rmean == 'individual')
            restrm <- pander('* restricted mean with variable upper limit')
        else
            restrm <- pander(paste('* restricted mean with upper limit = ', format(temp$end.time[1])))
    }

}

#' Pander method for smooth.spline class
#'
#' Prints an smooth.spline object in Pandoc's markdown.
#' @param x an smooth.spline object
#' @param ... igroned parameters
#' @export
pander.smooth.spline <- function(x, ...) {

    x <- as.list(capture.output(x)[-1:-3])
    pandoc.list(x, add.end.of.list = FALSE)

}


#' Pander method for rlm class
#'
#' Prints an rlm object in Pandoc's markdown.
#' @param x an rlm object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.rlm <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            if (!is.null(x$call))
                caption <- pandoc.formula.return(x$call$formula, text = 'Fitting linear model by robust regression:')
            else
                caption <- 'Fitting linear model by robust regression'
        else
            caption <- get.caption()
    }

    if (x$converged)
        cat('Converged in', length(x$conv), 'iterations\n')
    else
        cat('Ran', length(x$conv), 'iterations without convergence\n')

    coef <- x$coefficients
    pandoc.table(coef, caption = caption, ...)
    nobs <- length(x$residuals)
    rdf <- nobs - length(coef)
    cat('Degrees of freedom:', nobs, 'total;', rdf, 'residual\n\n')

    if (nzchar(mess <- naprint(x$na.action)))
        cat('  (', mess, ')\n', sep = '')

    cat('Scale estimate:', format(signif(x$s, 3)), '\n')

}


#' Pander method for stat.table class
#'
#' Prints an stat.table object in Pandoc's markdown.
#' @param x an stat.table object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.stat.table <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    if (length(dim(x)) == 2)
        results <- pandoc.table(t(x), caption = caption, ...)

    if (length(dim(x)) == 3) {

        xx <- list()

        for (i in 1:dim(x)[2])
            xx[[i]] <- x[, i, ]

        xx <- do.call(rbind, xx)
        xx <- apply(xx, c(1, 2), format, digits = panderOptions('digits'))
        dn <- dimnames(x)
        lgroup <- list(names(dn)[2], dn[[2]])
        tgroup <- names(dn)[3]
        c.s <- length(dn[[3]])
        xx <- rbind(colnames(xx), xx)
        xx <- cbind(unlist(lgroup), xx)
        xx <- rbind(c(rep('', c.s), tgroup), xx)
        colnames(xx) <- NULL
        pandoc.table(xx, caption = caption, emphasize.rows = c(1, 2), emphasize.cols = 1, ...)
    }

}


#' Pander method for sessionInfo class
#'
#' Prints an sessionInfo object in Pandoc's markdown.
#' @param x an sessionInfo object
#' @param locale (defaut:\code{TRUE}) if to print locale output
#' @param compact (defaut:\code{TRUE}) if output shoud be compact (ommiting extra line breaks and spaces, inline printing of lists)
#' @param ... ignored parameters
#' @export
pander.sessionInfo <- function (x, locale = TRUE, compact = TRUE, ...) {

    mkLabel <- function(L, n) {
        vers <- sapply(L[[n]], function(x) x[['Version']])
        pkg <- sapply(L[[n]], function(x) x[['Package']])
        sprintf('%s(v.%s)', pkg, vers)
    }

    cat(pandoc.strong(x$R.version$version.string), '\n\n', sep = '')
    cat(pandoc.strong('Platform:'), x$platform, '\n\n', sep = ' ')

    if (locale) {
        cat(pandoc.strong('locale:'))
        cat('\n')
        pander(gsub('[/]', '||', strsplit(x$locale, ';', fixed = TRUE)[[1]]), ...)
        cat('\n')
    }

    if (compact) {
        attached.base.packages <- pander.return(x$basePkgs, quote = FALSE, ...)
        other.attached.packages <- pander.return(mkLabel(x, 'otherPkgs'), quote = FALSE, ...)
        load.via.namespaces <- pander.return(mkLabel(x, 'loadedOnly'), quote = FALSE, ...)
    } else {
        attached.base.packages <- pandoc.list.return(x$basePkgs, add.end.of.list = FALSE, ...)
        other.attached.packages <- pandoc.list.return(mkLabel(x, 'otherPkgs'), add.end.of.list = FALSE, ...)
        load.via.namespaces <- pandoc.list.return(mkLabel(x, 'loadedOnly'), add.end.of.list = FALSE, ...)
    }

    cat('\n')
    cat(pandoc.strong('attached base packages:'), '\n')
    cat(attached.base.packages)

    if (!is.null(x$otherPkgs)) {
        cat('\n\n')
        cat(pandoc.strong('other attached packages:'), '\n')
        cat(other.attached.packages)
    }

    if (!is.null(x$loadedOnly)) {
        cat('\n\n')
        cat(pandoc.strong('loaded via a namespace (and not attached):'), '\n')
        cat(load.via.namespaces)
    }

    invisible(x)

}


#' Pander method for microbenchmark class
#'
#' Prints an microbenchmark object in Pandoc's markdown.
#' @param x an microbenchmark object
#' @param caption caption (string) to be shown under the table
#' @param expr.labels expression labels that will replace default ones (similar to rownames, which microbenchmark class table does not have)
#' @param unit units in which values should be printed (for example second, microseconds, etc.). Should be one of “ns”, “us”, “ms”, “s”, “t”, “hz”, “khz”, “mhz”, “eps”, “f”
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.microbenchmark <- function(x, caption = attr(x, 'caption'), expr.labels, unit, ...) {

    xs <- summary(x, unit = unit)

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- paste('Unit: ', attr(xs, 'unit'), sep = '')
        else
            caption <- get.caption()
    }

    if (!missing(expr.labels)) {
        xs[, 1] <- as.vector(xs[, 1])
        xs[1:length(expr.labels), 1] <- expr.labels
    }

    pander(xs, caption = caption, ...)

}


#' Pander method for function class
#'
#' Prints an function object in Pandoc's markdown.
#' @param x an function object
#' @param add.name (defaut:\code{FALSE}) if to add function name to output or just to print a body
#' @param verbatim (defaut:\code{TRUE}) if to add tabulation, so pandoc conversion will rander it properly
#' @param syntax.highlighting (defaut:\code{FALSE}) if to add hyghlighting tag for R syntax
#' @param ... ignored parameters
#' @export
pander.function <- function(x, add.name = FALSE, verbatim = TRUE, syntax.highlighting = FALSE, ...) {

    fname <- substitute(x)
    ps <- ''

    if (syntax.highlighting)
        cat('```r\n')
    else
        ps <- ifelse(verbatim, '\t', '')

    if (!is.null(add.name) && add.name)
        cat(ps, fname, ' <- ', sep = '')

    for (line in deparse(x))
        cat(ps, line, '\n', sep = '')

    if (syntax.highlighting)
        cat('```')

}


#' Pander method for tabular class
#'
#' Renders an tabular object in Pandoc's markdown.
#' @param x an function object
#' @param caption caption (string) to be shown under the table
#' @param digits number of digits of precision
#' @param emphasize.rownames (defaut:\code{TRUE}) if rownames should be highlighted
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
#' @examples
#' library(tables)
#' pander(tabular(as.factor(am) ~ (mpg+hp+qsec) * (mean+median), data = mtcars), split.tables = Inf)
#' pander(tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
#' (Sepal.Length + Sepal.Width)*(mean + sd), data=iris ), split.tables = Inf)
#' Sex <- factor(sample(c("Male", "Female"), 100, rep=TRUE))
#' Status <- factor(sample(c("low", "medium", "high"), 100, rep=TRUE))
#' z <- rnorm(100)+5
#' fmt <- function(x) {
#'    s <- format(x, digits=2)
#'    even <- ((1:length(s)) %% 2) == 0
#'    s[even] <- sprintf("(%s)", s[even])
#'    s
#' }
#' tab <- tabular( Justify(c)*Heading()*z*Sex*Heading(Statistic)*Format(fmt())*(mean+sd)
#'                ~ Status )
#' pander(tab, emphasize.rownames = FALSE)
pander.tabular <- function(x, caption = attr(x, 'caption'), emphasize.rownames = TRUE, digits = panderOptions('digits'), ...) {
    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()
    data <- as.matrix(x, format = T, rowLabels = F, colLabels = F, digits = digits)
    rlabels <- attr(x, "rowLabels")
    rlabels[is.na(rlabels)] <- ""
    clabels <- attr(x, "colLabels")
    clabels[is.na(clabels)] <- ""
    if (!is.null(colnames(rlabels))) { # needed for case of more complex tabular structure (see examples)
        cl <- colnames(rlabels)
        data <- cbind(rlabels, data)
        clabels <- cbind(rbind(matrix("",
                                      nrow = (nrow(clabels) - 1),
                                      ncol = length(cl)),
                               colnames(rlabels)),
                         clabels)
    }
    clabels <- apply(clabels, c(2), paste, collapse = "\\ \n")
    colnames(data) <- clabels
    if (emphasize.rownames)
        pandoc.table(data, caption = caption, keep.line.breaks = TRUE, emphasize.cols = 1:length(cl), ...)
    else
        pandoc.table(data, caption = caption, keep.line.breaks = TRUE, ...)
}
