#' Generic pander method
#'
#' Prints an R object in Pandoc's markdown.
#' @param x an R object
#' @param ... optional parameters passed to special methods and/or raw \code{pandoc.*} functions
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @note This function can be called by \code{pander} and \code{pandoc} too.
#' @references \itemize{
#'   \item John MacFarlane (2013): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#'   \item David Hajage (2011): _ascii. Export R objects to several markup languages._ \url{https://cran.r-project.org/package=ascii}
#'   \item Hlavac, Marek (2013): _stargazer: LaTeX code for well-formatted regression and summary statistics tables._ \url{https://cran.r-project.org/package=stargazer}
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

    if (isTRUE(panderOptions('knitr.auto.asis')) &&
        isTRUE(getOption('knitr.in.progress')) &&
        requireNamespace('knitr', quietly = TRUE)) {

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
            if (panderOptions('knitr.auto.asis') != kaao) {
                panderOptions('knitr.auto.asis', kaao)
            }

            ## revert R output back to normal
            sink()
            sink(type = 'message')
            close(con)

            ## restore the final line break
            if (tail(stdout, 1) == '') {
                stdout <- c(stdout, '')
            }

            return(knitr::asis_output(paste(stdout, collapse = '\n')))
        })
    }

    UseMethod('pander', x)

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

    if (!is.null(names(x))) {
        return(pandoc.table(x))
    }

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

    if (!is.null(names(x))) {
        return(pandoc.table(x))
    }

    if (length(x) < 2) {
        cat(x)
    } else {
        cat(p(x))
    }

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

    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }

    res <- pandoc.image.return(as.character(x), caption)

    if (is.null(href)) {
        cat(res)
    } else {
        pandoc.link(href, res)
    }

}


#' Pander method for table class
#'
#' Prints a table object in Pandoc's markdown.
#' @param x a table object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.table <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }

    pandoc.table(x, caption = caption, ...)

}

#' Pander method for data.table class
#'
#' Prints a data.table object in Pandoc's markdown. Data.tables drop attributes (like row names) when called.
#' @param x a data.table object
#' @param caption caption (string) to be shown under the table
#' @param keys.as.row.names controls whether to use data.table key as row names when calling pandoc.table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.data.table <- function(x, caption = attr(x, 'caption'),
                              keys.as.row.names = TRUE, ...) {

    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }

    requireNamespace('data.table', quietly = TRUE)
    xx <- data.table::copy(x)
    if (keys.as.row.names && data.table::haskey(xx)) {
        firstkey <- data.table::key(xx)[1]
        row.names.dt <- as.character(xx[[firstkey]])
        xx <- xx[, setdiff(colnames(xx), firstkey), with = FALSE, drop = FALSE]
        data.table::setattr(xx, 'row.names', row.names.dt)
    }

    pandoc.table(xx, caption = caption, ...)

}

#' Pander method for data.frame class
#'
#' Prints a data.frame object in Pandoc's markdown.
#' @param x a data.frame object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.data.frame <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }

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
    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }
    pandoc.table(x, caption = caption, ...)
}


#' Pander method for cast_df class
#'
#' Prints a cast_df object in Pandoc's markdown.
#' @param x a cast_df object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.cast_df <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }

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
#' @param move.intercept by default, the Intercept is the first coefficient in the table, which can be moved to the bottom of the table
#' @param ... optional parameters passed to special methods and/or raw \code{pandoc.*} functions
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
pander.summary.lm <- function(x, caption = attr(x, 'caption'), covariate.labels,
                              omit, summary = TRUE, add.significance.stars = FALSE, move.intercept = FALSE, ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption)) {
            caption <- pandoc.formula.return(x$call$formula, text = 'Fitting linear model:')
        } else {
            caption <- get.caption()
        }
    }

    res <- as.data.frame(x$coeff)

    if (move.intercept && rownames(res)[1] == '(Intercept)' & nrow(res) > 1) {
        res <- res[c(2:nrow(res), 1), ]
    }

    if (!missing(omit)) {
        res <- res[!apply(sapply(omit, grepl, row.names(res)), 1, any), ]
    }

    if (!missing(covariate.labels)) {
        row.names(res)[1:length(covariate.labels)] <- covariate.labels
    }

    if (add.significance.stars) {
        res <- cbind(res, ' ' = add.significance.stars(res[, 4]))
    }

    if (summary) {
        pandoc.table(res, ...)
        if (class(x) == 'summary.glm') {
            cat('\n(Dispersion parameter for ', x$family$family, ' family taken to be ',
                format(x$dispersion), ')\n\n')
            stats <- cbind(paste(format(c('Null', 'Residual'), justify = 'right'), 'deviance:'),
                           apply(cbind(
                               format(unlist(x[c('null.deviance', 'deviance')]), digits = panderOptions('digits')),
                               ' on',
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


#' Pander method for summary.lm class
#'
#' Prints a summary.lm object in Pandoc's markdown.
#' @param x a summary.glm object
#' @param caption caption (string) to be shown under the table
#' @param covariate.labels vector to replace covariate lables in the table
#' @param omit vector of variable to omit for priting in resulting table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.lm <- function(x, caption = attr(x, 'caption'), covariate.labels, omit, ...)
    pander.summary.lm(summary(x),
                      caption = caption,
                      summary = FALSE,
                      covariate.labels = covariate.labels,
                      omit = omit, ...)


#' Pander method for summary.glm class
#'
#' Prints a summary.glm object in Pandoc's markdown.
#' @param x a summary.glm object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.glm <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption)) {
            caption <- sprintf('Fitting generalized (%s) linear model: %s',
                               paste(x$family$family, x$family$link, sep = '/'),
                               pandoc.formula.return(x$call$formula))
        } else {
            caption <- get.caption()
        }
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
        if (is.null(storage$caption)) {
            caption <- 'Analysis of Variance Model'
        } else {
            caption <- get.caption()
        }
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
#' @param add.significance.stars if significance stars should be shown for P value
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.anova <- function(x, caption = attr(x, 'caption'), add.significance.stars = FALSE, ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption)) {
            if (!is.null(attr(x, 'heading'))) {
                caption <- strsplit(attr(x, 'heading'), '\n')[[1]][1]
            }
        }
    }
    if (is.null(caption)) {
        caption <- get.caption()
    }
    if (add.significance.stars) {
        x <- cbind(x, ' ' = add.significance.stars(x[, 5]))
    }
    pandoc.table(x, caption = caption, ...)
    if (add.significance.stars) {
        cat('Signif. codes:  0 \'\\*\\*\\*\' 0.001 \'\\*\\*\' 0.01 \'\\*\' 0.05 \'.\' 0.1 \' \' 1\n')
    }
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
    if (n == 1) {
        pandoc.table(unclass(x[[1]][[1]]), caption, ...)
    } else {
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
        if (is.null(storage$caption)) {
            caption <- paste0(x$method, ': `', gsub('( and | by )', '`\\1`', paste(x$data.name, collapse='')), '`')
        } else {
            caption <- get.caption()
        }
    }

    ## we do not know which values are provided
    res <- data.frame(placeholder = 'FOO')

    ## add what we know
    if (!is.null(x$statistic)) {
        res$'Test statistic' <- as.numeric(x$statistic)
    }
    if (!is.null(x$parameter)) {
        res[names(x$parameter)] <- x$parameter
    }
    if (!is.null(x$p.value)) {
        res$'P value' <- paste(
          format(round(x$p.value, panderOptions('round')),
                 trim         = TRUE,
                 digits       = panderOptions('digits'),
                 decimal.mark = panderOptions('decimal.mark')),
          add.significance.stars(x$p.value))
    }
    if (!is.null(x$alternative)) {
        res['Alternative hypothesis'] <- x$alternative
    }
    if (!is.null(x$estimate)) {
        if (!is.null(names(x$estimate))) {
            res[names(x$estimate)] <- x$estimate
        } else {
            res['Estimate'] <- x$estimate
        }
    }

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
        if (is.null(storage$caption)) {
            caption <- 'Principal Components Analysis'
        } else {
            caption <- get.caption()
        }
    }

    pandoc.table(x$rotation, caption = caption, ...)
    if (summary) {
        pandoc.table(x$importance, ...)
    }
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
        if (is.null(storage$caption)) {
            caption <- sprintf('Kernel density of *%s* (bandwidth: %s)', x$data.name, format(x$bw))
        } else {
            caption <- get.caption()
        }
    }

    res <- data.frame('Coordinates' = as.numeric(summary(x$x)),
                      'Density values' = as.numeric(summary(x$y)),
                      check.names = FALSE)
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
    if (is.null(mc$indent)) {
        indent <- 0
    } else {
        indent <- eval(mc$indent, parent.frame(1))
    }

    ## replace missing values
    w <- which(is.na(x))
    if (length(w) > 0) {
        x[w] <- panderOptions('missing')
    }

    ## grab elements name (if any)
    x.names <- sapply(names(x), function(x) ifelse(x == '', '  *', sprintf('  * **%s**:', x)))
    if (length(x.names) == 0) {
        x.names <- rep('  *', length(x))
    }

    ## capture pandoc output of list element
    res <- paste(unlist(lapply(1:length(x), function(i) {

        res.i <- paste(capture.output(pander(x[[i]], indent = indent + 1)), collapse = '\n')
        if (grepl('\n', res.i) & !grepl('\n *\\* ', res.i)) {
            res.i <- sub('^\n', '\n\n', res.i)
            res.i <- pandoc.indent(res.i, 1)
        }

        paste(x.names[i], res.i)

    })), collapse = '\n') #nolint

    ## indent output
    res <- pandoc.indent(res, indent)

    ## add (comment): end of list (preventing conflicts with forthcoming pandoc blocks)
    if (indent == 0) {
        res <- paste0(res, '\n\n<!-- end of list -->\n')
    }

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

    if (panderOptions('evals.messages')) {
        if (!is.null(x$msg$messages)) {
            o <- paste0(o, ' **MESSAGE**', pandoc.footnote.return(x$msg$messages))
        }
        if (!is.null(x$msg$warnings)) {
            o <- paste0(o, ' **WARNING**', pandoc.footnote.return(x$msg$warnings))
        }
        if (!is.null(x$msg$error)) {
            o <- paste0(o, ' **ERROR**', pandoc.footnote.return(x$msg$errors))
        }
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


#' Pander method for CrossTable class
#'
#' Prints a CrossTable object in Pandoc's markdown.
#' @param x a CrossTable object
#' @param caption caption (string) to be shown under the table
#' @param digits number of digits of precision
#' @param total.r if to print row totals. Default values is taken from CrossTable object
#' @param total.c if to print column totals. Default values is taken from CrossTable object
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.CrossTable <- function(x, caption = attr(x, 'caption'), digits = panderOptions('digits'),
                              total.r = x$total.r, total.c = x$total.c, ...) {
    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }
    nr <- dim(x$t)[1]
    nc <- dim(x$t)[2]
    if (!is.null(rownames(x$t))) {
        nt <- cbind(rownames(x$t), x$t, x$rs)
    } else {
        nt <- cbind('&nbsp;', x$t, x$rs)
    }
    hdd <- 100
    appendlines <- function(nt, xx, hasttl = FALSE, haslbl = FALSE) {
        if (!hasttl) {
            xx <- cbind(xx, rep('', nr))
        }
        if (!haslbl) {
            xx <- cbind(rep('', nr), xx)
        }
        n <- dim(nt)[1] / nr
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
    if (!is.na(x$expected) && x$expected == TRUE) {
        xex <- outer(x$rs, x$cs, '*')
        xex <- xex / x$gt
        if (is.null(digits)) {
            digits <- 1
        }
        xx <- format(round(xex, digits), ...)
        xx <- cbind(rep('Expected N', nr), xx, rep('', nr))
        nt <- rbind(nt, xx)
        idx <- integer()
        for (i in 1:nr) {
            idx <- c(idx, i, i + nr)
        }
        nt <- nt[idx, ]
    }
    if (x$prop.chisq) {
        xx <- (x$CST$expected - x$t) ^ 2 / x$CST$expected
        xx <- format(round(xx, digits), trim = TRUE, ...)
        xx <- cbind('Chi-square', xx)
        nt <- appendlines(nt, xx, haslbl = TRUE)
    }
    if (!is.na(x$prop.row[1])) {
        xx <- cbind(x$prop.row, x$rs / x$gt)
        xx <- format(round(xx * hdd, digits), trim = TRUE, ...)
        xx <- matrix(paste(xx, '%', sep = ''), nrow = nr,
                 ncol = nc + 1)
        xx <- cbind('Row(%)', xx)
        nt <- appendlines(nt, xx, TRUE, haslbl = TRUE)
    }
    if (!is.na(x$prop.col[1])) {
        xx <- format(round(x$prop.col * hdd, digits), trim = TRUE,
                 ...)
        xx <- matrix(paste(xx, '%', sep = ''), nrow = nr,
                 ncol = nc)
        xx <- cbind('Column(%)', xx)
        nt <- appendlines(nt, xx, haslbl = TRUE)
    }
    if (!is.na(x$prop.tbl[1])) {
        xx <- format(round(x$prop.tbl * hdd, digits), trim = TRUE,
                 ...)
        xx <- matrix(paste(xx, '%', sep = ''), nrow = nr,
                 ncol = nc)
        xx <- cbind('Total(%)', xx)
        nt <- appendlines(nt, xx, haslbl = TRUE)
    }
    if (!is.na(x$resid) && x$resid == TRUE && x$expected == TRUE) {
        xx <- x$t - xex
        xx <- format(round(xx, digits), trim = TRUE, ...)
        xx <- cbind('Residual', xx)
        nt <- appendlines(nt, xx, haslbl = TRUE)
    }
    if (!is.na(x$sresid) && x$sresid == TRUE
        && x$expected == TRUE) {
        xx <- x$CST$residual
        xx <- format(round(xx, digits), trim = TRUE, ...)
        xx <- cbind('Std Residual', xx)
        nt <- appendlines(nt, xx, haslbl = TRUE)
    }
    if (!is.na(x$asr[1])) {
        xx <- format(round(x$asr, digits), trim = TRUE, ...)
        xx <- cbind('Adj Std Resid', xx)
        nt <- appendlines(nt, xx, haslbl = TRUE)
    }
    n <- dim(nt)[1] / nr
    idx <- seq(1, dim(nt)[1], n)
    nt <- rbind(nt, c(gettext('Total', domain = 'R-descr'), x$cs,
                    x$gt))
    if (!is.na(x$prop.col[1])) {
        nt <- rbind(nt, c('', sapply(round(hdd * x$cs / x$gt, digits), function(x) paste(x, '%', sep = '')), ''))
    }
    len <- dim(nt)[1]
    rownames(nt) <- as.character(1:len)

    ## merging and print
    ts <- ifelse(!is.na(x$prop.col[1]), 2, 1)
    or <- (nrow(nt) - ts) / nr
    nt.nr <- nrow(nt)
    nc <- ncol(nt)
    if (!is.null(total.r) && !total.r) {
        nc <- nc - 1
    }
    RowData <- x$RowData
    ColData <- x$ColData
    res <- NULL
    for (i in 1:nr) {
        res.r <- paste(pandoc.strong.return(nt[1 + or * (i - 1), 1]),
                       'N',
                       paste(nt[ (2 + or * (i - 1)) : (i * or), 1], collapse = '\\ \n'),
                   sep = '\\ \n')
        for (j in 2:nc) {
            res.r <- cbind(res.r,
                           paste('&nbsp;',
                                 paste(nt[ (1 + or * (i - 1)) : (i * or), j], collapse = '\\  \n'),
                                 sep = '\\ \n'))
        }
        res <- rbind(res, res.r)
    }
    res.r <- NULL
    if (is.null(total.c) || total.c) {
        for (j in 1:nc) {
            res.r <- cbind(res.r, paste(nt[ (nt.nr - ts + 1) : nt.nr, j], collapse = '\\ \n'))
        }
        res <- rbind(res, res.r)
    }
    cln <- c(ifelse(RowData != '', RowData, '&nbsp;'), colnames(x$t))
    if (is.null(total.r) || total.r) {
        cln <- c(cln, 'Total')
    }
    if (ColData != '') {
        cln.t <- paste(c('&nbsp;', ColData, rep('&nbsp;', nc - 2)), rep('\\\n', nc - 1), sep = '')
        cln <- paste(cln.t, cln, sep = '')
    }
    colnames(res) <- cln
    pandoc.table(res, caption = caption, keep.line.breaks = TRUE, ...)
}


#' Pander method for timeseries class
#'
#' Prints a timeseries object in Pandoc's markdown.
#' @param x a timeseries object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
#' @importFrom stats time cycle frequency start end time
pander.ts <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }

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
#' @param ... optional parameters passed to raw \code{pandoc.formula} function
#' @export
pander.formula <- function(x, max.width = 80, caption = attr(x, 'caption'), ...) {
    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }
    pandoc.formula(x, max.width = max.width, caption = caption, ...)
}


#' Pander method for call class
#'
#' Prints a call object in Pandoc's markdown.
#' @param x a call object
#' @param ... optional parameters passed to raw \code{pandoc.formula} function
#' @export
pander.call <- function(x, ...)
    pandoc.verbatim(deparse(x))

#' Pander method for name class
#'
#' Prints a call object in Pandoc's markdown.
#' @param x a name language object
#' @param ... ignored parameters
#' @export
pander.name <- function(x, ...)
    pander(deparse(x))

#' Pander method for coxph class
#'
#' Prints a coxph object in Pandoc's markdown.
#' @param x an coxph object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
#' @importFrom stats pchisq naprint
pander.coxph <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption)) {
            caption <- pandoc.formula.return(x$call$formula, text = 'Fitting Proportional Hazards Regression Model:')
        } else {
            caption <- get.caption()
        }
    }

    if (!is.null(x$fail)) {
        cat('  Coxph failed.', x$fail, '\n')
        return(invisible())
    }

    beta <- x$coef
    se <- sqrt(diag(x$var))

    if (is.null(x$naive.var)) {
        c.tab <- cbind(beta, exp(beta), se, beta / se, 1 - pchisq( (beta / se) ^ 2, 1))
        dimnames(c.tab) <- list(names(beta), c('coef', 'exp(coef)', 'se(coef)', 'z', 'p'))
    } else {
        c.tab <- cbind(beta, exp(beta), se, beta / se, signif(1 - pchisq( (beta / se) ^ 2, 1), 1))
        dimnames(c.tab) <- list(names(beta), c('coef', 'exp(coef)', 'robust se', 'z', 'p'))
    }

    pandoc.table(c.tab, caption = caption, ...)
    logtest <- -2 * (x$loglik[1] - x$loglik[2])

    if (is.null(x$df)) {
        df <- sum(!is.na(beta))
    } else {
        df <- round(sum(x$df), 2)
    }

    cat('Likelihood ratio test=', format(round(logtest, 2)), '  on ',
        df, ' df,', ' p=', format(1 - pchisq(logtest, df)), sep = '')

    omit <- x$na.action
    cat('  n=', x$n)
    if (!is.null(x$nevent)) {
        cat(', number of events=', x$nevent, '\n')
    } else {
        cat('\n')
    }

    if (length(omit)) {
        cat('   (', naprint(omit), ')\n', sep = '')
    }

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
        if (is.null(storage$caption)) {
            caption <- pandoc.formula.return(x$userCall, text = 'Fitting Conditional logistic regression:')
        } else {
            caption <- get.caption()
        }
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

    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }

    c.tab <- as.data.frame(x)
    c.tab <- cbind(pandoc.date.return(trunc(time(x)), simplified = TRUE), c.tab)

    if (length(colnames(x)) != 0) {
        colnames(c.tab) <- c('Period', colnames(x))
    } else {
        colnames(c.tab) <- c('Period', 'Value')
    }

    rownames(c.tab) <- NULL
    pandoc.table(c.tab, caption = caption, ...)

}

#' Pander method for summary.lme class
#'
#' Prints a lme object in Pandoc's markdown.
#' @param x a lme object
#' @param caption caption (string) to be shown under the table
#' @param summary (default:\code{TRUE}) if to print expender summary
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.summary.lme <- function(x, caption = attr(x, 'caption'), summary = TRUE, ...) {
    if (is.null(caption)) {
        if (is.null(storage$caption)) {
            caption <- sprintf('Linear mixed-effects model fit by %s : %s',
                               paste(sub('^[ ]*', '', ifelse(x$method == 'REML', 'REML', 'maximum likelihood'))),
                               pandoc.formula.return(x$call$fixed), collapse = '')
        } else {
            caption <- get.caption()
        }
    }
    res <- as.data.frame(x$tTable)
    if (summary) {
        pandoc.table(res, caption = pandoc.formula.return(x$call$fixed, text = 'Fixed effects: '),
                     split.tables = Inf, ...)
        pandoc.table(x$residuals, caption = 'Standardized Within-Group Residuals')
        pandoc.table(data.frame(
            'Observations'        = x$dims[['N']],
            'Groups'              = x$dims$ngrps[1:x$dims$Q],
            'Log-restricted-likelihood' = x$logLik,
            check.names = FALSE), keep.trailing.zeros = TRUE, caption = caption, digits = 4)
    } else {
        pandoc.table(res, caption = caption, ...)
    }
}

#' Pander method for lme class
#'
#' Prints a lme object in Pandoc's markdown.
#' @param x a lme object
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.lme <- function(x, caption = attr(x, 'caption'), ...)
    pander(summary(x), caption = caption, summary = FALSE, ...)


#' Pander method for describe class
#'
#' Prints a describe object in Pandoc's markdown.
#' @param x an describe object
#' @param caption caption (string) to be shown under the table
#' @param digits number of digits of precision
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.describe <- function(x, caption = attr(x, 'caption'), digits = panderOptions('digits'), ...) {
    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }
    if (length(dim(x)) == 1) {
        class(x) <- 'list'
        attr(x, 'call') <- NULL
        pander(x, digits = digits, caption = caption, ...)
    } else {
        class(x) <- 'data.frame'
        pander(x, digits = digits, caption = caption, ...)
    }
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
        if (is.null(storage$caption)) {
            caption <- pandoc.formula.return(x$call$formula, text = 'Call:')
        } else {
            caption <- get.caption()
        }
    }

    if (length(x$n) == 1) {
        z <- sign(x$exp - x$obs) * sqrt(x$chisq)
        temp <- c(x$obs, x$exp, z, signif(1 - pchisq(x$chisq, 1), panderOptions('digits')))
        names(temp) <- c('Observed', 'Expected', 'Z', 'p')
        temp <- t(temp)
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
        temp <- cbind(x$n, otmp, etmp, (otmp - etmp) ^ 2 / etmp, (otmp - etmp) ^ 2 / diag(x$var))
        dimnames(temp) <- list(names(x$n), c('N', 'Observed', 'Expected', '(O-E)^2/E', '(O-E)^2/V'))
        caption <- paste(caption, sprintf('Chisq = %f \non %d degrees of freedom, p = %f', x$chisq, df, p), sep = ' ')
    }

    temp <- as.data.frame(temp, checknames = FALSE)
    pandoc.table(temp, caption = caption, ...)

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
#' @importFrom utils getFromNamespace
pander.survfit <- function (x, caption = attr(x, 'caption'),
                            scale = 1, print.rmean = getOption('survfit.print.rmean'),
                            rmean = getOption('survfit.rmean'), ...) {

    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }

    omit <- x$na.action

    if (length(omit)){
        pander(list(naprint(omit)), list.type = 'none')
    }

    if (!missing(print.rmean) && is.logical(print.rmean) && missing(rmean)) {
        rmean <- ifelse(print.rmean, 'common', 'none')
    }

    if (is.null(rmean)) {
        if (is.logical(print.rmean)) {
            rmean <- ifelse(print.rmean, 'common', 'none')
        } else {
            rmean <- 'none'
        }
    }

    if (is.numeric(rmean)) {
        if (is.null(x$start.time)) {
            if (rmean < min(x$time)) {
                stop('Truncation point for the mean is < smallest survival')
            }
        } else {
            if (rmean < x$start.time) {
                stop('Truncation point for the mean is < smallest survival')
            }
        }
    } else {
        rmean <- match.arg(rmean, c('none', 'common', 'individual'))
        if (length(rmean) == 0) {
            stop('Invalid value for rmean option')
        }
    }

    temp <- getFromNamespace('survmean', 'survival')(x, scale = scale, rmean)
    pandoc.table(temp$matrix, caption = ...)

    if (rmean != 'none') {
        if (rmean == 'individual') {
            pander('* restricted mean with variable upper limit')
        } else {
            pander(paste('* restricted mean with upper limit = ', format(temp$end.time[1])))
        }
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
        if (is.null(storage$caption)) {
            if (!is.null(x$call)) {
                caption <- pandoc.formula.return(x$call$formula, text = 'Fitting linear model by robust regression:')
            } else {
                caption <- 'Fitting linear model by robust regression'
            }
        } else {
            caption <- get.caption()
        }
    }

    if (x$converged) {
        cat('Converged in', length(x$conv), 'iterations\n')
    } else {
        cat('Ran', length(x$conv), 'iterations without convergence\n')
    }

    coef <- x$coefficients
    pandoc.table(coef, caption = caption, ...)
    nobs <- length(x$residuals)
    rdf <- nobs - length(coef)
    cat('Degrees of freedom:', nobs, 'total;', rdf, 'residual\n\n')

    if (nzchar(mess <- naprint(x$na.action))) {
        cat('  (', mess, ')\n', sep = '')
    }

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

    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }

    if (length(dim(x)) == 2) {
        pandoc.table(t(x), caption = caption, ...)
        return(invisible())
    }

    if (length(dim(x)) == 3) {

        xx <- list()

        for (i in 1:dim(x)[2]) {
            xx[[i]] <- x[, i, ]
        }

        xx <- do.call(rbind, xx)
        xx <- apply(xx, c(1, 2), format, digits = panderOptions('digits'))
        dn <- dimnames(x)
        lgroup <- list(names(dn)[2], dn[[2]])
        tgroup <- names(dn)[3]
        c.s <- length(dn[[3]])
        xx <- rbind(colnames(xx), xx)
        xx <- cbind(unlist(lgroup), xx)
        xx <- rbind(c(rep('', c.s - 1), tgroup, ''), xx)
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
        attached.base.packages <- pander_return(x$basePkgs, quote = FALSE, ...)
        other.attached.packages <- pander_return(mkLabel(x, 'otherPkgs'), quote = FALSE, ...)
        load.via.namespaces <- pander_return(mkLabel(x, 'loadedOnly'), quote = FALSE, ...)
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
#' @param unit units in which values should be printed (for example second, microseconds, etc.). Should be one of ns, us, ms, s, t, hz, khz, mhz, eps, f
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.microbenchmark <- function(x, caption = attr(x, 'caption'), expr.labels, unit, ...) {

    xs <- summary(x, unit = unit)

    if (is.null(caption)) {
        if (is.null(storage$caption)) {
            caption <- paste('Unit: ', attr(xs, 'unit'), sep = '')
        } else {
            caption <- get.caption()
        }
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

    if (syntax.highlighting) {
        cat('```r\n')
    } else {
        ps <- ifelse(verbatim, '\t', '')
    }

    if (!is.null(add.name) && add.name) {
        cat(ps, fname, ' <- ', sep = '')
    }

    for (line in deparse(x)) {
        cat(ps, line, '\n', sep = '')
    }

    if (syntax.highlighting) {
        cat('```')
    }

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
pander.tabular <- function(x, caption = attr(x, 'caption'),
                           emphasize.rownames = TRUE, digits = panderOptions('digits'), ...) {
    if (is.null(caption) & !is.null(storage$caption)) {
        caption <- get.caption()
    }
    data <- as.matrix(x, format = T, rowLabels = F, colLabels = F, digits = digits)
    rlabels <- attr(x, 'rowLabels')
    rlabels[is.na(rlabels)] <- ''
    clabels <- attr(x, 'colLabels')
    clabels[is.na(clabels)] <- ''
    if (!is.null(colnames(rlabels))) {
        ## needed for case of more complex tabular structure (see examples)
        cl <- colnames(rlabels)
        data <- cbind(rlabels, data)
        clabels <- cbind(rbind(matrix('',
                                      nrow = (nrow(clabels) - 1),
                                      ncol = length(cl)),
                               colnames(rlabels)),
                         clabels)
    }
    clabels <- apply(clabels, c(2), paste, collapse = '\\ \n')
    colnames(data) <- clabels
    if (emphasize.rownames) {
        pandoc.table(data, caption = caption, keep.line.breaks = TRUE, emphasize.cols = 1:length(cl), ...)
    } else {
        pandoc.table(data, caption = caption, keep.line.breaks = TRUE, ...)
    }
}

#' Pander method for summary.table class
#'
#' Renders an summary.table object in Pandoc's markdown.
#' @param x an function object
#' @param caption caption (string) to be shown under the table
#' @param print.call (defaut:\code{TRUE}) if call should be printed
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.summary.table <- function(x, caption = attr(x, 'caption'), print.call = T, ...) {
    if (is.null(caption)) {
        if (!is.null(storage$caption)) {
            caption <- get.caption()
        } else {
            caption <- 'Test for independence of all factors'
        }
    }
    if (!is.null(x$call) && print.call) {
        cat('Call: ')
        print(x$call)
    }
    cat('Number of cases in table:', x$n.cases, '\n')
    cat('Number of factors:', x$n.vars, '\n')
    if (x$n.vars > 1) {
        ch <- x$statistic
        tdf <- data.frame('Chisq' = ch, 'df' = x$parameter, 'p-value' = x$p.value)
        pandoc.table(tdf, caption = caption, ...)
        if (!x$approx.ok) {
            cat('Chi-squared approximation may be incorrect\n')
        }
    }
}

#' Pander method for randomForest class
#'
#' Renders an randomForest object in Pandoc's markdown.
#' @param x an randomForest object
#' @param digits number of digits of precision
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.randomForest <- function (x, digits = panderOptions('digits'), ...) {
    cat('\nCall: ', pandoc.formula.return(x$call), '\n')
    cat('Type of random forest: ', x$type, '\n', sep = '')
    cat('Number of trees: ', x$ntree, '\n', sep = '')
    cat('No. of variables tried at each split: ', x$mtry, '\n\n', sep = '')
    if (x$type == 'classification') {
        if (!is.null(x$confusion)) {
            cat('OOB estimate of  error rate: ',
                round(x$err.rate[x$ntree, 'OOB'] * 100, digits = digits), '%\n', sep = '')
            pandoc.table(x$confusion, caption = 'Confusion Matrix', digits = digits, ...)
            if (!is.null(x$test$err.rate)) {
                cat('Test set error rate: ',
                    round(x$test$err.rate[x$ntree, 'Test'] * 100, digits = 2),
                    '%\n',
                    sep = '')
                pandoc.table(x$test$confusion, caption = 'Test Confusion Matrix', digits = digits, ...)
            }
        }
    }
    if (x$type == 'regression') {
        if (!is.null(x$mse)) {
            cat('Mean of squared residuals: ', x$mse[length(x$mse)], '\n', sep = '')
            cat('% Var explained: ', round(100 * x$rsq[length(x$rsq)], digits = 2), '\n', sep = '')
            if (!is.null(x$test$mse)) {
                cat('Test set MSE: ',
                    round(x$test$mse[length(x$test$mse)], digits = digits),
                    '\n', sep = '')
                cat('% Test Var explained: ',
                    round(100 * x$test$rsq[length(x$test$rsq)],
                          digits = 2), '\n', sep = '')
            }
        }
        if (!is.null(x$coefs)) {
            cat('Bias correction applied:\n')
            cat('Intercept: ', x$coefs[1], '\n')
            cat('Slope: ', x$coefs[2], '\n')
        }
    }
}

#' Prints an irts object from tseries package in Pandoc's markdown.
#' @param x an irts object
#' @param caption caption (string) to be shown under the table
#' @param format string passed to format when printing dates (POSIXct or POSIXt)
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.irts <- function(x, caption = attr(x, 'caption'), format = panderOptions('date'), ...) {

    if (is.null(caption) & !is.null(storage$caption)) {
            caption <- get.caption()
    }
    m <- as.matrix(x$value)
    colnames(m) <- NULL
    rownames(m) <- format(x$time, format = format)
    pander(m, caption = caption, ...)
}

#' Prints an summary.manova object from stats package in Pandoc's markdown.
#' @param x an summary.manova object
#' @param add.significance.stars if significance stars should be shown for P value
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.summary.manova <- function (x, caption = attr(x, 'caption'), add.significance.stars = FALSE, ...) {
    if (length(stats <- x$stats)) {
        pander.anova(stats, caption = caption, add.significance.stars = add.significance.stars, ...)
    } else {
        cat('No error degrees of freedom\n\n')
        pander(data.frame(Df = x$Df, row.names = x$row.names), caption = caption, ...)
    }
    invisible(x)
}

#' Pander method for manova class
#'
#' Prints an manova object in Pandoc's markdown.
#' @param x an manovv object
#' @param caption caption (string) to be shown under the table
#' @param add.significance.stars if significance stars should be shown for P value
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.manova <- function(x, caption = attr(x, 'caption'), add.significance.stars = FALSE, ...)
    pander(summary(x), caption = caption, add.significance.stars = add.significance.stars, ...)

#' Pander method for gtable class
#'
#' Renders an gtable object in Pandoc's markdown.
#' @param x an gtable object
#' @param zsort Sort by z values? Default \code{FALSE}
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.gtable <- function(x, zsort = FALSE, ...) {
    if (nrow(x$layout) == 0) {
        return()
    }
    pos <- as.data.frame(format(as.matrix(x$layout[c('t', 'r',
                                                     'b', 'l')])), stringsAsFactors = FALSE)
    grobNames <- vapply(x$grobs, as.character, character(1))
    info <- data.frame(z = x$layout$z, cells = paste('(', pos$t, '-', pos$b, ',', pos$l, '-', pos$r, ')', sep = ''),
                       name = x$layout$name,
                       grob = grobNames)
    if (zsort) {
        info <- info[order(x$layout$z), ]
    }
    pander(info)
}

#' Prints an summary.nls object from stats package in Pandoc's markdown.
#' @param x an summary.nls object
#' @param summary (defaut:\code{TRUE}) if used for summary.lm or lm
#' @param add.significance.stars if significance stars should be shown for P value
#' @param digits number of digits of precision
#' @param show.convergence (defaut:\code{FALSE}) if to print convergence info
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.summary.nls <- function(x, summary = TRUE, add.significance.stars = FALSE,
                               digits = panderOptions('digits'), show.convergence = FALSE, ...) {
    cat('\n')
    pandoc.formula(x$call$formula,
                   text = 'Fitting nonlinear regression model:',
                   add.line.breaks = TRUE)

    if (summary) {
        coef <- x$coefficients
        if (add.significance.stars) {
            coef[, 4] <- pander::add.significance.stars(coef[, 4])
        }
        pander(x$coefficients, caption = 'Parameters', ...)
        if (add.significance.stars) {
            cat('\nSignif. codes:  0 \'***\' 0.001 \'**\' 0.01 \'*\' 0.05 \'.\' 0.1 \' \' 1\n')
        }
        if (!is.null(x$correlation) && NCOL(x$correlation) > 1) {
            pander(x$correlation, caption = 'Correlation of Parameter Estimates', ...)
        }
        cat('\nResidual standard error:',
            format(signif(x$sigma, digits)), 'on', x$df[2L], 'degrees of freedom\n')
    } else {
        pandoc.table(x$coefficients[, 1], caption = 'Parameter Estimates', digits = digits, ...)
        cat('\nresidual sum-of-squares:', format(signif(x$sigma, digits)), '\n')
    }

    if (show.convergence && !is.null(x$convInfo)) {
        if (identical(x$call$algorithm, 'port')) {
            cat('\nAlgorithm \'port\', convergence message: ',
                x$convInfo$stopMessage, '\n', sep = '')
        } else {
            cat('\nNumber of iterations', ifelse(x$convInfo$isConv, 'to convergence:', 'till stop:'),
                x$convInfo$finIter, '\nAchieved convergence tolerance:',
                format(x$convInfo$finTol, digits = digits), '\n')
            if (!x$convInfo$isConv) {
                cat('Reason stopped:', x$convInfo$stopMessage, '\n')
            }
        }
    }
}

#' Prints an nls object from stats package in Pandoc's markdown.
#' @param x an nls object
#' @param show.convergence (defaut:\code{FALSE}) if to print convergence info
#' @param digits number of digits of precision
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.nls <- function(x, digits = panderOptions('digits'), show.convergence = FALSE, ...) {
    pander.summary.nls(summary(x), summary = FALSE, add.significance.stars = FALSE,
                       digits = digits, show.convergence = show.convergence, ...)
}

#' Prints an arima object from stats package in Pandoc's markdown.
#' @param x an arima object
#' @param digits number of digits of precision
#' @param se if to include standard error in coefficients table (default \code{TRUE})
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.Arima <- function(x, digits = panderOptions('digits'), se = TRUE, ...) {
    cat('\nCall:', pandoc.formula.return(x$call), '', sep = '\n')
    cn <- names(x$coef)
    coef <- matrix(x$coef, nrow = 1)
    colnames(coef) <- cn
    if (se && nrow(x$var.coef)) {
        ses <- rep_len(0, length(coef))
        ses[x$mask] <- sqrt(diag(x$var.coef))
        coef <- rbind(coef, s.e. = ses)
    }
    pandoc.table(coef, caption = 'Coefficients', digits = digits, ...)
    cm <- x$call$method
    if (is.null(cm) || cm != 'CSS')
        cat('\nsigma^2 estimated as ', format(x$sigma2, digits = digits),
            ':  log likelihood = ', format(round(x$loglik, 2)),
            ',  aic = ', format(round(x$aic, 2)), '\n', sep = '')
    else cat('\nsigma^2 estimated as ', format(x$sigma2, digits = digits),
             ':  part log likelihood = ', format(round(x$loglik, 2)),
             '\n', sep = '')
    invisible(x)
}

#' Prints an polr object from MASS package in Pandoc's markdown.
#' @param x an polr object
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.polr <- function (x, ...) {
    if (!is.null(cl <- x$call)) {
        cat('\nCall:', pandoc.formula.return(cl), '', sep = '\n')
    }
    if (length(coef(x))) {
        pandoc.table(coef(x), caption = 'Coefficients', ...)
    } else {
        cat('\nNo coefficients\n')
    }
    pandoc.table(x$zeta, caption = 'Intercepts', ...)
    cat('\nResidual Deviance:', format(x$deviance, nsmall = 2L), '\n')
    cat('AIC:', format(x$deviance + 2 * x$edf, nsmall = 2L), '\n')
    if (nzchar(mess <- naprint(x$na.action))) {
        cat('(', mess, ')\n', sep = '')
    }
    if (x$convergence > 0) {
        cat('Warning: did not converge as iteration limit reached\n')
    }
    invisible()
}

#' Prints an ols object from rms package in Pandoc's markdown.
#' @param x an ols object
#' @param long if to print the correlation matrix of parameter estimates. default(\code{FALSE})
#' @param coefs if to the table of model coefficients, standard errors, etc. default(\code{TRUE})
#' @param digits passed to \code{format}. Can be a vector specifying values for each column (has to be the same length as number of columns).
#' @param round passed to \code{round}. Can be a vector specifying values for each column (has to be the same length as number of columns). Values for non-numeric columns will be disregarded.
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
#' @importFrom stats quantile
pander.ols <- function (x, long = FALSE, coefs = TRUE,
                        digits = panderOptions('digits'), round = panderOptions('round'), ...) {
    requireNamespace('rms', quietly = TRUE)
    stats <- x$stats
    pen <- length(x$penalty.matrix) > 0
    resid <- x$residuals
    n <- length(resid)
    p <- length(x$coef) - (names(x$coef)[1] == 'Intercept')
    if (length(stats) == 0) {
        cat('n=', n, '   p=', p, '\n\n', sep = '')
    }
    ndf <- stats['d.f.']
    df <- c(ndf, n - ndf - 1, ndf)
    r2 <- stats['R2']
    sigma <- stats['Sigma']
    rdf <- df[2]
    rsqa <- 1 - (1 - r2) * (n - 1) / rdf
    lrchisq <- stats['Model L.R.']
    ci <- x$clusterInfo
    if (lst <- length(stats)) {
        misc <- rms::reListclean(Obs = stats['n'], sigma = sigma, d.f. = df[2], `Cluster on` = ci$name, Clusters = ci$n) #nolint
        lr <- rms::reListclean(`LR chi2` = lrchisq, d.f. = ndf, `Pr(> chi2)` = 1 - pchisq(lrchisq, ndf)) #nolint
        disc <- rms::reListclean(R2 = r2, `R2 adj` = rsqa, g = stats['g']) #nolint
        sdf <- multitable(list(misc, lr, disc))
        colnames(sdf) <- c('', 'Model Likelihood\nRatio Test', 'Discrimination\nIndexes')
        caption <- pandoc.formula.return(x$call$formula, text = 'Fitting linear model:')
        pandoc.table(sdf, keep.line.breaks = TRUE, caption = caption, ...)
    }
    if (rdf > 5) {
        if (length(dim(resid)) == 2) {
            rq <- apply(t(resid), 1, quantile)
            dimnames(rq) <- list(c('Min', '1Q', 'Median', '3Q',
                                   'Max'), dimnames(resid)[[2]])
        } else {
            rq <- quantile(resid)
            names(rq) <- c('Min', '1Q', 'Median', '3Q', 'Max')
        }
        pandoc.table(rq, caption = 'Residuals', ...)
    }
    else if (rdf > 0) {
        pandoc.table(resid, caption = 'Residuals', ...)
    }
    if (nsingular <- df[3] - df[1]) {
        cat(nsingular, 'coefficients not defined because of singularities', '\n')
    }
    se <- sqrt(diag(x$var))
    if (coefs) {
        obj <- list(coef = x$coefficients, se = se, errordf = rdf)
        U <- coef_mat(obj, coefs = coefs)
        pandoc.table(U, caption = 'Coeficients', ...)
    }
    if (!pen) {
        if (long && p > 0) {
            correl <- diag(1 / se) %*% x$var %*% diag(1 / se)
            dimnames(correl) <- dimnames(x$var)
            ll <- lower.tri(correl)
            correl[ll] <- format(round(correl[ll], digits = round), digits = digits, ...)
            correl[!ll] <- ''
            pandoc.table(correl[-1, - (p + 1), drop = FALSE],
                         caption = 'Correlation of Coefficients',
                         digits = digits,
                         round = round, ...)
        }
    }
    invisible()
}

#' Prints an summary.polr object from MASS package in Pandoc's markdown.
#' @param x an summary.polr object
#' @param digits number of digits of precision passed to format
#' @param round number of rounding digits passed to round
#' @param keep.trailing.zeros to show or remove trailing zeros in numbers on a column basis width
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.summary.polr <- function(x, digits = panderOptions('digits'), round = panderOptions('round'),
                                keep.trailing.zeros = panderOptions('keep.trailing.zeros'), ...) {
    if (!is.null(cl <- x$call)) {
        cat('\nCall:', pandoc.formula.return(cl), '', sep = '\n')
    }
    pc <- x$pc
    if (pc > 0) {
        pander(x$coefficients[seq_len(pc), , drop = FALSE], caption = 'Coeficients', #nolint
               digits = digits, round = round, keep.trailing.zeros = keep.trailing.zeros, ...)
    } else {
        cat('\nNo coefficients\n')
    }
    pander(x$coefficients[ (pc + 1L):nrow(x$coefficients), , drop = FALSE], caption = 'Intercepts', #nolint
          digits = digits, round = round, keep.trailing.zeros = keep.trailing.zeros, ...)
    cat('\nResidual Deviance:', format(x$deviance, nsmall = 2L), '\n\n')
    cat('AIC:', format(x$deviance + 2 * x$edf, nsmall = 2L), '\n\n')
    if (nzchar(mess <- naprint(x$na.action))) {
        cat('(', mess, ')\n\n', sep = '')
    }
    if (!is.null(correl <- x$correlation)) {
        ll <- lower.tri(correl)
        correl <- apply(correl, c(1, 2),
                        p, wrap = '', digits = digits, round = round, keep.trailing.zeros = keep.trailing.zeros)
        correl[!ll] <- ''
        pander(correl[-1L, -ncol(correl)],
               digits = digits, round = round, keep.trailing.zeros = keep.trailing.zeros,
               caption = 'Correlation of Coefficients', ...)
    }
    invisible(x)
}

#' Prints an survreg object from survival package in Pandoc's markdown.
#' @param x an survreg object
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.survreg <- function (x, ...) {
        pander(summary(x), summary = FALSE, ...)
}

#' Prints an survreg object from survival package in Pandoc's markdown.
#' @param x an survreg object
#' @param summary if summary should be printed
#' @param digits number of digits of precision passed to format
#' @param round number of rounding digits passed to round
#' @param keep.trailing.zeros to show or remove trailing zeros in numbers on a column basis width
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.summary.survreg <- function(x, summary = TRUE, digits = panderOptions('digits'),
                                   round = panderOptions('round'),
                                   keep.trailing.zeros = panderOptions('keep.trailing.zeros'), ...) {
    if (!is.null(cl <- x$call)) {
        cat('\nCall:', pandoc.formula.return(cl), '', sep = '\n\n')
    }
    if (!is.null(x$fail)) {
        cat(' Survreg failed.', x$fail, '\n\n')
        return(invisible())
    }
    if (summary) {
        pandoc.table(x$table, caption = 'Model statistics',
                     digits = digits, round = round, keep.trailing.zeros = keep.trailing.zeros, ...)
    } else {
        coef <- x$coef
        if (any(nas <- is.na(coef))) {
            if (is.null(names(coef))) {
                names(coef) <- paste('b', 1:length(coef), sep = '')
            }
            cat('\nCoefficients: (', sum(nas), ' not defined because of singularities)\n',
                sep = '')
        }
        pandoc.table(coef, caption = 'Coefficients',
                     digits = digits, round = round, keep.trailing.zeros = keep.trailing.zeros, ...)

    }
    if (nrow(x$var) == length(coef)) {
        cat('\nScale fixed at', format(x$scale), '\n')
    } else if (length(x$scale) == 1) {
        cat('\nScale=', format(x$scale), '\n')
    } else {
        pandoc.table(x$scale, caption = 'Scale', ...)
    }
    nobs <- length(x$linear)
    chi <- 2 * diff(x$loglik)
    df <- sum(x$df) - x$idf
    pandoc.table(data.frame('Loglik(model)' = x$loglik[2], 'Loglik(intercept only)' = x$loglik[1]), ...)
    if (df > 0) {
        cat('Chisq=', p(chi, wrap = ''), 'on', p(df, wrap = ''),
            'degrees of freedom, p=', p(signif(1 - pchisq(chi, df), 2), wrap = ''), '\n\n')
    } else {
        cat('\n')
    }
    if (summary) {
        if (x$robust) {
            cat('(Loglikelihood assumes independent observations)\n\n')
        }
        cat('Number of Newton-Raphson Iterations:', p(trunc(x$iter), wrap =), '\n\n') #nolint
    }
    omit <- x$na.action
    if (length(omit)) {
        cat('n=', nobs, ' (', naprint(omit), ')\n', sep = '')
    } else {
        cat('n=', nobs, '\n')
    }
    if (summary) {
        if (!is.null(correl <- x$correlation)) {
            p <- dim(correl)[2]
            if (p > 1) {
                ll <- lower.tri(correl)
                correl <- apply(correl, c(1, 2),
                                p, wrap = '', digits = digits, round = round, keep.trailing.zeros = keep.trailing.zeros)
                correl[!ll] <- ''
                pander(correl[-1L, -ncol(correl)],
                       digits = digits, round = round, keep.trailing.zeros = keep.trailing.zeros,
                       caption = 'Correlation of Coefficients', ...)
            }
        }
    }
    invisible()
}

#' Prints an lrm object from rms package in Pandoc's markdown.
#' @param x an lrm object
#' @param coefs if to the table of model coefficients, standard errors, etc. default(\code{TRUE})
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.lrm <- function (x, coefs = TRUE, ...)  {
    requireNamespace('rms', quietly = TRUE)
    ns <- x$non.slopes
    nstrata <- x$nstrata
    if (!length(nstrata)) {
        nstrata <- 1
    }
    pm <- x$penalty.matrix
    penaltyFactor <- NULL
    if (length(pm)) {
        psc <- ifelse(length(pm) == 1, sqrt(pm), sqrt(diag(pm)))
        penalty.scale <- c(rep(0, ns), psc)
        cof <- matrix(x$coef[ - (1:ns)], ncol = 1)
        pandoc.table(as.data.frame(x$penalty, row.names = ''), caption = 'Penalty factors', ...)
        penaltyFactor <- as.vector(t(cof) %*% pm %*% cof)
    }
    est.exp <- 1:ns
    if (length(x$est)) {
        est.exp <- c(est.exp, ns + x$est[x$est + ns <= length(x$coefficients)])
    }
    vv <- diag(x$var)
    cof <- x$coef
    stats <- x$stats
    maxd <- signif(stats['Max Deriv'], 1)
    ci <- x$clusterInfo
    misc <- rms::reListclean(Obs = stats['Obs'],
                     `Sum of weights` = stats['Sum of Weights'],
                     Strata = if (nstrata > 1) nstrata,
                     `Cluster on` = ci$name, Clusters = ci$n,
                     `max |deriv|` = maxd)
    if (length(x$freq) < 4) {
        names(x$freq) <- paste(' ', names(x$freq), sep = '')
        misc <- c(misc[1], x$freq, misc[-1])
    }
    lr <- rms::reListclean(`LR chi2` = stats['Model L.R.'],
                   d.f. = stats['d.f.'],
                   `Pr(> chi2)` = stats['P'],
                   Penalty = penaltyFactor)
    disc <- rms::reListclean(R2 = stats['R2'], g = stats['g'], gr = stats['gr'],
                     gp = stats['gp'], Brier = stats['Brier'])
    discr <- rms::reListclean(C = stats['C'], Dxy = stats['Dxy'], gamma = stats['Gamma'],
                      `tau-a` = stats['Tau-a'])
    sdf <- multitable(list(misc, lr, disc, discr))
    colnames(sdf) <- c('', 'Model Likelihood\nRatio Test',
                       'Discrimination\nIndexes', 'Rank Discrim.\nIndexes')
    caption <- pandoc.formula.return(x$call$formula, text = 'Fitting logistic regression model:')
    pandoc.table(sdf, keep.line.breaks = TRUE, caption, ...)
    if (coefs) {
        obj <- list(coef = cof, se = sqrt(vv), aux = if (length(pm)) penalty.scale, auxname = 'Penalty Scale')
        U <- coef_mat(obj, coefs = coefs)
        pandoc.table(U, caption = 'Coeficients', ...)
    }
    invisible()
}

#' Prints an orm object from rms package in Pandoc's markdown.
#' @param x an orm object
#' @param coefs if to the table of model coefficients, standard errors, etc. default(\code{TRUE})
#' @param intercepts if to print intercepts, by default, intercepts are only printed if there are fewer than 10 of them
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.orm <- function (x, coefs = TRUE, intercepts = x$non.slopes < 10, ...) {
    requireNamespace('rms', quietly = TRUE)
    ns <- x$non.slopes
    cik <- attr(x$coef, 'intercepts')
    if (length(cik) && intercepts) {
        warning('intercepts=TRUE not implemented for fit.mult.impute objects')
        intercepts <- FALSE
    }
    pm <- x$penalty.matrix
    penaltyFactor <- NULL
    if (length(pm)) {
        psc <- ifelse(length(pm) == 1, sqrt(pm), sqrt(diag(pm)))
        penalty.scale <- c(rep(0, ns), psc)
        cof <- matrix(x$coef[ - (1:ns)], ncol = 1)
        pandoc.table(as.data.frame(x$penalty, row.names = ''), caption = 'Penalty factors', ...)
        penaltyFactor <- as.vector(t(cof) %*% pm %*% cof)
    }
    vv <- diag(vcov(x, intercepts = ifelse(intercepts, 'all', 'none')))
    if (!intercepts) {
        if (!length(cik)) {
            nints <- ns
        } else if (length(cik) == 1 && cik == 0) {
            nints <- 0
        } else {
            length(cik)
        }
        ints.to.delete <- vector()
        if (ns != 0 && nints != 0) {
            ints.to.delete <- 1:nints
        }
        vv <- c(rep(NA, nints), vv)
    }
    cof <- x$coef
    stats <- x$stats
    maxd <- signif(stats['Max Deriv'], 1)
    ci <- x$clusterInfo
    misc <- rms::reListclean(Obs = stats['Obs'], `Unique Y` = stats['Unique Y'],
                     `Cluster on` = ci$name, Clusters = ci$n, `Median Y` = stats['Median Y'],
                     `max |deriv|` = maxd)
    if (length(x$freq) < 4) {
        names(x$freq) <- paste(' ', names(x$freq), sep = '')
        misc <- c(misc[1], x$freq, misc[-1])
    }
    lr <- rms::reListclean(`LR chi2` = stats['Model L.R.'], #nolint
                   d.f. = stats['d.f.'],
                   `Pr(> chi2)` = stats['P'],
                   `Score chi2` = stats['Score'],
                   `Pr(> chi2)` = stats['Score P'], Penalty = penaltyFactor)
    disc <- rms::reListclean(R2 = stats['R2'], g = stats['g'], gr = stats['gr'], #nolint
                     `|Pr(Y>=median)-0.5|` = stats['pdm'])
    discr <- rms::reListclean(rho = stats['rho']) #nolint
    sdf <- multitable(list(misc, lr, disc, discr))
    colnames(sdf) <- c('', 'Model Likelihood\nRatio Test',
                       'Discrimination\nIndexes', 'Rank Discrim.\nIndexes')
    caption <- switch(x$family, logistic = 'Logistic (Proportional Odds)',
                      probit = 'Probit', cauchit = 'Cauchy', loglog = '-log-log',
                      cloglog = 'Complementary log-log')
    caption <- paste(caption, 'Ordinal Regression Model')
    caption <- pandoc.formula.return(x$call$formula, text = caption)
    pandoc.table(sdf, keep.line.breaks = TRUE, caption, ...)
    if (coefs) {
        if (!intercepts) {
            j <- -ints.to.delete
            cof <- cof[j]
            vv <- vv[j]
            if (length(pm)) {
                penalty.scale <- penalty.scale[j]
            }
        }
        obj <- list(coef = cof, se = sqrt(vv), aux = if (length(pm)) penalty.scale, auxname = 'Penalty Scale')
        U <- coef_mat(obj, coefs = coefs)
        pandoc.table(U, caption = 'Coeficients', ...)
    }
    invisible()
}

#' Prints an Grm object from rms package in Pandoc's markdown.
#' @param x an Grm object
#' @param coefs if to the table of model coefficients, standard errors, etc. default(\code{TRUE})
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
#' @importFrom stats coef pchisq vcov
#' @rdname pander.glm.rms
pander.Glm <- function (x, coefs = TRUE, ...) {
    requireNamespace('rms', quietly = TRUE)
    cof <- coef(x)
    lr <- x$null.deviance - x$deviance
    dof <- x$rank - (names(cof)[1] == 'Intercept')
    pval <- 1 - pchisq(lr, dof)
    ci <- x$clusterInfo
    misc <- rms::reListclean(Obs = length(x$residuals), `Residual d.f.` = x$df.residual,
                     `Cluster on` = ci$name, Clusters = ci$n, g = x$g)
    lr <- rms::reListclean(`LR chi2` = lr, d.f. = dof, `Pr(> chi2)` = pval) #nolint
    sdf <- multitable(list(misc, lr))
    colnames(sdf) <- c('', 'Model Likelihood\nRatio Test')
    caption <- pandoc.formula.return(x$call$formula, text = 'General Linear Model')
    pandoc.table(sdf, keep.line.breaks = TRUE, caption, ...)
    if (coefs) {
        se <- sqrt(diag(vcov(x)))
        obj <- list(coef = cof, se = se)
        U <- coef_mat(obj, coefs = coefs)
        pandoc.table(U, caption = 'Coeficients', ...)
    }
    invisible()
}

#' Prints an cph object from rms package in Pandoc's markdown.
#' @param x an cph object
#' @param table if to print event frequency statistics. default(\code{TRUE})
#' @param conf.int set to e.g. .95 to print 0.95 confidence intervals on simple hazard ratios (which are usually meaningless as one-unit changes are seldom relevant and most models contain multiple terms per predictor)
#' @param coefs if to the table of model coefficients, standard errors, etc. default(\code{TRUE})
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
#' @importFrom stats qnorm
pander.cph <- function (x, table = TRUE, conf.int = FALSE, coefs = TRUE, ...) {
    requireNamespace('rms', quietly = TRUE)
    if (table && length(x$n) && is.matrix(x$n)) {
       pandoc.table(x$n, caption = 'Status', ...)
    }
    if (length(x$coef)) {
        stats <- x$stats
        ci <- x$clusterInfo
        misc <- rms::reListclean(Obs = stats['Obs'], Events = stats['Events'],
                         `Cluster on` = ci$name, Clusters = ci$n,
                         Center = x$center)
        lr <- rms::reListclean(`LR chi2` = stats['Model L.R.'], d.f. = stats['d.f.'],
                       `Pr(> chi2)` = stats['P'], `Score chi2` = stats['Score'],
                       `Pr(> chi2)` = stats['Score P'])
        disc <- rms::reListclean(R2 = stats['R2'], Dxy = stats['Dxy'],
                         g = stats['g'], gr = stats['gr'])
        sdf <- multitable(list(misc, lr, disc))
        colnames(sdf) <- c('', 'Model Likelihood\nRatio Test',
                           'Discrimination\nIndexes')
        caption <- pandoc.formula.return(x$call$formula, text = 'Cox Proportional Hazards Model')
        pandoc.table(sdf, keep.line.breaks = TRUE, caption, ...)
        beta <- x$coef
        se <- sqrt(diag(x$var))
        if (coefs) {
            se <- sqrt(diag(x$var))
            obj <- list(coef = x$coef, se = se)
            U <- coef_mat(obj, coefs = coefs)
            pandoc.table(U, caption = 'Coeficients', ...)
        }
        if (conf.int) {
            zcrit <- qnorm( (1 + conf.int) / 2 )
            tmp <- cbind(exp(beta), exp(-beta),
                         exp(beta - zcrit * se),
                         exp(beta + zcrit * se))
            dimnames(tmp) <- list(names(beta),
                                  c('exp(coef)', 'exp(-coef)',
                                    paste('lower ', p(conf.int, wrap = ''), sep = ''),
                                    paste('upper ', p(conf.int, wrap = ''), sep = '')))
            pandoc.table(tmp, caption = 'Confidence interval', ...)
        }
    }
    invisible()
}

#' Prints an summary.rms from rms package in Pandoc's markdown.
#' @param x an summary.rms object
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.summary.rms <- function (x, ...) {
    cstats <- dimnames(x)[[1]]
    for (i in 1:7) cstats <- cbind(cstats, signif(x[, i], 5))
    dimnames(cstats) <- list(rep('', nrow(cstats)), c('Factor', dimnames(x)[[2]][1:7]))
    pandoc.table(cstats, ...)
    if ( (A <- attr(x, 'adjust')) != '')
        cat('\nAdjusted to:', A, '\n')
    blab <- switch(attr(x, 'conf.type'),
                   `bootstrap nonparametric percentile` = 'Bootstrap nonparametric percentile confidence intervals',
                   `bootstrap BCa` = 'Bootstrap BCa confidence intervals',
                   `basic bootstrap` = 'Basic bootstrap confidence intervals',
                   '')
    if (blab != '') {
        cat('\n', blab, '\n', sep = '')
    }
    cat('\n')
    invisible()
}

#' Prints an ets object from forecast package in Pandoc's markdown.
#' @param x an ets object
#' @param digits number of digits of precision
#' @param ... optional parameters passed to raw \code{pandoc.table} function
#' @export
pander.ets <- function(x, digits = panderOptions('digits'), ...) {
    cat('\nCall:', pandoc.formula.return(x$call), '', sep = '\n')
    cat('Type of ets: ', x$method, '\n', sep = '')
    lambda <- x$lambda
    isn <- names(x$initstate)
    initstate <- matrix(x$initstate, nrow = 1)
    colnames(initstate) <- isn
    sp <- x$par['alpha']
    if (x$components[2] != 'N'){
        sp <- c(sp, x$par['beta'])
    }
    if (x$components[3] != 'N'){
        sp <- c(sp, x$par['gamma'])
    }
    if (x$components[4] != 'FALSE'){
        sp <- c(sp, x$par['phi'])
    }
    if (!is.null(lambda)){
        cat('\nBox-Cox transformation: lambda =', round(lambda, panderOptions('digits')), '\n')
    }
    pandoc.table(sp, caption = 'Smoothing parameters', digits = digits, ...)
    pandoc.table(initstate, caption = 'Initial states', digits = digits, ...)
    cat('\nsigma^2 estimated as', format(x$sigma2, digits = digits))
    if (!is.null(x$loglik)){
        cat(': log likelihood = ', format(round(x$loglik, 2)))
    }
    if (!is.null(x$aic)){
        cat(', aic = ', format(round(x$aic, 2)), '\n', sep = '')
    }
    invisible(x)
}
