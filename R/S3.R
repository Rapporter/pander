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
#' @aliases pander pander.return
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
pander <- function(x = NULL, ...)
    UseMethod('pander', x)
#' @export pander.return
pander.return <- function(...)
    capture.output(pander(...))

#' @S3method pander NULL
pander.NULL <- function(x, ...)
    return(invisible(NULL))

#' @S3method pander logical
pander.logical <- function(x, ...)
    cat(as.character(x))

#' @S3method pander image
pander.image <- function(x, caption = attr(x, 'caption'), href = attr(x, 'href'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    res <- pandoc.image.return(as.character(x), caption)

    if (is.null(href))
        cat(res)
    else
        pandoc.link(href, res)

}

#' @S3method pander table
pander.table <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    pandoc.table(x, caption = caption, ...)

}

#' @S3method pander data.frame
pander.data.frame <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    pandoc.table(x, caption = caption, ...)

}

#' @S3method pander matrix
pander.matrix <- function(x, caption = attr(x, 'caption'),  ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    pandoc.table(x, caption = caption, ...)

}

#' @S3method pander cast_df
pander.cast_df<- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption) & !is.null(storage$caption))
        caption <- get.caption()

    pandoc.table(as.data.frame(x), caption = caption, ...)

}

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
pander.lm <- function(x, caption = attr(x, 'caption'), covariate.labels, omit, summary = FALSE, ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- sprintf('Fitting linear model: %s', paste(sub('^[ ]*', '', deparse(x$call$formula)), collapse = ''))
        else
            caption <- get.caption()
    }

    xs  <- summary(x)
    res <- as.data.frame(xs$coeff)

    if (nrow(res) > 1)
        res <- res[c(2:nrow(res), 1), ]

    if (!missing(omit))
        res <- res[!apply(sapply(omit, grepl, row.names(res)), 1, any), ]

    if (!missing(covariate.labels))
        row.names(res)[1:length(covariate.labels)] <- covariate.labels

    if (summary) {
        pandoc.table(res, ...)
        pandoc.table(data.frame(
            'Observations'        = length(xs$residuals),
            'Residual Std. Error' = xs$sigma,
            '$R^2$'               = xs$r.squared,
            'Adjusted $R^2$'      = xs$adj.r.squared,
            check.names = FALSE), keep.trailing.zeros = TRUE, caption = caption, digits = 4)
    } else {

        pandoc.table(res, caption = caption, ...)

    }

}

#' @S3method pander glm
pander.glm <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- sprintf('Fitting generalized (%s) linear model: %s', paste(x$family$family, x$family$link, sep = '/'), paste(sub('^[ ]*', '', deparse(x$call$formula)), collapse = ''))
        else
            caption <- get.caption()
    }

    pandoc.table(summary(x)$coeff, caption = caption, ...)

}

#' @S3method pander aov
pander.aov <- function(x, caption = attr(x, 'caption'), ...) {

    res <- unclass(summary(x))[[1]]

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- 'Analysis of Variance Model'
        else
            caption <- get.caption()
    }

    pandoc.table(res, caption = caption, ...)
}

#' @S3method pander anova
pander.anova <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- strsplit(attr(x, 'heading'), '\n')[[1]][1]
        else
            caption <- get.caption()
    }

    pandoc.table(x, caption = caption, ...)

}

#' @S3method pander htest
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

#' @S3method pander prcomp
pander.prcomp <- function(x, caption = attr(x, 'caption'), ...) {

    if (is.null(caption)) {
        if (is.null(storage$caption))
            caption <- 'Principal Components Analysis'
        else
            caption <- get.caption()
    }

    pandoc.table(x$rotation, caption = caption, ...)
    pandoc.table(summary(x)$importance, ...)
}

#' @S3method pander density
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

#' @S3method pander ftable
pander.ftable <- function(x, ...)
    pandoc.table(x, ...)

#' @S3method pander CrossTable
pander.CrossTable <- function(x, caption = attr(x, 'caption'), ...){
  if (is.null(caption) & !is.null(storage$caption))
    caption <- get.caption()
  to.percent <- function(x, digits = 0){
    paste(round(x * 100, digits), "%",sep="")    
  }
  totals <- x$t
  row.labels <- row.names(totals)
  col.labels <- colnames(totals)
  row.size <- length(row.labels)
  col.size <- length(col.labels)
  row.name <- x$RowData
  col.name <- x$ColData  
  proportion.row <- apply(x$prop.row, c(1,2), to.percent)
  proportion.column <- apply(x$prop.col, c(1,2), to.percent)
  proportion.table <- apply(x$prop.tbl, c(1,2), to.percent)
  row.sum<- x$rs
  col.sum <- x$cs
  table.sum <- x$gt
  zeros <- rep(0, (col.size + 2) * (row.size + 1))
  constructed.table<- matrix(zeros, ncol=(col.size + 2))
  constructed.table <- as.table(constructed.table)
  colnames(constructed.table) <- c("&nbsp;",col.labels,"Total")
  new.row.labels <- vector()
  for (i in 1:row.size){
    constructed.table[i, 1] <- c(new.row.labels, paste(pandoc.strong.return(row.labels[i]), "N", "Row (%)", "Column(%)",sep="\\  \n"))
    for (j in 2:(col.size + 1)){
      constructed.table[i, j] <- paste("&nbsp;",totals[i, j - 1],
                                      proportion.row[i, j - 1],
                                      proportion.column[i, j - 1],
                                      proportion.table[i, j - 1], 
                                      sep="\\ \n")
    }
    constructed.table[i, col.size + 2] <- paste("&nbsp;", row.sum[i],
                                                to.percent(sum(totals[i,]/table.sum)),
                                                sep="\\ \n")
  }
  row.last <- "Total"
  for (i in 2:(col.size + 1))
    row.last[i] <- paste(col.sum[i - 1], to.percent(sum(totals[,i - 1])/table.sum), sep="\\ \n")
  row.last[col.size + 2] <- paste(table.sum, "", sep="\\ \n")
  constructed.table[row.size + 1, ] <- row.last
  row.names(constructed.table) <- new.row.labels
  pandoc.table(constructed.table, caption=caption, keep.line.breaks = TRUE, ...)
}

#' @S3method pander ts
pander.ts <- function(x, caption = attr(x, 'caption'), ...){
  if (is.null(caption) & !is.null(storage$caption))
    caption <- get.caption()
  if (!is.null(ncol(x))) {
    tp.1 <- trunc(time(x))
    tp.2 <- trunc(cycle(x))
    day.abb <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", 
                 "Sat")
    row.names <- switch(frequency(x), tp.1, "Arg2", "Arg3", 
                        paste(tp.1, c("Q1", "Q2", "Q3", "Q4")[tp.2], sep = " "), 
                        "Arg5", "Arg6", paste("Wk.", tp.1, " ", day.abb[tp.2], 
                                              sep = ""), "Arg8", "Arg9", "Arg10", "Arg11", 
                        paste(tp.1, month.abb[tp.2], sep = " "))
    t <- data.frame(x, row.names = row.names)
  } else {
    col.names <- switch(frequency(x), "Value", "Arg2", "Arg3", 
                        c("Q1", "Q2", "Q3", "Q4"), "Arg5", "Arg6", day.abb, 
                        "Arg8", "Arg9", "Arg10", "Arg11", month.abb)
    row.names <- seq(from = start(x)[1], to = end(x)[1])
    t <- data.frame(matrix(c(rep(NA, start(x)[2] - 1), 
                             x, rep(NA, frequency(x) - end(x)[2])), ncol = frequency(x), 
                           byrow = TRUE), row.names = row.names)
    names(t) <- col.names
  }
  pandoc.table(t, caption=caption, ...)
}
