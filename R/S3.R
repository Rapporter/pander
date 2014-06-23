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
            caption <- pandoc.formula.return(x$call$formula, "Fitting linear model:")
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
            caption <- sprintf('Fitting generalized (%s) linear model: %s', 
                               paste(x$family$family, x$family$link, sep = '/'), 
                               pandoc.formula.return(x$call$formula))
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

    if (is.null(caption))
        if (is.null(storage$caption))
            if (!is.null(attr(x, 'heading')))
                caption <- strsplit(attr(x, 'heading'), '\n')[[1]][1]
    if (is.null(caption))
        caption <- get.caption()

    pandoc.table(x, caption = caption, ...)

}
#' @S3method pander aovlist
pander.aovlist <- function(x, caption = attr(x, 'caption'), ...) {

  y <- summary(x)
  n <- length(y)
  if (n == 1)
    pandoc.table(unclass(y[[1]][[1]]), caption, ...)
  else {
    z <- y[[1]][[1]]
    for (i in 2:n){
        z <- rbind(z, y[[i]][[1]])
    }
    pandoc.table(z, caption, ...)
  }
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

#' @S3method pander POSIXlt
pander.POSIXlt <- function(x, ...)
  pandoc.date(x, ...)

#' @S3method pander POSIXct
pander.POSIXct <- function(x, ...)
  pandoc.date(x, ...)

#' @S3method pander Date
pander.Date <- function(x, ...)
  pandoc.date(x, ...)


#' @S3method pander ftable
pander.ftable <- function(x, ...)
    pandoc.table(x, ...)

#' @S3method pander mtable
pander.mtable <- function(x, caption = attr(x, 'caption'), ...){
  if (is.null(caption) & !is.null(storage$caption))
    caption <- get.caption()
  coefs <- ftable(as.table(x$coefficients), row.vars = rev(x$as.row), 
                  col.vars = rev(x$as.col))
  coefs <- as.data.frame(rbind(coefs, x$summaries))
  col.size <- length(colnames(coefs))
  row.size <- length(dimnames(x$coefficients)[[3]])
  zeros <- rep(0, (col.size) * (row.size))
  temp <- matrix(zeros, ncol=(col.size))
  temp <- as.table(temp)
  for (i in 1:row.size){
    tmp.row <- vector()
    s <- as.vector(rbind(as.vector(as.matrix(coefs[2 *i - 1,])), as.vector(as.matrix(coefs[2*i,]))))
    for (j in 1:col.size)
      tmp.row <- c(tmp.row, paste(s[2*j - 1], s[2 * j], sep="\\ \n"))
    temp[i,] <- tmp.row 
  }
  temp <- rbind(temp, x$summaries)
  rownames(temp) <- c(dimnames(x$coefficients)[[3]], rownames(x$summaries))
  colnames(temp) <- colnames(coefs)
  pandoc.table(temp, caption = caption, keep.line.breaks = TRUE, ...)
}

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

#' @S3method pander formula
pander.formula <- function(x, max.width = 80, caption = attr(x, 'caption')) {
  if (is.null(caption) & !is.null(storage$caption))
    caption <- get.caption()
  pandoc.formula(x, max.width = max.width, caption = caption)
}

#' @S3method pander call
pander.call <- function(x, ...) {
  pander.formula(x, ...)
}

#' @S3method pander coxph
pander.coxph <- function(x, caption = attr(x, 'caption'), ...) {
  if (is.null(caption)) {
    if (is.null(storage$caption))
      caption <- pandoc.formula.return(x$call$formula,"Fitting Proportional Hazards Regression Model:")
    else
      caption <- get.caption()
  }
  cox <- x
  beta <- cox$coef
  se <- sqrt(diag(cox$var))
  if (is.null(cox$naive.var)) {
    c.tab <- cbind(beta, exp(beta), se, beta/se,
                   1 - pchisq((beta/se)^2, 1))
    dimnames(c.tab) <- list(names(beta), c("coef", "exp(coef)",
                                           "se(coef)", "z", "p"))
  } else {
    c.tab <- cbind(beta, exp(beta), se, beta/se,
                   signif(1 - pchisq((beta/se)^2, 1), 1))
    dimnames(c.tab) <- list(names(beta), c("coef", "exp(coef)",
                                           "robust se", "z", "p"))
  }
  pandoc.table(c.tab, caption=caption,...)
  logtest <- -2 * (x$loglik[1] - x$loglik[2])
  if (is.null(x$df)) 
    df <- sum(!is.na(beta))
  else df <- round(sum(x$df), 2)
  cat("\n")
  cat("Likelihood ratio test=", format(round(logtest, 2)), 
      "  on ", df, " df,", " p=", format(1 - pchisq(logtest, 
                                                    df)), sep = "")
  omit <- x$na.action
  cat("  n=", x$n)
  if (!is.null(x$nevent)) 
    cat(", number of events=", x$nevent, "\n")
  else cat("\n")
  if (length(omit)) 
    cat("   (", naprint(omit), ")\n", sep = "")
}

#' @S3method pander clogit
pander.clogit <- function (x, caption = attr(x, 'caption'), ...) 
{
  if (is.null(caption)) {
    if (is.null(storage$caption))
      caption <- pandoc.formula.return(x$userCall, text = "Fitting Conditional logistic regression:")
    else
      caption <- get.caption()
  }
  pander.coxph(x, caption = caption, ...)
}

#' @S3method pander zoo
pander.zoo <- function(x, caption = attr(x, 'caption'), ...) {
  if (is.null(caption) & !is.null(storage$caption))
    caption <- get.caption()
  c.tab <- as.data.frame(x)
  c.tab <- cbind(pandoc.date.return(trunc(time(x)), simplified = TRUE),
                 c.tab)
  if (length(colnames(x)) != 0)
    colnames(c.tab) <- c("Period", colnames(z))
  else
    colnames(c.tab) <- c("Period", "Value")
  rownames(c.tab) <- NULL
  pandoc.table(c.tab, caption = caption, ...)
}

#' @S3method pander lme
pander.lme <- function(x, caption = attr(x, 'caption'), summary = FALSE, ...) {

  if (is.null(caption)) {
    if (is.null(storage$caption))
      caption <- sprintf('Linear mixed-effects model fit by %s : %s',
                         paste(sub('^[ ]*', '', ifelse(x$method == "REML", "REML", "maximum likelihood"))),
                         pandoc.formula.return(x$call$fixed), collapse = '')
    else
      caption <- get.caption()
  }

  xs  <- summary(x)
  res <- as.data.frame(xs$tTable)

  if (summary) {
    pandoc.table(res, caption = pandoc.formula.return(x$call$fixed, text = 'Fixed effects: '),split.tables = Inf,...)
    pandoc.table(xs$residuals, caption="Standardized Within-Group Residuals")
    pandoc.table(data.frame(
      'Observations'        = x$dims[["N"]],
      'Groups'              = x$dims$ngrps[1:x$dims$Q],
      'Log-restricted-likelihood' = x$logLik,
      check.names = FALSE), keep.trailing.zeros = TRUE, caption = caption, digits = 4)
  } else {

    pandoc.table(res, caption = caption, ...)

  }

}

#' @S3method pander describe
pander.describe <- function(x, caption = attr(x, 'caption'), short = TRUE, split.tables = 60, ...) {
  if (is.null(caption) & !is.null(storage$caption))
    caption <- get.caption()
  describe.single <- function(x, caption, short, split.tables, ...){
    if (length(x$units))
      des <- paste(des, " [", x$units, "]", sep = "")
    if (length(x$format))
      des <- paste(des, "  Format:", x$format, sep = "")
    dim.counts <- dim(x$count)
    if (is.null(dim.counts)) {
      counts <- as.character(x$count)
    }
    else {
      counts <- matrix(as.character(x$count), dim.counts[1],
                       dim.counts[2])
    }
    names(counts) <- names(x$count)
    counts <- pandoc.table.return(counts, caption = caption, split.tables = split.tables, ...)
    val <- x$values
    if (length(val)) {
      if (!is.matrix(val)) {
        if (length(val) == 10) {
          if (short) {
            low <- paste("lowest:", paste(val[1:5], collapse = " "))
            hi <- paste("highest:", paste(val[6:10], collapse = " "))
            if (nchar(low) + nchar(hi) + 2 > 80)
              val <- as.list(c(low, hi))
            else val <- as.list(paste(low, hi, sep = ", "))
            val <- pandoc.list.return(val, add.end.of.list = FALSE, ...)
          } else {
            if (is.null(dim(val))) {
              val <- as.character(val)
            }else {
              val <- matrix(as.character(val), dim(val)[1],
                            dim(val)[2])
            }
            names(val) <- names(x$values)
            val <- pandoc.table.return(val, split.tables = split.tables,...)
          }
        } else {
          val <- paste(names(val),
                       ifelse(val > 1, paste(" (",val, ")", sep = ""), ""), sep = "")
          val <- strwrap(val, exdent = 4)
          val <- as.list(sub("(^    )(.*)", "\t\\2", val))
          val <- pandoc.table(val,...)
        }
      } else {
        lev <- dimnames(val)[[2]]
        if (short && (mean(nchar(lev)) > 10 | length(lev) <
                        5)) {
          z <- ""
          len <- 0
          for (i in 1:length(lev)) {
            w <- paste(lev[i], " (", val[1, i], ", ", val[2,
                                                          i], "%)", sep = "")
            if (i == 1)
              z <- w
            else z <- paste(z, w, sep = ", ")
          }
          val <- pander.return(z)
        } else {
          dim.val <- dim(val)
          if (is.null(dim.val)) {
            val <- as.character(val)
          }
          else {
            val <- matrix(as.character(val), dim.val[1],
                          dim.val[2])
          }
          rownames(val) <- rownames(x$values)
          colnames(val) <- colnames(x$values)
          val <- pandoc.table.return(val, split.tables = split.tables,...)
        }
      }
    }
    cat(counts)
    cat(val, "\n")
  }
  at <- attributes(x)
  if (length(at$dimensions)) {
    cat(at$descript, "\n\n", at$dimensions[2], " Variables\t",
        at$dimensions[1], " Observations\n")
    if (length(at$naprint))
      cat("\n", at$naprint, "\n")
    for (z in x) {
      if (length(z) == 0)
        next
      describe.single(z, caption = paste("Variable ", z$descript, sep = ""), short = short, split.tables = split.tables,...)
    }
    if (length(at$missing.vars)) {
      cat("\nVariables with all observations missing:\n\n")
      pander(at$missing.vars, quote = FALSE)
    }
  }
  else
    describe.single(x, caption = caption, short = short, split.tables = split.tables,...)
  invisible()
}

#' @S3method pander survdiff
pander.survdiff <- function(x, caption = attr(x, 'caption'), ...) {
  if (is.null(caption)) {
    if (is.null(storage$caption))
      caption <- pandoc.formula.return(x$call$formula, text="Call:")
    else
      caption <- get.caption()
  }
  if (length(x$n) == 1) {
    z <- sign(x$exp - x$obs) * sqrt(x$chisq)
    temp <- c(x$obs, x$exp, z, signif(1 - pchisq(x$chisq, 
                                                 1), panderOptions("digits")))
    names(temp) <- c("Observed", "Expected", "Z", "p")
    temp <- t(temp)
    include.rownames = FALSE
  }
  else {
    if (is.matrix(x$obs)) {
      otmp <- apply(x$obs, 1, sum)
      etmp <- apply(x$exp, 1, sum)
    }
    else {
      otmp <- x$obs
      etmp <- x$exp
    }
    df <- sum(1 * (etmp > 0)) - 1
    p <- 1 - pchisq(x$chisq, df[!is.na(df)])
    temp <- cbind(x$n, otmp, etmp, ((otmp - etmp)^2)/etmp, 
                  ((otmp - etmp)^2)/diag(x$var))
    dimnames(temp) <- list(names(x$n), c("N", "Observed", 
                                         "Expected", "(O-E)^2/E", "(O-E)^2/V"))
    caption <- paste(caption, sprintf("Chisq = %f \non %d degrees of freedom, p = %f",
                                      x$chisq, 
                                      df, 
                                      p), sep = " ")
  }
  temp <- as.data.frame(temp, checknames = FALSE)
  pandoc.table(temp, caption = caption)
}

#' @S3method pander survfit
pander.survfit <- function (x, caption = attr(x, 'caption'), scale = 1, print.rmean = getOption("survfit.print.rmean"), rmean = getOption("survfit.rmean"), ...) {
  if (is.null(caption) & !is.null(storage$caption))
    caption <- get.caption()
  omit <- x$na.action
  na <- NULL
  if (length(omit)) 
    na <- pander(list(naprint(omit)), list.type = "none")
  if (!missing(print.rmean) && is.logical(print.rmean) && missing(rmean)) {
    if (print.rmean) 
      rmean <- "common"
    else rmean <- "none"
  }
  if (is.null(rmean)) {
    if (is.logical(print.rmean)) {
      if (print.rmean) 
        rmean <- "common"
      else rmean <- "none"
    }else rmean <- "none"
  }
  if (is.numeric(rmean)) {
    if (is.null(x$start.time)) {
      if (rmean < min(x$time)) 
        stop("Truncation point for the mean is < smallest survival")
    } else if (rmean < x$start.time) 
      stop("Truncation point for the mean is < smallest survival")
  } else {
    rmean <- match.arg(rmean, c("none", "common", "individual"))
    if (length(rmean) == 0) 
      stop("Invalid value for rmean option")
  }
  temp <- survival:::survmean(x, scale = scale, rmean)
  mat <- pandoc.table(temp$matrix, caption = ...)
  restrm <- NULL
  if (rmean != "none") {
    if (rmean == "individual") 
      restrm <- pander("* restricted mean with variable upper limit")
    else restrm <- pander(paste("* restricted mean with upper limit = ",
                                    format(temp$end.time[1])))
  }
}

#' @S3method pander smooth.spline
pander.smooth.spline <- function(x, ...) {
  x <- as.list(capture.output(x)[-1:-3])
  pandoc.list(x, add.end.of.list = FALSE)
}

#' @S3method pander rlm
pander.rlm <- function(x, caption = attr(x, 'caption'), ...) {
  if (is.null(caption)) {
    if (is.null(storage$caption))
      if (!is.null(x$call))
        caption <- pandoc.formula.return(x$call$formula, text = "Fitting linear model by robust regression:")
      else
        caption <- 'Fitting linear model by robust regression'
    else
      caption <- get.caption()
  }
  if (x$converged) 
    cat("Converged in", length(x$conv), "iterations\n")
  else 
    cat("Ran", length(x$conv), "iterations without convergence\n")
  coef <- x$coefficients
  pandoc.table(coef, caption = caption, ...)
  nobs <- length(x$residuals)
  rdf <- nobs - length(coef)
  cat("Degrees of freedom:", nobs, "total;", rdf, "residual\n\n")
  if (nzchar(mess <- naprint(x$na.action))) 
    cat("  (", mess, ")\n", sep = "")
  cat("Scale estimate:", format(signif(x$s, 3)), "\n")
}

#' @S3method pander stat.table
pander.stat.table <- function(x, caption = attr(x, 'caption'), ...){
  if (is.null(caption) & !is.null(storage$caption))
    caption <- get.caption()
  if (length(dim(x)) == 2) {
    results <- pandoc.table(t(x), caption = caption, ...)
  }
  if (length(dim(x)) == 3) {
    xx <- list()
    for (i in 1:dim(x)[2]) {
      xx[[i]] <- x[, i, ]
    }
    xx <- do.call(rbind, xx)
    xx <- apply(xx, c(1,2), format, digits=panderOptions("digits"))
    dn <- dimnames(x)
    lgroup <- list(names(dn)[2], dn[[2]])
    tgroup <- names(dn)[3]
    c.s <- length(dn[[3]])
    xx <- rbind(colnames(xx), xx)
    xx <- cbind(unlist(lgroup),xx)
    xx <- rbind(c(rep("", c.s), tgroup), xx)
    colnames(xx) <- NULL
    pandoc.table(xx, caption = caption, emphasize.rows = c(1,2), emphasize.cols = 1,...)
  }
}

#' @S3method pander sessionInfo
pander.sessionInfo <- function (x, locale = TRUE, compact = TRUE, ...) 
{
  mkLabel <- function(L, n) {
    vers <- sapply(L[[n]], function(x) x[["Version"]])
    pkg <- sapply(L[[n]], function(x) x[["Package"]])
    sprintf("%s(v.%s)", pkg, vers)
  }
  cat(pandoc.strong(x$R.version$version.string), "\n\n", sep = "")
  cat(pandoc.strong("Platform:"), x$platform, "\n\n", sep = " ")
  if (locale) {
    cat(pandoc.strong("locale:"))
    cat("\n")
    pander(gsub("[/]","||",strsplit(x$locale, ";", fixed = TRUE)[[1]]), ...)
    cat("\n")
  }
  if (compact){
    attached.base.packages <- pander.return(x$basePkgs, quote = FALSE, ...)
    other.attached.packages <- pander.return(mkLabel(x, "otherPkgs"), quote = FALSE, ...)
    load.via.namespaces <- pander.return(mkLabel(x, "loadedOnly"), quote = FALSE, ...)
  } else {
    attached.base.packages <- pandoc.list.return(x$basePkgs, add.end.of.list = FALSE, ...)
    other.attached.packages <- pandoc.list.return(mkLabel(x, "otherPkgs"), add.end.of.list = FALSE, ...)
    load.via.namespaces <- pandoc.list.return(mkLabel(x, "loadedOnly"), add.end.of.list = FALSE, ...)
  }
  cat("\n")
  cat(pandoc.strong("attached base packages:"),"\n")
  cat(attached.base.packages)
  if (!is.null(x$otherPkgs)) {
    cat("\n\n")
    cat(pandoc.strong("other attached packages:"),"\n")
    cat(other.attached.packages)
  }
  if (!is.null(x$loadedOnly)) {
    cat("\n\n")
    cat(pandoc.strong("loaded via a namespace (and not attached):"),"\n")
    cat(load.via.namespaces)
  }
  invisible(x)
}

