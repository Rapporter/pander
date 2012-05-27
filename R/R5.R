#' Reporting with Pandoc
#'
#' This \code{R5} reference class can hold bunch of elements (text or R objects) from which it tries to create a Pandoc-style markdown textfile. Exporting the report to several formats (like: pdf, docx, odt etc. - see Pandoc's documentation) is also possible, see examples below.
#' @export
#' @examples
#' ## Initialize a new Pandoc object
#' myReport <- Pandoc$new()
#'
#' ## Add author, title and date of document
#' myReport$author <- 'Gergely Daróczi'
#' myReport$title  <- 'Demo'
#'
#' ## Or it could be done while initializing
#' myReport <- Pandoc$new('Gergely Daróczi', 'Demo')
#'
#' ## Add some free text
#' myReport$add.paragraph('Hello there, this is a really short tutorial!')
#'
#' ## Add maybe a header for later stuff
#' myReport$add.paragraph('# Showing some raw R objects below')
#'
#' ## Adding a short matrix
#' myReport$add(matrix(5,5,5))
#'
#' ## Or a table with even # TODO: caption
#' myReport$add.paragraph('Hello table:')
#' myReport$add(table(mtcars$am, mtcars$gear))
#'
#' ## Or a "large" dataframe which barely fits on a page
#' myReport$add(mtcars)
#'
#' ## And a simple linear model with Anova tables
#' ml <- with(lm(mpg ~ hp + wt), data = mtcars)
#' myReport$add(ml)
#' myReport$add(anova(ml))
#' myReport$add(aov(ml))
#'
#' ## And do some principal component analysis at last
#' myReport$add(prcomp(USArrests))
#'
#' ## Sorry, I did not show how Pandoc deals with plots:
#' myReport$add(plot(1:10)) # TODO: caption
#'
#' ## Want to see the report? Just print it:
#' myReport
#'
#' ## Exporting to pdf (default)
#' myReport$export()
#'
#' ## Or to docx in tempdir():
#' myReport$format <- 'docx'
#' myReport$export(tempfile())
#'
#' ## You do not want to see the generated report after generation?
#' myReport$export(open = FALSE)
#' @importFrom rapport evals
Pandoc <- setRefClass('Pandoc', fields = list('author' = 'character', 'title' = 'character', 'date' = 'character', 'body' = 'list', 'format' = 'character'))

Pandoc$methods(initialize = function(author = 'Anonymous', title = base::sprintf('%s\'s report', author), date = base::date(), format = 'pdf', ...) {

    .self$author <- author
    .self$title  <- title
    .self$date   <- date
    .self$format <- format
    callSuper(...)

})

Pandoc$methods(add = function(x) .self$body <- c(.self$body, evals(deparse(match.call()[[2]]))))
Pandoc$methods(add.paragraph = function(x) .self$body <- c(.self$body, list(pandoc.p.return(x))))

Pandoc$methods(show = function(x) {

    ## starting timer for proc.time
    timer  <- proc.time()

    ## show header
    cat(pandoc.header(.self$title, style = 'setext'))
    cat(' written by *', .self$author, '* at *', .self$date, '*\n', sep = '')
    cat('\n', sprintf(' This report holds %s block(s).', length(.self$body)), '\n')

    ## show body (if not empty)
    if (length(.self$body) > 0) {

        cat(pandoc.horizontal.rule())

        lapply(.self$body, function(x) pander(x$output))

        ## show proc.tim)
        cat(pandoc.horizontal.rule())
        cat('\nProc. time: ', as.numeric(proc.time() - timer)[3], 'seconds. \n\n')

    } else
        cat('\n)')

})

Pandoc$methods(export = function(f, open = TRUE) {

    if (missing(f))
        f <- tempfile('pander-', getwd())
    fp    <- sprintf('%s.md', f)
    fe    <- sprintf('%s.%s', f, .self$format)
    timer <- proc.time()

    ## create pandoc file
    cat(pandoc.title.return(.self$title, .self$author, .self$date), file = fp)
    lapply(.self$body, function(x) cat(paste(capture.output(pander(x$output)), collapse = '\n'), file = fp, append = TRUE))

    ## convert
    system(sprintf('pandoc %s -o %s', shQuote(fp), shQuote(fe)), intern = TRUE)

    ## return
    cat('\nExported to *', f, '.[md|', format, ']* under ', as.numeric(proc.time() - timer)[3], ' seconds.\n\n', sep = '')

    if (open)
        open.file.in.OS(sprintf('%s.%s', f, .self$format))

    return(invisible(fp))

})

