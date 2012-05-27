#' @export
Pandoc <- setRefClass('Pandoc', fields = list('author' = 'character', 'title' = 'character', 'date' = 'character', 'body' = 'list', 'format' = 'character'))

Pandoc$methods(initialize = function(author = 'Anonymous', title = base::sprintf('%s\'s report', author), date = base::date(), format = 'pdf', ...) {

    .self$author <- author
    .self$title  <- title
    .self$date   <- date
    .self$format <- format
    callSuper(...)

})

Pandoc$methods(add = function(x) .self$body <- c(.self$body, list(x)))
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

        lapply(.self$body, pander)

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
    lapply(.self$body, function(x) cat(paste(capture.output(pander(x)), collapse = '\n'), file = fp, append = TRUE))

    ## convert
    system(sprintf('pandoc %s -o %s', shQuote(fp), shQuote(fe)), intern = TRUE)

    ## return
    cat('\nExported to *', f, '.[md|', format, ']* under ', as.numeric(proc.time() - timer)[3], ' seconds.\n\n', sep = '')

    if (open)
        open.file.in.OS(sprintf('%s.%s', f, .self$format))

    return(invisible(fp))

})

