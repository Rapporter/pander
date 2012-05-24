#' @export
report <- setRefClass('report', fields = list('author' = 'character', 'title' = 'character', 'date' = 'character', 'body' = 'list'))

report$methods(initialize = function(author = 'Anonymous', title = base::sprintf('%s\'s report', author), date = base::date(), ...) {

    .self$author <- author
    .self$title <- title
    .self$date   <- date
    callSuper(...)

})

report$methods(add = function(x) .self$body <- c(.self$body, list(x)))
report$methods(add.paragraph = function(x) .self$body <- c(.self$body, list(pandoc.p.return(x))))

report$methods(show = function(x) {

    ## starting timer for proc.time
    timer  <- proc.time()

    ## show header
    cat(pandoc.header(.self$title, style = 'setext'))
    cat(' written by *', .self$author, '* at ', .self$date, '\n', sep = '')
    cat('\n', sprintf(' This report holds %s block(s).', length(.self$body)), '\n')

    ## show body (if not empty)
    if (length(.self$body) > 0) {

        cat(pandoc.horizontal.rule())

        lapply(.self$body, Pandoc)

        ## show proc.tim
        cat(pandoc.horizontal.rule())
        cat('\nProc. time: ', as.numeric(proc.time() - timer)[3], 'seconds. \n\n')

    } else
        cat('\n)')

})
