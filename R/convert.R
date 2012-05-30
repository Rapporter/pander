#' Open file
#'
#' Tries to open a file with operating system's default program.
#' @param f file (with full path)
#' @references This function is a fork of David Hajage's \code{convert} function: \url{https://github.com/eusebe/ascii/blob/master/R/export.r}
#' @export
open.file.in.OS <- function(f) {

    if (missing(f))
        stop('No file to open!')

    f <- path.expand(f)

    if (!file.exists(f))
        stop('File not found!')

    if (grepl("w|W", .Platform$OS.type)) { # we are on Windows
        shell.exec(f)
    } else {
        if (grepl("darwin", version$os))   # Mac
            system(paste(shQuote("open"), shQuote(f)), wait = FALSE, ignore.stderr = TRUE)
        else                               # Linux-like
            system(paste(shQuote("/usr/bin/xdg-open"), shQuote(f)), wait = FALSE, ignore.stderr = TRUE)
    }

}

#' Converts Pandoc to other format
#'
#' Calling John MacFarlane's great program to convert specified file to other formats like \code{HTML}, \code{pdf}, \code{docx}, \code{odt} etc.
#' @param f Pandoc markdown format file path
#' @param format required output format. For all possible values here check out Pandoc homepage: \url{http://johnmacfarlane.net/pandoc/}
#' @param open try to open converted document with operating system's default program
#' @param options optionally passed arguments to Pandoc (instead of \code{pander}'s default)
#' @param proc.time optionally passed number in seconds which would be shown in the generated document's footer
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @note This function depends on \code{Pandoc} which should be pre-installed on user's machine.
#' @return Converted file's path.
#' @export
Pandoc.convert <- function(f, format = 'html', open = TRUE, options = '', proc.time) {

    ## check for Pandoc
    if (paste(suppressWarnings(tryCatch(system('pandoc -v', intern=T), error=function(x) 'NOPANDOC')), collapse='\n') == 'NOPANDOC')
        stop('It seems Pandoc is not installed :( Aborting.')

    f.out <- paste0(f, '.', format)

    ## add nifty HTML/CSS/JS components
    if (format == 'html') {
        portable.dirs <- c('fonts', 'images', 'javascripts', 'stylesheets')
        for (portable.dir in portable.dirs)
            file.copy(system.file(sprintf('includes/%s', portable.dir), package='rapport'), dirname(f), recursive  = TRUE)
        ## updated custom.js
        file.copy(system.file('includes/javascripts/custom.js', package='pander'), file.path(dirname(f), 'javascripts'), overwrite = TRUE)
        if (options == '')
            options <- sprintf('-H "%s" -A "%s"', system.file('includes/html/header.html', package='pander'), system.file('includes/html/footer.html', package='pander'))
    } else {
        if (options == '')
            options <- '--toc'
    }

    ## add other formats' templates
    ## TODO

    ## add footer to file
    if (!grepl('This report was generated', tail(readLines(f, warn = FALSE), 1)))
        cat(sprintf('\n\n-------\nThis report was generated with [R](http://www.r-project.org/) (%s) and [pander](https://github.com/daroczig/pander) (%s)%son %s platform.', sprintf('%s.%s', R.version$major, R.version$minor), packageDescription("pander")$Version, ifelse(missing(proc.time), ' ', sprintf(' in %s sec ', proc.time)), R.version$platform), file = f, append = TRUE)

    ## call Pandoc
    res <- suppressWarnings(tryCatch(system(sprintf('pandoc -s %s %s -o %s', options, f, f.out), intern=T), error=function(e) e))

    ## open
    if (open)
        open.file.in.OS(f.out)

    if (length(res) == 0)
        return(f.out)
    else
        stop(res)

}
