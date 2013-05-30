#' Open file
#'
#' Tries to open a file with operating system's default program.
#' @param f file (with full path)
#' @references This function is a fork of David Hajage's \code{convert} function: \url{https://github.com/eusebe/ascii/blob/master/R/export.r}
#' @export
openFileInOS <- function(f) {

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
            system(paste(shQuote("/usr/bin/xdg-open"), shQuote(f)), wait = FALSE, ignore.stdout = TRUE)
    }

}

#' Converts Pandoc to other format
#'
#' Calling John MacFarlane's great program to convert specified file (see \code{f} parameter below) or character vector {see \code{text} paramater} to other formats like \code{HTML}, \code{pdf}, \code{docx}, \code{odt} etc.
#' @param f Pandoc's markdown format file path. If URL is provided then the generated file's path is \code{tempfile()} but please bear in mind that this way only images with absolute path would shown up in the document.
#' @param text Pandoc's markdown format character vector. Treated as the content of \code{f} file - so the \code{f} parameter is ignored. The generated file's path is \code{tempfile()}.
#' @param format required output format. For all possible values here check out Pandoc homepage: \url{http://johnmacfarlane.net/pandoc/}
#' @param open try to open converted document with operating system's default program
#' @param options optionally passed arguments to Pandoc (instead of \code{pander}'s default)
#' @param footer add footer to document with meta-information
#' @param proc.time optionally passed number in seconds which would be shown in the generated document's footer
#' @param portable.html copy JS/CSS/images files to the HTML file's directory if converting to \code{HTML} without custom \code{options}
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @note This function depends on \code{Pandoc} which should be pre-installed on user's machine. See the \code{INSTALL} file of the package.
#' @return Converted file's path.
#' @export
#' @examples \dontrun{
#' Pandoc.convert(text = c('# Demo', 'with a paragraph'))
#' Pandoc.convert('http://rapporter.github.com/pander/minimal.md')
#' ## Note: the generated HTML is not showing images with relative path from the above file.
#' ## Based on that `pdf`, `docx` etc. formats would not work! If you want to convert an
#' ## online markdown file to other formats with this function, please pre-process the file
#' ## to have absolute paths instead.
#' }
Pandoc.convert <- function(f, text, format = 'html', open = TRUE, options = '', footer = TRUE, proc.time, portable.html = TRUE) {

    ## check for Pandoc
    if (system('pandoc -v', ignore.stdout = TRUE) != 0) {
        if (grepl("w|W", .Platform$OS.type))
            message('You may install Pandoc easily with "install.pandoc()" from the "installr" package.')
        stop("It seems Pandoc is not installed or path of binary is not found. Did you restarted R after Pandoc install? See installation details by running:\n\n\t readLines(system.file('includes/html/footer.html', package='pander'))\n")
    }

    ## dealing with provided character vector
    if (!missing(text)) {
        f <- tempfile()
        cat(text, file = f, sep = '\n')
    }

    ## force UTF-8 encoding
    if (!grepl('utf', Sys.getlocale())) {
        text <- iconv(readLines(f, warn = FALSE), from = '', to = 'UTF-8')
        con <- file(f, 'w', encoding = 'UTF-8')
        cat(text, file = con, sep = '\n')
        close(con)
    }

    ## dealing with URLs
    if (grepl('^https*://.*', f)) {
        f.dir <- tempdir()
        f.out <- paste0(tempfile(), '.', format)
    } else {
        f.dir <- dirname(f)
        f.out <- paste0(f, '.', format)
    }

    ## add nifty HTML/CSS/JS components
    if (format == 'html') {

        if (portable.html & options == '') {
            portable.dirs <- c('fonts', 'images', 'javascripts', 'stylesheets')
            for (portable.dir in portable.dirs)
                file.copy(system.file(sprintf('includes/%s', portable.dir), package='pander'), f.dir, recursive  = TRUE)
        }

        if (options == '')
            options <- sprintf('-H "%s" -A "%s"', system.file('includes/html/header.html', package='pander'), system.file('includes/html/footer.html', package='pander'))

    } else
        if (options == '')
            options <- '--toc'

    ## add other formats' templates
    ## TODO

    ## add footer to file
    if (footer)
        if (!grepl('This report was generated', tail(readLines(f, warn = FALSE), 1)))
            cat(sprintf('\n\n-------\nThis report was generated with [R](http://www.r-project.org/) (%s) and [pander](https://github.com/rapporter/pander) (%s)%son %s platform.', sprintf('%s.%s', R.version$major, R.version$minor), packageDescription("pander")$Version, ifelse(missing(proc.time), ' ', sprintf(' in %s sec ', format(proc.time))), R.version$platform), file = f, append = TRUE)

    ## set specified dir
    wd <- getwd()
    setwd(f.dir)

    ## call Pandoc
    res <- suppressWarnings(tryCatch(system(sprintf('pandoc -f markdown -s %s %s -o %s', options, shQuote(f), shQuote(f.out)), intern = TRUE), error = function(e) e))

    ## revert settings
    setwd(wd)

    ## return
    if (!is.null(attr(res, "status"))) {
        warning(paste0('Pandoc had some problems while converting to ', format, ': \n', res))
    } else {
        if (open)
            openFileInOS(f.out)
        return(f.out)
    }

}
