#' Open file
#'
#' Tries to open a file with operating system's default program.
#' @param f file (with full path)
#' @references This function is a fork of David Hajage's \code{convert} function: \url{https://github.com/eusebe/ascii/blob/master/R/export.r}
#' @export
openFileInOS <- function(f) {

    if (missing(f)) {
        stop('No file to open!')
    }

    f <- path.expand(f)

    if (!file.exists(f)) {
        stop('File not found!')
    }

    if (grepl('w|W', .Platform$OS.type)) {
        ## we are on Windows
        shell.exec(f) #nolint
    } else {
        if (grepl('darwin', version$os)) {
            ## Mac
            system(paste(shQuote('open'), shQuote(f)), wait = FALSE, ignore.stderr = TRUE)
        } else {
            ## Linux
            system(paste(shQuote('/usr/bin/xdg-open'), shQuote(f)), #nolint
                   wait = FALSE,
                   ignore.stdout = TRUE)
        }
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
#' @param portable.html instead of using local files, rather linking JS/CSS files to an online CDN for portability and including base64-encoded images if converting to \code{HTML} without custom \code{options}
#' @param pandoc.binary path to \code{pandoc}'s binary if not found in the path
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @note This function depends on \code{Pandoc} which should be pre-installed on user's machine. See the \code{INSTALL} file of the package.
#' @return Converted file's path.
#' @importFrom tools file_path_sans_ext
#' @importFrom utils packageDescription
#' @export
#' @examples \dontrun{
#' Pandoc.convert(text = c('# Demo', 'with a paragraph'))
#' Pandoc.convert('http://rapporter.github.io/pander/minimal.md')
#' # Note: the generated HTML is not showing images with relative path from the above file.
#' # Based on that `pdf`, `docx` etc. formats would not work! If you want to convert an
#' # online markdown file to other formats with this function, please pre-process the file
#' # to have absolute paths instead.
#' }
Pandoc.convert <- function(f, text, format = 'html', open = TRUE, options = '',
                           footer = FALSE, proc.time, portable.html = TRUE,
                           pandoc.binary = panderOptions('pandoc.binary')) {

    ## check for Pandoc
    if (pandoc.binary == '') {
        if (grepl('w|W', .Platform$OS.type))
            message('You may install Pandoc easily with "install.pandoc()" from the "installr" package.')
        stop("It seems Pandoc is not installed or path of binary is not found. Did you restarted R after Pandoc install? See installation details by running:\n\n\t readLines(system.file('includes/html/footer.html', package='pander'))\n") #nolint
    }
    if (!file.exists(pandoc.binary)) {
        stop(paste('Pandoc binary is not found at provided location:', pandoc.binary))
    }

    ## dealing with provided character vector
    if (!missing(text)) {
        f <- tempfile()
        cat(text, file = f, sep = '\n')
    }

    ## content
    rl <- readLines(f, warn = FALSE)

    ## dealing with URLs
    if (grepl('^https*://.*', f)) {
        f.dir <- tempdir()
        f.out <- paste0(tempfile(), '.', format)
    } else {
        f.dir <- dirname(f)
        f.out <- paste0(file_path_sans_ext(f), '.', format)
    }

    ## force UTF-8 encoding #nolint
    ## if (!grepl('utf', Sys.getlocale())) { #nolint

        ## convert content to UTF-8
        text <- iconv(readLines(f, warn = FALSE), from = '', to = 'UTF-8')

        ## do not touch original input file
        if (!missing(f)) {
            f <- tempfile()
            ## remove tempfile if not needed any more
            on.exit(unlink(f))
        }

        ## write content with UTF-8 encoding
        con <- file(f, 'w', encoding = 'UTF-8')
        cat(text, file = con, sep = '\n')
        close(con)

    ## } #nolint

    ## add nifty HTML/CSS/JS components
    if (format == 'html') {

        if (options == '') {

            options <- sprintf('-A "%s"', system.file('includes/html/footer.html', package = 'pander'))

            if (portable.html) {
                options <- paste('--self-contain', options)
            }

        }

    } else {

        if (options == '' && any(grepl('^#', rl))) {
            options <- '--toc'
        }

    }

    ## add other formats' templates
    ## TODO

    ## add footer to file
    if (footer) {
        if (length(rl) > 0 && !grepl('This report was generated', tail(rl, 1))) {
            cat(sprintf('\n\n-------\nThis report was generated with [R](http://www.r-project.org/) (%s) and [pander](https://github.com/rapporter/pander) (%s)%son %s platform.', sprintf('%s.%s', R.version$major, R.version$minor), packageDescription('pander')$Version, ifelse(missing(proc.time), ' ', sprintf(' in %s sec ', format(proc.time))), R.version$platform), file = f, append = TRUE) #nolint
        }
    }

    ## set specified dir
    wd <- getwd()
    setwd(f.dir)

    ## call Pandoc
    cmd <- sprintf('%s -f markdown -s %s %s -o %s', pandoc.binary, options, shQuote(f), shQuote(f.out))
    if (grepl('w|W', .Platform$OS.type)) {
        res <- suppressWarnings(tryCatch(shell(cmd, intern = TRUE), error = function(e) e)) #nolint
    } else {
        res <- suppressWarnings(tryCatch(system(cmd, intern = TRUE), error = function(e) e))
    }

    ## inject HTML header
    if (format == 'html'
        && grepl(sprintf('^(--self-contain )*-A "%s"$', system.file('includes/html/footer.html', package='pander')), options)) { #nolint

        rl <- readLines(f.out, warn = FALSE)
        he <- grep('</head>', rl)
        ho <- readLines(system.file('includes/html/header.html', package = 'pander'), warn = FALSE)

        if (portable.html) {
            ch <- ho
        } else {
            ch <- gsub('http://cdn.rapporter.net/pander', system.file('includes/', package = 'pander'), ho)
        }

        writeLines(c(rl[1 : (he - 1)], ch, rl[he:length(rl)]), f.out)

    }

    ## revert settings
    setwd(wd)

    ## return
    if (!is.null(attr(res, 'status'))) {
        warning(paste0('Pandoc had some problems while converting to ',
                       format, ': \n\n', paste(res, collapse = '\n'),
                       '\n\nPandoc was called as:\n\n\t', cmd))
    } else {
        if (open) {
            openFileInOS(f.out)
        }
        return(f.out)
    }

}
