#' Brew in pandoc format
#'
#' This function behaves just like \code{brew} except for the \code{<\%=...\%>} tags, where \code{Pandoc.brew} first translate the R object found between the tags to Pandoc markdown before passing to the \code{cat} function.
#'
#' This parser tries to be smart in some ways:
#'
#' \itemize{
#'      \item a block (R commands between the tags) could return a value in the middle of the block and do something else without any output in the rest (but only one returned value per block!)
#'      \item plots and images are grabbed in the document, rendered to a png file and pander method would result in a Pandoc markdown formatted image link (so the image would be shown/included in the exported document). The images' are put in \code{plots} directory in current \code{getwd()} or to the specified \code{output} file's directory.
#'      \item all warnings/messages and errors are recorded in the blocks and returned in the document as a footnote
#' }
#'
#' Please see my Github page for details (\url{http://daroczig.github.com/pander/#brew-to-pandoc}) and examples (\url{http://daroczig.github.com/pander/#examples}).
#' @param file file path of the brew template. As this is passed to \code{readLines}, \code{file} could be an URL too, but not over SSL (for that latter \code{RCurl} would be needed).
#' @param output (optional) file path of the output file
#' @param convert string: format of required output document (besides Pandoc's markdown). Pandoc is called if set via \code{Pandoc.convert} and the converted document could be also opened automatically (see below).
#' @param open try to open converted document with operating system's default program
#' @param graph.hi.res render high resolution images of plots? Default is \code{FALSE} except for HTML output.
#' @param text character vector (treated as the content of the \code{file}
#' @param envir environment where to \code{brew} the template
#' @note Only one of the input parameters (\code{file} or \code{text}) is to be used at once!
#' @export
#' @return converted file name with full path if \code{convert} is set, none otherwise
#' @references \itemize{
#'      \item Jeffrey Horner (2011). _brew: Templating Framework for Report Generation._ \url{http://CRAN.R-project.org/package=brew}
#'      \item John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' }
#' @examples \dontrun{
#' Pandoc.brew(text = 'string:<%="foobar"%>\nimage:<%=plot(1:10)%>\nerror:<%=mean(no.R.object.like.this)%>')
#'
#' text <- paste('# Header', '', 'What a lovely list:\n<%=as.list(runif(10))%>', 'A wide table:\n<%=mtcars[1:3, ]%>', 'And a nice chart:\n\n<%=plot(1:10)%>', sep = '\n')
#' Pandoc.brew(text = text)
#' Pandoc.brew(text = text, output = tempfile(), convert = 'html')
#' Pandoc.brew(text = text, output = tempfile(), convert = 'pdf')
#'
#' ## package bundled examples
#' Pandoc.brew(system.file('examples/minimal.brew', package='pander'))
#' Pandoc.brew(system.file('examples/minimal.brew', package='pander'), output = tempfile(), convert = 'html')
#' Pandoc.brew(system.file('examples/short-code-long-report.brew', package='pander'))
#' Pandoc.brew(system.file('examples/short-code-long-report.brew', package='pander'), output = tempfile(), convert = 'html')
#' }
#' @importFrom brew brew
Pandoc.brew <- function(file = stdin(), output = stdout(), convert = FALSE, open = TRUE, graph.hi.res = FALSE, text = NULL, envir = new.env()) {

    timer <- proc.time()
    output.stdout <- deparse(substitute(output)) == 'stdout()'

    if (identical(convert, FALSE))
        open <- FALSE
    else
        if (output.stdout)
            stop('A file name should be provided while converting a document.')

    ## in HTML it's cool to have high resolution images too
    if ((missing(graph.hi.res)) & (convert == 'html'))
        graph.hi.res <- TRUE

    if (!output.stdout) {
        basedir    <- dirname(output)
        graph.name <- paste0(basename(output), '-%n')
        graph.dir  <- file.path(basedir, 'plots')
    } else {
        graph.name <- '%t'
        graph.dir  <- 'plots'
    }

    if (is.null(text))
        text <- paste(readLines(file, warn = FALSE), collapse = '\n')
    ## text <- gsub('<%=(.*?)%>','<%%\\1%%>', text) # this idea failed as brew templates are evaluated at the end of the file so loops fails

    ## Pandoc.cat fn
    Pandoc.evals <- function(..., envir = parent.frame(), cache = evals.option('cache')) {
        res <- evals(unlist(...), env = envir, graph.dir = graph.dir, graph.name = graph.name, hi.res = graph.hi.res)
        for (r in res)
            pander(r)
    }

    res <- capture.output(brew(text = text, envir = envir))

    ## remove absolute path from image links
    if (!output.stdout)
        res <- gsub(sprintf(']\\(%s/', basedir), ']\\(', res)

    cat(remove.extra.newlines(paste(res, collapse = '\n')), file = output)

    if (is.character(convert))
        Pandoc.convert(output, format = convert, open = open, proc.time = as.numeric(proc.time() - timer)[3])

}

######################################################################################
## This is a forked/patched version of `brew` package made by Jeffrey Horner (c) 2007.
## Original sources can be found at: http://cran.r-project.org/web/packages/brew/
######################################################################################
## Changes:
###########
##  * `<%= ... %>` tags can multiple expressions
##  * raw results are also returned not just `cat`ed
######################################################################################

BRTEXT <- 1
BRCODE <- 2
BRCOMMENT <- 3
BRCATCODE <- 4
DELIM <- list()
DELIM[[BRTEXT]] <- c("","")
DELIM[[BRCODE]] <- c("<%","%>")
DELIM[[BRCOMMENT]] <- c("<%#","%>")
DELIM[[BRCATCODE]] <- c("<%=","%>")

.bufLen <- 0

`.brew.cached` <- function(envir = parent.frame()) {

    text <- get('text')
    brew.cat <- function(from,to) cat(text[from:to], sep = '', collapse = '')
    assign('.brew.cat',brew.cat, envir = envir)

    code <- get('code')
    ret <- try(eval(code, envir = envir))

    invisible(ret)
}

`brew` <- function(text = NULL, envir = parent.frame(), run = TRUE, parseCode = TRUE) {

    if (is.character(text) && nchar(text[1]) > 0)
        icon <- textConnection(text[1])
    else
        stop('Invalid input.')

    if (!is.environment(envir)){
        warning('envir is not a valid environment')
        return(invisible(NULL))
    }

    state <- BRTEXT
    text <- code <- tpl <- character(.bufLen)
    textLen <- codeLen <- as.integer(0)
    textStart <- as.integer(1)
    line <- ''

    while(TRUE){
        if (!nchar(line)){
            line <- readLines(icon,1)
            if (length(line) != 1) break
            line <- paste(line,"\n",sep='')
        }
        if (state == BRTEXT){

            spl <- strsplit(line,DELIM[[BRCODE]],fixed=TRUE)[[1]]

            # Beginning markup found
            if (length(spl) > 1){

                if (nchar(spl[1])) {
                    text[textLen+1] <- spl[1]
                    textLen <- textLen + 1
                }
                line <- paste(spl[-1],collapse='<%')

                # We know we've found this so far, so go ahead and set up state.
                state <- BRCODE

                # Now let's search for additional markup.
                if (regexpr('^=',spl[2]) > 0){
                    state <- BRCATCODE
                    line <- sub('^=','',line)
                } else if (regexpr('^#',spl[2]) > 0){
                    state <- BRCOMMENT
                }

                if (textStart <= textLen) {
                    code[codeLen+1] <- paste('.brew.cat(',textStart,',',textLen,')',sep='')
                    codeLen <- codeLen + 1
                    textStart <- textLen + 1
                }
            } else {
                text[textLen+1] <- line
                textLen <- textLen + 1
                line <- ''
            }
        } else {
            if (regexpr("%>",line,perl=TRUE) > 0){
                spl <- strsplit(line,"%>",fixed=TRUE)[[1]]
                line <- paste(spl[-1],collapse='%>')

                n <- nchar(spl[1])
                # test  for '-' immediately preceding %> will strip trailing newline from line
                if (n > 0) {
                    if (substr(spl[1],n,n) == '-') {
                        line <- substr(line,1,nchar(line)-1)
                        spl[1] <- substr(spl[1],1,n-1)
                    }
                    text[textLen+1] <- spl[1]
                    textLen <- textLen + 1
                }

                # We've found the end of a brew section, but we only care if the
                # section is a BRCODE or BRCATCODE. We just implicitly drop BRCOMMENT sections
                if (state == BRCODE){
                    code[codeLen+1] <- paste(text[textStart:textLen],collapse='')
                    codeLen <- codeLen + 1
                } else if (state == BRCATCODE){
                    code[codeLen + 1] <- paste("cat(Pandoc.evals(c(", paste(sapply(text[textStart:textLen], deparse), collapse = ","),")))", sep = "")
                    codeLen <- codeLen + 1
                }
                textStart <- textLen + 1
                state <- BRTEXT
            } else if (regexpr("<%",line,perl=TRUE) > 0){
                stop("Oops! Someone forgot to close a tag. We saw: ",DELIM[[state]][1],' and we need ',DELIM[[state]][2])
            } else {
                text[textLen+1] <- line
                textLen <- textLen + 1
                line <- ''
            }
        }
    }
    if (state == BRTEXT){
        if (textStart <= textLen) {
            code[codeLen+1] <- paste('.brew.cat(',textStart,',',textLen,')',sep='')
            codeLen <- codeLen + 1
            textStart <- textLen + 1
        }
    } else {
        stop("Oops! Someone forgot to close a tag. We saw: ",DELIM[[state]][1],' and we need ',DELIM[[state]][2])
    }

    if (run){

        brew.env <- new.env(parent=globalenv())
        assign('text',text,brew.env)
        assign('code',parse(text=code,srcfile=NULL),brew.env)
        brew.cached <- .brew.cached
        environment(brew.cached) <- brew.env
        return(brew.cached(envir=envir))

    } else if (parseCode){

        brew.env <- new.env(parent=globalenv())
        assign('text',text,brew.env)
        assign('code',parse(text=code,srcfile=NULL),brew.env)
        brew.cached <- .brew.cached
        environment(brew.cached) <- brew.env
        invisible(brew.cached)

    } else {

        invisible(list(text=text,code=code))

    }
}
