#' Brew in pandoc format
#'
#' This function behaves just like \code{brew} except for the \code{<\%=...\%>} tags, where \code{Pandoc.brew} first translate the R object found between the tags to Pandoc's markdown before passing to the \code{cat} function.
#'
#' This parser tries to be smart in some ways:
#'
#' \itemize{
#'      \item a block (R commands between the tags) could return any value at any part of the block and there are no restrictions about the number of returned R objects
#'      \item plots and images are grabbed in the document, rendered to a png file and \code{pander} method would result in a Pandoc's markdown formatted image link (so the image would be shown/included in the exported document). The images are put in \code{plots} directory in current \code{getwd()} or to the specified \code{output} file's directory.
#'      \item all warnings/messages and errors are recorded in the blocks and returned in the document as a footnote
#' }
#'
#' Please see my Github page for details (\url{http://rapporter.github.com/pander/#brew-to-pandoc}) and examples (\url{http://rapporter.github.com/pander/#examples}).
#' @param file file path of the brew template. As this is passed to \code{readLines}, \code{file} could be an URL too, but not over SSL (for that latter \code{RCurl} would be needed).
#' @param output (optional) file path of the output file
#' @param convert string: format of required output document (besides Pandoc's markdown). Pandoc is called if set via \code{Pandoc.convert} and the converted document could be also opened automatically (see below).
#' @param open try to open converted document with operating system's default program
#' @param graph.name character string (default to \code{\%t} when \code{output} is set to \code{stdout} and \code{paste0(basename(output), '-\%n')} otherwise) passed to \code{\link{evals}}.  Besides \code{\link{evals}}'s possible tags \code{\%i} is also available which would be replaced by the chunk number (and optionally an integer which would handle nested \code{brew} calls) and \code{\%I} with the order of the current expression.
#' @param graph.dir character string (default to \code{tempdir()} when \code{output} is set to \code{stdout} and \code{dirname(graph.name)} otherwise) passed to \code{\link{evals}}
#' @param graph.hi.res render high resolution images of plots? Default is \code{FALSE} except for HTML output.
#' @param text character vector (treated as the content of the \code{file}
#' @param envir environment where to \code{brew} the template
#' @param append should append or rather overwrite (default) the \code{output} markdown text file? Please note that this option only affects the markdown file and not the optionally created other formats.
#' @param ... additional parameters passed to \code{\link{Pandoc.convert}}
#' @note Only one of the input parameters (\code{file} or \code{text}) is to be used at once!
#' @export
#' @importFrom utils tail head capture.output
#' @return converted file name with full path if \code{convert} is set, none otherwise
#' @references \itemize{
#'      \item Jeffrey Horner (2011). _brew: Templating Framework for Report Generation._ \url{https://cran.r-project.org/package=brew}
#'      \item John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' }
#' @examples \dontrun{
#' text <- paste('# Header', '',
#'   'What a lovely list:\n<%=as.list(runif(10))%>',
#'   'A wide table:\n<%=mtcars[1:3, ]%>',
#'   'And a nice chart:\n\n<%=plot(1:10)%>', sep = '\n')
#' Pandoc.brew(text = text)
#' Pandoc.brew(text = text, output = tempfile(), convert = 'html')
#' Pandoc.brew(text = text, output = tempfile(), convert = 'pdf')
#'
#' ## pi is awesome
#' Pandoc.brew(text='<%for (i in 1:5) {%>\n Pi has a lot (<%=i%>) of power: <%=pi^i%><%}%>')
#'
#' ## package bundled examples
#' Pandoc.brew(system.file('examples/minimal.brew', package='pander'))
#' Pandoc.brew(system.file('examples/minimal.brew', package='pander'),
#'   output = tempfile(), convert = 'html')
#' Pandoc.brew(system.file('examples/short-code-long-report.brew', package='pander'))
#' Pandoc.brew(system.file('examples/short-code-long-report.brew', package='pander'),
#'   output = tempfile(), convert = 'html')
#'
#' ## brew returning R objects
#' str(Pandoc.brew(text='Pi equals to <%=pi%>.
#' And here are some random data:\n<%=runif(10)%>'))
#'
#' str(Pandoc.brew(text='# Header <%=1%>\nPi is <%=pi%> which is smaller then <%=2%>.
#' foo\nbar\n <%=3%>\n<%=mtcars[1:2,]%>'))
#'
#' str(Pandoc.brew(text='<%for (i in 1:5) {%>
#' Pi has a lot (<%=i%>) of power: <%=pi^i%><%}%>'))
#' }
Pandoc.brew <- function(file = stdin(), output = stdout(), convert = FALSE, open = TRUE,
                        graph.name, graph.dir, graph.hi.res = FALSE, text = NULL,
                        envir = parent.frame(), append = FALSE, ...) {

    timer <- proc.time()
    output.stdout <- deparse(substitute(output)) == 'stdout()'

    if (identical(convert, FALSE)) {
        open <- FALSE
    } else {
        if (output.stdout) {
            stop('A file name should be provided while converting a document.')
        }
    }

    if (!output.stdout) {
        basedir    <- dirname(output)
        if (missing(graph.name)) {
            graph.name <- paste0(basename(output), '-%n')
        }
        if (missing(graph.dir)) {
            graph.dir  <- file.path(basedir, 'plots')
        }
    } else {
        if (missing(graph.name)) {
            graph.name <- '%t'
        }
        if (missing(graph.dir)) {
            graph.dir  <- file.path(tempdir(), 'plots')
        }
    }

    if (is.null(text)) {
        text <- paste(readLines(file, warn = FALSE), collapse = '\n')
    }

    ## id of chunk
    assign('cmdID', 0, envir = debug)
    assign('chunkID', 0, envir = debug)
    assign('nested', debug$nested + 1, envir = debug)
    assign('nestedID', debug$nestedID + 1, envir = debug)

    ## helper fn
    showCode <- function(..., envir = parent.frame(), cache = evalsOptions('cache')) {

        ## increment chunk ID
        assign('chunkID', debug$chunkID + 1, envir = debug)

        ## evaluate chunk
        res <- evals(unlist(...), env = envir, graph.dir = graph.dir, graph.name = graph.name, hi.res = graph.hi.res)

        ## format 'em
        for (r in res) {

            r.pander <- tryCatch(pander_return(r), error = function(e) e)
            if (inherits(r.pander, 'error')) {
                r.pander <- paste0('Internal `pander` error: `',
                                   r.pander$message,
                                   '` while running: `',
                                   r$src,
                                   '`\n\nPlease [report the issue](https://github.com/Rapporter/pander/issues/new) with a reproducible example to help developers fix this ASAP.') #nolint
            }
            r$output <- r.pander
            cat(paste(r.pander, collapse = '\n'))

            localstorage <- get('.storage', envir = envir)
            localstorage.last <- tail(localstorage, 1)[[1]]
            localstorage.last.text <- ifelse(is.null(localstorage.last$text$eval), '', localstorage.last$text$eval)

            if ('image' %in% r$type
                || length(r.pander) > 1
                || grepl('\n$', localstorage.last.text)
                || is.null(localstorage.last$text$eval)) {
                type <- 'block'
            } else {
                type <- 'inline'
            }

            if (type == 'inline') {

                localstorage[[length(localstorage)]]$text <- list(raw = paste0(localstorage.last$text$raw, paste0('<%=', r$src, '%>')), #nolint
                                                                  eval = paste0(localstorage.last$text$eval, r.pander)) #nolint
                localstorage[[length(localstorage)]]$chunks <- list(raw = c(localstorage.last$chunks$raw, paste0('<%=', r$src, '%>')), #nolint
                                                                    eval = c(localstorage.last$chunks$eval, ifelse(length(r.pander) == 0, '', r.pander))) #nolint
                localstorage[[length(localstorage)]]$msg <- list(messages = c(localstorage.last$msg$messages, r$msg$messages), #nolint
                                                                 warnings = c(localstorage.last$msg$warnings, r$msg$warnings), #nolint
                                                                 errors = c(localstorage.last$msg$errors, r$msg$errors)) #nolint

            } else {
                localstorage <- c(localstorage, list(list(type = 'block', robject = r)))
            }

            assign('.storage', localstorage, envir = envir)

        }
    }
    assign('showCode', showCode, envir = envir)

    assign('.storage', NULL, envir = envir)
    res <- capture.output(brew(text = text, envir = envir))

    ## remove absolute path from image links
    if (!output.stdout) {
        res <- gsub(sprintf(']\\(%s/', basedir), ']\\(', res, fixed = TRUE)
    }

    cat(remove.extra.newlines(paste(res, collapse = '\n')), '\n', file = output, append = append)

    if (is.character(convert)) {
        Pandoc.convert(output, format = convert, open = open, proc.time = as.numeric(proc.time() - timer)[3], ...)
    }

    ## there is no sense of chunkID outside of brew
    assign('chunkID', NULL, envir = debug)
    assign('cmdID', NULL, envir = debug)
    assign('nested', debug$nested - 1, envir = debug)
    if (debug$nested == 0) {
        assign('nestedID', 0, envir = debug)
    }

    invisible(get('.storage', envir = envir))

}


######################################################################################
# This is a forked/patched version of `brew` package made by Jeffrey Horner (c) 2007.
# Original sources can be found at: https://cran.r-project.org/package=brew
######################################################################################

BRTEXT <- 1
BRCODE <- 2
BRCOMMENT <- 3
BRCATCODE <- 4
DELIM <- list()
DELIM[[BRTEXT]] <- c('', '')
DELIM[[BRCODE]] <- c('<%', '%>')
DELIM[[BRCOMMENT]] <- c('<%#', '%>')
DELIM[[BRCATCODE]] <- c('<%=', '%>')

#' Patched brew
#'
#' This is a forked/patched version of `brew` package made by Jeffrey Horner (c) 2007. See: \code{References} about the original version.
#'
#' This custom function can do more and also less compared to the original \code{brew} package. First of all the internal caching mechanism (and other, from \code{pander} package POV needless features) of `brew` is removed for some extra profits:
#' \itemize{
#'      \item multiple R expressions can be passed between \code{<\%= ... \%>} tags,
#'      \item the text of the file and also the evaluated R objects are (invisibly) returned in a structured list, which can be really useful while post-processing the results of `brew`.
#' }
#' @param text character vector
#' @param envir environment
#' @return \code{brew}ed document to \code{stdout} and raw results while evaluating the \code{text} in a structured list.
#' @note This function should be never called directly (use \code{brew::brew} instead) as being a helper function of \code{Pandoc.brew}.
#' @seealso \code{\link{Pandoc.brew}}
#' @references Jeffrey Horner (2011). _brew: Templating Framework for Report Generation._ \url{https://cran.r-project.org/package=brew}
#' @keywords internal
`brew` <- function(text = NULL, envir = parent.frame()) {

    if (is.character(text) && nchar(text[1]) > 0) {
        icon <- textConnection(text[1])
    } else {
        stop('Invalid input.')
    }

    if (!is.environment(envir)) {
        stop('Invalid environment')
    }

    state <- BRTEXT
    text <- code <- character(0)
    textLen <- codeLen <- as.integer(0)
    textStart <- as.integer(1)
    line <- ''

    while (TRUE) {
        if (!nchar(line)){
            line <- readLines(icon, 1)
            if (length(line) != 1){
                break
            }
            line <- paste(line, '\n', sep = '')
        }
        if (state == BRTEXT){

            spl <- strsplit(line, DELIM[[BRCODE]], fixed = TRUE)[[1]]

            ## Beginning markup found
            if (length(spl) > 1){

                if (nchar(spl[1])) {
                    text[textLen + 1] <- spl[1]
                    textLen <- textLen + 1
                }
                line <- paste(spl[-1], collapse = '<%')

                ## We know we've found this so far, so go ahead and set up state.
                state <- BRCODE

                ## Now let's search for additional markup.
                if (regexpr('^=', spl[2]) > 0){
                    state <- BRCATCODE
                    line <- sub('^=', '', line)
                } else if (regexpr('^#', spl[2]) > 0){
                    state <- BRCOMMENT
                }

                if (textStart <= textLen) {
                    code[codeLen + 1] <- paste('showText(', textStart, ',', textLen, ')', sep = '')
                    codeLen <- codeLen + 1
                    textStart <- textLen + 1
                }
            } else {
                text[textLen + 1] <- line
                textLen <- textLen + 1
                line <- ''
            }
        } else {
            if (regexpr('%>', line, perl = TRUE) > 0){
                spl <- strsplit(line, '%>', fixed = TRUE)[[1]]
                line <- paste(spl[-1], collapse = '%>')

                n <- nchar(spl[1])
                ## test  for '-' immediately preceding %> will strip trailing newline from line
                if (n > 0) {
                    if (substr(spl[1], n, n) == '-') {
                        line <- substr(line, 1, nchar(line) - 1)
                        spl[1] <- substr(spl[1], 1, n - 1)
                    }
                    text[textLen + 1] <- spl[1]
                    textLen <- textLen + 1
                }

                ## We've found the end of a brew section, but we only care if the
                ## section is a BRCODE or BRCATCODE. We just implicitly drop BRCOMMENT sections
                if (state == BRCODE){
                    code[codeLen + 1] <- paste(text[textStart:textLen], collapse = '')
                    codeLen <- codeLen + 1
                } else if (state == BRCATCODE){
                    code[codeLen + 1] <- paste0('showCode(',
                                                deparse(paste(text[textStart:textLen], collapse = '\n')), ')')
                    codeLen <- codeLen + 1
                }
                textStart <- textLen + 1
                state <- BRTEXT
            } else if (regexpr('<%', line, perl = TRUE) > 0){
                stop('Oops! Someone forgot to close a tag. We saw: ',
                     DELIM[[state]][1], ' and we need ',
                     DELIM[[state]][2])
            } else {
                text[textLen + 1] <- line
                textLen <- textLen + 1
                line <- ''
            }
        }
    }
    if (state == BRTEXT){
        if (textStart <= textLen) {
            code[codeLen + 1] <- paste('showText(', textStart, ',', textLen, ')', sep = '')
            codeLen <- codeLen + 1
            textStart <- textLen + 1
        }
    } else {
        stop('Oops! Someone forgot to close a tag. We saw: ',
             DELIM[[state]][1], ' and we need ',
             DELIM[[state]][2], call. = FALSE)
    }

    showText <- function(from, to) {

        localtexts <- text[from:to]
        for (localtext in localtexts) {

            cat(localtext)

            if (grepl('^#+[ \t]+', localtext)) {
                heading.level <- nchar(gsub('^(#{1,6})[ \t]+.*', '\\1', localtext))
                localtext <- gsub('^#{1,6}[ \t]+', '', localtext)
                type <- 'heading'
            } else {
                type <- 'text'
            }

            localstorage <- get('.storage', envir = envir)
            localstorage.last <- tail(localstorage, 1)[[1]]
            localstorage.last.text <- localstorage.last$text$eval
            localstorage.last.type <- ifelse(is.null(localstorage.last$type), '', localstorage.last$type)

            if (localstorage.last.type == 'block' & type == 'text' & localtext != '\n') {
                localstorage.last.pander  <- localstorage.last$robject$output

                ## we had an inline chunk in the beginning of the line converted to block
                if (!'image' %in% localstorage.last$robject$type && length(localstorage.last.pander) <= 1) {
                    localstorage <- c(localstorage[-length(localstorage)],
                                      list(list(type = 'text',
                                           text = list(
                                               raw  = paste0('<%=', localstorage.last$robject$src,  '%>', localtext),
                                               eval = paste0(localstorage.last.pander, localtext)),
                                           chunks = list(
                                               raw = paste0('<%=', localstorage.last$robject$src,  '%>'),
                                               eval = localstorage.last.pander
                                               ),
                                           msg = list(
                                               messages = localstorage.last$robject$msg$messages,
                                               warnings = localstorage.last$robject$msg$warnings,
                                               errors   = localstorage.last$robject$msg$errors
                                               ))))
                } else {
                ## leave that block as is and add localtext as new
                    localstorage <- c(localstorage,
                                      list(list(type = type,
                                                text = list(raw = localtext, eval = localtext),
                                                chunks = list(raw = NULL, eval = NULL),
                                                msg = list(messages = NULL, warnings = NULL, errors = NULL))))
                }

            } else {

                ## text continues
                if (is.character(localstorage.last.text) &&
                    (type == 'text') &&
                    ifelse(localstorage.last.type == 'heading', !grepl('\n', localstorage.last.text), TRUE)) {
                    localstorage[[length(localstorage)]]$text <- list(raw = paste0(localstorage.last$text$raw, localtext), eval = paste0(localstorage.last.text, localtext)) #nolint
                } else {
                ## new text starts here
                    localstorage <- c(localstorage,
                                      list(list(type = type,
                                                text = list(raw = localtext, eval = localtext),
                                                chunks = list(raw = NULL, eval = NULL),
                                                msg = list(messages = NULL, warnings = NULL, errors = NULL))))
                }
            }

            if (type == 'heading') {
                localstorage[[length(localstorage)]]$level <- heading.level
            }

            assign('.storage', localstorage, envir = envir)

        }

    }

    assign('showText', showText, envir = envir)

    e <- tryCatch(eval(parse(text = code), envir = envir), error = function(e) e)

    if (inherits(e, 'error')) {

        msg <- e$message
        assign('last', list(code = code, text = text, error = msg), envir = debug) # debug

        brcodes <- code[!grepl('^show', code)]
        if (length(brcodes) > 0) {
            brcodes <- p(brcodes, wrap = '`')
            if (grepl('[Uu]nexpected', msg)) {
                stop(paste0('`',
                            sub('.*([Uu]nexpected [a-zA-Z0-9\\(\\)\'\\{\\} ]*)( at character|\n).*', '\\1', msg),
                            '` in your BRCODEs: ', brcodes), call. = FALSE)
            } else {
                stop(sprintf('Error (`%s`) in your BRCODEs: %s', msg, brcodes), call. = FALSE)
            }
        } else {

            stop(paste0('Error: ', p(msg, wrap = '`')), call. = FALSE)
        }
    } else {
        assign('last', list(code = code, text = text, result = e), envir = debug) # debug
    }

    invisible()

}
