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
#' @param file file path of the brew template
#' @param output (optional) file path of the output file
#' @param convert string: format of required output document (besides Pandoc's markdown). Pandoc is called if set via \code{Pandoc.convert} and the converted document could be also opened automatically (see below).
#' @param open try to open converted document with operating system's default program
#' @param text character vector (treated as the content of the \code{file}
#' @param envir environment where to \code{brew} the template
#' @note Only one of the input parameters (\code{file} or \code{text}) is to be used at once!
#' @export
#' @return converted file name with full path if \code{convert} is set, none otherwise
#' @references \itemize{
#'      \item Jeffrey Horner (2011). _brew: Templating Framework for Report Generation._ \url{http://CRAN.R-project.org/package=brew}
#'      \item John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' }
#' @examples
#' Pandoc.brew(text = 'string:<%="sfasfas"%>\nimage:<%=plot(1:10)%>\nerror:<%=mean(no.R.object.like.this)%>')
#'
#' text <- paste('# Header', '', '<%=as.list(runif(10))%>', '<%=mtcars[1:3, ]%>', '<%=plot(1:10)%>', sep = '\n')
#' Pandoc.brew(text = text)
#' Pandoc.brew(text = text, output = tempfile(), convert = 'html')
#' Pandoc.brew(text = text, output = tempfile(), convert = 'pdf')
#'
#' ## For a longer example checkout README.brew in this installed package or online: \url{https://github.com/daroczig/pander/blob/master/inst/README.brew}
#' @importFrom brew brew
Pandoc.brew <- function(file = stdin(), output = stdout(), convert = FALSE, open = TRUE, text = NULL, envir = new.env()) {

    timer <- proc.time()
    output.stdout <- deparse(substitute(output)) == 'stdout()'

    if (identical(convert, FALSE))
        open <- FALSE
    else
        if (output.stdout)
            stop('A file name should be provided while converting a document.')

    if (!output.stdout)
        graph.dir <- file.path(dirname(output), 'plots')
    else
        graph.dir <- 'plots'

    if (is.null(text))
        text <- paste(readLines(file, warn = FALSE), collapse = '\n')
    ## text <- gsub('<%=(.*?)%>','<%%\\1%%>', text) # this idea failed as brew templates are evaluated at the end of the file so loops fails

    ## Pandoc.cat fn
    Pandoc.evals <- function(..., envir = parent.frame()) {
        #return(capture.output(str(list(...))))
        src <- list(...)
        r <- evals(src, env = envir, graph.dir = graph.dir)[[1]]
        o <- pander(r$output)
        if (!is.null(r$msg$error))
            o <- paste0(o, ' **ERROR**', pandoc.footnote.return(r$msg$errors))
        if (!is.null(r$msg$warnings))
            o <- paste0(o, ' **WARNING**', pandoc.footnote.return(r$msg$warnings))
        o
    }

    ## patching brew
    brew <- brew::brew
    brew.body <- deparse(body(brew))
    if (trim.spaces(brew.body[156]) != "code[codeLen + 1] <- paste(\"cat(\", paste(text[textStart:textLen],")
        stop('Unsupported brew version :(')
    brew.body[156] <- "code[codeLen + 1] <- paste(\"cat(Pandoc.evals(c(\", paste(sapply(text[textStart:textLen], deparse), collapse = \",\"), "
    brew.body[157] <- "\")))\", sep = \"\")"
    body(brew) <- parse(text = brew.body)
    `.brew.cached` <- brew:::`.brew.cached`
    b <- deparse(body(`.brew.cached`))
    b[16] <- "ret <- Pandoc.evals(sapply(code, deparse), envir = envir)"
    body(`.brew.cached`) <- parse(text = b)

    res <- capture.output(brew(text = text, envir = envir))

    ## remove absolute path from image links
    if (!output.stdout)
        res <- gsub(sprintf(']\\(%s/', dirname(output)), ']\\(', res)

    cat(remove.extra.newlines(paste(res, collapse = '\n')), file = output)

    if (is.character(convert))
        Pandoc.convert(output, format = convert, open = open, proc.time = as.numeric(proc.time() - timer)[3])

}

