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

    ## patching brew
    brew <- brew::brew
    brew.body <- deparse(body(brew))
    if (trim.spaces(brew.body[156]) != "code[codeLen + 1] <- paste(\"cat(\", paste(text[textStart:textLen],")
        stop('Unsupported brew version :(')
    brew.body[156] <- "code[codeLen + 1] <- paste(\"cat(Pandoc.evals(c(\", paste(sapply(text[textStart:textLen], deparse), collapse = \",\"), "
    brew.body[157] <- "\")))\", sep = \"\")"
    body(brew) <- parse(text = brew.body)
#    `.brew.cached` <- brew:::`.brew.cached`
#    b <- deparse(body(`.brew.cached`))
#    b[16] <- "ret <- Pandoc.evals(sapply(code, deparse), envir = envir, cache = FALSE)"
#    body(`.brew.cached`) <- parse(text = b)

    res <- capture.output(brew(text = text, envir = envir))

    ## remove absolute path from image links
    if (!output.stdout)
        res <- gsub(sprintf(']\\(%s/', basedir), ']\\(', res)

    cat(remove.extra.newlines(paste(res, collapse = '\n')), file = output)

    if (is.character(convert))
        Pandoc.convert(output, format = convert, open = open, proc.time = as.numeric(proc.time() - timer)[3])

}

