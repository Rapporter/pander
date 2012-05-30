#' Brew in pandoc format
#'
#' This function behaves just like \code{brew} except for the \code{<\%=...\%>} tags, where \code{Pandoc.brew} first translate the R object found between the tags to Pandoc markdown before passing to the \code{cat} function.
#'
#' This parser tries to be smart in some ways:
#'
#' \itemize{
#'      \item a block (R commands between the tags) could return a value in the middle of the block and do something else without any output in the rest (but only one returned value per block!)
#'      \item plots and images are grabbed in the document, rendered to a png file and pander method would result in a Pandoc markdown formatted image link (so the image would be shown/included in the exported document). The images' are put in \code{images} directory in current \code{getwd()} or to the specified \code{output} file's directory.
#'      \item all warnings/messages and errors are recorded in the blocks and returned in the document as a footnote
#' }
#' @param file file path of the brew template
#' @param output (optional) file path of the output file
#' @param text character vector (treated as the content of the \code{file}
#' @param envir environment where to \code{brew} the template
#' @note Only one of the input parameters (\code{file} or \code{text}) is to be used at once!
#' @export
#' @references \itemize{
#'      \item Jeffrey Horner (2011). _brew: Templating Framework for Report Generation._ \url{http://CRAN.R-project.org/package=brew}
#'      \item John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' }
#' @examples
#' text <- paste('# Header', '', '<%=as.list(runif(10))%>', '<%=mtcars[1:3, ]%>', '<%=plot(1:10)%>', sep = '\n')
#' Pandoc.brew(text = text)
#'
#' ## For a longer example checkout README.brew in this installed package or online: \url{https://github.com/daroczig/pander/blob/master/inst/README.brew}
#' @importFrom brew brew
Pandoc.brew <- function(file = stdin(), output = stdout(), text = NULL, envir = new.env()) {

    if (deparse(substitute(output)) != 'stdout()')
        graph.dir <- file.path(dirname(output), 'images')
    else
        graph.dir <- 'images'

    if (is.null(text))
        text <- paste(readLines(file, warn = FALSE), collapse = '\n')
    text <- gsub('<%=(.*?)%>','<%%\\1%%>', text)

    res <- capture.output(brew(text = text, tplParser = function(x) {

        x <- gsub('\n', '', x)
        res <- evals(list(x), env = parent.frame(), graph.dir = graph.dir)[[1]]

        o   <- pander.return(res$output, caption = res$msg$messages)
        if (length(o) == 0)
            o <- res$stdout

        o <- paste(o, collapse = '\n')

        if (!is.null(res$msg$errors))
            o <- paste0(o, ' **ERROR**', pandoc.footnote.return(res$msg$errors))
        if (!is.null(res$msg$warnings))
            o <- paste0(o, ' **WARNING**', pandoc.footnote.return(res$msg$warnings))

        o

     }, envir = envir))

    cat(remove.extra.newlines(paste(res, collapse = '\n')), file = output)

}
