#' Brew in pandoc format
#'
#' This function behaves just like \code{brew} except for the \code{<\%=...\%>} tags, where \code{Pandoc.brew} first translate the R object (where \code{length("the R object")} returns \code{1}) found between the tags to Pandoc markdown before passing to \code{cat} function.
#' @param file file path of the brew template
#' @param output (optional) file path of the output file
#' @param text character vector (treated as the content of the \code{file}
#' @note Only one of the input parameters (\code{file} or \code{text}) is to be used at once!
#' @export
#' @references Jeffrey Horner (2011). _brew: Templating Framework for Report Generation._ \url{http://CRAN.R-project.org/package=brew}
#' @examples
#' text <- paste('# Header', '', '<%=as.list(runif(10))%>', '<%=mtcars[1:3, ]%>', '<%=plot(1:10)%>', sep = '\n')
#' Pandoc.brew(text = text)
#' @importFrom brew brew
Pandoc.brew <- function(file = stdin(), output = stdout(), text = NULL, envir = new.env()) {

    if (is.null(text))
        text <- paste(readLines(file, warn = FALSE), collapse = '\n')
    text <- gsub('<%=(.*?)%>','<%%\\1%%>', text)

    res <- capture.output(brew(text = text, tplParser = function(x) {

        x <- gsub('\n', '', x)
        #return(capture.output(str(evals(list(x))[[1]])))
        res <- evals(list(x), env = parent.frame())[[1]]
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
