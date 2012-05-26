#' Brew in pandoc format
#' @param file file path of the brew template
#' @param output (optional) file path of the output file
#' @param text character vector (treated as the content of the \code{file}
#' @note Only one of the input parameters (\code{file} or \code{text}) is to be used at once!
#' @export
#' @examples \dontrun{
#' text <- paste('# Header', '', '<%=as.list(runif(10))%>', '<%=mtcars[1:3, ]%>', '<%=plot(1:10)%>', sep = '\n')
#' Pandoc.brew(text = text)
#' }
Pandoc.brew <- function(file = stdin(), output = stdout(), text = NULL) {

    if (is.null(text))
        text <- readLines(file, warn = FALSE)
    text <- gsub('<%=(.*?)%>','<%%\\1%%>', text)

    res <- capture.output(brew(text = text, tplParser = function(x) {

        res <- evals(x)[[1]]
        res <- pander.return(res$output, caption = res$msg$messages)
        paste(res, collapse = '\n')

     }))

    cat(remove.extra.newlines(paste(res, collapse = '\n')), file = output)
}
