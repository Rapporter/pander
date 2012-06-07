.onLoad <- function(libname, pkgname)
{
    ## pander settings
    options('pander' = list(
                'digits'        = 2,
                'decimal.mark'  = '.',
                'round'         = Inf,
                'header.style'  = 'atx',
                'list.style'    = 'bullet',
                'table.style'   = 'multiline'
                ))
}


#' Querying/setting pander option
#'
#' To list all \code{pander} options, just run this function without any parameters provided. To query only one value, pass the first parameter. To set that, use the \code{value} parameter too.
#'
#' The following \code{pander} options are available:
#'
#' \itemize{
#'      \item \code{digits}: numeric (default: \code{2}) passed to \code{format}
#'      \item \code{decimal.mark}: numeric (default: \code{.}) passed to \code{format}
#'      \item \code{round}: numeric (default: \code{Inf}) passed to \code{round}
#'      \item \code{list.style}: \code{'bullet'}, \code{'ordered'} or \code{'roman'}
#'      \item \code{table.style}: \code{'atx'} or \code{'setext'}
#' }
#' @param o option name (string). See below.
#' @param value value to assign (optional)
#' @export
#' @examples
#' pander.option()
#' pander.option('digits')
#' pander.option('digits', 5)
pander.option <- function(o, value) {

    res <- getOption('pander')

    ## just querying
    if (missing(value)) {

        if (missing(o))
            return(res)

        if (o %in% names(res))
            return(res[[o]])

        pandoc.header('Possible pander options:', style = 'setext')
        pandoc.list(names(res))
        stop('Wrong option queried.')

    } else {

        if (!o %in% names(res))
            stop(paste('Invalid option name:', o))

        res[[o]] <- value
        options('pander' = res)

    }

}
