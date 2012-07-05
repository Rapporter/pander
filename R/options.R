.onLoad <- function(libname, pkgname)
{
    ## pander settings
    options('pander' = list(
                'digits'                = 4,
                'decimal.mark'          = '.',
                'round'                 = Inf,
                'date'                  = '%Y/%m/%d %X',
                'header.style'          = 'atx',
                'list.style'            = 'bullet',
                'table.style'           = 'multiline',
                'table.split.table'     = 80,
                'table.split.cells'     = 30,
                'evals.messages'        = TRUE,
                'p.wrap'                = '_',
                'p.sep'                 = ', ',
                'p.copula'              = 'and'
                ))

    ## evals options
    options('evals' = list(
                'parse'                 = TRUE,
                'cache'                 = TRUE,
                'cache.mode'            = 'environment',
                'cache.dir'             = '.cache',
                'cache.time'            = 0.1,
                'cache.copy.images'     = FALSE,
                'classes'               = NULL,
                'hooks'                 = NULL,
                'lenght'                = Inf,
                'output'                = 'all',
                'env'                   = NULL,
                'graph.nomargin'        = FALSE,
                'graph.name'            = '%t',
                'graph.dir'             = 'plots',
                'graph.output'          = 'png',
                'width'                 = 480,
                'height'                = 480,
                'res'                   = 72,
                'hi.res'                = FALSE,
                'hi.res.width'          = 960,
                'graph.env'             = FALSE,
                'graph.recordplot'      = FALSE
                ))
}

## general (temporary) storage for pander's stuff
storage <- new.env()

## cache storage
cached.results <- new.env()

## cache hash storage
hash.cache.obj       <- new.env() # raw R objects of which hash was computed before
hash.cache.hash      <- new.env() # the computed hash of the above R objects
hash.cache.last.used <- new.env() # when was the hash last queried

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
#'      \item \code{date}: string (default: \code{'\%Y/\%m/\%d \%X'}) passed to \code{format} when printing dates (\code{POSIXct} or \code{POSIXt})
#'      \item \code{list.style}: \code{'bullet'}, \code{'ordered'} or \code{'roman'} passed to \code{\link{pandoc.list}}. Please not that this has no effect on \code{pander} methods.
#'      \item \code{table.style}: \code{'atx'} or \code{'setext'} passed to \code{\link{pandoc.table}} and also affects \code{pander} methods.
#'      \item \code{table.split.table}: numeric passed to \code{\link{pandoc.table}} and also affects \code{pander} methods. This option tells \code{pander} where to split too wide tables. The default value (\code{80}) suggests the conventional number of characters used in a line, feel free to change (e.g. to \code{Inf} to disable this feature) if you are not using a VT100 terminal any more :)
#'      \item \code{table.split.cells}: numeric (default: \code{30}) passed to \code{\link{pandoc.table}} and also affects \code{pander} methods. This option tells \code{pander} where to split too wide cells with line breaks. Set \code{Inf} to disable.
#'      \item \code{evals.messages}:
#' }
#' @param o option name (string). See below.
#' @param value value to assign (optional)
#' @export
#' @seealso \code{\link{evals.option}}
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

        pandoc.header('Possible `pander` options:', style = 'setext')
        pandoc.list(names(res))
        stop('Wrong option queried.')

    } else {

        if (!o %in% names(res))
            stop(paste('Invalid option name:', o))

        res[[o]] <- value
        options('pander' = res)

    }

}


#' Querying/setting evals option
#'
#' To list all \code{evals} options, just run this function without any parameters provided. To query only one value, pass the first parameter. To set that, use the \code{value} parameter too.
#'
#' The following \code{evals} options are available:
#'
#' \itemize{
#'      \item \code{parse}: if \code{TRUE} the provided \code{txt} elements would be merged into one string and parsed to logical chunks. This is useful if you would want to get separate results of your code parts - not just the last returned value, but you are passing the whole script in one string. To manually lock lines to each other (e.g. calling a \code{plot} and on next line adding an \code{abline} or \code{text} to it), use a plus char (\code{+}) at the beginning of each line which should be evaluated with the previous one(s). If set to \code{FALSE}, \code{evals} would not try to parse R code, it would get evaluated in separate runs - as provided. Please see examples of \code{\link{evals}}.
#'      \item \code{cache}: caching the result of R calls if set to \code{TRUE}
#'      \item \code{cache.mode}: cached results could be stored in an \code{environment} in \emph{current} R session or let it be permanent on \code{disk}.
#'      \item \code{cache.dir}: path to a directory holding cache files if \code{cache.mode} set to \code{disk}. Default to \code{.cache} in current working directory.
#'      \item \code{cache.time}: number of seconds to limit caching based on \code{proc.time}. If set to \code{0}, all R commands, if set to \code{Inf}, none is cached (despite the \code{cache} parameter).
#'      \item \code{cache.copy.images}: copy images to new files if an image is returned from cache? If set to \code{FALSE} (default) the "old" path would be returned.
#'      \item \code{classes}: a vector or list of classes which should be returned. If set to \code{NULL} (by default) all R objects will be returned.
#'      \item \code{hooks}: list of hooks to be run for given classes in the form of \code{list(class = fn)}. If you would also specify some parameters of the function, a list should be provided in the form of \code{list(fn, param1, param2=NULL)} etc. So the hooks would become \code{list(class1=list(fn, param1, param2=NULL), ...)}. See example below. A default hook can be specified too by setting the class to \code{'default'}. This can be handy if you do not want to define separate methods/functions to each possible class, but automatically apply the default hook to all classes not mentioned in the list. You may also specify only one element in the list like: \code{hooks=list('default' = pander.return)}. Please note, that nor error/warning messages, nor stdout is captured (so: updated) while running hooks!
#'      \item \code{length}: any R object exceeding the specified length will not be returned. The default value (\code{Inf}) does not filter out any R objects.
#'      \item \code{output}: a character vector of required returned values. This might be useful if you are only interested in the \code{result}, and do not want to save/see e.g. \code{messages} or \code{print}ed \code{output}. See examples of \code{\link{evals}}.
#'      \item \code{env}: environment where evaluation takes place. If not set (by default), a new temporary environment is created.
#'      \item \code{graph.nomargin}: should \code{\link{evals}} try to keep plots' margins minimal?
#'      \item \code{graph.name}: set the file name of saved plots which is \code{\link{tempfile}} by default. A simple character string might be provided where \code{\%d} would be replaced by the index of the generating \code{txt} source, \code{\%n} with an incremented integer in \code{graph.dir} with similar file names and \code{\%t} by some random characters. A function's name to be \code{eval}uated can be passed here too.
#'      \item \code{graph.dir}: path to a directory where to place generated images. If the directory does not exist, \code{\link{evals}} try to create that. Default set to \code{plots} in current working directory.
#'      \item \code{graph.output}: set the required file format of saved plots. Currently it could be any of  \code{grDevices}: \code{png}, \code{bmp}, \code{jpeg}, \code{jpg}, \code{tiff}, \code{svg} or \code{pdf}.
#'      \item \code{width}: width of generated plot in pixels for even vector formats
#'      \item \code{height}: height of generated plot in pixels for even vector formats
#'      \item \code{res}: nominal resolution in \code{ppi}. The height and width of vector images will be calculated based in this.
#'      \item \code{hi.res}: generate high resolution plots also? If set to \code{TRUE}, each R code parts resulting an image would be run twice.
#'      \item \code{hi.res.width}: width of generated high resolution plot in pixels for even vector formats. The \code{height} and \code{res} of high resolution image is automatically computed based on the above options to preserve original plot aspect ratio.
#'      \item \code{graph.env}: save the environments in which plots were generated to distinct files (based on \code{graph.name}) with \code{env} extension?
#'      \item \code{graph.recordplot}: save the plot via \code{recordPlot} to distinct files (based on \code{graph.name}) with \code{recodplot} extension?
#' }
#' @param o option name (string). See below.
#' @param value value to assign (optional)
#' @export
#' @seealso \code{\link{evals}} \code{\link{pander.option}}
#' @examples
#' evals.option()
#' evals.option('cache')
#' evals.option('cache', FALSE)
evals.option <- function(o, value) {

    res <- getOption('evals')

    ## just querying
    if (missing(value)) {

        if (missing(o))
            return(res)

        if (o %in% names(res))
            return(res[[o]])

        pandoc.header('Possible `evals` options:', style = 'setext')
        pandoc.list(names(res))
        stop('Wrong option queried.')

    } else {

        if (!o %in% names(res))
            stop(paste('Invalid option name:', o))

        res[[o]] <- value
        options('evals' = res)

    }

}
