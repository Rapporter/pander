.onLoad <- function(libname, pkgname) {
    ## pander settings
    options('pander' = list(
                'digits'                   = 4,
                'decimal.mark'             = '.',
                'formula.caption.prefix'   = 'Formula: ',
                'big.mark'                 = '',
                'round'                    = Inf,
                'keep.trailing.zeros'      = FALSE,
                'keep.line.breaks'         = FALSE,
                'missing'                  = NA,
                'date'                     = '%Y/%m/%d %X',
                'header.style'             = 'atx',
                'list.style'               = 'bullet',
                'table.style'              = ifelse(
                    getOption('jupyter.in_kernel', FALSE),
                    ## The jupyter notebook does not understand the multiline table format
                    'rmarkdown',
                    'multiline'),
                'table.emphasize.rownames' = TRUE,
                'table.split.table'        = 80,
                'table.split.cells'        = 30,
                'table.caption.prefix'     = 'Table: ',
                'table.continues'          = 'Table continues below',
                'table.continues.affix'    = '(continued below)',
                'table.alignment.default'  = 'centre',
                'table.alignment.rownames' = 'centre',
                'use.hyphening'            = FALSE,
                'evals.messages'           = TRUE,
                'p.wrap'                   = '_',
                'p.sep'                    = ', ',
                'p.copula'                 = ' and ',
                'plain.ascii'              = FALSE,
                'graph.nomargin'           = TRUE,
                'graph.fontfamily'         = 'sans',
                'graph.fontcolor'          = 'black',
                'graph.fontsize'           = 12,
                'graph.grid'               = TRUE,
                'graph.grid.minor'         = TRUE,
                'graph.grid.color'         = 'grey',
                'graph.grid.lty'           = 'dashed',
                'graph.boxes'              = FALSE,
                'graph.legend.position'    = 'right',
                'graph.background'         = 'white',
                'graph.panel.background'   = 'transparent',
                'graph.colors'             = c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#E69F00"), #nolint
                'graph.color.rnd'          = FALSE,
                'graph.axis.angle'         = 1,
                'graph.symbol'             = 1,
                'knitr.auto.asis'          = TRUE,
                'pandoc.binary'            = Sys.which('pandoc')
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
                'length'                = Inf,
                'output'                = 'all',
                'graph.unify'           = FALSE,
                'graph.name'            = '%t',
                'graph.dir'             = 'plots',
                'graph.output'          = 'png',
                'width'                 = 480,
                'height'                = 480,
                'res'                   = 72,
                'hi.res'                = FALSE,
                'hi.res.width'          = 960,
                'graph.env'             = FALSE,
                'graph.recordplot'      = FALSE,
                'graph.RDS'             = FALSE,
                'log'                   = NULL
                ))
}

## general (temporary) storage for pander's stuff
storage           <- new.env()
storage$caption   <- NULL
storage$alignment <- NULL
debug             <- new.env()
debug$nested      <- 0
debug$nestedID    <- 0

## cache storage
cached.results <- new.env() # cache of results from evals
cached.environments <- new.env() # cache of changes to the environment (assignments in particular)

## cache hash storage
hash.cache.obj       <- new.env() # raw R objects of which hash was computed before
hash.cache.hash      <- new.env() # the computed hash of the above R objects
hash.cache.last.used <- new.env() # when was the hash last queried

## masked plots
masked.plots <- new.env()
masked.plots$plot <- masked.plots$barplot <- masked.plots$lines <- masked.plots$pie <- masked.plots$boxplot <- masked.plots$polygon <- masked.plots$points <- masked.plots$legend <- masked.plots$hist <- masked.plots$pairs <- masked.plots$stripchart <- masked.plots$clusplot <- masked.plots$text <- function (...) { #nolint

    mc      <- match.call()
    fn      <- deparse(mc[[1]])
    fn.pkg  <- gsub('.*library/|/help.*', '', help(fn)[1])
    fn.orig <- parse(text = paste0(fn.pkg, '::', fn))[[1]]
    mc      <- match.call(get(fn, envir = .GlobalEnv))

    if (!(!is.null(mc$plot) && !mc$plot)) {

        ## pander options
        fc  <- panderOptions('graph.fontcolor')
        fbs <- panderOptions('graph.fontsize')
        bc  <- panderOptions('graph.background')
        gc  <- panderOptions('graph.grid.color')
        cex <- fbs / 12
        cs <- panderOptions('graph.colors')
        if (panderOptions('graph.color.rnd'))
            cs <- sample(cs)
        cb <- cs[1]

        ## global par update
        if (!fn %in% c('text')) {
            par(
                family   = panderOptions('graph.fontfamily'),
                cex      = cex, cex.axis = cex * 0.8, cex.lab = cex, cex.main = cex * 1.2, cex.sub = cex,
                bg       = bc, # nolint TODO: how could we color only the inner plot area globally? Not like: https://stat.ethz.ch/pipermail/r-help/2003-May/033971.html
                las      = panderOptions('graph.axis.angle'),
                lwd      = 2,
                pch      = panderOptions('graph.symbol'),
                col.axis = fc, col.lab = fc, col.main = fc, col.sub = fc)
        }

        ## remove margins

        if (panderOptions('graph.nomargin') & !fn %in% c('text')) {
            par(mar = c(4.1, 4.3, 2.1, 0.1))
        }

        ## default: grid is added to all plots
        doAddGrid <- TRUE

        ## update colors
        if (is.null(mc$col) & is.null(mc$color))
            mc$col <- cb
        if (fn == 'boxplot')
            mc$border <- 'black'
        if (fn == 'clusplot') {
            mc$col <- NULL
            if (is.null(mc$color))
                mc$color <- TRUE
            if (is.null(mc$shade))
                mc$shade <- TRUE
            if (is.null(mc$labels))
                mc$labels <- 4
            if (is.null(mc$col.p))
                mc$col.p <- 'black'
            if (is.null(mc$col.clus))
                mc$col.clus <- cs
        }

        ## remove boxes
        if (fn %in% c('pairs', 'stripchart')) {
            doAddGrid <- FALSE
            par(fg = fc)
        } else {
            if (panderOptions('graph.boxes'))
                par(fg = gc)
            else
                par(fg = bc)
        }

        if (fn == 'pie')
            mc$col <- cs

    }

    ## call
    mc[[1]] <- fn.orig
    eval(mc, envir = parent.frame())

    ## grid
    if (all(par()$mfrow == 1) & panderOptions('graph.grid') & doAddGrid & !(!is.null(mc$plot) && !mc$plot)) {

        g <- tryCatch(grid(lty = panderOptions('graph.grid.lty'),
                           col = panderOptions('graph.grid.color'),
                           lwd = 0.5),
                      error = function(e) e)
        if (!inherits(g, 'error')) {
            if (panderOptions('graph.grid.minor'))
                g <- tryCatch(add.minor.ticks(2, 2, grid = TRUE), error = function(e) e)
        }
        if (inherits(g, 'error'))
            warning('Applying default formatting to image is somehow compromised (the result could differ from what you specified in `panderOptions`). Hints: printing `lattice`/`ggplot2` is not needed and tweaking `base` plots with `par` might have some side-effects!') #nolint

    }
}


#' Querying/setting pander option
#'
#' To list all \code{pander} options, just run this function without any parameters provided. To query only one value, pass the first parameter. To set that, use the \code{value} parameter too.
#'
#' The following \code{pander} options are available:
#'
#' \itemize{
#'      \item \code{digits}: numeric (default: \code{2}) passed to \code{format}. Can be a vector specifying values for each column (has to be the same length as number of columns). Values for non-numeric columns will be disregarded.
#'      \item \code{decimal.mark}: string (default: \code{.}) passed to \code{format}
#'      \item \code{formula.caption.prefix}: string (default: \code{'Formula: '}) passed to \code{\link{pandoc.formula}} to be used as caption prefix. Be sure about what you are doing if changing to other than \code{'Formula: '} or \code{':'}.
#'      \item \code{big.mark}: string (default: '') passed to \code{format}.
#'      \item \code{round}: numeric (default: \code{Inf}) passed to \code{round}. Can be a vector specifying values for each column (has to be the same length as number of columns). Values for non-numeric columns will be disregarded.
#'      \item \code{keep.trailing.zeros}: boolean (default: \code{FALSE}) to show or remove trailing zeros in numbers
#'      \item \code{keep.line.breaks}: boolean (default: \code{FALSE}) to keep or remove line breaks from cells in a table
#'      \item \code{missing}: string (default: \code{NA}) to replace missing values in vectors, tables etc.
#'      \item \code{date}: string (default: \code{'\%Y/\%m/\%d \%X'}) passed to \code{format} when printing dates (\code{POSIXct} or \code{POSIXt})
#'      \item \code{header.style}: \code{'atx'} or \code{'setext'} passed to \code{\link{pandoc.header}}
#'      \item \code{list.style}: \code{'bullet'}, \code{'ordered'} or \code{'roman'} passed to \code{\link{pandoc.list}}. Please not that this has no effect on \code{pander} methods.
#'      \item \code{table.style}: \code{'multiline'}, \code{'grid'}, \code{'simple'} or \code{'rmarkdown'} passed to \code{\link{pandoc.table}}
#'      \item \code{table.emphasize.rownames}: boolean (default: \code{TRUE}) if row names should be highlighted
#'      \item \code{table.split.table}: numeric passed to \code{\link{pandoc.table}} and also affects \code{pander} methods. This option tells \code{pander} where to split too wide tables. The default value (\code{80}) suggests the conventional number of characters used in a line, feel free to change (e.g. to \code{Inf} to disable this feature) if you are not using a VT100 terminal any more :)
#'      \item \code{table.split.cells}: numeric or numeric vector (default: \code{30}) passed to \code{\link{pandoc.table}} and also affects \code{pander} methods. This option tells \code{pander} where to split too wide cells with line breaks. Numeric vector specifies values for cells separately. Set \code{Inf} to disable.
#'      \item \code{table.caption.prefix}: string (default: \code{'Table: '}) passed to \code{\link{pandoc.table}} to be used as caption prefix. Be sure about what you are doing if changing to other than \code{'Table: '} or \code{':'}.
#'      \item \code{table.continues}: string (default: \code{'Table continues below'}) passed to \code{\link{pandoc.table}} to be used as caption for long (split) without a use defined caption
#'      \item \code{table.continues.affix}: string (default: \code{'(continued below)'}) passed to \code{\link{pandoc.table}} to be used as an affix concatenated to the user defined caption for long (split) tables
#'      \item \code{table.alignment.default}: string (default: \code{centre}) that defines the default alignment of cells. Can be \code{left}, \code{right} or \code{centre} that latter can be also spelled as \code{center}.
#'      \item \code{table.alignment.rownames}: string (default: \code{centre}) that defines the alignment of rownames in tables. Can be \code{left}, \code{right} or \code{centre} that latter can be also spelled as \code{center}.
#'      \item \code{use.hyphening}: boolean (default: \code{FALSE}) if try to use hyphening when splitting large cells according to table.split.cells. Requires \pkg{sylly}.
#'      \item \code{evals.messages}: boolean (default: \code{TRUE}) passed to \code{evals}' \code{pander} method specifying if messages should be rendered
#'      \item \code{p.wrap}: a string (default: \code{'_'}) to wrap vector elements passed to \code{p} function
#'      \item \code{p.sep}: a string (default: \code{', '}) with the main separator passed to \code{p} function
#'      \item \code{p.copula}: a string (default: \code{' and '}) with ending separator passed to \code{p} function
#'      \item \code{plain.ascii}: boolean (default: \code{FALSE}) to define if output should be in plain ascii or not
#'      \item \code{graph.nomargin}: boolean (default: \code{TRUE}) if trying to keep plots' margins at minimal
#'      \item \code{graph.fontfamily}: string (default: \code{'sans'}) specifying the font family to be used in images. Please note, that using a custom font on Windows requires \code{grDevices:::windowsFonts} first.
#'      \item \code{graph.fontcolor}: string (default: \code{'black'}) specifying the default font color
#'      \item \code{graph.fontsize}: numeric (default: \code{12}) specifying the \emph{base} font size in pixels. Main title is rendered with \code{1.2} and labels with \code{0.8} multiplier.
#'      \item \code{graph.grid}: boolean (default: \code{TRUE}) if a grid should be added to the plot
#'      \item \code{graph.grid.minor}: boolean (default: \code{TRUE}) if a miner grid should be also rendered
#'      \item \code{graph.grid.color}: string (default: \code{'grey'}) specifying the color of the rendered grid
#'      \item \code{graph.grid.lty}: string (default: \code{'dashed'}) specifying the line type of grid
#'      \item \code{graph.boxes}: boolean (default: \code{FALSE}) if to render a border around of plot (and e.g. around strip)
#'      \item \code{graph.legend.position}: string (default: \code{'right'}) specifying the position of the legend: 'top', 'right', 'bottom' or 'left'
#'      \item \code{graph.background}: string (default: \code{'white'}) specifying the plots main background's color
#'      \item \code{graph.panel.background}: string (default: \code{'transparent'}) specifying the plot's main panel background. Please \emph{note}, that this option is not supported with \code{base} graphics.
#'      \item \code{graph.colors}: character vector of default color palette (defaults to a colorblind theme: \url{http://jfly.iam.u-tokyo.ac.jp/color/}). Please \emph{note} that this update work with \code{base} plots by appending the \code{col} argument to the call if not set.
#'      \item \code{graph.color.rnd}: boolean (default: \code{FALSE}) specifying if the palette should be reordered randomly before rendering each plot to get colorful images
#'      \item \code{graph.axis.angle}: numeric (default: \code{1}) specifying the angle of axes' labels. The available options are based on \code{par(les)} and sets if the labels should be:
#'      \itemize{
#'              \item \code{1}: parallel to the axis,
#'              \item \code{2}: horizontal,
#'              \item \code{3}: perpendicular to the axis or
#'              \item \code{4}: vertical.
#'      }
#'      \item \code{graph.symbol}: numeric (default: \code{1}) specifying a symbol (see the \code{pch} parameter of \code{par})
#'      \item \code{knitr.auto.asis}: boolean (default: \code{TRUE}) if the results of \code{pander} should be considered as \code{'asis'} in \code{knitr}. Equals to specifying \code{results='asis'} in the R chunk, so thus there is no need to do so if set to \code{TRUE}.
#'      \item \code{pandoc.binary}: full path of \code{pandoc}'s binary. By default, \code{pandoc} is in the path.
#' }
#' @param o option name (string). See below.
#' @param value value to assign (optional)
#' @export
#' @seealso \code{\link{evalsOptions}}
#' @examples \dontrun{
#' panderOptions()
#' panderOptions('digits')
#' panderOptions('digits', 5)
#' }
panderOptions <- function(o, value) {

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

        ## fix assigning NULL to a list element
        if (is.null(value)) {
            res[o] <- list(NULL)
        } else {
            res[[o]] <- value
        }

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
#'      \item \code{hooks}: list of hooks to be run for given classes in the form of \code{list(class = fn)}. If you would also specify some parameters of the function, a list should be provided in the form of \code{list(fn, param1, param2=NULL)} etc. So the hooks would become \code{list(class1=list(fn, param1, param2=NULL), ...)}. See examples of \code{\link{evals}}. A default hook can be specified too by setting the class to \code{'default'}. This can be handy if you do not want to define separate methods/functions to each possible class, but automatically apply the default hook to all classes not mentioned in the list. You may also specify only one element in the list like: \code{hooks=list('default' = pander_return)}. Please note, that nor error/warning messages, nor stdout is captured (so: updated) while running hooks!
#'      \item \code{length}: any R object exceeding the specified length will not be returned. The default value (\code{Inf}) does not filter out any R objects.
#'      \item \code{output}: a character vector of required returned values. This might be useful if you are only interested in the \code{result}, and do not want to save/see e.g. \code{messages} or \code{print}ed \code{output}. See examples of \code{\link{evals}}.
#'      \item \code{graph.unify}: boolean (default: \code{FALSE}) that determines if \code{evals} should try to unify the style of (\code{base}, \code{lattice} and \code{ggplot2}) plots? If set to \code{TRUE}, some \code{panderOptions()} would apply.
#'      \item \code{graph.name}: set the file name of saved plots which is \code{\link{tempfile}} by default. A simple character string might be provided where \code{\%d} would be replaced by the index of the generating \code{txt} source, \code{\%n} with an incremented integer in \code{graph.dir} with similar file names and \code{\%t} by some random characters. A function's name to be \code{eval}uated can be passed here too.
#'      \item \code{graph.dir}: path to a directory where to place generated images. If the directory does not exist, \code{\link{evals}} try to create that. Default set to \code{plots} in current working directory.
#'      \item \code{graph.output}: set the required file format of saved plots. Currently it could be any of  \code{grDevices}: \code{png}, \code{bmp}, \code{jpeg}, \code{jpg}, \code{tiff}, \code{svg} or \code{pdf}. Set to \code{NA} not to save plots at all and tweak that setting with \code{capture.plot()} on demand.
#'      \item \code{width}: width of generated plot in pixels for even vector formats
#'      \item \code{height}: height of generated plot in pixels for even vector formats
#'      \item \code{res}: nominal resolution in \code{ppi}. The height and width of vector images will be calculated based in this.
#'      \item \code{hi.res}: generate high resolution plots also? If set to \code{TRUE}, each R code parts resulting an image would be run twice.
#'      \item \code{hi.res.width}: width of generated high resolution plot in pixels for even vector formats. The \code{height} and \code{res} of high resolution image is automatically computed based on the above options to preserve original plot aspect ratio.
#'      \item \code{graph.env}: save the environments in which plots were generated to distinct files (based on \code{graph.name}) with \code{env} extension?
#'      \item \code{graph.recordplot}: save the plot via \code{recordPlot} to distinct files (based on \code{graph.name}) with \code{recodplot} extension?
#'      \item \code{graph.RDS}: save the raw R object returned (usually with \code{lattice} or \code{ggplot2}) while generating the plots to distinct files (based on \code{graph.name}) with \code{RDS} extension?
#'      \item \code{log}: \code{NULL} or  an optionally passed \emph{logger name} from \pkg{futile.logger} to record all info, trace, debug and error messages.
#' }
#' @param o option name (string). See below.
#' @param value value to assign (optional)
#' @export
#' @seealso \code{\link{evals}} \code{\link{panderOptions}}
#' @examples
#' evalsOptions()
#' evalsOptions('cache')
#' evalsOptions('cache', FALSE)
evalsOptions <- function(o, value) {

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

        ## fix assigning NULL to a list element
        if (is.null(value)) {
            res[o] <- list(NULL)
        } else {
            res[[o]] <- value
        }

        options('evals' = res)

    }

}
