#' Evaluate with messages
#'
#' This function takes text(s) of R code and \code{eval}s all at one run - returning a list with four elements. See \code{Details}.
#'
#' \code{eval.msgs} returns a detailed list of the result of evaluation:
#' \itemize{
#'     \item \emph{src} - character vector of specified R code.
#'     \item \emph{result} - result of evaluation. \code{NULL} if nothing is returned. If any R code returned an R object while evaluating then the \emph{last} R object will be returned as a raw R object. If a graph is plotted in the end of the given R code (remember: \emph{last} R object), it would be automatically printed (see e.g. \code{lattice} and \code{ggplot2}).
#'      \item \emph{output} - character vector of printed version (\code{capture.output}) of \code{result}
#'     \item \emph{type} - class of generated output. "NULL" if nothing is returned, "error" if some error occurred.
#'     \item \emph{msg} - possible messages grabbed while evaluating specified R code with the following structure:
#'     \itemize{
#'         \item \emph{messages} - character vector of possible diagnostic message(s)
#'         \item \emph{warnings} - character vector of possible warning message(s)
#'         \item \emph{errors} - character vector of possible error message(s)
#'     }
#'     \item \emph{stdout} - character vector of possibly printed texts to standard output (console)
#' }
#' @param src character values containing R code
#' @param env environment where evaluation takes place. If not set (by default), a new temporary environment is created.
#' @return a list of parsed elements each containing: \code{src} (the command run), \code{result} (R object: \code{NULL} if nothing returned), \code{print}ed \code{output}, \code{type} (class of returned object if any), informative/wawrning and error messages (if any returned by the command run, otherwise set to \code{NULL}) and possible \code{stdout}t value. See Details above.
#' @seealso \code{\link{evals}}
#' @export
#' @examples \dontrun{
#' eval.msgs('1:5')
#' eval.msgs('x <- 1:5')
#' eval.msgs('lm(mtcars$hp ~ mtcars$wt)')
#'
#' ## plots
#' eval.msgs('plot(runif(100))')
#' eval.msgs('histogram(runif(100))')
#'
#' ## error handling
#' eval.msgs('runiff(23)')
#' eval.msgs('runif is a nice function')
#' eval.msgs('no.R.object.like.that')
#'
#' ## messages
#' eval.msgs(c('message("FOO")', '1:2'))
#' eval.msgs(c('warning("FOO")', '1:2'))
#' eval.msgs(c('message("FOO");message("FOO");warning("FOO")', '1:2'))
#' eval.msgs('warning("d");warning("f");1')
#'
#' ## stdout
#' eval.msgs('cat("writing to console")')
#' eval.msgs('cat("writing to console");1:4')
#' }
eval.msgs <- function(src, env = NULL) {

    if (is.null(env))
        env <- new.env()

    ## grab warnings and messages
    warnings <- NULL
    warning.handler <- function(w) {
        warnings <<- c(warnings, w$message)
        invokeRestart("muffleWarning")
    }

    messages <- NULL
    message.handler <- function(m) {
        messages <<- c(messages, sub('\n$', '', m$message))
    }

    ## grab stdout
    stdout <- vector("character")
    con <- textConnection("stdout", "wr", local=TRUE)
    sink(con, split = FALSE)

    result <- suppressMessages(withCallingHandlers(tryCatch(withVisible(eval(parse(text=src), envir = env)), error = function(e) e), warning = warning.handler, message = message.handler))

    sink()
    close(con)
    if (length(stdout) == 0)
        stdout <- NULL

    ## error handling
    if (inherits(result, 'error')) {

        error <- result$message

        if (grepl('unexpected symbol', error))
            error <- sub('<text>:([0-9]*):([0-9]*): unexpected symbol\n.*[0-9]*:(.*)\n.*[ \t]*$', 'Unexpected symbol at character \\2 in line \\1: `\\3`', error)

        result <- output <- NULL
        type   <- 'error'

    } else {

        error <- NULL

    }

    ## check if printing is needed
    if (!is.null(result)) {

        if (result$visible) {

            output <- vector("character")
            con <- textConnection("output", "wr", local=TRUE)
            sink(con, split = FALSE)

            result <- print(result$value)

            sink()
            close(con)

        } else {

            result <- output <- NULL

        }
    }

    if (is.null(error))
        type  <- class(result)

    ## return
    list(src    = src,
         result = result,
         output = output,
         type   = type,
         msg    = list(
             messages = messages,
             warnings = warnings,
             errors   = error),
         stdout = stdout)

}


#' Evaluate and Process R Code
#'
#' This function takes either a vector/list of \emph{strings} with actual R code, which it to be \code{parse}d to separate elements. Each list element is \code{eval}uated in a special environment, and a detailed list of results is returned for each logical part of the R code: a character value with R code, resulting R object, printed output, class of resulting R object, possible informative/warning/error messages and anything written to \code{stdout}. If a graph is plotted in the given text, the returned object is a string specifying the path to the saved file. Please see Details below.
#' If \code{parse} option set to \code{FALSE}, then the returned list's length equals to the length of the \code{parse}d input - as each string is evaluated as separate R code in the same environment. If a nested list of R code or a concatenated string (separated by \code{\\n} or \code{;}) is provided like \code{list(c('runif(1)', 'runif(1)'))} with \code{parse=FALSE}, then everything is \code{eval}ed at one run so the length of returned list equals to one or the length of the provided nested list. See examples below.
#'
#' As \code{\link{evals}} tries to grab the plots internally, pleas do not run commands that set graphic device or \code{dev.off}. E.g. running \code{evals(c('png("/tmp/x.png")', 'plot(1:10)', 'dev.off()'))} would fail. \code{print}ing of \code{lattice} and \code{ggplot2} objects is not needed, \code{evals} would deal with that automatically.
#'
#' The generated image file(s) of the plots can be fine-tuned by some specific options, please check out \code{graph.output}, \code{width}, \code{height}, \code{res}, \code{hi.res}, \code{hi.res.width}, \code{hi.res.height} and \code{hi.res.res} parameters. Most of these options are better not to touch, see details of parameters below.
#'
#' Returned result values: list with the following elements
#' \itemize{
#'     \item \emph{src} - character vector of specified R code.
#'     \item \emph{result} - result of evaluation. \code{NULL} if nothing is returned. If any R code returned an R object while evaluating then the \emph{last} R object will be returned as a raw R object. If a graph is plotted in the given text, the returned object is a string (with \code{class} set to \code{image}) specifying the path to the saved image file. If graphic device was touched, then no other R objects will be returned.
#'      \item \emph{output} - character vector of printed version (\code{capture.output}) of \code{result}
#'     \item \emph{type} - class of generated output. "NULL" if nothing is returned, "error" if some error occurred.
#'     \item \emph{msg} - possible messages grabbed while evaluating specified R code with the following structure:
#'     \itemize{
#'         \item \emph{messages} - character vector of possible diagnostic message(s)
#'         \item \emph{warnings} - character vector of possible warning message(s)
#'         \item \emph{errors} - character vector of possible error message(s)
#'     }
#'     \item \emph{stdout} - character vector of possibly printed texts to standard output (console)
#' }
#'
#' Please check the examples carefully below to get a detailed overview of \code{\link{evals}}.
#' @param txt a character vector containing R code. This could be a list/vector of lines of code or a simple string holding R code separated by \code{;} or \code{\\n}.
#' @param parse if \code{TRUE} the provided \code{txt} elements would be merged into one string and parsed to logical chunks. This is useful if you would want to get separate results of your code parts - not just the last returned value, but you are passing the whole script in one string. To manually lock lines to each other (e.g. calling a \code{plot} and on next line adding an \code{abline} or \code{text} to it), use a plus char (\code{+}) at the beginning of each line which should be evaluated with the previous one(s). If set to \code{FALSE}, \code{evals} would not try to parse R code, it would get evaluated in separate runs - as provided. Please see examples below.
#' @param classes a vector or list of classes which should be returned. If set to \code{NULL} (by default) all R objects will be returned.
#' @param hooks list of hooks to be run for given classes in the form of \code{list(class = fn)}. If you would also specify some parameters of the function, a list should be provided in the form of \code{list(fn, param1, param2=NULL)} etc. So the hooks would become \code{list(class1=list(fn, param1, param2=NULL), ...)}. See example below. A default hook can be specified too by setting the class to \code{'default'}. This can be handy if you do not want to define separate methods/functions to each possible class, but automatically apply the default hook to all classes not mentioned in the list. You may also specify only one element in the list like: \code{hooks=list('default' = pander.return)}. Please note, that nor error/warning messages, nor stdout is captured (so: updated) while running hooks!
#' @param length any R object exceeding the specified length will not be returned. The default value (\code{Inf}) does not filter out any R objects.
#' @param output a character vector of required returned values. This might be useful if you are only interested in the \code{result}, and do not want to save/see e.g. \code{messages} or \code{print}ed \code{output}. See examples below.
#' @param env environment where evaluation takes place. If not set (by default), a new temporary environment is created.
#' @param graph.nomargin should \code{evals} try to keep plots' margins minimal?
#' @param graph.name set the file name of saved plots which is \code{\link{tempfile}} by default. A simple character string might be provided where \code{\%d} would be replaced by the index of the generating \code{txt} source, \code{\%n} with an incremented integer in \code{graph.dir} with similar file names and \code{\%t} by some random characters. A function's name to be \code{eval}uated can be passed here too.
#' @param graph.dir path to a directory where to place generated images. If the directory does not exist, \code{evals} try to create that. Default set to NULL as using \code{\link{tempfile}}s.
#' @param graph.output set the required file format of saved plots. Currently it could be any of  \code{grDevices}': \code{png}, \code{bmp}, \code{jpeg}, \code{jpg}, \code{tiff}, \code{svg} or \code{pdf}.
#' @param width width of generated plot in pixels for even vector formats
#' @param height height of generated plot in pixels for even vector formats
#' @param res nominal resolution in \code{ppi}. The height and width of vector images will be calculated based in this.
#' @param hi.res generate high resolution plots also? If set to \code{TRUE}, each R code parts resulting an image would be run twice.
#' @param hi.res.width width of generated high resolution plot in pixels for even vector formats
#' @param hi.res.height height of generated high resolution plot in pixels for even vector formats. This value can be left blank to be automatically calculated to match original plot aspect ratio.
#' @param hi.res.res nominal resolution of high resolution plot in ppi. The height and width of vector plots will be calculated based in this. This value can be left blank to be automatically calculated to fit original plot scales.
#' @param graph.env save the environments in which plots were generated to distinct files (based on \code{graph.name}) with \code{env} extension?
#' @param graph.recordplot save the plot via \code{\link{recordPlot}} to distinct files (based on \code{graph.name}) with {recodplot} extension?
#' @param ... optional parameters passed to graphics device (e.g. \code{bg}, \code{pointsize} etc.)
#' @return a list of parsed elements each containing: \code{src} (the command run), \code{result} (R object: \code{NULL} if nothing returned, path to image file if a plot was generated), \code{print}ed \code{output}, \code{type} (class of returned object if any), informative/wawrning and error messages (if any returned by the command run, otherwise set to \code{NULL}) and possible \code{stdout}t value. See Details above.
#' @seealso \code{\link{eval.msgs}}
#' @examples \dontrun{
#' # parsing several lines of R code
#' txt <- readLines(textConnection('x <- rnorm(100)
#'   runif(10)
#'   warning("Lorem ipsum foo-bar-foo!")
#'   plot(1:10)
#'   qplot(rating, data = movies, geom = "histogram")
#'   y <- round(runif(100))
#'   cor.test(x, y)
#'   crl <- cor.test(runif(10), runif(10))
#'   table(mtcars$am, mtcars$cyl)
#'   ggplot(mtcars) + geom_point(aes(x = hp, y = mpg))'))
#' evals(txt)
#'
#' ## parsing a list of commands
#' txt <- list('df <- mtcars',
#'  c('plot(mtcars$hp, pch = 19)','text(mtcars$hp, label = rownames(mtcars), pos = 4)'),
#'  'ggplot(mtcars) + geom_point(aes(x = hp, y = mpg))')
#' evals(txt)
#'
#' ## the same commands in one string but also evaluating the `plot` with `text` (note the leading "+" on the beginning of `text...` line)
#' txt <- 'df <- mtcars
#'  plot(mtcars$hp, pch = 19)
#'  +text(mtcars$hp, label = rownames(mtcars), pos = 4)
#'  ggplot(mtcars) + geom_point(aes(x = hp, y = mpg))'
#' evals(txt)
#' ## but it would fail without parsing
#' evals(txt, parse = FALSE)
#' ## if you do not want to auto-parse the text, use the above list/vector method
#'
#' ## returning only a few classes
#' txt <- readLines(textConnection('rnorm(100)
#'   list(x = 10:1, y = "Godzilla!")
#'   c(1,2,3)
#'    matrix(0,3,5)'))
#' evals(txt, classes = 'numeric')
#' evals(txt, classes = c('numeric', 'list'))
#'
#' ## handling messages
#' evals('message(20)')
#' evals('message(20);message(20)', parse = FALSE)
#'
#' ## adding a caption to a plot (`plot` is started with a `+`!)
#' evals('set.caption("FOO"); +plot(1:10)')     # TODO
#'
#' ## handling warnings
#' evals('chisq.test(mtcars$gear, mtcars$hp)')
#' evals(list(c('chisq.test(mtcars$gear, mtcars$am)', 'pi', 'chisq.test(mtcars$gear, mtcars$hp)')), parse = F)
#' evals(c('chisq.test(mtcars$gear, mtcars$am)', 'pi', 'chisq.test(mtcars$gear, mtcars$hp)'))
#'
#' ## handling errors
#' evals('runiff(20)')
#' evals('Old MacDonald had a farm\\dots')
#' evals('## Some comment')
#' evals(c('runiff(20)', 'Old MacDonald had a farm?'))
#' evals(list(c('runiff(20)', 'Old MacDonald had a farm?')), parse = F)
#' evals(c('mean(1:10)', 'no.R.function()'))
#' evals(list(c('mean(1:10)', 'no.R.function()')), parse = F)
#' evals(c('no.R.object', 'no.R.function()', 'very.mixed.up(stuff)'))
#' evals(list(c('no.R.object', 'no.R.function()', 'very.mixed.up(stuff)')), parse = F)
#' evals(c('no.R.object', 'Old MacDonald had a farm\\dots', 'pi'))
#' evals('no.R.object;Old MacDonald had a farm\\dots;pi', parse = F)
#' evals(list(c('no.R.object', 'Old MacDonald had a farm\\dots', 'pi')), parse = F)
#'
#' ## graph options
#' evals('plot(1:10)')
#' evals('plot(1:10);plot(2:20)')
#' evals('plot(1:10)', graph.output = 'jpg')
#' evals('plot(1:10)', height = 800)
#' evals('plot(1:10)', height = 800, hi.res = T)
#' evals('plot(1:10)', graph.output = 'pdf', hi.res = T)
#' evals('plot(1:10)', res = 30)
#' evals('plot(1:10)', graph.name = 'myplot')
#' evals(list('plot(1:10)', 'plot(2:20)'), graph.name = 'myplots-%d')
#' evals('plot(1:10)', graph.env = TRUE)
#' evals('x <- runif(100);plot(x)', graph.env = TRUE)
#' evals(c('plot(1:10)', 'plot(2:20)'), graph.env = TRUE)
#' evals(c('x <- runif(100)', 'plot(x)','y <- runif(100)', 'plot(y)'), graph.env = TRUE)
#' evals(list(c('x <- runif(100)', 'plot(x)'), c('y <- runif(100)', 'plot(y)')), graph.env = TRUE, parse = F)
#' evals('plot(1:10)', graph.recordplot = TRUE)
#' ## unprinted lattice plot
#' evals('histogram(mtcars$hp)', graph.recordplot = TRUE)
#'
#' ## hooks
#' txt <- 'runif(1:4); matrix(runif(25), 5, 5); 1:5'
#' hooks <- list('numeric' = round, 'matrix' = pander.return)
#' evals(txt, hooks = hooks)
#' ## using pander's default hook
#' evals(txt, hooks = list('default' = pander.return))
#' evals('22/7', hooks = list('numeric' = round))
#' evals('matrix(runif(25), 5, 5)', hooks = list('matrix' = round))
#'
#' ## setting default hook
#' evals(c('runif(10)', 'matrix(runif(9), 3, 3)'), hooks = list('default'=round))
#' ## round all values except for matrices
#' evals(c('runif(10)', 'matrix(runif(9), 3, 3)'), hooks = list(matrix = 'print', 'default' = round))
#'
#' # advanced hooks
#' hooks <- list('numeric' = list(round, 2), 'matrix' = list(round, 1))
#' evals(txt, hooks = hooks)
#'
#' # return only returned values
#' evals(txt, output = 'result')
#'
#' # return only messages (for checking syntax errors etc.)
#' evals(txt, output = 'msg')
#'
#' # check the length of returned values and do not return looong R objects
#' evals('runif(10)', length = 5)
#'
#' # note the following will not be filtered!
#' evals('matrix(1,1,1)', length = 1)
#'
#' # if you do not want to let such things be eval-ed in the middle of a string use it with other filters :)
#' evals('matrix(1,1,1)', length = 1, classes = 'numeric')
#'
#'# hooks & filtering
#' evals('matrix(5,5,5)', hooks = list('matrix' = pander.return), output = 'result')
#'
#' # eval-ing chunks in given environment
#' myenv <- new.env()
#' evals('x <- c(0,10)', env = myenv)
#' evals('mean(x)', env = myenv)
#' rm(myenv)
#' # note: if you had not specified 'myenv', the second 'evals' would have failed
#' evals('x <- c(0,10)')
#' evals('mean(x)')
#' }
#' @export
evals <- function(txt, parse = TRUE, classes = NULL, hooks = NULL, length = Inf, output = c('all', 'src', 'result', 'output', 'type', 'msg', 'stdout'), env = NULL, graph.nomargin = TRUE, graph.name = '%t', graph.dir = tempdir(), graph.output = c('png', 'bmp', 'jpeg', 'jpg', 'tiff', 'svg', 'pdf'), width = 480, height = 480, res= 72, hi.res = FALSE, hi.res.width = 960, hi.res.height = 960*(height/width), hi.res.res = res*(hi.res.width/width), graph.env = FALSE, graph.recordplot = FALSE, ...){

    if (missing(txt))
        stop('No R code provided to evaluate!')

    ## parse provided code after concatenating
    if (parse) {

        txt.parsed <- tryCatch(parse(text = txt), error = function(e) e)

        ## skip parsing on syntax error
        if (!inherits(txt.parsed, 'error')) {

            txt <- sapply(txt.parsed, function(x) paste(deparse(x), collapse = ' '))
            if (length(txt) == 0)
                stop('No R code provided to evaluate!')

            ## (re)merge lines on demand (based on `+` at the beginning of file)
            txt.sep <- c(which(!grepl('^\\+', txt)), length(txt)+1)
            txt <-  lapply(1:(length(txt.sep)-1), function(i) txt[txt.sep[i]:(txt.sep[i+1]-1)])
            txt <- rapply(txt, function(x) sub('^\\+', '', x), how = 'replace')

        }
    }

    if (!identical(file.info(graph.dir)$isdir, TRUE))
        if (!dir.create(graph.dir, showWarnings = FALSE, recursive = TRUE))
            stop(sprintf('Something is definitely wrong with `graph.dir`: %s!', graph.dir))

    output <- match.arg(output, several.ok = TRUE)

    if (sum(grepl('all', output)) > 0)
        output <- c('src', 'result', 'output', 'type', 'msg', 'stdout')

    if (!any(is.list(hooks), is.null(hooks)))
        stop('Wrong list of hooks provided!')

    graph.output <- match.arg(graph.output)
    if (graph.output == 'jpg')
        graph.output <- 'jpeg'

    ## env for running all lines of code -> eval()
    if (is.null(env))
        env <- new.env()
    if (!is.environment(env))
        stop('Wrong env parameter (not an environment) provided!')

    `%d` <- 0
    lapply(txt, function(src) {

        `%d` <<- `%d` + 1

        clear.devs <- function()
            while (!is.null(dev.list()))
                dev.off(as.numeric(dev.list()[1]))

        clear.devs()

        ## env for optional high resolution images
        if (hi.res)
            env.hires <- env

        ## init (potential) img file
        file.name <- gsub('%d', `%d`, eval(graph.name), fixed = TRUE)
        file <- sprintf('%s.%s', file.name, graph.output)
        if (grepl('%t', graph.name)) {
            if (length(strsplit(sprintf('placeholder%splaceholder', file.name), '%t')[[1]]) > 2)
                stop('File name contains more then 1 "%t"!')
            rep <- strsplit(file, '%t')[[1]]
            file <- tempfile(pattern = rep[1], tmpdir = graph.dir, fileext = rep[2])
            file.name <- sub(sprintf('.%s$', graph.output), '', file)
        } else {
            file <- file.path(gsub('\\', '/', graph.dir, fixed = TRUE), file)
            file.name <- file.path(gsub('\\', '/', graph.dir, fixed = TRUE), file.name)
        }
        if (grepl('%n', file.name)) {
            if (length(strsplit(sprintf('placeholder%splaceholder', file.name), '%n')[[1]]) > 2)
                stop('File name contains more then 1 "%n"!')
            similar.files <- list.files(graph.dir, pattern = sprintf('^%s\\.(jpeg|tiff|png|svg|bmp)$', gsub('%t', '[a-z0-9]*', gsub('%d|%n|%D', '[[:digit:]]*', basename(file.name)))))
            if (length(similar.files) > 0) {
                similar.files <- sub('\\.(jpeg|tiff|png|svg|bmp)$', '', similar.files)
                rep <- gsub('%t', '[a-z0-9]*', gsub('%d|%D', '[[:digit:]]*', strsplit(basename(file.name), '%n')[[1]]))
                `%n` <- max(as.numeric(gsub(paste(rep, collapse = '|'), '', similar.files))) + 1
            } else
                `%n` <- 1
            file.name <- gsub('%n', `%n`, file.name, fixed = TRUE)
            file <- gsub('%n', `%n`, file, fixed = TRUE)
        }

        if (graph.output %in% c('bmp', 'jpeg', 'png', 'tiff'))
            do.call(graph.output, list(file, width = width, height = height, res = res, ...))
        if (graph.output == 'svg')
            do.call(graph.output, list(file, width = width/res, height = height/res, ...)) # TODO: font-family?
        if (graph.output == 'pdf')
            do.call('cairo_pdf', list(file, width = width/res, height = height/res, ...)) # TODO: font-family?

        ## remove margins
        if (graph.nomargin) {
            if (require(lattice, quietly = T))
                lattice::trellis.par.set(layout.heights = list(top.padding = 0.1, bottom.padding = 0.1), layout.widths = list(right.padding = 0.1, left.padding = 0.4))
            par(mar=c(4, 4, 2.1, 0.1))
        }

        ## start recordPlot
        dev.control(displaylist = "enable")

        ## eval
        res <- eval.msgs(src, env = env)

        ## grab recorded.plot
        if (!is.null(dev.list())) {
            recorded.plot <- recordPlot()
            dev.control("inhibit")
        }
        clear.devs()

        ## error handling
        if (!is.null(res$msg$errors))
            return(res)

        result <- res$result
        graph  <- ifelse(exists('recorded.plot'), ifelse(is.null(recorded.plot[[1]]), FALSE, file), FALSE)

        ## we have a graph
        if (is.character(graph)) {

            ## save recorded plot on demand
            if (graph.recordplot) {
                saveRDS(recorded.plot, file = sprintf('%s.recordplot', file.name))
            }

            result <- graph
            class(result) <- "image"

            ## saving environment on demand
            if (graph.env)
                save(list = ls(envir = env), file = sprintf('%s.env', file.name), envir = env)

            ## generate high resolution images on demand
            if (hi.res) {

                file.hi.res <- sprintf('%s-hires.%s', file.name, graph.output)

                ## initialize high resolution image file
                if (graph.output %in% c('bmp', 'jpeg', 'png', 'tiff')) {
                    do.call(graph.output, list(file.hi.res, width = hi.res.width, height = hi.res.height, res = hi.res.res, ...))
                } else {

                    if (.Platform$OS.type == 'unix')    # a symlink would be fine for vector formats on a unix-like OS
                        file.symlink(file, file.hi.res)
                    else                                # we have no option to do so on Windows (to be backward compatible)
                        do.call(graph.output, list(file.hi.res, width = hi.res.width/hi.res.res, height = hi.res.height/hi.res.res, ...)) # TODO: font-family?
                }

                ## render high resolution image (if needed)
                if ((graph.output %in% c('bmp', 'jpeg', 'png', 'tiff')) | (.Platform$OS.type != 'unix')) {
                    eval.msgs(src, env = env.hires)      # we need eval.msgs() here instead of simple eval() to prevent unprinted lattice/ggplot2 objects' issues
                    clear.devs()
                }

                ## add "href" attribute to returned R object
                attr(result, 'href') <- file.hi.res
            }
        }

        ## check length
        if (length(result) > length)
            result <- NULL

        ## check classes
        if (!is.null(classes))
            if (!inherits(result, classes))
                result <- output <- NULL

        ## run hooks if specified
        if (!is.null(hooks))
            if (inherits(result, names(hooks))) {
                fn <- hooks[[class(result)]]; params <- list(result)
                if (is.list(fn)) {
                    params <- list(result, fn[[-1]])
                    fn <- fn[[1]]
                }
                result <- do.call(fn, params)
            } else {
                if ('default' %in% names(hooks)) {
                    fn <- hooks[['default']]; params <- list(result)
                    if (is.list(fn)) {
                        params <- list(result, fn[[-1]])
                        fn <- fn[[1]]
                    }
                    result <- do.call(fn, params)
                }
            }

        ## add captured attributes
        if (!is.null(pander:::storage$caption) & !is.null(result)) {

            attr(result, 'caption') <- pander:::storage$caption
            assign('caption', NULL , envir = pander:::storage)

        }

        ## return list at last
        res <- list(src      = src,
                    result   = result,
                    output   = res$output,
                    type     = class(result),
                    msg      = list(
                        messages = res$msg$messages,
                        warnings = res$msg$warnings,
                        errors   = res$msg$errors),
                    stdout   = res$msg$stdout
                    )

        res <- res[output]
        class(res) <- 'evals'
        return(res)

    })
}


