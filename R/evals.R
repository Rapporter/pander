#' Evaluate with messages
#'
#' This function takes text(s) of R code and \code{\link{eval}}s all at one run then returns a list with four elements:
#'
#' \itemize{
#'     \item \emph{src} - character vector of specified R code.
#'     \item \emph{output} - generated output. \code{NULL} if nothing is returned. If any string returned an R object while evaluating then the \emph{last} R object will be returned as a raw R object. If a graph is plotted in the given text, the returned object is a string specifying the path to the saved png in temporary directory (see: \code{\link{tempfile}}). If multiple plots was run in the same run (see: nested lists as inputs above) then the last plot is saved. If graphic device was touched, then no other R objects will be returned.
#'     \item \emph{type} - class of generated output. "NULL" if nothing is returned, "image" if the graphic device was touched, "error" if some error occurred.
#'     \item \emph{msg} - possible messages grabbed while evaluating specified R code with the following structure:
#'     \itemize{
#'         \item \emph{messages} - string of possible diagnostic message(s)
#'         \item \emph{warnings} - string of possible warning message(s)
#'         \item \emph{errors} - string of possible error message(s)
#'     }
#'     \item \emph{stdout} - character vector of possibly printed texts to standard output (console)
#' }
#' @param src character values containing R code
#' @param env environment where evaluation takes place. If not set (by default), a new temporary environment is created.
#' @return  a list of parsed elements each containing: src (the command run), output (what the command returns, \code{NULL} if nothing returned, path to image file if a plot was generated), type (class of returned object if any), messages: warnings (if any returned by the command run, otherwise set to \code{NULL}) and errors (if any returned by the command run, otherwise set to \code{NULL}) and possible stdout value. See Details above.
#' @seealso \code{\link{evals}}
#' @export
#' @examples \dontrun{
#' eval.msgs('1:5')
#' eval.msgs(c('1:3', 'runiff(23)'))
#' eval.msgs(c('1:5', '3:5'))
#' eval.msgs(c('pi', '1:10', 'NULL'))
#' eval.msgs('pi')
#' eval.msgs('1:2')
#' identical(evals('pi')[[1]], eval.msgs('pi'))
#' eval.msgs(c('message("FOO")', '1:2'))
#' eval.msgs(c('caption("FOO")', '1:2'))
#' eval.msgs('cat("writing to console")')
#' }
#' @importFrom evaluate evaluate
eval.msgs <- function(src, env = NULL) {

    if (is.null(env)) env <- new.env()

    warnings <- NULL
    warning.handler <- function(w) {
        warnings <<- w
        invokeRestart("muffleWarning")
    }
    messages <- NULL
    message.handler <- function(m) {
        messages <<- m$message
    }

    ## grab stdout
    stdout <- vector("character")
    con <- textConnection("stdout", "wr", local=TRUE)
    sink(con, split = FALSE)

    returns <- suppressMessages(withCallingHandlers(tryCatch(eval(parse(text=src), envir = env), error = function(e) e), warning = warning.handler, message = message.handler))

    sink()
    close(con)
    if (length(stdout) == 0)
        stdout <- NULL

    ## error handling
    error <- grep('error', lapply(returns, function(x) class(x)))
    error <- c(error, grep('error', class(returns)))
    if (length(error) > 0) {
        error <- returns$message
        returns <- NULL
    } else
    error <- NULL

    ## warnings
    warnings <- warnings$message    # only last warning is returned!

    list(src    = src,
         output = returns,
         type   = class(returns),
         msg    = list(
             messages = messages,
             warnings = warnings,
             errors   = error),
         stdout = stdout)
}


#' Evaluate and Check R Code
#'
#' This function takes either a list of integer indices which point to position of R code in \code{body} character vector, or a vector/list of strings with actual R code, then evaluates each list element, and returns a list with four elements: a character value with R code, generated output, class of generated output and possible error/warning messages. If a graph is plotted in the given text, the returned object is a string specifying the path to the saved png in temporary directory. Please see Details below.
#'
#' If input strings are given as vector or not nested list (or even only one string), the returned list's length equals to the length of the input - as each string is evaluated as separate R code in the same environment. If a nested list is provided like \code{list(c('runif(1)', 'runif(1)'))} then all strings found in a list element is \code{\link{eval}}ed at one run so the length of returned list equals to the length of parent list. See examples below.
#'
#' As \code{\link{evals}} tries to grab the plots internally, pleas do not run commands that set graphic device or \code{\link{dev.off}} if you want to use \code{\link{evals}} to save the images and return the path of generated png(s). E.g. running \code{evals(c('png("/tmp/x.png")', 'plot(1:10)', 'dev.off()'))} would fail.
#'
#' The generated image file(s) of the plots can be fine-tuned by some specific options, please check out \code{graph.output}, \code{width}, \code{height}, \code{res}, \code{hi.res}, \code{hi.res.width}, \code{hi.res.height} and \code{hi.res.res}. Most of these options are better not to touch, see details of parameters below.
#'
#' Returned result values: list with the following elements
#' \itemize{
#'     \item \emph{src} - a character vector of specified R code.
#'     \item \emph{output} - generated output. \code{NULL} if nothing is returned. If any string returned an R object while \code{\link{eval}}ing then the \emph{last} R object will be returned as a raw R object. If a graph is plotted in the given text, the returned object is a string specifying the path to the saved png in temporary directory (see: \code{\link{tempfile}}). If multiple plots was run in the same run (see: nested lists as inputs above) then the last plot is saved. If graphic device was touched, then no other R objects will be returned.
#'     \item \emph{type} - class of generated output. "NULL" if nothing is returned, "image" if the graphic device was touched, "error" if some error occurred.
#'     \item \emph{msg} - possible messages grabbed while \code{\link{eval}}ing specified R code with the following structure:
#'     \itemize{
#'         \item \emph{messages} - string of possible diagnostic message(s)
#'         \item \emph{warnings} - string of possible warning message(s)
#'         \item \emph{errors} - string of possible error message(s)
#'     }
#'     \item \emph{stdout} - character vector of possibly printed texts to standard output (console)
#' }
#'
#' With \code{check.output} options set to \code{FALSE}, \code{\link{evals}} will not check each line of passed R code for outputs to speed up runtime. This way the user is required to pass only reliable and well structured/formatted text to \code{\link{evals}}. A list to check before running code in \code{\link{evals}}:
#'
#' \itemize{
#'     \item the code should return on the last line of the passed code (if it returns before that, it would not be grabbed),
#'     \item the code should always return something on the last line (if you do not want to return anything, add \code{NULL} as the last line),
#'     \item \code{ggplot2} and \code{\link{lattice}} graphs should be always printed (of course on the last line),
#'     \item a code part resulting in a plot should not alter variables and data sets,
#'     \item the code should be checked before live run with \code{check.output} option set to \code{TRUE} just to be sure if everything goes OK.
#' }
#'
#' Please check the examples carefully below to get a detailed overview of \code{\link{evals}}.
#' @param txt a list with character values containing R code
#' @param ind a list with numeric indices pointing to R code in \code{body}
#' @param body a character vector that contains template body
#' @param classes a vector or list of classes which should be returned. If set to \code{NULL} (by default) all R objects will be returned.
#' @param hooks list of hooks to be run for given classes in the form of \code{list(class=fn)}. If you	would also specify some parameters of the function, a list should be provided in the form of \code{list(fn, param1, param2=NULL)} etc. So the hooks would become \code{list(class1=list(fn, param1, param2=NULL), ...)}. See example below. A default hook can be specified too by setting the class to \code{'default'}. This can be handy if you do not want to define separate methods/functions to each possible class, but automatically apply the default hook to all classes not mentioned in the list. You may also specify only one element in the list like: \code{hooks=list('default' = pander.return)}. Please note, that nor error/warning messages, nor stdout is captured while running hooks!
#' @param length R object exceeding the specified length will not be returned. The default value (\code{Inf}) does not have any restrictions.
#' @param output a character vector of required returned values. See below.
#' @param env environment where evaluation takes place. If not set (by default), a new temporary environment is created.
#' @param check.output to check each line of \code{txt} for outputs. If set to \code{TRUE} you would result in some overhead as all commands have to be run twice (first to check if any output was generated and if so in which part(s), later the R objects are to be grabbed). With \code{FALSE} settings \code{evals} runs much faster, but as now checks are made, some requirements apply, see Details.
#' @param graph.nomargin should \code{evals} try to keep plots' margins minimal?
#' @param graph.name set the file name of saved plots which is a \code{\link{tempfile}} by default. A simple character string might be provided where \code{\%d} would be replaced by the index of the generating \code{txt} source, \code{\%n} with an incremented integer in \code{graph.dir} with similar file names and \code{\%t} by some random characters. Default is set to call \code{\link{tempfile}} instead.
#' @param graph.dir path to an existing directory where to place generated images. Default set to NULL as using \code{\link{tempfile}}s.
#' @param graph.output set the required file format of saved plots
#' @param width width of generated plot in pixels for even vector formats (!)
#' @param height height of generated plot in pixels for even vector formats (!)
#' @param res nominal resolution in ppi. The height and width of vector plots will be calculated based in this.
#' @param hi.res generate high resolution plots also?
#' @param hi.res.width width of generated high resolution plot in pixels for even vector formats (!)
#' @param hi.res.height height of generated high resolution plot in pixels for even vector formats (!). This value can be left blank to be automatically calculated to match original plot aspect ratio.
#' @param hi.res.res nominal resolution of high resolution plot in ppi. The height and width of vector plots will be calculated based in this. This value can be left blank to be automatically calculated to fit original plot scales.
#' @param graph.env save the environments in which plots were generated to distinct files with \code{env} extension?
#' @param graph.recordplot save the plot via \code{\link{recordPlot}} to distinct files with {recodplot} extension?
#' @param ... optional parameters passed to graphics device (e.g. \code{bg}, \code{pointsize} etc.)
#' @return a list of parsed elements each containing: src (the command run), output (what the command returns, \code{NULL} if nothing returned, path to image file if a plot was generated), type (class of returned object if any), messages: warnings (if any returned by the command run, otherwise set to \code{NULL}) and errors (if any returned by the command run, otherwise set to \code{NULL}) and possible stdout value. See Details above.
#' @seealso \code{\link{eval.msgs}}
#' @examples \dontrun{
#' # parsing line-by-line
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
#' evals('message(20)', check.output = FALSE)
#' evals('caption("FOO"); plot(1:10)')
#'
#' ## handling warnings
#' evals('chisq.test(mtcars$gear, mtcars$hp)')
#' evals(list(c('chisq.test(mtcars$gear, mtcars$am)', 'pi', 'chisq.test(mtcars$gear, mtcars$hp)')))
#' evals(c('chisq.test(mtcars$gear, mtcars$am)', 'pi', 'chisq.test(mtcars$gear, mtcars$hp)'))
#'
#' ## handling errors
#' evals('runiff(20)')
#' evals('Old MacDonald had a farm\\dots')
#' evals('## Some comment')
#' evals(list(c('runiff(20)', 'Old MacDonald had a farm?')))
#' evals(c('mean(1:10)', 'no.R.function()'))
#' evals(list(c('mean(1:10)', 'no.R.function()')))
#' evals(c('no.R.object', 'no.R.function()', 'very.mixed.up(stuff)'))
#' evals(list(c('no.R.object', 'no.R.function()', 'very.mixed.up(stuff)')))
#' evals(c('no.R.object', 'Old MacDonald had a farm\\dots', 'pi'))
#' evals(list(c('no.R.object', 'Old MacDonald had a farm\\dots', 'pi')))
#'
#' ## graph options
#' evals('plot(1:10)')
#' evals('plot(1:10)', graph.output = 'jpg')
#' evals('plot(1:10)', height = 800)
#' evals('plot(1:10)', height = 800, hi.res = T)
#' evals('plot(1:10)', graph.output = 'pdf', hi.res = T)
#' evals('plot(1:10)', res = 30)
#' evals('plot(1:10)', graph.name = 'myplot')
#' evals(list('plot(1:10)', 'plot(2:20)'), graph.name = 'myplots-%INDEX')
#' evals('plot(1:10)', graph.name = getOption('graph.name'))
#' evals('plot(1:10)', graph.env = TRUE)
#' evals(list(c('x <- runif(100)', 'plot(x)')), graph.env = TRUE)
#' evals(c('plot(1:10)', 'plot(2:20)'), graph.env = TRUE)
#' evals(list(c('x <- runif(100)', 'plot(x)'), c('y <- runif(100)', 'plot(y)')), graph.env = TRUE)
#' evals('plot(1:10)', graph.recordplot = TRUE)
#' evals('histogram(mtcars$hp)', graph.recordplot = TRUE)
#' evals(list(c('x <- runif(100)', 'plot(x)')), graph.recordplot = TRUE)
#' evals(c('plot(1:10)', 'plot(2:20)'), graph.recordplot = TRUE)
#' evals('plot(10:100)', graph.output = 'pdf')
#' evals('runif(10)', graph.output = 'pdf')
#'
#' ## hooks
#' hooks <- list('numeric' = round, 'matrix' = ascii)
#' evals(txt, hooks = hooks)
#' evals('22/7', hooks = list('numeric' = round))
#' evals('matrix(runif(25), 5, 5)', hooks = list('matrix' = round))
#'
#' ## using pander's default hook
#' evals('22/7', hooks = list('default' = pander.return))
#'
#' ## setting default hook
#' evals(c('runif(10)', 'matrix(runif(9), 3, 3)'), hooks = list('default'=round))
#' ## round all values except for matrices
#' evals(c('runif(10)', 'matrix(runif(9), 3, 3)'), hooks = list(matrix = 'print', 'default' = round))
#'
#' # advanced hooks
#' fun <- function(x, asciiformat) paste(capture.output(print(ascii(x), asciiformat)), collapse = '\n')
#' hooks <- list('numeric' = list(round, 2), 'matrix' = list(fun, "rest"))
#' evals(txt, hooks = hooks)
#'
#' # return only returned values
#' evals(txt, output = 'output')
#'
#' # return only messages (for checking syntax errors etc.)
#' evals(txt, output = 'msg')
#'
#' # check the length of returned values
#' evals('runif(10)', length = 5)
#'
#' # note the following will not be filtered!
#' evals('matrix(1,1,1)', length = 1)
#'
#' # if you do not want to let such things be eval-ed in the middle of a string use it with other filters :)
#' evals('matrix(1,1,1)', length = 1, classes = 'numeric')
#'
#'# hooks & filtering
#' evals('matrix(5,5,5)', hooks = list('matrix' = ascii), output = 'output')
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
#' @importFrom evaluate evaluate is.error is.warning is.message
evals <- function(txt = NULL, ind = NULL, body = NULL, classes = NULL, hooks = NULL, length = Inf, output = c('all', 'src', 'output', 'type', 'msg', 'stdout'), env = NULL, check.output = TRUE, graph.nomargin = TRUE, graph.name = '%t', graph.dir = tempdir(), graph.output = c('png', 'bmp', 'jpeg', 'jpg', 'tiff', 'svg', 'pdf'), width = 480, height = 480, res= 72, hi.res = FALSE, hi.res.width = 960, hi.res.height = 960*(height/width), hi.res.res = res*(hi.res.width/width), graph.env = FALSE, graph.recordplot = FALSE, ...){

    if (!xor(missing(txt), missing(ind)))
        stop('either a list of text or a list of indices should be provided')

    if (!is.null(ind)){
        if (is.null(body))
            stop('you must provide body vector')

        txt <- lapply(ind, function(x) body[x])
    }

    if (!identical(file.info(graph.dir)$isdir, TRUE))
        if (!dir.create(graph.dir, showWarnings = FALSE, recursive = TRUE))
            stop(sprintf('Something is definitely wrong with `graph.dir`: %s!', graph.dir))

    output <- match.arg(output, several.ok = TRUE)

    if (sum(grepl('all', output)) > 0)
        output <- c('src', 'output', 'type', 'msg', 'stdout')

    if (!any(is.list(hooks), is.null(hooks)))
        stop('Wrong list of hooks provided!')

    graph.output <- match.arg(graph.output)
    if (graph.output == 'jpg')
        graph.output <- 'jpeg'

    ## env for running all lines of code -> eval()
    if (is.null(env)) env <- new.env()
    if (!is.environment(env)) stop('Wrong env parameter (not an environment) provided!')
    ## env for checking output before truly eval-ing -> evaluate()
    if (check.output)
        env.evaluate <- env
    ## env for optional high resolution images while checking outputs
    if (hi.res & check.output)
        env.hires <- env

    `%d` <- 0
    lapply(txt, function(src) {

        `%d` <<- `%d` + 1

        clear.devs <- function()
            while (!is.null(dev.list()))
                dev.off(as.numeric(dev.list()[1]))

        clear.devs()

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
            if (require(lattice))
                trellis.par.set(layout.heights = list(top.padding = 0.1, bottom.padding = 0.1), layout.widths = list(right.padding = 0.1, left.padding = 0.1))
            par(mar=c(4, 4, 2.1, 0.1))
        }

        dev.control(displaylist = "enable")

        if (check.output) {
            ## running evaluate for checking outputs and grabbing warnings/errors
            eval <- suppressWarnings(tryCatch(evaluate(src, envir = env.evaluate), error = function(e) e))

            if (!is.null(dev.list())) {
                recorded.plot <- recordPlot()
                dev.control("inhibit")
            }

            ## error handling
            error <- sapply(eval, is.error)
            if (any(error)) {
                error <- paste(sapply(which(error), function(x) eval[[x]]$message), collapse = ' + ')
            } else {
                if (is.error(eval))
                    error <- eval$message
            }

            if (all(is.character(error))) {

                res <- list(src = src,
                            output = NULL,
                            type   = 'error',
                            msg    = list(
                                messages = NULL,
                                warnings = NULL,
                                errors   = error
                                ),
                            stdout = NULL
                            )

                return(res[output])
            }

            ## warnings
            warnings <- sapply(eval, is.warning)
            if (any(warnings))
                warnings <- paste(sapply(eval[warnings], function(x) x$message), collapse = " + ")
            else
                warnings <- NULL

            ## messages
            messages <- sapply(eval, is.message)
            if (any(messages))
                messages <- gsub('\n', '', paste(sapply(eval[messages], function(x) x$message), collapse = " + "))
            else
                messages <- NULL
            ### good code survived here!

            ### checking out which element produced the output               ## outRageous coding starts here
            ## removing messages/errors
            eval.no.msg <- eval[sapply(eval, function(x) {if (is.list(x)) all(names(x) == 'src') else TRUE})]
            ## which elements are the sources?
            eval.sources.n <- which(sapply(eval.no.msg, function(x) {if (!is.null(names(x))) (all(names(x) == 'src')) else FALSE}))
            ## the sources
            eval.sources <- eval.no.msg[eval.sources.n]
            ## which sources do output?
            eval.sources.outputs <- eval.sources.n[which(sapply(eval.sources.n, function(x) {
                if (x+1 > length(eval.no.msg))
                    FALSE
                else
                    class(eval.no.msg[[x+1]]) == "character"
            }))]
            ## which is the last element that produces output?              ## Rage /off
            if (length(eval.sources.outputs) > 0)
                eval.sources.last.outputs <- tail(eval.sources.outputs, 1)

            ## graph was produced?
            clear.devs()
            graph <- ifelse(exists('recorded.plot'), ifelse(is.null(recorded.plot[[1]]), FALSE, file), FALSE)
            ## any returned value?
            if (length(eval.sources.outputs) > 0) {
                if (is.logical(graph)) {

                    ## grab stdout
                    stdout <- vector("character")
                    con <- textConnection("stdout", "wr", local=TRUE)
                    sink(con, split = FALSE)

                    ## if last element returns value (happily)
                    if (eval.sources.last.outputs == tail(eval.sources.n, 1)) {
                        returns <- suppressWarnings(eval(parse(text = src), envir = env))
                    } else {    # if not the last element returns the value (lame)
                        ## eval in temp environment all elements before last element that really do output
                        env.temp <- env
                        ## run commands before the last element which does output something
                        if (eval.sources.last.outputs != 1)
                            lapply(1:(which(eval.sources.last.outputs == eval.sources.n)-1), function(i) {
                                suppressWarnings(eval(parse(text = eval.sources[[i]]$src), envir = env.temp))
                            })
                        ## grab output at last with last element with output
                        returns <- suppressWarnings(eval(parse(text = eval.sources[[eval.sources.last.outputs]]$src), envir = env.temp))
                        ## and run all stuff in main environment for consistency
                        suppressWarnings(eval(parse(text = src), envir = env))
                    }

                    sink()
                    close(con)
                    if (length(stdout) == 0)
                        stdout <- NULL

                }
            } else {
                returns <- NULL
                stdout  <- NULL
            }
        } else {
            res <- eval.msgs(src, env = env)
            if (!is.null(dev.list())) {
                recorded.plot <- recordPlot()
                dev.control("inhibit")
            }
            clear.devs()
            if (!is.null(res$msg$errors))
                return(res)
            returns  <- res$output
            warnings <- res$msg$warnings
            messages <- res$msg$messages
            graph    <- ifelse(exists('recorded.plot'), ifelse(is.null(recorded.plot[[1]]), FALSE, file), FALSE)
            stdout   <- res$stdout
        }

        ## save recorded plot on demand
        if (is.character(graph) & graph.recordplot) {
            saveRDS(recorded.plot, file = sprintf('%s.recordplot', file.name))
        }

        if (is.character(graph)) {
            returns <- graph
            class(returns) <- "image"

            ## saving environment on demand
            if (graph.env)
                save(list = ls(envir = env), file = sprintf('%s.env', file.name), envir = env)

            ## generate high resolution images if needed
            if (hi.res) {

                file.hi.res <- sprintf('%s-hires.%s', file.name, graph.output)

                if (graph.output %in% c('bmp', 'jpeg', 'png', 'tiff')) {
                    do.call(graph.output, list(file.hi.res, width = hi.res.width, height = hi.res.height, res = hi.res.res, ...))
                } else {

                    if (.Platform$OS.type == 'unix')
                        file.symlink(file, file.hi.res)
                    else
                        do.call(graph.output, list(file.hi.res, width = hi.res.width/hi.res.res, height = hi.res.height/hi.res.res, ...)) # TODO: font-family?
                }

                if ((graph.output %in% c('bmp', 'jpeg', 'png', 'tiff')) | (.Platform$OS.type != 'unix')) {
                    if (check.output)
                        suppressWarnings(eval(parse(text = src), envir = env.hires))
                    else
                        suppressWarnings(eval(parse(text = src), envir = env))
                    clear.devs()
                }

            }
        } else {

            unlink(file)

            if (hi.res & check.output)
                env.hires <- env
        }

        ## check length
        if (length(returns) > length) returns <- NULL

        ## check classes
        if (!is.null(classes))
            if (!(class(returns) %in% classes))
                returns <- NULL

        ## run hooks if specified
        if (!is.null(hooks))
            ## Q: why not inherits(returns, names(hooks)) ?
            if (class(returns) %in% names(hooks)) {
                fn <- hooks[[class(returns)]]; params <- list(returns)
                if (is.list(fn)) {
                    params <- list(returns, fn[[-1]])
                    fn <- fn[[1]]
                }
                returns <- do.call(fn, params)
            } else {
                if ('default' %in% names(hooks)) {
                    fn <- hooks[['default']]; params <- list(returns)
                    if (is.list(fn)) {
                        params <- list(returns, fn[[-1]])
                        fn <- fn[[1]]
                    }
                    returns <- do.call(fn, params)
                }
            }

        ## return list at last
        res <- list(src      = src,
                    output   = returns,
                    type     = class(returns),
                    msg      = list(
                        messages = messages,
                        warnings = warnings,
                        errors   = NULL),
                    stdout   = stdout
                    )

        return(res[output])
    })
}


