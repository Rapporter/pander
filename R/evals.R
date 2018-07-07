#' Evaluate with messages
#'
#' This function takes text(s) of R code and \code{eval}s all at one run - returning a list with four elements. See \code{Details}.
#'
#' \code{eval.msgs} returns a detailed list of the result of evaluation:
#' \itemize{
#'     \item \emph{src} - character vector of specified R code.
#'     \item \emph{result} - result of evaluation. \code{NULL} if nothing is returned. If any R code returned an R object while evaluating then the \emph{last} R object will be returned as a raw R object. If a graph is plotted in the end of the given R code (remember: \emph{last} R object), it would be automatically printed (see e.g. \code{lattice} and \code{ggplot2}).
#'      \item \emph{output} - character vector of printed version (\code{capture.output}) of \code{result}
#'     \item \emph{type} - class of generated output. 'NULL' if nothing is returned, 'error' if some error occurred.
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
#' @param showInvisible return \code{invisible} results?
#' @param graph.unify should \code{eval.msgs} try to unify the style of (\code{lattice} and \code{ggplot2}) plots? If set to \code{TRUE} (by default), some \code{panderOptions()} would apply. Please note that this argument has no effect on \code{base} plots, use \code{evals} instead.
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
eval.msgs <- function(src, env = NULL, showInvisible = FALSE, graph.unify = evalsOptions('graph.unify')) {

    if (is.null(env)) {
        env <- new.env()
    }

    ## grab warnings and messages
    warnings <- NULL
    warning.handler <- function(w) {
        warnings <<- c(warnings, w$message)
        invokeRestart('muffleWarning')
    }

    messages <- NULL
    message.handler <- function(m) {
        messages <<- c(messages, sub('\n$', '', m$message))
    }

    ## grab stdout
    stdout <- vector('character')
    con <- textConnection('stdout', 'wr', local = TRUE)
    sink(con, split = FALSE)

    result <- suppressMessages(withCallingHandlers(tryCatch(withVisible(eval(parse(text = src), envir = env)),
                                                            error = function(e) e),
                                                   warning = warning.handler, message = message.handler))

    sink()
    close(con)
    if (length(stdout) == 0)
        stdout <- NULL

    ## error handling
    if (inherits(result, 'error')) {

        error  <- result$message
        result <- NULL

        if (grepl('^<text>:[0-9]*:[0-9]: unexpected ', error)) {
            error <- sub('<text>:([0-9]*):([0-9]*): unexpected ([a-zA-Z ]*)\n.*[0-9]*:(.*)\n.*[ \t]*$',
                         'Unexpected \\3 at character \\2 in line \\1: `\\4`',
                         error)
        }

    } else
        error <- NULL

    ## check if printing is needed
    if (!is.null(result)) {
        rv  <- result$value
        rvc <- class(rv)

        if (result$visible | showInvisible) {

            ## unify images
            if (graph.unify) {

                fs <- panderOptions('graph.fontsize')
                ff <- panderOptions('graph.fontfamily')
                fc <- panderOptions('graph.fontcolor')
                gc <- panderOptions('graph.grid.color')
                gl <- panderOptions('graph.grid.lty')
                cs <- panderOptions('graph.colors')
                if (panderOptions('graph.color.rnd'))
                    cs <- sample(cs)
                cb <- cs[1] # base color
                aa <- panderOptions('graph.axis.angle')
                bc <- panderOptions('graph.background')
                pc <- panderOptions('graph.panel.background')
                tc <- ifelse(pc == 'transparent', bc, pc) # 'transparent' color
                gs <- panderOptions('graph.symbol')

                ## lattice/trellis
                if ('trellis' %in% rvc) {

                    ## margin
                    if (panderOptions('graph.nomargin')) {
                        rv$par.settings$layout.heights <- list(top.padding = 0.4,
                                                               bottom.padding = 0,
                                                               main = 1.4,
                                                               main.key.padding = 0)
                        rv$par.settings$layout.widths  <- list(right.padding = -1,
                                                               left.padding = 0.4)
                    }

                    ## font family
                    rv$par.settings$axis.text <- rv$par.settings$add.text <- rv$par.settings$par.xlab.text <- rv$par.settings$par.ylab.text <- rv$par.settings$par.zlab.text <- rv$par.settings$par.sub.text <- rv$par.settings$par.main.text <- list(fontfamily = ff, col = fc) #nolint
                    rv$par.settings$fontsize  <- list(text = fs, points = fs * 0.8)

                    ## boxes
                    rv$par.settings$strip.background$col     <- 'transparent'
                    if (!panderOptions('graph.boxes')) {
                        rv$par.settings$strip.border$col     <- 'transparent'
                        rv$par.settings$axis.line$col        <- 'transparent'
                    } else {
                        rv$par.settings$strip.border$col     <- gc
                        rv$par.settings$axis.line$col        <- gc
                    }
                    rv$par.settings$between                  <- list(x = 0.4, y = 0.4)

                    ## colors
                    rv$par.settings$background$col           <- bc
                    rv$par.settings$panel.background$col     <- pc
                    rv$par.settings$plot.line$col            <- rv$par.settings$box.rectangle$fill <- rv$par.settings$box.rectangle$col <- rv$par.settings$plot.polygon$col <- cb #nolint
                    rv$par.settings$superpose.symbol$col     <- rv$par.settings$superpose.symbol$col <- cs
                    rv$par.settings$superpose.polygon$border <- rv$par.settings$plot.polygon$border <- tc
                    rv$par.settings$box.umbrella             <- list(col = 'black', lty = 'solid', lwd = 2)
                    rv$par.settings$plot.line$lwd            <- 2
                    ## pch
                    rv$par.settings$plot.symbol$pch          <- rv$par.settings$superpose.symbol$pch <- gs
                    ##if (gs > 20 & gs <= 25 ) {}
                    rv$par.settings$plot.symbol$col          <- cb
                    rv$par.settings$superpose.symbol$col     <- cs
                    rv$par.settings$plot.symbol$fill         <- rv$par.settings$superpose.symbol$fill <- tc

                    ## grid
                    if (panderOptions('graph.grid')) {
                        rv$par.settings$reference.line$col <- gc
                        rv$par.settings$reference.line$lty <- gl
                        rv$par.settings$reference.line$lwd <- 0.7
                        rv$axis <- add.lattice.grid
                        if (panderOptions('graph.grid.minor')) {
                            rv$xscale.components <- add.lattice.xsubticks
                            rv$yscale.components <- add.lattice.ysubticks
                        }
                    }

                    ## axis angle
                    switch(aa,
                           '0' = rv$y.scales$rot <- c(90, 90),
                           '2' = rv$x.scales$rot <- c(90, 90),
                           '3' = {
                                    rv$y.scales$rot <- c(90, 90)
                                    rv$x.scales$rot <- c(90, 90)
                            })
                    ## legend
                    if (!is.null(rv$legend)) {
                        rv$legend <- rv$legend[1]
                        names(rv$legend) <- panderOptions('graph.legend.position')
                    }

                }

                ## ggplot2
                if ('ggplot' %in% rvc) {

                    ## margin
                    if (panderOptions('graph.nomargin')) {
                        rv$theme$plot.margin  <- grid::unit(c(0.1, 0.1, 0.1, 0), 'lines')
                    }

                    ## font family
                    rv$theme$plot.title       <- ggplot2::element_text(colour = fc,
                                                                       family = ff,
                                                                       face = 'bold',
                                                                       size = fs * 1.2)
                    rv$theme$axis.text.x      <- rv$theme$axis.text.y <- rv$theme$legend.text <- ggplot2::element_text(colour = fc, family = ff, face = 'plain', size = fs * 0.8) #nolint
                    rv$theme$axis.title.x     <- ggplot2::element_text(colour = fc,
                                                                       family = ff,
                                                                       face = 'plain',
                                                                       size = fs)
                    rv$theme$strip.text.x     <- ggplot2::element_text(colour = fc,
                                                                       family = ff,
                                                                       face = 'plain',
                                                                       size = fs)
                    rv$theme$axis.title.y     <- ggplot2::element_text(colour = fc,
                                                                       family = ff,
                                                                       face = 'plain',
                                                                       size = fs,
                                                                       angle = 90)
                    rv$theme$legend.title     <- ggplot2::element_text(colour = fc,
                                                                       family = ff,
                                                                       face = 'italic',
                                                                       size = fs)
                    rv$theme$strip.text.y     <- ggplot2::element_text(colour = fc,
                                                                       family = ff,
                                                                       face = 'plain',
                                                                       size = fs,
                                                                       angle = -90)

                    ## boxes
                    if (!panderOptions('graph.boxes')) {
                        rv$theme$legend.key       <- rv$theme$strip.background <- ggplot2::element_rect(colour = 'transparent', fill = 'transparent') #nolint
                        rv$theme$panel.border     <- ggplot2::element_rect(fill = NA,
                                                                           colour = tc)
                        rv$theme$panel.background <- ggplot2::element_rect(fill = pc,
                                                                           colour = tc)
                    } else {
                        rv$theme$legend.key       <- rv$theme$strip.background <- ggplot2::element_rect(colour = gc, fill = 'transparent') #nolint
                        rv$theme$panel.border     <- ggplot2::element_rect(fill = NA,
                                                                           colour = gc)
                        rv$theme$panel.background <- ggplot2::element_rect(fill = pc,
                                                                           colour = gc)
                    }

                    ## colors
                    rv$theme$plot.background  <- ggplot2::element_rect(fill = bc, colour = NA)
                    rv$theme$axis.ticks       <- ggplot2::element_line(colour = gc, size = 0.2)

                    ## point shape still has to be updated
                    for (i in length(rv$layers)) {
                        if (packageVersion('ggplot2') <= '1.0.1') {
                            if (rv$layers[[i]]$geom$objname %in% c('point')) {
                                rv$layers[[i]]$geom_params$size   <- 3
                                rv$layers[[i]]$geom_params$shape  <- gs
                            }
                        } else {
                            if (inherits(rv$layers[[i]]$geom, 'GeomPoint')) {
                                rv$layers[[i]]$aes_params$size  <- 3
                                rv$layers[[i]]$aes_params$shape <- gs
                            }
                        }
                    }

                    ## geom colors
                    if (is.null(rv$labels$colour) & is.null(rv$labels$fill)) {

                        ## update layers with one color
                        ## this is an ugly hack but `update_geom_defaults`
                        ## is not reversible :(
                        if (packageVersion('ggplot2') <= '1.0.1') {
                            for (i in 1:length(rv$layers)) {
                                if (rv$layers[[i]]$geom$objname %in% c('histogram', 'bar')) {
                                    rv$layers[[i]]$geom_params$fill   <- cs[i]
                                    rv$layers[[i]]$geom_params$colour <- tc
                                } else {
                                    if (rv$layers[[i]]$geom$objname %in% c('boxplot')) {
                                        rv$layers[[i]]$geom_params$fill   <- cs[i]
                                        rv$layers[[i]]$geom_params$colour <- 'black'
                                    } else {
                                        if (rv$layers[[i]]$geom$objname %in% c('point')) {
                                            rv$layers[[i]]$geom_params$fill   <- tc
                                            rv$layers[[i]]$geom_params$colour <- cs[i]
                                        } else
                                            rv$layers[[i]]$geom_params$colour <- cs[i]
                                    }
                                }
                            }
                        } else {
                            for (i in 1:length(rv$layers)) {
                                if (inherits(rv$layers[[i]]$geom, 'GeomBar')) {
                                    rv$layers[[i]]$aes_params$fill   <- cs[i]
                                    rv$layers[[i]]$aes_params$colour <- tc
                                } else {
                                    if (inherits(rv$layers[[i]]$geom, 'GeomBoxplot')) {
                                        rv$layers[[i]]$aes_params$fill   <- cs[i]
                                        rv$layers[[i]]$aes_params$colour <- 'black'
                                    } else {
                                        if (inherits(rv$layers[[i]]$geom, 'GeomPoint')) {
                                            rv$layers[[i]]$aes_params$fill   <- tc
                                            rv$layers[[i]]$aes_params$colour <- cs[i]
                                        } else
                                            rv$layers[[i]]$aes_params$colour <- cs[i]
                                    }
                                }
                            }
                        }

                    } else {

                        ## we have a possible color scale (only dealing with discrete scales)
                        if (is.null(rv$labels$colour)) {

                            ## the scale of geom_tile is continuous
                            if (packageVersion('ggplot2') <= '1.0.1') {
                                tile <- rv$layers[[1]]$geom$objname %in% 'tile'
                            } else {
                                tile <- inherits(rv$layers[[1]]$geom, 'GeomTile')
                            }

                            if (length(rv$scales$scales) == 0 & !tile) {
                                rv <- rv + ggplot2::scale_fill_manual(values = cs)
                            } else {
                                ## we still might have something without a guide
                                if (!'continuous' %in% unlist(lapply(rv$scales$scales, class)) & !tile)
                                    rv <- rv + ggplot2::scale_fill_manual(values = cs, guide = FALSE)
                            }

                        } else {

                            if (length(rv$scales$scales) == 0) {
                                rv <- rv + ggplot2::scale_colour_manual(values = cs)
                            } else {
                                ## we still might have something without a guide
                                if (!'continuous' %in% unlist(lapply(rv$scales$scales, class)))
                                    rv <- rv + ggplot2::scale_colour_manual(values = cs, guide = FALSE)
                            }

                        }

                        ## no solid shapes
                        rv <- rv + ggplot2::scale_shape(solid = FALSE)

                    }

                    ## grid
                    if (!panderOptions('graph.grid')) {
                        rv$theme$panel.grid.minor <- rv$theme$panel.grid.major <- ggplot2::element_blank()
                    } else {
                        if (!panderOptions('graph.grid.minor')) {
                            rv$theme$panel.grid.major <- ggplot2::element_line(colour = gc,
                                                                               size = 0.2,
                                                                               linetype = gl)
                            rv$theme$panel.grid.minor <- ggplot2::element_blank()
                        } else {
                            rv$theme$panel.grid.minor <- ggplot2::element_line(colour = gc,
                                                                               size = 0.1,
                                                                               linetype = gl)
                            rv$theme$panel.grid.major <- ggplot2::element_line(colour = gc,
                                                                               size = 0.2,
                                                                               linetype = gl)
                        }
                    }

                    ## axis angle
                    if (aa == 0) {
                        rv$theme$axis.text.y <- ggplot2::element_text(colour = fc,
                                                                      family = ff,
                                                                      face = 'plain',
                                                                      size = fs * 0.8,
                                                                      angle = 90)
                    }
                    if (aa == 2) {
                        rv$theme$axis.text.x <- ggplot2::element_text(colour = fc,
                                                                      family = ff,
                                                                      face = 'plain',
                                                                      size = fs * 0.8,
                                                                      angle = 90,
                                                                      hjust = 1)
                    }
                    if (aa == 3) {
                        rv$theme$axis.text.y <- ggplot2::element_text(colour = fc,
                                                                      family = ff,
                                                                      face = 'plain',
                                                                      size = fs * 0.8,
                                                                      angle = 90)
                        rv$theme$axis.text.x <- ggplot2::element_text(colour = fc,
                                                                      family = ff,
                                                                      face = 'plain',
                                                                      size = fs * 0.8,
                                                                      angle = 90,
                                                                      hjust = 1)
                    }

                    ## legend
                    if (!is.null(rv$theme$legend.position)) {
                        if (rv$theme$legend.position != 'none') {
                            rv$theme$legend.position <- panderOptions('graph.legend.position')
                        }
                    } else {
                        rv$theme$legend.position <- panderOptions('graph.legend.position')
                    }

                }

            }

            ## grab output
            output <- vector('character')
            con <- textConnection('output', 'wr', local = TRUE)
            sink(con, split = FALSE)

            p.result <- tryCatch(print(rv), error = function(e) e)

            ## error while printing
            if (inherits(p.result, 'error')) {
                error  <- p.result$message
            }

            result <- rv

            sink()
            close(con)

        } else {

            result <- output <- NULL

        }
    }

    if (is.null(error)) {
        type  <- rvc
    } else {
        result <- output <- NULL
        type   <- 'error'
    }

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
#'     \item \emph{type} - class of generated output. 'NULL' if nothing is returned, 'error' if some error occurred.
#'     \item \emph{msg} - possible messages grabbed while evaluating specified R code with the following structure:
#'     \itemize{
#'         \item \emph{messages} - character vector of possible diagnostic message(s)
#'         \item \emph{warnings} - character vector of possible warning message(s)
#'         \item \emph{errors} - character vector of possible error message(s)
#'     }
#'     \item \emph{stdout} - character vector of possibly printed texts to standard output (console)
#' }
#'
#' By default \code{evals} tries to \emph{cache} results. This means that if evaluation of some R commands take too much time (specified in \code{cache.time} parameter), then \code{evals} would save the results in a file and return from there on next exact R code's evaluation. This caching algorithm tries to be smart as checks not only the passed R sources, but all variables inside that and saves the hash of those.
#'
#' Technical details of the caching algorithm:
#' \itemize{
#'      \item Each passed R chunk is \code{parse}d to single commands.
#'      \item Each parsed command's part (let it be a function, variable, constant etc.) \code{eval}uated (as a \code{name}) separately to a \code{list}. This list describes the unique structure and the content of the passed R commands, and has some IMHO really great benefits (see examples below).
#'      \item A hash if computed to each list element and cached too in \code{pander}'s local environments. This is useful if you are using large data frames, just imagine: the caching algorithm would have to compute the hash for the same data frame each time it's touched! This way the hash is recomputed only if the R object with the given name is changed.
#'      \item The list is \code{serialize}d and an \code{SHA-1} hash is computed for that - which is unique and there is no real risk of collision.
#'      \item If \code{evals} can find the cached results in a file named to the computed hash, then it is returned on the spot.
#'      \item Otherwise the call is evaluated and the results are optionally saved to cache (e.g. if \code{cache} is active, if the \code{proc.time()} of the evaluation is higher then it is defined in \code{cache.time} etc.).
#' }
#'
#' This is a quite secure way of caching, but if you would encounter any issues, just set \code{cache} to \code{FALSE} or tweak other cache parameters. While setting \code{cache.dir}, please do think about what you are doing and move your \code{graph.dir} accordingly, as \code{evals} might result in returning an image file path which is not found any more on your file system!
#'
#' Also, if you have generated a plot and rendered that to e.g. \code{png} before and later try to get e.g. \code{pdf} - it would fail with \code{cache} on. Similarly you cannot render a high resolution image of a cached image, but you have to (temporary) disable caching.
#'
#' The default \code{evals} options could be set globally with \code{\link{evalsOptions}}, e.g. to switch off the cache just run \code{evalsOptions('cache', FALSE)}.
#'
#' Please check the examples carefully below to get a detailed overview of \code{\link{evals}}.
#' @param txt a character vector containing R code. This could be a list/vector of lines of code or a simple string holding R code separated by \code{;} or \code{\\n}.
#' @param parse if \code{TRUE} the provided \code{txt} elements would be merged into one string and parsed to logical chunks. This is useful if you would want to get separate results of your code parts - not just the last returned value, but you are passing the whole script in one string. To manually lock lines to each other (e.g. calling a \code{plot} and on next line adding an \code{abline} or \code{text} to it), use a plus char (\code{+}) at the beginning of each line which should be evaluated with the previous one(s). If set to \code{FALSE}, \code{evals} would not try to parse R code, it would get evaluated in separate runs - as provided. Please see examples below.
#' @param cache caching the result of R calls if set to \code{TRUE}. Please note the caching would not work if \code{parse} set to \code{FALSE} or syntax error is to be found.
#' @param cache.mode cached results could be stored in an \code{environment} in \emph{current} R session or let it be permanent on \code{disk}.
#' @param cache.dir path to a directory holding cache files if \code{cache.mode} set to \code{disk}. Default to \code{.cache} in current working directory.
#' @param cache.time number of seconds to limit caching based on \code{proc.time}. If set to \code{0}, all R commands, if set to \code{Inf}, none is cached (despite the \code{cache} parameter).
#' @param cache.copy.images copy images to new file names if an image is returned from the \emph{disk} cache? If set to \code{FALSE} (default), the cached path would be returned.
#' @param showInvisible return \code{invisible} results?
#' @param classes a vector or list of classes which should be returned. If set to \code{NULL} (by default) all R objects will be returned.
#' @param hooks list of hooks to be run for given classes in the form of \code{list(class = fn)}. If you would also specify some parameters of the function, a list should be provided in the form of \code{list(fn, param1, param2=NULL)} etc. So the hooks would become \code{list(class1=list(fn, param1, param2=NULL), ...)}. See example below. A default hook can be specified too by setting the class to \code{'default'}. This can be handy if you do not want to define separate methods/functions to each possible class, but automatically apply the default hook to all classes not mentioned in the list. You may also specify only one element in the list like: \code{hooks=list('default' = pander_return)}. Please note, that nor error/warning messages, nor stdout is captured (so: updated) while running hooks!
#' @param length any R object exceeding the specified length will not be returned. The default value (\code{Inf}) does not filter out any R objects.
#' @param output a character vector of required returned values. This might be useful if you are only interested in the \code{result}, and do not want to save/see e.g. \code{messages} or \code{print}ed \code{output}. See examples below.
#' @param env environment where evaluation takes place. If not set (by default), a new temporary environment is created.
#' @param graph.unify should \code{evals} try to unify the style of (\code{base}, \code{lattice} and \code{ggplot2}) plots? If set to \code{TRUE}, some \code{panderOptions()} would apply. By default this is disabled not to freak out useRs :)
#' @param graph.name set the file name of saved plots which is \code{\link{tempfile}} by default. A simple character string might be provided where \code{\%d} would be replaced by the index of the generating \code{txt} source, \code{\%n} with an incremented integer in \code{graph.dir} with similar file names and \code{\%t} by some unique random characters. While running in \code{\link{Pandoc.brew}} other indices could be triggered like \code{\%i} and \code{\%I}.
#' @param graph.dir path to a directory where to place generated images. If the directory does not exist, \code{evals} try to create that. Default set to \code{plots} in current working directory.
#' @param graph.output set the required file format of saved plots. Currently it could be any of  \code{grDevices}': \code{png}, \code{bmp}, \code{jpeg}, \code{jpg}, \code{tiff}, \code{svg} or \code{pdf}.
#' @param width width of generated plot in pixels for even vector formats
#' @param height height of generated plot in pixels for even vector formats
#' @param res nominal resolution in \code{ppi}. The height and width of vector images will be calculated based in this.
#' @param hi.res generate high resolution plots also? If set to \code{TRUE}, each R code parts resulting an image would be run twice.
#' @param hi.res.width width of generated high resolution plot in pixels for even vector formats
#' @param hi.res.height height of generated high resolution plot in pixels for even vector formats. This value can be left blank to be automatically calculated to match original plot aspect ratio.
#' @param hi.res.res nominal resolution of high resolution plot in ppi. The height and width of vector plots will be calculated based in this. This value can be left blank to be automatically calculated to fit original plot scales.
#' @param graph.env save the environments in which plots were generated to distinct files (based on \code{graph.name}) with \code{env} extension?
#' @param graph.recordplot save the plot via \code{recordPlot} to distinct files (based on \code{graph.name}) with \code{recodplot} extension?
#' @param graph.RDS save the raw R object returned (usually with \code{lattice} or \code{ggplot2}) while generating the plots to distinct files (based on \code{graph.name}) with \code{RDS} extension?
#' @param log an optionally passed \emph{logger name} from \pkg{futile.logger} to record all info, trace, debug and error messages. Logging to the console can be done by specifying e.g. \code{flog.namespace()}, and log to a file by previously calling \code{flog.appender} and \code{appender.file} on the given \emph{logger name}.
#' @param ... optional parameters passed to graphics device (e.g. \code{bg}, \code{pointsize} etc.)
#' @return a list of parsed elements each containing: \code{src} (the command run), \code{result} (R object: \code{NULL} if nothing returned, path to image file if a plot was generated), \code{print}ed \code{output}, \code{type} (class of returned object if any), informative/wawrning and error messages (if any returned by the command run, otherwise set to \code{NULL}) and possible \code{stdout}t value. See Details above.
#' @seealso \code{\link{eval.msgs}} \code{\link{evalsOptions}}
#' @examples \dontrun{
#' # parsing several lines of R code
#' txt <- readLines(textConnection('x <- rnorm(100)
#'   runif(10)
#'   warning('Lorem ipsum foo-bar-foo!')
#'   plot(1:10)
#'   qplot(rating, data = movies, geom = 'histogram')
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
#' ## the same commands in one string but also evaluating the `plot` with `text`
#' ## (note the leading '+' on the beginning of `text...` line)
#' txt <- 'df <- mtcars
#'  plot(mtcars$hp, pch = 19)
#'  +text(mtcars$hp, label = rownames(mtcars), pos = 4)
#'  ggplot(mtcars) + geom_point(aes(x = hp, y = mpg))'
#' evals(txt)
#' ## but it would fail without parsing
#' evals(txt, parse = FALSE)
#'
#' ## handling messages
#' evals('message(20)')
#' evals('message(20);message(20)', parse = FALSE)
#'
#' ## adding a caption to a plot
#' evals('set.caption("FOO"); plot(1:10)')
#' ## `plot` is started with a `+` to eval the codes in the same chunk
#' ## (no extra chunk with NULL result)
#' evals('set.caption("FOO"); +plot(1:10)')
#'
#' ## handling warnings
#' evals('chisq.test(mtcars$gear, mtcars$hp)')
#' evals(list(c('chisq.test(mtcars$gear, mtcars$am)', 'pi',
#'   'chisq.test(mtcars$gear, mtcars$hp)')), parse = FALSE)
#' evals(c('chisq.test(mtcars$gear, mtcars$am)',
#'   'pi',
#'   'chisq.test(mtcars$gear, mtcars$hp)'))
#'
#' ## handling errors
#' evals('runiff(20)')
#' evals('Old MacDonald had a farm\\dots')
#' evals('## Some comment')
#' evals(c('runiff(20)', 'Old MacDonald had a farm?'))
#' evals(list(c('runiff(20)', 'Old MacDonald had a farm?')), parse = FALSE)
#' evals(c('mean(1:10)', 'no.R.function()'))
#' evals(list(c('mean(1:10)', 'no.R.function()')), parse = FALSE)
#' evals(c('no.R.object', 'no.R.function()', 'very.mixed.up(stuff)'))
#' evals(list(c('no.R.object', 'no.R.function()', 'very.mixed.up(stuff)')), parse = FALSE)
#' evals(c('no.R.object', 'Old MacDonald had a farm\\dots', 'pi'))
#' evals('no.R.object;Old MacDonald had a farm\\dots;pi', parse = FALSE)
#' evals(list(c('no.R.object', 'Old MacDonald had a farm\\dots', 'pi')), parse = FALSE)
#'
#' ## graph options
#' evals('plot(1:10)')
#' evals('plot(1:10);plot(2:20)')
#' evals('plot(1:10)', graph.output = 'jpg')
#' evals('plot(1:10)', height = 800)
#' evals('plot(1:10)', height = 800, hi.res = TRUE)
#' evals('plot(1:10)', graph.output = 'pdf', hi.res = TRUE)
#' evals('plot(1:10)', res = 30)
#' evals('plot(1:10)', graph.name = 'myplot')
#' evals(list('plot(1:10)', 'plot(2:20)'), graph.name = 'myplots-%d')
#' evals('plot(1:10)', graph.env = TRUE)
#' evals('x <- runif(100);plot(x)', graph.env = TRUE)
#' evals(c('plot(1:10)', 'plot(2:20)'), graph.env = TRUE)
#' evals(c('x <- runif(100)', 'plot(x)','y <- runif(100)', 'plot(y)'), graph.env = TRUE)
#' evals(list(
#'     c('x <- runif(100)', 'plot(x)'),
#'     c('y <- runif(100)', 'plot(y)')),
#'   graph.env = TRUE, parse = FALSE)
#' evals('plot(1:10)', graph.recordplot = TRUE)
#' ## unprinted lattice plot
#' evals('histogram(mtcars$hp)', graph.recordplot = TRUE)
#'
#' ## caching
#' system.time(evals('plot(mtcars)'))
#' system.time(evals('plot(mtcars)'))                # running again to see the speed-up :)
#' system.time(evals('plot(mtcars)', cache = FALSE)) # cache disabled
#'
#' ## caching mechanism does check what's inside a variable:
#' x <- mtcars
#' evals('plot(x)')
#' x <- cbind(mtcars, mtcars)
#' evals('plot(x)')
#' x <- mtcars
#' system.time(evals('plot(x)'))
#'
#' ## stress your CPU - only once!
#' evals('x <- sapply(rep(mtcars$hp, 1e3), mean)')   # run it again!
#'
#' ## play with cache
#' require(lattice)
#' evals('histogram(rep(mtcars$hp, 1e5))')
#' ## nor run the below call
#' ## that would return the cached version of the above call :)
#' f <- histogram
#' g <- rep
#' A <- mtcars$hp
#' B <- 1e5
#' evals('f(g(A, B))')#'
#'
#' ## or switch off cache globally:
#' evalsOptions('cache', FALSE)
#' ## and switch on later
#' evalsOptions('cache', TRUE)
#'
#' ## evaluate assignments inside call to evals
#' ## changes to environments are cached properly and retreived
#' evalsOptions('cache.time', 0)
#' x <- 2
#' evals('x <- x^2')[[1]]$result
#' evals('x <- x^2; x + 1')[[2]]$result
#' evalsOptions('cache.time', 0.1)
#'
#' ## returning only a few classes
#' txt <- readLines(textConnection('rnorm(100)
#'   list(x = 10:1, y = 'Godzilla!')
#'   c(1,2,3)
#'    matrix(0,3,5)'))
#' evals(txt, classes = 'numeric')
#' evals(txt, classes = c('numeric', 'list'))
#'
#' ## hooks
#' txt <- 'runif(1:4); matrix(runif(25), 5, 5); 1:5'
#' hooks <- list('numeric' = round, 'matrix' = pander_return)
#' evals(txt, hooks = hooks)
#' ## using pander's default hook
#' evals(txt, hooks = list('default' = pander_return))
#' evals('22/7', hooks = list('numeric' = round))
#' evals('matrix(runif(25), 5, 5)', hooks = list('matrix' = round))
#'
#' ## setting default hook
#' evals(c('runif(10)', 'matrix(runif(9), 3, 3)'),
#'   hooks = list('default'=round))
#' ## round all values except for matrices
#' evals(c('runif(10)', 'matrix(runif(9), 3, 3)'),
#'   hooks = list(matrix = 'print', 'default' = round))
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
#' # if you do not want to let such things be eval-ed in the middle of a string
#' # use it with other filters :)
#' evals('matrix(1,1,1)', length = 1, classes = 'numeric')
#'
#' # hooks & filtering
#' evals('matrix(5,5,5)',
#'   hooks = list('matrix' = pander_return),
#'   output = 'result')
#'
#' # eval-ing chunks in given environment
#' myenv <- new.env()
#' evals('x <- c(0,10)', env = myenv)
#' evals('mean(x)', env = myenv)
#' rm(myenv)
#' # note: if you had not specified 'myenv', the second 'evals' would have failed
#' evals('x <- c(0,10)')
#' evals('mean(x)')
#'
#' # log
#' x <- evals('1:10', log = 'foo')
#' # trace log
#' evalsOptions('cache.time', 0)
#' x <- evals('1:10', log = 'foo')
#' x <- evals('1:10', log = 'foo')
#' # log to file
#' t <- tempfile()
#' flog.appender(appender.file(t), name = 'evals')
#' x <- evals('1:10', log = 'evals')
#' readLines(t)
#' # permanent log for all events
#' evalsOptions('log', 'evals')
#' flog.threshold(TRACE, 'evals')
#' evals('foo')
#' }
#' @export
#' @importFrom digest digest
#' @importFrom grDevices dev.list dev.off dev.control dev.list recordPlot
#' @importFrom utils packageVersion object.size
evals <- function(txt, parse = evalsOptions('parse'), cache = evalsOptions('cache'), cache.mode = evalsOptions('cache.mode'), cache.dir = evalsOptions('cache.dir'), cache.time = evalsOptions('cache.time'), cache.copy.images = evalsOptions('cache.copy.images'), showInvisible = FALSE, classes = evalsOptions('classes'), hooks = evalsOptions('hooks'), length = evalsOptions('length'), output = evalsOptions('output'), env = NULL, graph.unify = evalsOptions('graph.unify'), graph.name = evalsOptions('graph.name'), graph.dir = evalsOptions('graph.dir'), graph.output = evalsOptions('graph.output'), width = evalsOptions('width'), height = evalsOptions('height'), res = evalsOptions('res'), hi.res = evalsOptions('hi.res'), hi.res.width = evalsOptions('hi.res.width'), hi.res.height = 960 * (height / width), hi.res.res = res * (hi.res.width / width), graph.env = evalsOptions('graph.env'), graph.recordplot = evalsOptions('graph.recordplot'), graph.RDS = evalsOptions('graph.RDS'), log = evalsOptions('log'), ...) { #nolint

    if (missing(txt)) {
        stop('No R code provided to evaluate!')
    }
    txt.original <- paste(txt, collapse = '\n')

    ## logging constant
    logging <- !is.null(log) && requireNamespace('futile.logger', quietly = TRUE)

    ## parse provided code after concatenating
    if (parse) {

        txt.parsed <- tryCatch(parse(text = txt), error = function(e) e)

        ## skip parsing on syntax error and disable cache
        if (inherits(txt.parsed, 'error')) {
            cache <- FALSE
        } else {

            txt <- sapply(txt.parsed, function(x) paste(deparse(x), collapse = '\n'))

            ## return NULL on missing R code (e.g. comments)
            if (length(txt) == 0) {
                return(list(structure(list(src      = txt.original,
                                           result   = NULL,
                                           output   = NULL,
                                           type     = NULL,
                                           msg      = list(
                                               messages = NULL,
                                               warnings = NULL,
                                               errors   = NULL),
                                           stdout   = NULL
                                           ), 'class' = 'evals')))
            }

            ## (re)merge lines on demand (based on `+` at the beginning of line)
            txt.sep <- c(which(!grepl('^\\+', txt)), length(txt) + 1)
            txt <-  lapply(1:(length(txt.sep) - 1), function(i) txt[txt.sep[i] : (txt.sep[i + 1] - 1)])
            txt <- rapply(txt, function(x) sub('^\\+', '', x), how = 'replace')

        }
    } else {
        cache <- FALSE
    }

    ## check provided dirs
    if (!identical(file.info(graph.dir)$isdir, TRUE) &&
        !dir.create(graph.dir, showWarnings = FALSE, recursive = TRUE)) {
        stop(sprintf('Something is definitely wrong with `graph.dir`: %s!', graph.dir))
    }
    if (cache.mode == 'disk' &&
        !identical(file.info(cache.dir)$isdir, TRUE) &&
        !dir.create(cache.dir, showWarnings = FALSE, recursive = TRUE)) {
                stop(sprintf('Something is definitely wrong with `cache.dir`: %s!', cache.dir))
    }

    ## check provided parameters
    if ('all' %in% output) {
        output <- c('src', 'result', 'output', 'type', 'msg', 'stdout')
    } else if (length(setdiff(output, c('src', 'result', 'output', 'type', 'msg', 'stdout'))) != 0) {
        stop('Wrong parameter supplied to output')
    }

    if (!is.null(hooks) && !is.list(hooks)) {
        stop('Wrong list of hooks provided!')
    }

    if (!is.character(graph.name)) {
        stop('Wrong graph.name (!character) specified!')
    }

    if (!is.na(graph.output) && graph.output == 'jpg') {
        graph.output <- 'jpeg'
    }

    ## env for running all lines of code
    if (is.null(env) || !is.environment(env)) {
        env <- new.env()
    }
    for (p in c('plot', 'barplot', 'lines', 'pie', 'boxplot', 'polygon',
                'points', 'legend', 'hist', 'pairs', 'stripchart')) {
        if (exists(p, envir = env, inherits = FALSE)) {
            stop(paste0('Using a reserved word as variable: `', p, '`'))
        }
    }

    `%d` <- 0

    ## main loop
    lapply(txt, function(src) {

        ## log R expression
        if (logging) {
            futile.logger::flog.info(paste('Command run:', gsub('[ ]+', ' ', gsub('\n', ' ', src))), name = log)
        }

        if (!is.na(graph.output)) {

            ## get image file name
            `%d` <<- `%d` + 1
            file.name <- gsub('%d', `%d`, graph.name, fixed = TRUE)

            ## chunk ID
            if (length(debug$chunkID) > 0) {
                if ((length(debug$nestedID) > 0) && (debug$nestedID > 1)) {
                    file.name <- gsub('%i', paste0(debug$nestedID, '_', debug$chunkID), file.name, fixed = TRUE)
                } else {
                    file.name <- gsub('%i', debug$chunkID, file.name, fixed = TRUE)
                }
            }
            if (length(debug$cmdID) > 0) {
                assign('cmdID', debug$cmdID + 1, envir = debug)
                file.name <- gsub('%I', debug$cmdID, file.name, fixed = TRUE)
            }
            file <- sprintf('%s.%s', file.name, graph.output)

            ## tempfile
            if (grepl('%t', graph.name)) {
                if (length(strsplit(sprintf('placeholder%splaceholder', file.name), '%t')[[1]]) > 2) {
                    stop('File name contains more then 1 "%t"!')
                }
                rep <- strsplit(file, '%t')[[1]]
                file <- tempfile(pattern = rep[1], tmpdir = graph.dir, fileext = rep[2])
                file.name <- sub(sprintf('.%s$', graph.output), '', file)
            } else {
                file <- file.path(graph.dir, file)
                file.name <- file.path(graph.dir, file.name)
            }
            file <- gsub('\\', '/', file, fixed = TRUE)
            file.name <- gsub('\\', '/', file.name, fixed = TRUE)

            ## similar files counter
            if (grepl('%n', file.name)) {
                if (length(strsplit(sprintf('placeholder%splaceholder', file.name), '%n')[[1]]) > 2) {
                    stop('File name contains more then 1 "%n"!')
                }
                similar.files <- list.files(graph.dir, pattern = sprintf('^%s\\.(jpeg|tiff|png|svg|bmp|pdf)$', gsub('%t', '[a-z0-9]*', gsub('%d|%n|%i', '[[:digit:]]*', basename(file.name))))) #nolint
                if (length(similar.files) > 0) {
                    similar.files <- sub('\\.(jpeg|tiff|png|svg|bmp|pdf)$', '', similar.files)
                    rep <- gsub('%t', '[a-z0-9]*',
                                gsub('%d|%i', '[[:digit:]]*',
                                     strsplit(basename(file.name), '%n')[[1]]))
                    `%n` <- max(as.numeric(gsub(paste(rep, collapse = '|'), '', similar.files))) + 1
                } else {
                    `%n` <- 1
                }
                file.name <- gsub('%n', `%n`, file.name, fixed = TRUE)
                file <- gsub('%n', `%n`, file, fixed = TRUE)
            }

        }

        ## checking cache
        if (cache) {

            ## helper functions
            hash_of_eval_or_deparse <- function(x, x.deparse) {
                ## compute the hash of the given 'name' by evaluating that or by deparsing if the prior would fail
                v <- tryCatch(eval(x, envir = env), error = function(e) e)
                if (inherits(v, 'error')) {
                    return(digest(x.deparse))
                }
                hash_from_cache(v, x.deparse)
            }

            hash_from_cache <- function(x, x.deparse) {
                ## get the hash of the object from local cache if possible, compute it and save to cache otherwise
                if (exists(x.deparse, envir = hash.cache.obj, inherits = FALSE)) {
                    if (identical(x, get(x.deparse, envir = hash.cache.obj))) {
                        assign(x.deparse, as.integer(Sys.time()), envir = hash.cache.last.used)
                        return(get(x.deparse, envir = hash.cache.hash))
                    }
                }
                x.hash <- digest(x, 'sha1')
                assign(x.deparse, x, envir = hash.cache.obj)
                assign(x.deparse, x.hash, envir = hash.cache.hash)
                assign(x.deparse, as.integer(Sys.time()), envir = hash.cache.last.used)
                return(x.hash)
            }

            get_call_parts <- function(call) {
                ## extracting each function's and variable's hash from the call
                lapply(call, function(x){
                           switch(mode(x),
                                  'name' = hash_of_eval_or_deparse(x, deparse(x)),
                                  'call' = get_call_parts(x),
                                  digest(deparse(x), 'sha1')
                                  )
                       })
            }

            ## get the hash of the call based on the hash of all `names`
            cached <- digest(list(call = get_call_parts(parse(text = src)),
                                  storage = digest(list(storage, panderOptions(), evalsOptions()), 'sha1')),
                             'sha1')

            if (cache.mode == 'disk') {

                cached <- file.path(cache.dir, cached)
                cached.env <- paste0(cached, '.ENV')

                ## load cached result
                if (file.exists(cached)) {
                    cached.result <- readRDS(cached)
                }

                ## load the modified R objects of the cached code
                if (file.exists(cached.env)) {
                    load(file = cached.env, envir = env)
                }

            } else {
                ## cache is in environment
                ## load cached result
                if (exists(cached, envir = cached.results, inherits = FALSE)) {
                    cached.result <- get(cached, envir = cached.results)
                }

                ## if the cached expression changed the environment (for example assignment),
                ## retrieve the respected changes too (see examples)
                if (exists(cached, envir = cached.environments, inherits = FALSE)) {
                    cached.objs <- get(cached, envir = cached.environments)
                    sapply(names(cached.objs), function(x) assign(x, cached.objs[[x]], envir = env))
                }

            }

            if (exists('cached.result')) {

                if (inherits(cached.result$result, 'image')) {

                    if (cache.copy.images && cache.mode == 'disk' && !is.na(graph.output)) {

                        ## we are copying img file + possibly extra from cache dir
                        file.copy(paste0(cached, '.', graph.output), file)
                        if (graph.recordplot) {
                            file.copy(paste0(cached, '.recordedplot'), sprintf('%s.recordplot', file.name))
                        }
                        if (graph.RDS) {
                            file.copy(paste0(cached, '.RDS'), sprintf('%s.RDS', file.name))
                        }
                        if (graph.env) {
                            file.copy(paste0(cached, '.env'), sprintf('%s.env', file.name))
                        }
                        if (hi.res) {
                            file.copy(paste0(cached, '-hires.', graph.output),
                                      sprintf('%s-hires.%s', file.name, graph.output))
                        }

                        cached.result$result <- file
                        class(cached.result$result) <- 'image'
                        if (logging) {
                            futile.logger::flog.trace(paste('Image copied from cache:', file), name = log)
                        }
                        return(cached.result)

                    } else {

                        ## we are checking in plots' dir if the img file exists
                        cached.image.file <- as.character(cached.result$result)
                        if (file.exists(cached.image.file)) {
                            if (logging) {
                                futile.logger::flog.trace(paste('Image found in cache:', cached.image.file), name = log)
                            }
                            return(cached.result)
                        } else {
                            warning(sprintf('The image file referenced in cache (%s) is no longer available: the image is recreated (%s).', shQuote(cached.image.file), shQuote(file)), call. = FALSE) #nolint
                        }

                    }

                } else {
                    if (logging) {
                        futile.logger::flog.trace('Returning cached R object.', name = log)
                    }
                    return(cached.result)
                }

            } # cached result not found

            ## starting timer
            timer <- proc.time()

        }

        ## clear graphics device (if there would be any open)
        clear.devs <- function() {
            if (!is.na(graph.output)) {
                sapply(dev.list(), dev.off)
            }
        }
        clear.devs()

        if (!is.na(graph.output)) {

            ## init (potential) img file
            pbg <- panderOptions('graph.background')
            if (graph.output %in% c('bmp', 'jpeg', 'png', 'tiff')) {
                if (capabilities('cairo')) {
                    do.call(graph.output, list(file,
                                               width = width,
                                               height = height,
                                               res = res,
                                               bg = pbg,
                                               type = 'cairo', ...))
                } else {
                    do.call(graph.output, list(file,
                                               width = width,
                                               height = height,
                                               res = res,
                                               bg = pbg, ...))
                }
            }
            if (graph.output == 'svg') {
                do.call(graph.output, list(file,
                                           width = width / res,
                                           height = height / res,
                                           bg = pbg, ...)) # TODO: font-family?
            }
            if (graph.output == 'pdf') {
                do.call('cairo_pdf', list(file,
                                          width = width / res,
                                          height = height / res,
                                          bg = pbg, ...)) # TODO: font-family?
            }

            ## start recordPlot
            dev.control(displaylist = 'enable')
        }

        ## if caching: save the initial environment's objects' hashes
        if (cache) {
            objs <- ls(envir = env)
            objs <- setdiff(objs, c('.storage', 'showCode', 'showText'))
            objs.hash <- sapply(objs, function(x) hash_of_eval_or_deparse(as.name(x), x))
        }

        ## add modified base plot functions to update colors, `par` settings and adding a grid
        if (graph.unify) {
            sapply(ls(envir = masked.plots), function(x) {
                assign(x, get(x, envir = masked.plots, inherits = FALSE), envir = env)
            })
        }

        ## env for optional high resolution images
        if (hi.res && !is.na(graph.output)) {
            env.hires <- env
        }

        ## eval
        res <- eval.msgs(src, env = env, showInvisible = showInvisible, graph.unify = graph.unify)

        ## grab recorded.plot
        if (!is.na(graph.output) && !is.null(dev.list())) {
            recorded.plot <- recordPlot()
            dev.control('inhibit')
        }

        ## did we produce a plot?
        if (is.na(graph.output)) {
            graph <- FALSE
        } else {
            graph <- ifelse(exists('recorded.plot'), ifelse(is.null(recorded.plot[[1]]), FALSE, file), FALSE)
        }

        ## close grDevice
        clear.devs()

        ## check if image file was created
        if (is.character(graph)) {
            if (!file.exists(file)) {
                res$msg$errors <- c(res$msg$errors, paste('Image file not written by:', paste(src, collapse = ';')))
            }
        }

        ## remove dummy img file (1px) on Windows if created
        if (grepl('w|W', .Platform$OS.type)) {
            if (!is.character(graph)) {
                if (file.exists(file)) {
                    unlink(file)
                }
            }
        }

        ## error handling
        if (!is.null(res$msg$errors)) {

            ## removing injected base::plot fns
            if (graph.unify) {
                rm(list = ls(envir = masked.plots), envir = env)
            }

            class(res) <- 'evals'
            if ('plot.new has not been called yet' %in% res$msg$errors) {
                res$msg$errors <- 'plot.new has not been called yet - Please note that all R commands are parsed and evaluated separately. To override this default behavior, add a plus sign (+) as the first character of the line(s) to evaluate with the prior one(s).' #nolint
            }
            if (logging) {
                futile.logger::flog.error(res$msg$errors, name = log)
            }
            return(res)
        }

        result <- res$result

        ## we have a graph
        if (is.character(graph)) {

            ## log image file name
            if (logging) {
                futile.logger::flog.trace(paste('Image file written:', file), name = log)
            }

            ## save recorded plot on demand
            if (graph.recordplot) {
                saveRDS(recorded.plot, file = sprintf('%s.recordplot', file.name))
            }

            ## save plot RDS on demand
            if (graph.RDS) {
                if (!is.null(result)) {
                    saveRDS(result, file = sprintf('%s.RDS', file.name))
                }
            }

            result <- graph
            class(result) <- 'image'

            ## saving environment on demand
            if (graph.env) {
                save(list = ls(envir = env), file = sprintf('%s.env', file.name), envir = env)
            }

            ## generate high resolution images on demand
            if (hi.res) {

                file.hi.res <- sprintf('%s-hires.%s', file.name, graph.output)

                ## initialize high resolution image file
                if (graph.output %in% c('bmp', 'jpeg', 'png', 'tiff')) {

                    if (capabilities('cairo')) {
                        do.call(graph.output, list(file.hi.res,
                                                   width = hi.res.width,
                                                   height = hi.res.height,
                                                   res = hi.res.res,
                                                   bg = pbg,
                                                   type = 'cairo', ...))
                    } else {
                        do.call(graph.output, list(file.hi.res,
                                                   width = hi.res.width,
                                                   height = hi.res.height,
                                                   res = hi.res.res,
                                                   bg = pbg, ...))
                    }

                } else {

                    if (.Platform$OS.type == 'unix') {
                        ## a symlink would be fine for vector formats on a unix-like OS
                        file.symlink(file, file.hi.res)
                    } else {
                        ## we have no option to do so on Windows (to be backward compatible)
                        do.call(graph.output, list(file.hi.res,
                                                   width = hi.res.width / hi.res.res,
                                                   height = hi.res.height / hi.res.res,
                                                   bg = pbg, ...)) # TODO: font-family?
                    }
                }

                ## render high resolution image (if needed)
                if ((graph.output %in% c('bmp', 'jpeg', 'png', 'tiff')) | (.Platform$OS.type != 'unix')) {
                    ## we need eval.msgs() here instead of simple eval()
                    ## to prevent unprinted lattice/ggplot2 objects' issues
                    eval.msgs(src, env = env.hires)
                    clear.devs()
                }

                ## add 'href' attribute to returned R object
                attr(result, 'href') <- file.hi.res
            }
        }

        ## check length
        if (length(result) > length) {
            result <- NULL
        }

        ## check classes
        if (!is.null(classes)) {
            if (!inherits(result, classes)) {
                result <- output <- NULL
            }
        }

        ## caption
        if (!is.null(storage$caption) & !is.null(result)) {
            attr(result, 'caption') <- get.caption()
        }

        ## alignment of tables
        if (!is.null(storage$alignment) & !is.null(result)) {
            attr(result, 'alignment') <- get.alignment(result)
        }

        ## highlight cells
        if (!is.null(result)) {
            result <- get.emphasize(result)
        }

        ## run hooks if specified
        if (!is.null(hooks)) {
            hook.name <- ifelse(inherits(result, names(hooks)), class(result), 'default')
            fn <- hooks[[hook.name]];
            params <- list(result)
            if (!is.null(fn)) {
                if (is.list(fn)) {
                    params <- list(result, fn[[-1]])
                    fn <- fn[[1]]
                }
                if (logging) {
                    futile.logger::flog.trace(paste('Calling hook for', hook.name), name = log)
                }
                result <- do.call(fn, params)
            }
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

        ## removing injected base::plot fns
        if (graph.unify) {
            rm(list = ls(envir = masked.plots), envir = env)
        }

        ## save to cache
        if (cache) {

            ## comparing the resulting environment's objects' hashes with the original ones
            objs.res      <- ls(envir = env)
            objs.res      <- setdiff(objs.res, c('.storage', 'showCode', 'showText', '.graph.dir', '.graph.name'))
            objs.res.hash <- sapply(objs.res, function(x) hash_of_eval_or_deparse(as.name(x), x))
            change        <- setdiff(objs.res, objs)
            common        <- intersect(objs.res, objs)
            changed       <- c(change, unlist(sapply(common, function(x) {
                                                                if (objs.hash[[x]] != objs.res.hash[[x]])
                                                                    return(x)
                                                            },
                                                     USE.NAMES = FALSE)))
            if (cache.mode == 'disk') {
                if (as.numeric(proc.time() - timer)[3] > cache.time) {

                    ## save cached result
                    saveRDS(res, file = cached)

                    ## save the modified R objects of the cached code
                    if (length(changed) > 0) {
                        save(list = changed, envir = env, file = paste0(cached, '.ENV'))
                    }

                    ## save plot related files
                    if ('image' %in% class(result)) {
                        file.copy(file, paste0(cached, '.', graph.output))
                        if (graph.recordplot) {
                            file.copy(sprintf('%s.recordplot', file.name), paste0(cached, '.recordedplot'))
                        }
                        if (graph.RDS) {
                            if (!is.null(result)) {
                                file.copy(sprintf('%s.RDS', file.name), paste0(cached, '.RDS'))
                            }
                        }
                        if (graph.env) {
                            file.copy(sprintf('%s.env', file.name), paste0(cached, '.env'))
                        }
                        if (hi.res) {
                            file.copy(sprintf('%s-hires.%s', file.name, graph.output),
                                      paste0(cached, '-hires.', graph.output))
                        }
                    }
                    if (logging) {
                        futile.logger::flog.trace('Cached result', name = log)
                    }
                }

            } else {

                if (as.numeric(proc.time() - timer)[3] > cache.time) {

                    ## save cached result
                    assign(cached, res, envir = cached.results)

                    ## save changes to environment so when expression result is retreived from cache
                    ## changes to environment will be retreived also (see example)
                    if (length(changed) > 0) {
                        assign(cached, mget(changed, envir = env), envir = cached.environments)
                    }

                    if (logging) {
                        futile.logger::flog.trace('Cached result', name = log)
                    }
                }
            }
        }

        ## log
        if (logging) {
            if (!is.null(res$msg$warnings)) {
                futile.logger::flog.warn(res$msg$warnings, name = log)
            }
            if (!is.null(res$result) && res$type != 'image') {
                futile.logger::flog.debug(paste0(
                    'Returned object: class = ',
                    res$type,
                    ', length = ',
                    length(res$result),
                    ', dim = ',
                    paste(dim(res$result), collapse = '/'),
                    ', size = ',
                    object.size(res$result),
                    ' bytes'
                ), name = log)
            }
        }

        ## return
        res

    })
}

#' Redraws plot saved in file
#'
#' This function is a wrapper around \code{redrawPlot}.
#' @param file path and name of an rds file containing a plot object to be redrawn
#' @references Thanks to Jeroen Ooms \url{https://stat.ethz.ch/pipermail/r-devel/2012-January/062973.html}, JJ Allaire \url{https://github.com/rstudio/rstudio/commit/eb5f6f1db4717132c2ff111f068ffa6e8b2a5f0b}, and Gabriel Becker.
#' @seealso \code{\link{evals}}
#' @export
redraw.recordedplot <- function(file) {

    plot <- tryCatch(readRDS(file), error = function(e) e)

    if (inherits(plot, 'error')) {
        stop(paste('Cannot read file:', plot$message))
    }

    redrawPlot(plot)
}


#' Redraw a recordedplot, grid, trellis, or ggplot2 plot.
#'
#' This function redraws the plot represented by \code{rec_plot}. It can redraw grid/trellis/ggplot2/etc plots, as well as \code{recordedplot} objects. For \code{recordedplot} objects it acts as a wrapper around \code{replayPlot} with memory tweaks to fix native symbol address errors when the recordedplot was loaded from an rda/rds file.
#' @param rec_plot the plot object to redraw
#' @references Thanks to Jeroen Ooms \url{https://stat.ethz.ch/pipermail/r-devel/2012-January/062973.html}, JJ Allaire \url{https://github.com/rstudio/rstudio/commit/eb5f6f1db4717132c2ff111f068ffa6e8b2a5f0b}, and Gabriel Becker.
#' @seealso \code{\link{redraw.recordedplot}}
#' @export
#' @importFrom methods is
#' @importFrom grDevices replayPlot
redrawPlot <- function(rec_plot) {
    ## this allows us to deal with trellis/grid/ggplot objects as well ...
    if (!is(rec_plot, 'recordedplot')) {
        res <- try(print(rec_plot))
        if (is(res, 'error')) {
            stop(res)
        }
    } else {
        if (getRversion() < '3.0.0') {
            for (i in 1:length(rec_plot[[1]])) {
                #@jeroenooms
                if ('NativeSymbolInfo' %in% class(rec_plot[[1]][[i]][[2]][[1]])) {
                    rec_plot[[1]][[i]][[2]][[1]] <- getNativeSymbolInfo(rec_plot[[1]][[i]][[2]][[1]]$name)
                }
            }
        } else {
            for (i in 1:length(rec_plot[[1]])) {
                #@jjallaire
                symbol <- rec_plot[[1]][[i]][[2]][[1]]
                if ('NativeSymbolInfo' %in% class(symbol)) {
                    if (!is.null(symbol$package)) {
                        name <- symbol$package[['name']]
                    } else {
                        name <- symbol$dll[['name']]
                    }
                    pkg_dll <- getLoadedDLLs()[[name]]
                    native_sumbol <- getNativeSymbolInfo(name = symbol$name,
                                                        PACKAGE = pkg_dll, withRegistrationInfo = TRUE)
                    rec_plot[[1]][[i]][[2]][[1]] <- native_sumbol
                }
            }
        }
        if (is.null(attr(rec_plot, 'pid')) || attr(rec_plot, 'pid') != Sys.getpid()) {
            warning('Loading plot snapshot from a different session with possible side effects or errors.')
            attr(rec_plot, 'pid') <- Sys.getpid()
        }
        suppressWarnings(replayPlot(rec_plot))
    }
}
