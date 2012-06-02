\name{evals}
\alias{evals}
\title{Evaluate and Check R Code}
\usage{
  evals(txt = NULL, ind = NULL, body = NULL,
    classes = NULL, hooks = NULL, length = Inf,
    output = c("all", "src", "output", "type", "msg", "stdout"),
    env = NULL, check.output = TRUE, graph.nomargin = TRUE,
    graph.name = "\%t", graph.dir = tempdir(),
    graph.output = c("png", "bmp", "jpeg", "jpg", "tiff", "svg", "pdf"),
    width = 480, height = 480, res = 72, hi.res = FALSE,
    hi.res.width = 960,
    hi.res.height = 960 * (height/width),
    hi.res.res = res * (hi.res.width/width),
    graph.env = FALSE, graph.recordplot = FALSE, ...)
}
\arguments{
  \item{txt}{a list with character values containing R
  code}

  \item{ind}{a list with numeric indices pointing to R code
  in \code{body}}

  \item{body}{a character vector that contains template
  body}

  \item{classes}{a vector or list of classes which should
  be returned. If set to \code{NULL} (by default) all R
  objects will be returned.}

  \item{hooks}{list of hooks to be run for given classes in
  the form of \code{list(class=fn)}. If you would also
  specify some parameters of the function, a list should be
  provided in the form of \code{list(fn, param1,
  param2=NULL)} etc. So the hooks would become
  \code{list(class1=list(fn, param1, param2=NULL), ...)}.
  See example below. A default hook can be specified too by
  setting the class to \code{'default'}. This can be handy
  if you do not want to define separate methods/functions
  to each possible class, but automatically apply the
  default hook to all classes not mentioned in the list.
  You may also specify only one element in the list like:
  \code{hooks=list('default' = pander.return)}. Please
  note, that nor error/warning messages, nor stdout is
  captured while running hooks!}

  \item{length}{R object exceeding the specified length
  will not be returned. The default value (\code{Inf}) does
  not have any restrictions.}

  \item{output}{a character vector of required returned
  values. See below.}

  \item{env}{environment where evaluation takes place. If
  not set (by default), a new temporary environment is
  created.}

  \item{check.output}{to check each line of \code{txt} for
  outputs. If set to \code{TRUE} you would result in some
  overhead as all commands have to be run twice (first to
  check if any output was generated and if so in which
  part(s), later the R objects are to be grabbed). With
  \code{FALSE} settings \code{evals} runs much faster, but
  as now checks are made, some requirements apply, see
  Details.}

  \item{graph.nomargin}{should \code{evals} try to keep
  plots' margins minimal?}

  \item{graph.name}{set the file name of saved plots which
  is a \code{\link{tempfile}} by default. A simple
  character string might be provided where \code{\%d} would
  be replaced by the index of the generating \code{txt}
  source, \code{\%n} with an incremented integer in
  \code{graph.dir} with similar file names and \code{\%t}
  by some random characters. Default is set to call
  \code{\link{tempfile}} instead.}

  \item{graph.dir}{path to an existing directory where to
  place generated images. Default set to NULL as using
  \code{\link{tempfile}}s.}

  \item{graph.output}{set the required file format of saved
  plots}

  \item{width}{width of generated plot in pixels for even
  vector formats (!)}

  \item{height}{height of generated plot in pixels for even
  vector formats (!)}

  \item{res}{nominal resolution in ppi. The height and
  width of vector plots will be calculated based in this.}

  \item{hi.res}{generate high resolution plots also?}

  \item{hi.res.width}{width of generated high resolution
  plot in pixels for even vector formats (!)}

  \item{hi.res.height}{height of generated high resolution
  plot in pixels for even vector formats (!). This value
  can be left blank to be automatically calculated to match
  original plot aspect ratio.}

  \item{hi.res.res}{nominal resolution of high resolution
  plot in ppi. The height and width of vector plots will be
  calculated based in this. This value can be left blank to
  be automatically calculated to fit original plot scales.}

  \item{graph.env}{save the environments in which plots
  were generated to distinct files with \code{env}
  extension?}

  \item{graph.recordplot}{save the plot via
  \code{\link{recordPlot}} to distinct files with
  {recodplot} extension?}

  \item{...}{optional parameters passed to graphics device
  (e.g. \code{bg}, \code{pointsize} etc.)}
}
\value{
  a list of parsed elements each containing: src (the
  command run), output (what the command returns,
  \code{NULL} if nothing returned, path to image file if a
  plot was generated), type (class of returned object if
  any), messages: warnings (if any returned by the command
  run, otherwise set to \code{NULL}) and errors (if any
  returned by the command run, otherwise set to
  \code{NULL}) and possible stdout value. See Details
  above.
}
\description{
  This function takes either a list of integer indices
  which point to position of R code in \code{body}
  character vector, or a vector/list of strings with actual
  R code, then evaluates each list element, and returns a
  list with four elements: a character value with R code,
  generated output, class of generated output and possible
  error/warning messages. If a graph is plotted in the
  given text, the returned object is a string specifying
  the path to the saved png in temporary directory. Please
  see Details below.
}
\details{
  If input strings are given as vector or not nested list
  (or even only one string), the returned list's length
  equals to the length of the input - as each string is
  evaluated as separate R code in the same environment. If
  a nested list is provided like \code{list(c('runif(1)',
  'runif(1)'))} then all strings found in a list element is
  \code{\link{eval}}ed at one run so the length of returned
  list equals to the length of parent list. See examples
  below.

  As \code{\link{evals}} tries to grab the plots
  internally, pleas do not run commands that set graphic
  device or \code{\link{dev.off}} if you want to use
  \code{\link{evals}} to save the images and return the
  path of generated png(s). E.g. running
  \code{evals(c('png("/tmp/x.png")', 'plot(1:10)',
  'dev.off()'))} would fail.

  The generated image file(s) of the plots can be
  fine-tuned by some specific options, please check out
  \code{graph.output}, \code{width}, \code{height},
  \code{res}, \code{hi.res}, \code{hi.res.width},
  \code{hi.res.height} and \code{hi.res.res}. Most of these
  options are better not to touch, see details of
  parameters below.

  Returned result values: list with the following elements
  \itemize{ \item \emph{src} - a character vector of
  specified R code.  \item \emph{output} - generated
  output. \code{NULL} if nothing is returned. If any string
  returned an R object while \code{\link{eval}}ing then the
  \emph{last} R object will be returned as a raw R object.
  If a graph is plotted in the given text, the returned
  object is a string specifying the path to the saved png
  in temporary directory (see: \code{\link{tempfile}}). If
  multiple plots was run in the same run (see: nested lists
  as inputs above) then the last plot is saved. If graphic
  device was touched, then no other R objects will be
  returned.  \item \emph{type} - class of generated output.
  "NULL" if nothing is returned, "image" if the graphic
  device was touched, "error" if some error occurred.
  \item \emph{msg} - possible messages grabbed while
  \code{\link{eval}}ing specified R code with the following
  structure: \itemize{ \item \emph{messages} - string of
  possible diagnostic message(s) \item \emph{warnings} -
  string of possible warning message(s) \item \emph{errors}
  - string of possible error message(s) } \item
  \emph{stdout} - character vector of possibly printed
  texts to standard output (console) }

  With \code{check.output} options set to \code{FALSE},
  \code{\link{evals}} will not check each line of passed R
  code for outputs to speed up runtime. This way the user
  is required to pass only reliable and well
  structured/formatted text to \code{\link{evals}}. A list
  to check before running code in \code{\link{evals}}:

  \itemize{ \item the code should return on the last line
  of the passed code (if it returns before that, it would
  not be grabbed), \item the code should always return
  something on the last line (if you do not want to return
  anything, add \code{NULL} as the last line), \item
  \code{ggplot2} and \code{\link{lattice}} graphs should be
  always printed (of course on the last line), \item a code
  part resulting in a plot should not alter variables and
  data sets, \item the code should be checked before live
  run with \code{check.output} option set to \code{TRUE}
  just to be sure if everything goes OK. }

  Please check the examples carefully below to get a
  detailed overview of \code{\link{evals}}.
}
\examples{
\dontrun{
# parsing line-by-line
txt <- readLines(textConnection('x <- rnorm(100)
  runif(10)
  warning("Lorem ipsum foo-bar-foo!")
  plot(1:10)
  qplot(rating, data = movies, geom = "histogram")
  y <- round(runif(100))
  cor.test(x, y)
  crl <- cor.test(runif(10), runif(10))
  table(mtcars$am, mtcars$cyl)
  ggplot(mtcars) + geom_point(aes(x = hp, y = mpg))'))
evals(txt)

## parsing a list of commands
txt <- list('df <- mtcars',
 c('plot(mtcars$hp, pch = 19)','text(mtcars$hp, label = rownames(mtcars), pos = 4)'),
 'ggplot(mtcars) + geom_point(aes(x = hp, y = mpg))')
evals(txt)

## returning only a few classes
txt <- readLines(textConnection('rnorm(100)
  list(x = 10:1, y = "Godzilla!")
  c(1,2,3)
   matrix(0,3,5)'))
evals(txt, classes = 'numeric')
evals(txt, classes = c('numeric', 'list'))

## handling messages
evals('message(20)')
evals('message(20)', check.output = FALSE)
evals('caption("FOO"); plot(1:10)')

## handling warnings
evals('chisq.test(mtcars$gear, mtcars$hp)')
evals(list(c('chisq.test(mtcars$gear, mtcars$am)', 'pi', 'chisq.test(mtcars$gear, mtcars$hp)')))
evals(c('chisq.test(mtcars$gear, mtcars$am)', 'pi', 'chisq.test(mtcars$gear, mtcars$hp)'))

## handling errors
evals('runiff(20)')
evals('Old MacDonald had a farm\\\\dots')
evals('## Some comment')
evals(list(c('runiff(20)', 'Old MacDonald had a farm?')))
evals(c('mean(1:10)', 'no.R.function()'))
evals(list(c('mean(1:10)', 'no.R.function()')))
evals(c('no.R.object', 'no.R.function()', 'very.mixed.up(stuff)'))
evals(list(c('no.R.object', 'no.R.function()', 'very.mixed.up(stuff)')))
evals(c('no.R.object', 'Old MacDonald had a farm\\\\dots', 'pi'))
evals(list(c('no.R.object', 'Old MacDonald had a farm\\\\dots', 'pi')))

## graph options
evals('plot(1:10)')
evals('plot(1:10)', graph.output = 'jpg')
evals('plot(1:10)', height = 800)
evals('plot(1:10)', height = 800, hi.res = T)
evals('plot(1:10)', graph.output = 'pdf', hi.res = T)
evals('plot(1:10)', res = 30)
evals('plot(1:10)', graph.name = 'myplot')
evals(list('plot(1:10)', 'plot(2:20)'), graph.name = 'myplots-\%INDEX')
evals('plot(1:10)', graph.name = getOption('graph.name'))
evals('plot(1:10)', graph.env = TRUE)
evals(list(c('x <- runif(100)', 'plot(x)')), graph.env = TRUE)
evals(c('plot(1:10)', 'plot(2:20)'), graph.env = TRUE)
evals(list(c('x <- runif(100)', 'plot(x)'), c('y <- runif(100)', 'plot(y)')), graph.env = TRUE)
evals('plot(1:10)', graph.recordplot = TRUE)
evals('histogram(mtcars$hp)', graph.recordplot = TRUE)
evals(list(c('x <- runif(100)', 'plot(x)')), graph.recordplot = TRUE)
evals(c('plot(1:10)', 'plot(2:20)'), graph.recordplot = TRUE)
evals('plot(10:100)', graph.output = 'pdf')
evals('runif(10)', graph.output = 'pdf')

## hooks
hooks <- list('numeric' = round, 'matrix' = ascii)
evals(txt, hooks = hooks)
evals('22/7', hooks = list('numeric' = round))
evals('matrix(runif(25), 5, 5)', hooks = list('matrix' = round))

## using pander's default hook
evals('22/7', hooks = list('default' = pander.return))

## setting default hook
evals(c('runif(10)', 'matrix(runif(9), 3, 3)'), hooks = list('default'=round))
## round all values except for matrices
evals(c('runif(10)', 'matrix(runif(9), 3, 3)'), hooks = list(matrix = 'print', 'default' = round))

# advanced hooks
fun <- function(x, asciiformat) paste(capture.output(print(ascii(x), asciiformat)), collapse = '\\n')
hooks <- list('numeric' = list(round, 2), 'matrix' = list(fun, "rest"))
evals(txt, hooks = hooks)

# return only returned values
evals(txt, output = 'output')

# return only messages (for checking syntax errors etc.)
evals(txt, output = 'msg')

# check the length of returned values
evals('runif(10)', length = 5)

# note the following will not be filtered!
evals('matrix(1,1,1)', length = 1)

# if you do not want to let such things be eval-ed in the middle of a string use it with other filters :)
evals('matrix(1,1,1)', length = 1, classes = 'numeric')

# hooks & filtering
evals('matrix(5,5,5)', hooks = list('matrix' = ascii), output = 'output')

# eval-ing chunks in given environment
myenv <- new.env()
evals('x <- c(0,10)', env = myenv)
evals('mean(x)', env = myenv)
rm(myenv)
# note: if you had not specified 'myenv', the second 'evals' would have failed
evals('x <- c(0,10)')
evals('mean(x)')
}
}
\seealso{
  \code{\link{eval.msgs}}
}

