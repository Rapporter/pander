\name{eval.msgs}
\alias{eval.msgs}
\title{Evaluate with messages}
\usage{
  eval.msgs(src, env = NULL)
}
\arguments{
  \item{src}{character values containing R code}

  \item{env}{environment where evaluation takes place. If
  not set (by default), a new temporary environment is
  created.}
}
\value{
  a list of parsed elements each containing: src (the
  command run), output (what the command returns, type
  (class of returned object if any), messages: messages,
  warnings and errors and possible stdout value. See
  Details above.
}
\description{
  This function takes text(s) of R code and \code{eval}s
  all at one run - returning a list with four elements. See
  \code{Details}.
}
\details{
  \code{eval.msgs} returns a detailed list of the result of
  evaluation: \itemize{ \item \emph{src} - character vector
  of specified R code.  \item \emph{result} - result of
  evaluation. \code{NULL} if nothing is returned. If any R
  code returned an R object while evaluating then the
  \emph{last} R object will be returned as a raw R object.
  If a graph is plotted in the end of the given R code
  (remember: \emph{last} R object), it would be
  automatically printed (see e.g. \code{lattice} and
  \code{ggplot2}).  \item \emph{output} - character vector
  of printed version (\code{capture.output}) of
  \code{result} \item \emph{type} - class of generated
  output. "NULL" if nothing is returned, "error" if some
  error occurred.  \item \emph{msg} - possible messages
  grabbed while evaluating specified R code with the
  following structure: \itemize{ \item \emph{messages} -
  character vector of possible diagnostic message(s) \item
  \emph{warnings} - character vector of possible warning
  message(s) \item \emph{errors} - character vector of
  possible error message(s) } \item \emph{stdout} -
  character vector of possibly printed texts to standard
  output (console) }
}
\examples{
\dontrun{
eval.msgs('1:5')
eval.msgs('x <- 1:5')
eval.msgs('lm(mtcars$hp ~ mtcars$wt)')

## plots
eval.msgs('plot(runif(100))')
eval.msgs('histogram(runif(100))')

## error handling
eval.msgs('runiff(23)')
eval.msgs('runif is a nice function')
eval.msgs('no.R.object.like.that')

## messages
eval.msgs(c('message("FOO")', '1:2'))
eval.msgs(c('warning("FOO")', '1:2'))
eval.msgs(c('message("FOO");message("FOO");warning("FOO")', '1:2'))
eval.msgs('warning("d");warning("f");1')

## stdout
eval.msgs('cat("writing to console")')
}
}
\seealso{
  \code{\link{evals}}
}

