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
  This function takes text(s) of R code and
  \code{\link{eval}}s all at one run then returns a list
  with four elements:
}
\details{
  \itemize{ \item \emph{src} - character vector of
  specified R code.  \item \emph{output} - generated
  output. \code{NULL} if nothing is returned. If any string
  returned an R object while evaluating then the
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
  evaluating specified R code with the following structure:
  \itemize{ \item \emph{messages} - string of possible
  diagnostic message(s) \item \emph{warnings} - string of
  possible warning message(s) \item \emph{errors} - string
  of possible error message(s) } \item \emph{stdout} -
  character vector of possibly printed texts to standard
  output (console) }
}
\examples{
\dontrun{
eval.msgs('1:5')
eval.msgs(c('1:3', 'runiff(23)'))
eval.msgs(c('1:5', '3:5'))
eval.msgs(c('pi', '1:10', 'NULL'))
eval.msgs('pi')
eval.msgs('1:2')
identical(evals('pi')[[1]], eval.msgs('pi'))
eval.msgs(c('message("FOO")', '1:2'))
eval.msgs(c('caption("FOO")', '1:2'))
eval.msgs('cat("writing to console")')
}
}
\seealso{
  \code{\link{evals}}
}

