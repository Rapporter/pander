\name{pandoc.table.return}
\alias{pandoc.table}
\alias{pandoc.table.return}
\title{Create a table}
\usage{
  pandoc.table.return(t, caption = NULL, digits = 2,
    decimal.mark = ".", justify = "left",
    style = c("multiline", "grid", "simple"))
}
\arguments{
  \item{t}{data frame, matrix or table}

  \item{caption}{string}

  \item{digits}{see \code{prettyNum}}

  \item{decimal.mark}{see \code{prettyNum}}

  \item{justify}{see \code{prettyNum}}

  \item{style}{which Pandoc style to use: \code{simple},
  \code{multiline} or grid}
}
\value{
  By default this function outputs (see: \code{cat}) the
  result. If you would want to catch the result instead,
  then call the function ending in \code{.return}.
}
\description{
  Creates a pandoc style "grid" table with optional
  caption.
}
\details{
  This function will try to make pretty the provided R
  object's content like: rounding numbers, auto-recognizing
  if row names should be included etc.
}
\note{
  Pandoc does not support justify parameter for grid tables
  ATM. ## TODO: multiline?
}
\examples{
pandoc.table(mtcars)

## caption
pandoc.table(mtcars, 'Motor Trend Car Road Tests')

## other input/output formats
pandoc.table(mtcars[, 1:3], decimal.mark = ',')
pandoc.table(mtcars[, 1:3], decimal.mark = ',', justify = 'right')
pandoc.table(matrix(sample(1:1000, 25), 5, 5))
pandoc.table(matrix(runif(25), 5, 5))
pandoc.table(matrix(runif(25), 5, 5), digits = 5)
pandoc.table.return(table(mtcars$am))
pandoc.table(table(mtcars$am, mtcars$gear))
pandoc.table(table(state.division, state.region))
pandoc.table(table(state.division, state.region), justify = 'centre')

m <- data.frame(a=c(1, -500, 10320, 23, 77), b=runif(5), c=c('a', 'bb', 'ccc', 'dddd', 'eeeee'))
pandoc.table(m)
pandoc.table(m, justify = c('right', 'left', 'centre'))

## splitting up too wide tables
pandoc.table(mtcars)
pandoc.table(mtcars, caption = 'Only once after the first part!')

## table with newlines in cells
t <- data.frame(a = c('hundreds\\nof\\nmouses', '3 cats'), b=c('FOO is nice', 'BAR\\nBAR2'))
pandoc.table(t)

## exporting tables in other Pandoc styles
pandoc.table(m)
pandoc.table(m, style = "grid")
pandoc.table(m, style = "simple")
pandoc.table(t)
pandoc.table(t, style = "grid")
tryCatch(pandoc.table(t, style = "simple"), error = function(e) 'Yeah, no newline support in simple tables')
}
\references{
  John MacFarlane (2012): _Pandoc User's Guide_.
  \url{http://johnmacfarlane.net/pandoc/README.html}
}

