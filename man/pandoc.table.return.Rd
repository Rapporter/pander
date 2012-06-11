\name{pandoc.table.return}
\alias{pandoc.table}
\alias{pandoc.table.return}
\title{Create a table}
\usage{
  pandoc.table.return(t, caption = NULL,
    digits = pander.option("digits"),
    decimal.mark = pander.option("decimal.mark"),
    round = pander.option("round"), justify = "left",
    style = c("multiline", "grid", "simple"),
    split.cells = 30)
}
\arguments{
  \item{t}{data frame, matrix or table}

  \item{caption}{string}

  \item{digits}{passed to \code{format}}

  \item{decimal.mark}{passed to \code{format}}

  \item{round}{passed to \code{round}}

  \item{justify}{see \code{prettyNum}}

  \item{style}{which Pandoc style to use: \code{simple},
  \code{multiline} or grid}

  \item{split.cells}{where to split cells' text with line
  breaks. Default to \code{30}, to disbale set to
  \code{Inf}.}
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
pandoc.table(matrix(runif(25),5,5), round = 1)
pandoc.table(table(mtcars$am))
pandoc.table(table(mtcars$am, mtcars$gear))
pandoc.table(table(state.division, state.region))
pandoc.table(table(state.division, state.region), justify = 'centre')

m <- data.frame(a=c(1, -500, 10320, 23, 77), b=runif(5), c=c('a', 'bb', 'ccc', 'dddd', 'eeeee'))
pandoc.table(m)
pandoc.table(m, justify = c('right', 'left', 'centre'))

## splitting up too wide tables
pandoc.table(mtcars)
pandoc.table(mtcars, caption = 'Only once after the first part!')

## tables with line breaks in cells
## Note: line breaks are removed from table content and added automatically based on "split.cells" parameter!
t <- data.frame(a = c('hundreds\\nof\\nmouses', '3 cats'), b=c('FOO is nice', 'BAR\\nBAR2'))
pandoc.table(t)
pandoc.table(t, split.cells = 5)

## exporting tables in other Pandoc styles
pandoc.table(m)
pandoc.table(m, style = "grid")
pandoc.table(m, style = "simple")
pandoc.table(t, style = "grid")
pandoc.table(t, style = "grid", split.cells = 5)
pandoc.table(t, style = "simple")
tryCatch(pandoc.table(t, style = "simple", split.cells = 5), error = function(e) 'Yeah, no newline support in simple tables')
}
\references{
  John MacFarlane (2012): _Pandoc User's Guide_.
  \url{http://johnmacfarlane.net/pandoc/README.html}
}

