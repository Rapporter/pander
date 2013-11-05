\name{pandoc.table.return}
\alias{pandoc.table}
\alias{pandoc.table.return}
\title{Create a table}
\usage{
  pandoc.table.return(t, caption,
    digits = panderOptions("digits"),
    decimal.mark = panderOptions("decimal.mark"),
    big.mark = panderOptions("big.mark"),
    round = panderOptions("round"), justify,
    style = c("multiline", "grid", "simple", "rmarkdown"),
    split.tables = panderOptions("table.split.table"),
    split.cells = panderOptions("table.split.cells"),
    keep.trailing.zeros = panderOptions("keep.trailing.zeros"),
    emphasize.rows, emphasize.cols, emphasize.cells,
    emphasize.strong.rows, emphasize.strong.cols,
    emphasize.strong.cells, ...)
}
\arguments{
  \item{t}{data frame, matrix or table}

  \item{caption}{caption (string) to be shown under the
  table}

  \item{digits}{passed to \code{format}}

  \item{decimal.mark}{passed to \code{format}}

  \item{big.mark}{passed to \code{format}}

  \item{round}{passed to \code{round}}

  \item{justify}{defines alignment in cells passed to
  \code{format}. Can be \code{left}, \code{right} or
  \code{centre}, which latter can be also spelled as
  \code{center}. Defaults to \code{centre}.}

  \item{style}{which Pandoc style to use: \code{simple},
  \code{multiline}, \code{grid} or \code{rmarkdown}}

  \item{split.tables}{where to split wide tables to
  separate tables. The default value (\code{80}) suggests
  the conventional number of characters used in a line,
  feel free to change (e.g. to \code{Inf} to disable this
  feature) if you are not using a VT100 terminal any more
  :)}

  \item{split.cells}{where to split cells' text with line
  breaks. Default to \code{30}, to disable set to
  \code{Inf}.}

  \item{keep.trailing.zeros}{to show or remove trailing
  zeros in numbers on a column basis width}

  \item{emphasize.rows}{a vector for a two dimensional
  table specifying which rows to emphasize}

  \item{emphasize.cols}{a vector for a two dimensional
  table specifying which cols to emphasize}

  \item{emphasize.cells}{a vector for one-dimensional
  tables or a matrix like structure with two columns for
  row and column indexes to be emphasized in two
  dimensional tables. See e.g. \code{which(..., arr.ind =
  TRUE)}}

  \item{emphasize.strong.rows}{see \code{emphasize.rows}
  but in bold}

  \item{emphasize.strong.cols}{see \code{emphasize.cols}
  but in bold}

  \item{emphasize.strong.cells}{see \code{emphasize.cells}
  but in bold}

  \item{...}{unsupported extra arguments directly placed
  into \code{/dev/null}}
}
\value{
  By default this function outputs (see: \code{cat}) the
  result. If you would want to catch the result instead,
  then call \code{pandoc.table.return} instead.
}
\description{
  Creates a Pandoc's markdown style table with optional
  caption and some other tweaks. See 'Details' below.
}
\details{
  This function takes any tabular data as its first
  argument and will try to make it pretty like: rounding
  and applying \code{digits} and custom \code{decimal.mark}
  to numbers, auto-recognizing if row names should be
  included, setting alignment of cells and dropping
  trailing zeros by default.

  \code{pandoc.table} also tries to split large cells with
  line breaks or even the whole table to separate parts on
  demand. Other arguments lets the use to highlight some
  rows/cells/cells in the table with italic or bold text
  style.

  For more details please see the parameters above and
  passed arguments of \code{\link{panderOptions}}.
}
\note{
  If \code{caption} is missing, then the value is first
  checked in \code{t} object's \code{caption} attribute and
  if not found in an internal buffer set by
  \code{link{set.caption}}. \code{justify} parameter works
  similarly, see \code{\link{set.alignment}} for details.
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

m <- data.frame(a = c(1, -500, 10320, 23, 77),
  b = runif(5),
  c = c('a', 'bb', 'ccc', 'dddd', 'eeeee'))
pandoc.table(m)
pandoc.table(m, justify = c('right', 'left', 'centre'))

## splitting up too wide tables
pandoc.table(mtcars)
pandoc.table(mtcars, caption = 'Only once after the first part!')

## tables with line breaks in cells
## NOTE: line breaks are removed from table content
## and added automatically based on "split.cells" parameter!
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
tryCatch(pandoc.table(t, style = "simple", split.cells = 5),
  error = function(e) 'Yeah, no newline support in simple tables')
pandoc.table(t, style = "rmarkdown")

## highlight cells
t <- mtcars[1:3, 1:5]
pandoc.table(t$mpg, emphasize.cells = 1)
pandoc.table(t$mpg, emphasize.strong.cells = 1)
pandoc.table(t$mpg, emphasize.cells = 1, emphasize.strong.cells = 1)
pandoc.table(t$mpg, emphasize.cells = 1:2)
pandoc.table(t$mpg, emphasize.strong.cells = 1:2)
pandoc.table(t, emphasize.cells = which(t > 20, arr.ind = TRUE))
pandoc.table(t, emphasize.cells = which(t == 6, arr.ind = TRUE))
## with helpers
emphasize.cols(1)
emphasize.rows(1)
pandoc.table(t)

emphasize.strong.cells(which(t > 20, arr.ind = TRUE))
pandoc.table(t)
}
\references{
  John MacFarlane (2012): _Pandoc User's Guide_.
  \url{http://johnmacfarlane.net/pandoc/README.html}
}
\seealso{
  \code{\link{set.caption}}, \code{\link{set.alignment}}
}

