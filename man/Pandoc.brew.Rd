\name{Pandoc.brew}
\alias{Pandoc.brew}
\title{Brew in pandoc format}
\usage{
  Pandoc.brew(file = stdin(), output = stdout(),
    text = NULL, envir = new.env())
}
\arguments{
  \item{file}{file path of the brew template}

  \item{output}{(optional) file path of the output file}

  \item{text}{character vector (treated as the content of
  the \code{file}}

  \item{envir}{environment where to \code{brew} the
  template}
}
\description{
  This function behaves just like \code{brew} except for
  the \code{<\%=...\%>} tags, where \code{Pandoc.brew}
  first translate the R object found between the tags to
  Pandoc markdown before passing to the \code{cat}
  function.
}
\details{
  This parser tries to be smart in some ways:

  \itemize{ \item a block (R commands between the tags)
  could return a value in the middle of the block and do
  something else without any output in the rest (but only
  one returned value per block!)  \item plots and images
  are grabbed in the document, rendered to a png file and
  pander method would result in a Pandoc markdown formatted
  image link (so the image would be shown/included in the
  exported document). The images' are put in \code{plots}
  directory in current \code{getwd()} or to the specified
  \code{output} file's directory.  \item all
  warnings/messages and errors are recorded in the blocks
  and returned in the document as a footnote }
}
\note{
  Only one of the input parameters (\code{file} or
  \code{text}) is to be used at once!
}
\examples{
text <- paste('# Header', '', '<\%=as.list(runif(10))\%>', '<\%=mtcars[1:3, ]\%>', '<\%=plot(1:10)\%>', sep = '\\n')
Pandoc.brew(text = text)

## For a longer example checkout README.brew in this installed package or online: \\url{https://github.com/daroczig/pander/blob/master/inst/README.brew}
}
\references{
  \itemize{ \item Jeffrey Horner (2011). _brew: Templating
  Framework for Report Generation._
  \url{http://CRAN.R-project.org/package=brew} \item John
  MacFarlane (2012): _Pandoc User's Guide_.
  \url{http://johnmacfarlane.net/pandoc/README.html} }
}

