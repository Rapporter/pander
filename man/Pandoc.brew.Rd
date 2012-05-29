\name{Pandoc.brew}
\alias{Pandoc.brew}
\title{Brew in pandoc format}
\usage{
  Pandoc.brew(file = stdin(), output = stdout(),
    text = NULL)
}
\arguments{
  \item{file}{file path of the brew template}

  \item{output}{(optional) file path of the output file}

  \item{text}{character vector (treated as the content of
  the \code{file}}
}
\description{
  This function behaves just like \code{brew} except for
  the \code{<\%=...\%>} tags, where \code{Pandoc.brew}
  first translate the R object (where \code{length("the R
  object")} returns \code{1}) found between the tags to
  Pandoc markdown before passing to \code{cat} function.
}
\note{
  Only one of the input parameters (\code{file} or
  \code{text}) is to be used at once!
}
\examples{
text <- paste('# Header', '', '<\%=as.list(runif(10))\%>', '<\%=mtcars[1:3, ]\%>', '<\%=plot(1:10)\%>', sep = '\\n')
Pandoc.brew(text = text)
}
\references{
  Jeffrey Horner (2011). _brew: Templating Framework for
  Report Generation._
  \url{http://CRAN.R-project.org/package=brew}
}

