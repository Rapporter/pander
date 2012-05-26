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
  Brew in pandoc format
}
\note{
  Only one of the input parameters (\code{file} or
  \code{text}) is to be used at once!
}
\examples{
\dontrun{
text <- paste('# Header', '', '<\%=as.list(runif(10))\%>', '<\%=mtcars[1:3, ]\%>', '<\%=plot(1:10)\%>', sep = '\\n')
Pandoc.brew(text = text)
}
}

