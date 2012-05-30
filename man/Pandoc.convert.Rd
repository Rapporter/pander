\name{Pandoc.convert}
\alias{Pandoc.convert}
\title{Converts Pandoc to other format}
\usage{
  Pandoc.convert(f, format = "html", options = "",
    proc.time)
}
\arguments{
  \item{f}{Pandoc markdown format file path}

  \item{format}{required output format. For all possible
  values here check out Pandoc homepage:
  \url{http://johnmacfarlane.net/pandoc/}}

  \item{options}{optionally passed arguments to Pandoc
  (instead of \code{pander}'s default)}

  \item{proc.time}{optionally passed number in seconds
  which would be shown in the generated document's footer}
}
\value{
  Converted file's path.
}
\description{
  Calling John MacFarlane's great program to convert
  specified file to other formats like \code{HTML},
  \code{pdf}, \code{docx}, \code{odt} etc.
}
\note{
  This function depends on \code{Pandoc} which should be
  pre-installed on user's machine.
}
\references{
  John MacFarlane (2012): _Pandoc User's Guide_.
  \url{http://johnmacfarlane.net/pandoc/README.html}
}

