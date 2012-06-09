\name{Pandoc.convert}
\alias{Pandoc.convert}
\title{Converts Pandoc to other format}
\usage{
  Pandoc.convert(f, text, format = "html", open = TRUE,
    options = "", footer = TRUE, proc.time)
}
\arguments{
  \item{f}{Pandoc markdown format file path. If URL is
  provided then the generated file's path is
  \code{tempfile()}.}

  \item{text}{Pandoc markdown format character vector.
  Treated as the content of \code{f} file - so the \code{f}
  parameter is ignored. The generated file's path is
  \code{tempfile()}.}

  \item{format}{required output format. For all possible
  values here check out Pandoc homepage:
  \url{http://johnmacfarlane.net/pandoc/}}

  \item{open}{try to open converted document with operating
  system's default program}

  \item{options}{optionally passed arguments to Pandoc
  (instead of \code{pander}'s default)}

  \item{footer}{add footer to document with
  meta-information}

  \item{proc.time}{optionally passed number in seconds
  which would be shown in the generated document's footer}
}
\value{
  Converted file's path.
}
\description{
  Calling John MacFarlane's great program to convert
  specified file (see \code{f} parameter below) or
  character vector {see \code{text} paramater} to other
  formats like \code{HTML}, \code{pdf}, \code{docx},
  \code{odt} etc.
}
\note{
  This function depends on \code{Pandoc} which should be
  pre-installed on user's machine. See the \code{INSTALL}
  file of the package.
}
\references{
  John MacFarlane (2012): _Pandoc User's Guide_.
  \url{http://johnmacfarlane.net/pandoc/README.html}
}

