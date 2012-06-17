\name{pander.option}
\alias{pander.option}
\title{Querying/setting pander option}
\usage{
  pander.option(o, value)
}
\arguments{
  \item{o}{option name (string). See below.}

  \item{value}{value to assign (optional)}
}
\description{
  To list all \code{pander} options, just run this function
  without any parameters provided. To query only one value,
  pass the first parameter. To set that, use the
  \code{value} parameter too.
}
\details{
  The following \code{pander} options are available:

  \itemize{ \item \code{digits}: numeric (default:
  \code{2}) passed to \code{format} \item
  \code{decimal.mark}: numeric (default: \code{.}) passed
  to \code{format} \item \code{round}: numeric (default:
  \code{Inf}) passed to \code{round} \item
  \code{list.style}: \code{'bullet'}, \code{'ordered'} or
  \code{'roman'} passed to \code{\link{pandoc.list}}.
  Please not that this has no effect on \code{pander}
  methods.  \item \code{table.style}: \code{'atx'} or
  \code{'setext'} passed to \code{\link{pandoc.table}} and
  also affects \code{pander} methods.  \item
  \code{table.split.table}: numeric passed to
  \code{\link{pandoc.table}} and also affects \code{pander}
  methods. This option tells \code{pander} where to split
  too wide tables. The default value (\code{80}) suggests
  the conventional number of characters used in a line,
  feel free to change (e.g. to \code{Inf} to disable this
  feature) if you are not using a VT100 terminal any more
  :) \item \code{table.split.cells}: numeric (default:
  \code{30}) passed to \code{\link{pandoc.table}} and also
  affects \code{pander} methods. This option tells
  \code{pander} where to split too wide cells with line
  breaks. Set \code{Inf} to disable. }
}
\examples{
pander.option()
pander.option('digits')
pander.option('digits', 5)
}
\seealso{
  \code{\link{evals.option}}
}

