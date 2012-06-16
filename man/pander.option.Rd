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
  \code{'roman'} \item \code{table.style}: \code{'atx'} or
  \code{'setext'} }
}
\examples{
pander.option()
pander.option('digits')
pander.option('digits', 5)
}
\seealso{
  \code{\link{evals.option}}
}

