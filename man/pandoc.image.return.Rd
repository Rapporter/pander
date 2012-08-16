\name{pandoc.image.return}
\alias{pandoc.image}
\alias{pandoc.image.return}
\title{Create pandoc image tags}
\usage{
  pandoc.image.return(img, caption = NULL)
}
\arguments{
  \item{img}{image path}

  \item{caption}{text}
}
\value{
  By default this function outputs (see: \code{cat}) the
  result. If you would want to catch the result instead,
  then call the function ending in \code{.return}.
}
\description{
  Creates a Pandoc's markdown format image hyperlink.
}
\examples{
pandoc.image('foo.png')
pandoc.image('foo.png', 'Nice image, huh?')
}
\references{
  John MacFarlane (2012): _Pandoc User's Guide_.
  \url{http://johnmacfarlane.net/pandoc/README.html}
}

