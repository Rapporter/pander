\name{Pandoc}
\alias{Pandoc}
\title{Generic Pandoc method}
\usage{
  Pandoc(x, ...)
}
\arguments{
  \item{x}{an R object}

  \item{...}{optional parameters}
}
\value{
  By default this function outputs (see: \code{cat}) the
  result. If you would want to catch the result instead,
  then call the function ending in \code{.return}.
}
\description{
  Prints an R object in pandoc style markdown.
}
\examples{
## Vectors
Pandoc(1:10)
Pandoc(letters)
Pandoc(mtcars$am)

## Arrays
Pandoc(mtcars)
Pandoc(table(mtcars$am))
Pandoc(table(mtcars$am, mtcars$gear))

## Tests
Pandoc(ks.test(runif(50), runif(50)))
Pandoc(chisq.test(table(mtcars$am, mtcars$gear)))
Pandoc(t.test(extra ~ group, data = sleep))

## Models
ml <- with(lm(mpg ~ hp + wt), data = mtcars)
Pandoc(ml)
Pandoc(anova(ml))
Pandoc(aov(ml))
## Dobson (1990) Page 93: Randomized Controlled Trial (examples from: ?glm)
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
m <- glm(counts ~ outcome + treatment, family=poisson())
Pandoc(m)
Pandoc(anova(m))
Pandoc(aov(m))
}
\author{
  Gergely Daróczi
}
\references{
  \itemize{ \item John MacFarlane (2012): _Pandoc User's
  Guide_.
  \url{http://johnmacfarlane.net/pandoc/README.html} \item
  David Hajage (2011): _ascii. Export R objects to several
  markup languages._
  \url{http://CRAN.R-project.org/package=ascii} }
}

