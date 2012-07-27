\name{set.alignment}
\alias{set.alignment}
\title{Sets alignment for tables}
\usage{
  set.alignment(align = "center", row.names = "left")
}
\arguments{
  \item{x}{character vector which length equals to one
  (would be repeated \code{n} times) ot \code{n} - where
  \code{n} equals to the number of columns in the following
  table}
}
\description{
  This is a helper function to be used \emph{inside brew
  blocks} to update the alignment (\code{justify} parameter
  of \code{pandoc.table}) of the returning table.
}

