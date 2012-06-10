\name{Pandoc.brew}
\alias{Pandoc.brew}
\title{Brew in pandoc format}
\usage{
  Pandoc.brew(file = stdin(), output = stdout(),
    convert = FALSE, open = TRUE, graph.hi.res = FALSE,
    text = NULL, envir = new.env())
}
\arguments{
  \item{file}{file path of the brew template. As this is
  passed to \code{readLines}, \code{file} could be an URL
  too, but not over SSL (for that latter \code{RCurl} would
  be needed).}

  \item{output}{(optional) file path of the output file}

  \item{convert}{string: format of required output document
  (besides Pandoc's markdown). Pandoc is called if set via
  \code{Pandoc.convert} and the converted document could be
  also opened automatically (see below).}

  \item{open}{try to open converted document with operating
  system's default program}

  \item{graph.hi.res}{render high resolution images of
  plots? Default is \code{FALSE} except for HTML output.}

  \item{text}{character vector (treated as the content of
  the \code{file}}

  \item{envir}{environment where to \code{brew} the
  template}
}
\value{
  converted file name with full path if \code{convert} is
  set, none otherwise
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

  Please see my Github page for details
  (\url{http://daroczig.github.com/pander/#brew-to-pandoc})
  and examples
  (\url{http://daroczig.github.com/pander/#examples}).
}
\note{
  Only one of the input parameters (\code{file} or
  \code{text}) is to be used at once!
}
\examples{
\dontrun{
Pandoc.brew(text = 'string:<\%="foobar"\%>\\nimage:<\%=plot(1:10)\%>\\nerror:<\%=mean(no.R.object.like.this)\%>')

text <- paste('# Header', '', 'What a lovely list:\\n<\%=as.list(runif(10))\%>', 'A wide table:\\n<\%=mtcars[1:3, ]\%>', 'And a nice chart:\\n\\n<\%=plot(1:10)\%>', sep = '\\n')
Pandoc.brew(text = text)
Pandoc.brew(text = text, output = tempfile(), convert = 'html')
Pandoc.brew(text = text, output = tempfile(), convert = 'pdf')

## package bundled examples
Pandoc.brew(system.file('examples/minimal.brew', package='pander'))
Pandoc.brew(system.file('examples/minimal.brew', package='pander'), output = tempfile(), convert = 'html')
Pandoc.brew(system.file('examples/short-code-long-report.brew', package='pander'))
Pandoc.brew(system.file('examples/short-code-long-report.brew', package='pander'), output = tempfile(), convert = 'html')
}
}
\references{
  \itemize{ \item Jeffrey Horner (2011). _brew: Templating
  Framework for Report Generation._
  \url{http://CRAN.R-project.org/package=brew} \item John
  MacFarlane (2012): _Pandoc User's Guide_.
  \url{http://johnmacfarlane.net/pandoc/README.html} }
}

