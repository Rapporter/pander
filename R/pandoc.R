#' Indent text
#'
#' Indent all (optionally concatenated) lines of provided text with given level.
#' @param x character vector
#' @param level integer
#' @export
#' @examples
#' pandoc.indent('FOO', 1)
#' pandoc.indent(pandoc.table.return(table(mtcars$gear)), 2)
#' cat(pandoc.indent(pandoc.table.return(table(mtcars$gear)), 3))
pandoc.indent <- function(x, level = 0) {

    if (!is.character(x)) {
        stop('Only character strings are allowed.')
    }
    indent <- repChar(' ', level * 4)
    res <- paste0(indent, gsub('\n', paste0('\n', indent), x))

    ## remove wasted space
    res <- gsub(' *\n', '\n', res)
    res <- sub(' *$', '', res)

    res

}


#' Paragraphs
#'
#' Pandoc's markdown paragraph.
#' @param x character vector
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.p
#' @seealso \code{\link{pandoc.emphasis}} \code{\link{pandoc.strikeout}} \code{\link{pandoc.verbatim}}
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.p('FOO')
#' pandoc.p(c('Lorem', 'ipsum', 'lorem ipsum'))
pandoc.p.return <- function(x)
    add.blank.lines(add.blank.lines(paste(x, collapse = '\n')))

#' @export
pandoc.p <- function(...)
    cat(pandoc.p.return(...))


#' @keywords internal
pandoc.add.formatting <- function(x, f) {

    attributes(x) <- NULL
    if (!is.vector(x)) {
        stop('Sorry, vectors only!')
    }

    ## escape chars
    f.e  <- gsub('*', '\\*', f, fixed = TRUE)

    ## remove trailing or leading spaces
    x    <- trim.spaces(x)

    ## do not stack formatting chars
    w    <- which(!grepl(sprintf('^%s.*%s$', f.e, f.e), x) & x != '')

    ## add an extra space if the string starts with a formatting char
    x[w] <- sapply(x[w],
                   function(x) ifelse(grepl(paste0('^', f.e), x), paste0('\\ ', x), x),
                   USE.NAMES = FALSE)

    ## add formatting chars
    x[w] <- paste0(f, x[w], f)

    return(x)

}


#' Strong emphasis
#'
#' Pandoc's markdown strong emphasis format (e.g. \code{**FOO**}) is added to character string.
#' @param x character vector
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.strong
#' @seealso \code{\link{pandoc.emphasis}} \code{\link{pandoc.strikeout}} \code{\link{pandoc.verbatim}}
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.strong('FOO')
#' pandoc.strong(c('FOO', '**FOO**'))
#' pandoc.strong.return('FOO')
pandoc.strong.return <- function(x)
    pandoc.add.formatting(x, '**')

#' @export
pandoc.strong <- function(...)
    cat(pandoc.strong.return(...))


#' Emphasis
#'
#' Pandoc's markdown emphasis format (e.g. \code{*FOO*}) is added to character string.
#' @param x character vector
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.emphasis
#' @seealso \code{\link{pandoc.strong}} \code{\link{pandoc.strikeout}} \code{\link{pandoc.verbatim}}
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.emphasis('FOO')
#' pandoc.emphasis(c('FOO', '*FOO*'))
#' pandoc.emphasis.return('FOO')
pandoc.emphasis.return <- function(x)
    pandoc.add.formatting(x, '*')

#' @export
pandoc.emphasis <- function(...)
    cat(pandoc.emphasis.return(...))


#' Add strikeout
#'
#' Pandoc's markdown strikeout format (e.g. \code{~~FOO~~}) is added to character string.
#' @param x character vector
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.strikeout
#' @seealso \code{\link{pandoc.emphasis}} \code{\link{pandoc.strong}} \code{\link{pandoc.verbatim}}
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.strikeout('FOO')
#' pandoc.strikeout(c('FOO', '~~FOO~~'))
#' pandoc.strikeout.return('FOO')
pandoc.strikeout.return <- function(x)
    pandoc.add.formatting(x, '~~') #nolint

#' @export
pandoc.strikeout <- function(...)
    cat(pandoc.strikeout.return(...))


#' Add verbatim
#'
#' Pandoc's markdown verbatim format (e.g. \code{`FOO`}) is added to character string.
#' @param x character vector
#' @param style show code \code{inline} or in a separate (\code{indent}ed or \code{delim}ited) block
#' @param attrs (optionally) pass ID, classes and any attribute to the \code{delimited} block
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.verbatim
#' @seealso \code{\link{pandoc.emphasis}} \code{\link{pandoc.strikeout}} \code{\link{pandoc.strong}}
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' # different styles/formats
#' pandoc.verbatim('FOO')
#'
#' src <- c('FOO', 'indent', 'BAR' )
#' pandoc.verbatim(src)
#' pandoc.verbatim.return(src)
#' pandoc.verbatim(c('FOOO\nBAR  ', ' I do R'), 'indent')
#' pandoc.verbatim(c('FOOO\nBAR  ', ' I do R'), 'delim')
#'
#' # add highlighting and HTML/LaTeX ID and classes (even custom attribute)
#' pandoc.verbatim(c('cat("FOO")', 'mean(bar)'), 'delim', '.R #MyCode custom_var="10"')
pandoc.verbatim.return <- function(x, style = c('inline', 'indent', 'delim'), attrs = '') {

    style <- match.arg(style)
    if (style != 'delim' & !missing(attrs)) {
        warning('Providing attrs is only meaningful with delimited blocks.')
    }

    switch(style,
           'inline' = paste0('`', trim.spaces(paste(x, collapse = ' ')), '`'),
           'indent' = sprintf('\n%s\n',
                              paste(paste0(repChar(' ', 4), unlist(strsplit(trim.spaces(x), '\n')), collapse = '\n'))),
           'delim'  = paste0('\n', repChar('`', 7), ifelse(attrs == '', '', sprintf('{%s}', attrs)), '\n', paste(trim.spaces(x), collapse = '\n'), '\n', repChar('`', 7), '\n') #nolint
           )

}

#' @export
pandoc.verbatim <- function(...)
    cat(pandoc.verbatim.return(...))


#' Create pandoc link
#' Pandoc's markdown format link.
#' @param url hyperlink
#' @param text link text
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.link
#' @examples
#' pandoc.link('http://r-project.org')
#' pandoc.link('http://r-project.org', 'R')
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
pandoc.link.return <- function(url, text = url)
    sprintf('[%s](%s)', text, url)

#' @export
pandoc.link <- function(...)
    cat(pandoc.link.return(...))


#' Create pandoc image tags
#'
#' Creates a Pandoc's markdown format image hyperlink.
#' @param img image path
#' @param caption text
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.image
#' @seealso \code{\link{set.caption}}
#' @note The \code{caption} text is read from an internal buffer which defaults to \code{NULL}. To update that, call \code{link{set.caption}} before.
#' @examples
#' pandoc.image('foo.png')
#' pandoc.image('foo.png', 'Nice image, huh?')
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
pandoc.image.return <- function(img, caption = storage$caption) {

    check_caption(caption)

    if (is.null(caption)) {
        caption <- ''
    }

    ## truncating caption buffer if needed
    if (!is.null(storage$caption)) {
        storage$caption <- NULL
    }

    sprintf('![%s](%s)', caption, img)
}

#' @export
pandoc.image <- function(...)
    cat(pandoc.image.return(...))


#' Footnote
#'
#' Creates a Pandoc's markdown format footnote.
#' @param x character vector
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.footnote
#' @examples
#' pandoc.footnote('Automatically numbered footnote, right?')
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
pandoc.footnote.return <- function(x)
    gsub('[\n][\n]*', '\n', sprintf('^[%s]', paste(x, collapse = ' ')))

#' @export
pandoc.footnote <- function(...)
    cat(pandoc.footnote.return(...))


#' Create horizontal rule
#'
#' Creates a Pandoc's markdown format horizontal line with trailing and leading newlines.
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.horizontal.rule
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
pandoc.horizontal.rule.return <- function()
    add.blank.lines('---')

#' @export
pandoc.horizontal.rule <- function(...)
    cat(pandoc.horizontal.rule.return(...))


#' Create header
#'
#' Creates a (Pandoc's) markdown style header with given level.
#' @param x character vector
#' @param level integer
#' @param style atx or setext type of heading
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.header
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.header('Foo!', 4)
#' pandoc.header('Foo!', 2, 'setext')
#' pandoc.header('Foo **bar**!', 1, 'setext')
pandoc.header.return <- function(x, level = 1, style = c('atx', 'setext')) {

    if (missing(style)) {
        style <- panderOptions('header.style')
    } else {
        style <- match.arg(style)
    }

    if (!is.numeric(level)) {
        stop('Wrong level provided!')
    }
    if (any( (style == 'atx' & level > 6), (style == 'setext' & level > 2))) {
        stop('Too high level provided!')
    }
    if (level < 1) {
        stop('Too low level provided!')
    }

    res <- switch(style,
                  'atx'    = paste(repChar('#', level), x),
                  'setext' = paste(x, repChar(ifelse(level == 1, '=', '-'), nchar(x, type = 'width')), sep = '\n')
                  )

    add.blank.lines(res)

}

#' @export
pandoc.header <- function(...)
    cat(pandoc.header.return(...))


#' Create title block
#'
#' Creates a Pandoc's markdown style title block with optional author, title and date fields.
#' @param author character vector or semicolon delimited list of authors without line break
#' @param title character vector of lines of title or multiline string with \code{\\n} separators
#' @param date any string fit in one line
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.title
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.title('Tom', 'Render pandoc in R', '2012-05-16')
#' pandoc.title(c('Tom', 'Jerry'), 'Render pandoc in R', '2012-05-16')
#' pandoc.title('Tom; Jerry', 'Render pandoc in R', '2012-05-16')
#' pandoc.title('Tom; Jerry', c('Render', 'pandoc', 'in R'), '2012-05-16')
#' pandoc.title('Tom; Jerry', 'Render\n    pandoc \n    in R', '2012-05-16')
#'
#' ## missing fields
#'
#' pandoc.title('Tom; Jerry', 'Render pandoc in R')
#' pandoc.title('Tom; Jerry')
#' pandoc.title(title = 'Render pandoc in R', date= '2012-05-16')
pandoc.title.return <- function(author = '', title = '', date = '') {

    if (author == '' && title == '' && date != '') {
        stop('You cannot create a title with only date specified!')
    }
    ## updating title tags
    if (length(author) > 1 && author != '') {
        author <- paste('%', paste(author[author != ''], collapse = '; '))
    }
    if (length(title) > 1 && title != '') {
        title  <- paste0(
            '% ',
            gsub('[\t ][\t ]*', '  ', gsub('\n', '\n  ', paste(title[title != ''], collapse = '\n'))))
    }

    ## formatting result
    if (title == '') {
        ## author
        res <- paste0('%\n', author)
    } else {
        if (date == '') {
            if (author == '') {
                ## title
                res <- title
            } else {
                ## author and title
                res <- paste(title, author, sep = '\n')
            }
        } else {
            date <- paste0('% ', gsub('\n', ' ', date)[1])
            if (author == '') {
                ## title and date
                res <- paste(title, '%', date, sep = '\n')
            } else {
                ## author, title and date
                res <- paste(title, author, date, sep = '\n')
            }
        }
    }

    sprintf('%s\n', res)

}

#' @export
pandoc.title <- function(...)
    cat(pandoc.title.return(...))


#' Create a list
#'
#' Creates a Pandoc's markdown format list from provided character vector/list.
#' @param elements character vector of strings
#' @param style the required style of the list
#' @param loose adding a newline between elements
#' @param add.line.breaks adding a leading and trailing newline before/after the list
#' @param add.end.of.list adding a separator comment after the list
#' @param indent.level the level of indent
#' @param missing string to replace missing values
#' @param ... extra arguments passed by from parent call, disregarded
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.list
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' ## basic lists
#' pandoc.list(letters[1:5])
#' pandoc.list(letters[1:5])
#' pandoc.list(letters[1:5], 'ordered')
#' pandoc.list(letters[1:5], 'roman')
#' pandoc.list(letters[1:5], loose = TRUE)
#'
#' ## nested lists
#' l <- list("First list element",
#'   rep.int('sub element', 5),
#'   "Second element",
#'   list('F', 'B', 'I', c('phone', 'pad', 'talics')))
#' pandoc.list(l)
#' pandoc.list(l, loose = TRUE)
#' pandoc.list(l, 'roman')
#'
#' ## complex nested lists
#' pandoc.list(list('one', as.list(2)))
#' pandoc.list(list('one', list('two')))
#' pandoc.list(list('one', list(2:3)))
#' @importFrom utils as.roman
pandoc.list.return <- function(elements, style = c('bullet', 'ordered', 'roman'), loose = FALSE, add.line.breaks = TRUE, add.end.of.list = TRUE, indent.level = 0, missing = panderOptions('missing'), ...) { #nolint

    ## checks
    if (!is.logical(loose)) {
        stop('Wrong argument provided: loose')
    }

    ## default values
    if (missing(style)) {
        style <- panderOptions('list.style')
    } else {
        style <- match.arg(style)
    }

    ## replace missing values
    w <- which(is.na(elements))
    if (length(w) > 0) {
        elements[w] <- missing
    }

    ## helpers
    elements.l <- length(elements)
    marker     <- switch(style,
                         'bullet'  = rep('* ', elements.l),
                         'ordered' = paste0(1:elements.l, '. '),
                         'roman'   = paste0(as.roman(1:elements.l), '. '))

    ## number of elements should be more than one
    if (elements.l == 0) {
        return('')
    }

    ## recursive call
    i.lag <- 0
    res <- ifelse(add.line.breaks, '\n', '')
    res <- paste(sapply(1:elements.l, function(i) {
        if (length(elements[[i]]) == 1 && !is.list(elements[[i]])) {
            paste0(paste(rep(' ', indent.level * 4), collapse = ''), marker[i - i.lag], elements[i])
        } else {
            i.lag <<- i.lag + 1
            pandoc.list.return(elements[[i]], style, loose, FALSE, FALSE, indent.level + 1)
        }}),
        collapse = '\n', ifelse(loose, '\n', ''))

    ## closing tag
    if (add.end.of.list) {
        res <- paste0(res, ifelse(loose, '', '\n\n'), '<!-- end of list -->\n')
    }
    if (add.line.breaks) {
        res <- add.blank.lines(res)
    }

    return(res)

}

#' @export
pandoc.list <- function(...)
    cat(pandoc.list.return(...))


#' Create a table
#'
#' Creates a Pandoc's markdown style table with optional caption and some other tweaks. See 'Details' below.
#'
#' This function takes any tabular data as its first argument and will try to make it pretty like: rounding and applying \code{digits} and custom \code{decimal.mark} to numbers, auto-recognizing if row names should be included, setting alignment of cells and dropping trailing zeros by default.
#'
#' \code{pandoc.table} also tries to split large cells with line breaks or even the whole table to separate parts on demand. Other arguments lets the use to highlight some rows/cells/cells in the table with italic or bold text style.
#'
#' For more details please see the parameters above and passed arguments of \code{\link{panderOptions}}.
#' @param t data frame, matrix or table
#' @param caption caption (string) to be shown under the table
#' @param digits passed to \code{format}. Can be a vector specifying values for each column (has to be the same length as number of columns).
#' @param decimal.mark passed to \code{format}
#' @param big.mark passed to \code{format}
#' @param round passed to \code{round}. Can be a vector specifying values for each column (has to be the same length as number of columns). Values for non-numeric columns will be disregarded.
#' @param missing string to replace missing values
#' @param justify defines alignment in cells passed to \code{format}. Can be \code{left}, \code{right} or \code{centre}, which latter can be also spelled as \code{center}. Defaults to \code{centre}. Can be abbreviated to a string consisting of the letters \code{l}, \code{c} and \code{r} (e.g. 'lcr' instead of c('left', 'centre', 'right').
#' @param style which Pandoc style to use: \code{simple}, \code{multiline}, \code{grid} or \code{rmarkdown}
#' @param split.tables where to split wide tables to separate tables. The default value (\code{80}) suggests the conventional number of characters used in a line, feel free to change (e.g. to \code{Inf} to disable this feature) if you are not using a VT100 terminal any more :)
#' @param split.cells where to split cells' text with line breaks. Default to \code{30}, to disable set to \code{Inf}. Can be also supplied as a vector, for each cell separately (if length(split.cells) == number of columns + 1, then first value in split.cells if for row names, and others are for columns). Supports relative (percentage) parameters in combination with split.tables.
#' @param keep.trailing.zeros to show or remove trailing zeros in numbers on a column basis width
#' @param keep.line.breaks (default: \code{FALSE}) if to keep or remove line breaks from cells in a table
#' @param plain.ascii (default: \code{FALSE}) if output should be in plain ascii (without markdown markup) or not
#' @param use.hyphening boolean (default: \code{FALSE}) if try to use hyphening when splitting large cells according to table.split.cells. Requires \pkg{sylly}.
#' @param row.names if \code{FALSE}, row names are suppressed. A character vector of row names can also be specified here. By default, row names are included if \code{rownames(t)} is neither
#'   \code{NULL} nor identical to \code{1:nrow(x)}
#' @param col.names a character vector of column names to be used in the table
#' @param emphasize.rownames boolean (default: \code{TRUE}) if row names should be highlighted
#' @param emphasize.rows deprecated for \code{emphasize.italics.rows} argument
#' @param emphasize.cols deprecated for \code{emphasize.italics.cols} argument
#' @param emphasize.cells deprecated for \code{emphasize.italics.cells} argument
#' @param emphasize.italics.rows a vector for a two dimensional table specifying which rows to emphasize
#' @param emphasize.italics.cols a vector for a two dimensional table specifying which cols to emphasize
#' @param emphasize.italics.cells a vector for one-dimensional tables or a matrix like structure with two columns for row and column indexes to be emphasized in two dimensional tables. See e.g. \code{which(..., arr.ind = TRUE)}
#' @param emphasize.strong.rows see \code{emphasize.italics.rows} but in bold
#' @param emphasize.strong.cols see \code{emphasize.italics.cols} but in bold
#' @param emphasize.strong.cells see \code{emphasize.italics.cells} but in bold
#' @param emphasize.verbatim.rows see \code{emphasize.italics.rows} but in verbatim
#' @param emphasize.verbatim.cols see \code{emphasize.italics.cols} but in verbatim
#' @param emphasize.verbatim.cells see \code{emphasize.italics.cells} but in verbatim
#' @param ... unsupported extra arguments directly placed into \code{/dev/null}
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call \code{pandoc.table.return} instead.
#' @export
#' @useDynLib pander
#' @importFrom Rcpp evalCpp
#' @importFrom stats ftable
#' @aliases pandoc.table
#' @seealso \code{\link{set.caption}}, \code{\link{set.alignment}}
#' @note If \code{caption} is missing, then the value is first checked in \code{t} object's \code{caption} attribute and if not found in an internal buffer set by \code{link{set.caption}}. \code{justify} parameter works similarly, see \code{\link{set.alignment}} for details.
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.table(mtcars)
#'
#' # caption
#' pandoc.table(mtcars, 'Motor Trend Car Road Tests')
#'
#' # other input/output formats
#' pandoc.table(mtcars[, 1:3], decimal.mark = ',')
#' pandoc.table(mtcars[, 1:3], decimal.mark = ',', justify = 'right')
#' pandoc.table(matrix(sample(1:1000, 25), 5, 5))
#' pandoc.table(matrix(runif(25), 5, 5))
#' pandoc.table(matrix(runif(25), 5, 5), digits = 5)
#' pandoc.table(matrix(runif(25),5,5), round = 1)
#' pandoc.table(table(mtcars$am))
#' pandoc.table(table(mtcars$am, mtcars$gear))
#' pandoc.table(table(state.division, state.region))
#' pandoc.table(table(state.division, state.region), justify = 'centre')
#'
#' m <- data.frame(a = c(1, -500, 10320, 23, 77),
#'   b = runif(5),
#'   c = c('a', 'bb', 'ccc', 'dddd', 'eeeee'))
#' pandoc.table(m)
#' pandoc.table(m, justify = c('right', 'left', 'centre'))
#' pandoc.table(m, justify = 'rlc') # Same as upper statement
#'
#' ## splitting up too wide tables
#' pandoc.table(mtcars)
#' pandoc.table(mtcars, caption = 'Only once after the first part!')
#'
#' ## tables with line breaks in cells
#' ## NOTE: line breaks are removed from table content in case keep.line.breaks is set to FALSE
#' ## and added automatically based on "split.cells" parameter!
#' t <- data.frame(a = c('hundreds\nof\nmouses', '3 cats'), b=c('FOO is nice', 'BAR\nBAR2'))
#' pandoc.table(t)
#' pandoc.table(t, split.cells = 5)
#'
#' ## exporting tables in other Pandoc styles
#' pandoc.table(m)
#' pandoc.table(m, style = "grid")
#' pandoc.table(m, style = "simple")
#' pandoc.table(t, style = "grid")
#' pandoc.table(t, style = "grid", split.cells = 5)
#' tryCatch(pandoc.table(t, style = "simple", split.cells = 5),
#'   error = function(e) 'Yeah, no newline support in simple tables')
#'
#' ## highlight cells
#' t <- mtcars[1:3, 1:5]
#' pandoc.table(t$mpg, emphasize.italics.cells = 1)
#' pandoc.table(t$mpg, emphasize.strong.cells = 1)
#' pandoc.table(t$mpg, emphasize.italics.cells = 1, emphasize.strong.cells = 1)
#' pandoc.table(t$mpg, emphasize.italics.cells = 1:2)
#' pandoc.table(t$mpg, emphasize.strong.cells = 1:2)
#' pandoc.table(t, emphasize.italics.cells = which(t > 20, arr.ind = TRUE))
#' pandoc.table(t, emphasize.italics.cells = which(t == 6, arr.ind = TRUE))
#' pandoc.table(t, emphasize.verbatim.cells = which(t == 6, arr.ind = TRUE))
#' pandoc.table(t, emphasize.verbatim.cells = which(t == 6, arr.ind = TRUE),
#'  emphasize.italics.rows = 1)
#' ## with helpers
#' emphasize.cols(1)
#' emphasize.rows(1)
#' pandoc.table(t)
#'
#' emphasize.strong.cells(which(t > 20, arr.ind = TRUE))
#' pandoc.table(t)
#'
#' ### plain.ascii
#' pandoc.table(mtcars[1:3, 1:3], plain.ascii = TRUE)
#'
#' ### keep.line.breaks
#' x <- data.frame(a="Pandoc\nPackage")
#' pandoc.table(x)
#' pandoc.table(x, keep.line.breaks = TRUE)
#'
#' ## split.cells
#' x <- data.frame(a = "foo bar", b = "foo bar")
#' pandoc.table(x, split.cells = 4)
#' pandoc.table(x, split.cells = 7)
#' pandoc.table(x, split.cells = c(4, 7))
#' pandoc.table(x, split.cells = c("20%", "80%"), split.tables = 30)
#'
#' y <- c("aa aa aa", "aaa aaa", "a a a a a", "aaaaa", "bbbb bbbb bbbb", "bb bbb bbbb")
#' y <- matrix(y, ncol = 3, nrow = 2)
#' rownames(y) <- c("rowname one", "rowname two")
#' colnames(y) <- c("colname one", "colname two", "colname three")
#' pandoc.table(y, split.cells = 2)
#' pandoc.table(y, split.cells = 6)
#' pandoc.table(y, split.cells = c(2, 6, 10))
#' pandoc.table(y, split.cells = c(2, Inf, Inf))
#'
#' ## first value used for rownames
#' pander(y, split.cells = c(5, 2, Inf, Inf))
#' pandoc.table(y, split.cells = c(5, 2, Inf, 5, 3, 10))
#'
#' ## when not enough reverting to default values
#' pandoc.table(y, split.cells = c(5, 2))
#'
#' ## split.cells with hyphenation
#' x <- data.frame(a = "Can be also supplied as a vector, for each cell separately",
#'        b = "Can be also supplied as a vector, for each cell separately")
#' pandoc.table(x, split.cells = 10, use.hyphening = TRUE)
pandoc.table.return <- function(t, caption, digits = panderOptions('digits'), decimal.mark = panderOptions('decimal.mark'), big.mark = panderOptions('big.mark'), round = panderOptions('round'), missing = panderOptions('missing'), justify, style = c('multiline', 'grid', 'simple', 'rmarkdown', 'jira'), split.tables = panderOptions('table.split.table'), split.cells = panderOptions('table.split.cells'), keep.trailing.zeros = panderOptions('keep.trailing.zeros'), keep.line.breaks = panderOptions('keep.line.breaks'), plain.ascii = panderOptions('plain.ascii'), use.hyphening = panderOptions('use.hyphening'), row.names, col.names, emphasize.rownames = panderOptions('table.emphasize.rownames'), emphasize.rows, emphasize.cols, emphasize.cells, emphasize.strong.rows, emphasize.strong.cols, emphasize.strong.cells, emphasize.italics.rows, emphasize.italics.cols, emphasize.italics.cells, emphasize.verbatim.rows, emphasize.verbatim.cols, emphasize.verbatim.cells, ...) { #nolint

    row.names.provided <- !missing(row.names)

    ## expands cells for output
    table.expand <- function(cells, cols.width, justify, sep.cols) {
        .Call('pander_tableExpand_cpp', PACKAGE = 'pander', cells, cols.width, justify, sep.cols, style)
    }

    ## cell conversion to plain-ascii (deletion of markup characters)
    to.plain.ascii <- function(x){
        x <- gsub('[\\\\]', '', x) # backslashes
        x <- gsub('&nbsp;', ' ', x)  # table non-breaking space
        x <- gsub('[*]+([^\\*.]*)[*]+', '\\1', x) # emphasis and strong
        x <- gsub('^[`]|[`]$', '', x) # verbatium
        x <- gsub('^[~]{2}|[~]{2}$', '', x) # strikeout
        gsub('^[_]|[_]$', '', x) # italic
    }

    ## split single cell with line breaks based on max.width
    split.single.cell <- function(x, max.width){
        if (!is.character(x)) {
            x <- as.character(x)
        }
        ## as.character(NA) remains NA, which causes isses with nchar since 2015-04-23
        ## https://stat.ethz.ch/pipermail/r-devel/2015-April/071007.html
        if (is.na(x)) {
            x <- 'NA'
        }
        if (!style %in% c('simple', 'rmarkdown')) {
            ## split
            if (nchar(x) == nchar(encodeString(x)) && !use.hyphening) {
                x <- paste(strwrap(x, width = max.width + 1), collapse = '\n')
            } else {
                ## dealing with CJK chars + also it does not count \n, \t, etc.
                ## this happens because width - counts only the number of columns
                ## cat will use to print the string in a monospaced font.
                if (!keep.line.breaks){
                    x <- gsub('\n', ' ', x)
                    x <- splitLine(x, max.width, use.hyphening)
                } else {
                    lines <- strsplit(x, '\\n')[[1]]
                    x <- ''
                    for (line in lines) {
                        sl <- splitLine(line, max.width, use.hyphening)
                        x <- paste0(x, sl, sep = '\n')
                    }
                }
            }
        }else{
            x <- gsub('^\\s+|\\s+$', '', x)
        }
        x
    }

    split.large.cells <- function(cells, for.rownames = FALSE) {

        ## if we have a single value, extend it to a vector to do less checks laters
        if (length(split.cells) == 1) {
            split.cells <- rep(split.cells, length(cells))
        }
        if (for.rownames) {
            ## in case it is used for rownames, we only need the first value
            split.cells <- rep(split.cells[1], length(cells))
        }

        res <- NULL
        rn <- rownames(cells)
        ## single value and vectors/lists
        if (length(dim(cells)) < 2) {

                ## discard first value which was for rownames
                if (!for.rownames && (length(split.cells) >= length(cells) + 1)) {
                    split.cells <- split.cells[-1]
                }

                if (length(cells) > length(split.cells)) {
                    warning('length of split.cells vector is smaller than data. Default value will be used for other cells') #nolint
                    split.cells <- c(split.cells, rep(panderOptions('table.split.cells'), length(cells) - length(split.cells))) #nolint
                }
                res <- sapply(seq_along(cells), function(x, i) split.single.cell(x[i], max.width = split.cells[i]), x = cells, USE.NAMES = FALSE) #nolint

        } else {
            ## matrixes and tables
            ## discard first value which was for rownames
            if (length(split.cells) >= dim(cells)[2] + 1) {
                split.cells <- split.cells[-1]
            }

            if (dim(cells)[2] > length(split.cells)) {
                warning('length of split.cells vector is smaller than data. Default value will be used for other cells')
                split.cells <- c(split.cells, rep(panderOptions('table.split.cells'),
                                                  dim(cells)[2] - length(split.cells)))
            }

            for (j in 1:dim(cells)[2]) {
                res <- cbind(res, sapply(cells[, j], split.single.cell, max.width = split.cells[j], USE.NAMES = FALSE))
            }

        }
        rownames(res) <- rn
        ## return
        res

    }

    align.hdr <- function(t.width, justify) {
        justify.vec <- rep(justify, length.out = length(t.width))
        dashes <- mapply(function(justify, width)
                         switch(
                             justify,
                             left = paste0(':', repChar('-', width + 1)),
                             right = paste0(repChar('-', width + 1), ':'),
                             centre = paste0(':', repChar('-', width), ':')
                             ),
                         justify.vec, t.width)
        hdr <- paste0('|', paste(dashes, collapse = '|'), '|')
        return(hdr)
    }
    is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5)
        abs(x - round(x)) < tol
    check.highlight.parameters <- function(x, num, num2) {
        if (!all(is.wholenumber(x))){
            stop('Only integers (whole numbers) can be passed to highlight table cell(s), row(s) or column(s).')
        }
        if (!all(x > 0)) {
            stop('Only positive numbers can be passed to highlight table cell(s), row(s) or column(s).')
        }
        if (missing(num2)) {
            if (!is.vector(x)) {
                stop('Only a vector or NULL can be passed to highlight table cell(s), row(s) or column(s).')
            }
            if (!all(x < (num + 1))) {
                stop(paste('Too high number passed that should be kept below', num + 1))
            }
        } else {
            if (ncol(x) != 2) {
                stop('A matrix like structure should be passed to highlight cells of a table with two columns for row and column indexes - just like what is returned by `which(..., arr.ind = TRUE)`.') #nolint
            }
            if (!all(x[, 1] < (num + 1))) {
                stop(paste('Too high number passed for row indexes that should be kept below', num + 1))
            }
            if (!all(x[, 2] < (num2 + 1))) {
                stop(paste('Too high number passed for column indexes that should be kept below', num + 1))
            }
        }
    }

    ## #########################################################################
    ## converting the table into intermediate representation
    ## #########################################################################

    ## revert dplyr to a normal data.frame
    if (inherits(t, 'tbl_df')) {
        t <- as.data.frame(t)
    }

    ## we have a high-dimensional table to be converted to 2D
    if (length(dim(t)) > 2){
        t <- ftable(t)

    } else {

        ## we have a pseudo-table with only one or NULL dimension
        if (length(dim(t)) < 2) {

            tn <- names(t)

            ## print.summaryDefault stores NAs in a very special way
            ## see eg: pander(summary(c(Sys.Date(), NA)))
            if ( (inherits(t, 'Date') || inherits(t, 'POSIXct')) & length(attr(t, 'NAs'))) {
                tn <- c(tn, 'NAs')
            }

            ## matrix/rbind will coerce cells to atomic (e.g. Date to numeric)
            ## so we should first convert these values to a character vector
            if (!is.numeric(t)) {
                t <- as.character(t)
            }

            ## make a table
            t <- rbind(matrix(nrow = 0, ncol = length(t)), t)

            ## restore col/row names
            colnames(t) <- tn
            rownames(t) <- NULL

            ## special conversion for emphasize.cells, emphasize.strong.cells
            if (!missing(emphasize.cells)) {
                emphasize.cells <- cbind(rep(1, length(emphasize.cells)), emphasize.cells)
            }
            if (!missing(emphasize.verbatim.cells)) {
                emphasize.verbatim.cells <- cbind(rep(1, length(emphasize.verbatim.cells)), emphasize.verbatim.cells)
            }
            if (!missing(emphasize.italics.cells)) {
                emphasize.italics.cells <- cbind(rep(1, length(emphasize.italics.cells)), emphasize.italics.cells)
            }
            if (!missing(emphasize.strong.cells)) {
                emphasize.strong.cells <- cbind(rep(1, length(emphasize.strong.cells)), emphasize.strong.cells)
            }

        } else {
            if (dim(t)[1] == 0) {
                ## check for empty objects
                if (!is.null(colnames(t)) && length(colnames(t)) > 0) {
                    t <- matrix(data = pandoc.strong.return(colnames(t)), nrow = 1)
                } else {
                    warning('Object is empty and without header. No output will be produced')
                    return(invisible())
                }
            }
        }
    }


    ## #########################################################################
    ## check passed parameters and set sane defaults
    ## #########################################################################

    ## check correct split.cells param
    if (length(split.cells) == 0) {
        warning('split.cells is a vector of length 0, reverting to default value')
        split.cells <- panderOptions('table.split.cells')
    }

    ## check for relative split.cells
    if (all(grepl('%$', split.cells))){
        d <- dim(t)[2]
        split.cells <- as.numeric(gsub('%$', '', split.cells))
        if (sum(split.cells) == 100){
            if (is.infinite(split.tables)){
                warning('Split.tables is an infinite value, so split cells can\'t be suplied as relative value. Reverting to default') #nolint
                split.cells <- panderOptions('table.split.cells')
            } else{
                d <- ifelse(length(rownames(t)) != 0, d, d + 1)
                if (length(split.cells) < d) {
                    cat('d - ', d, '\n')
                    warning('Using relative split.cells require a value for every column and rownames. Reverting to default') #nolint
                    split.cells <- panderOptions('table.split.cells')
                } else {
                    split.cells <- round(split.cells * 0.01 * split.tables)
                }
            }
        } else {
            warning('Supplied relative values don\'t add up to 100%. Reverting to default')
            split.cells <- panderOptions('table.split.cells')
        }
    }

    ## initializing
    mc  <- match.call()
    if (is.null(mc$style)) {
        style <- panderOptions('table.style')
    } else {
        style <- match.arg(style)
    }
    if (row.names.provided) {
      if (identical(row.names, FALSE)) {
        rownames(t) <- NULL
        row.names.provided <- FALSE
      } else {
        rownames(t) <- row.names
      }
    }
    if (!missing(col.names)) {
      colnames(t) <- col.names
    }
    if (is.null(mc$justify)) {
        if (is.null(attr(t, 'alignment'))) {
            if (inherits(t, 'ftable')) {
                justify <- get.alignment(format(t), remove.obious.rownames = !row.names.provided)
            } else {
                justify <- get.alignment(t, remove.obious.rownames = !row.names.provided)
            }
        } else {
            justify <- attr(t, 'alignment')
        }
    }
    if (is.null(mc$caption)) {
        if (is.null(attr(t, 'caption'))) {
            caption <- get.caption()
        } else {
            caption <- attr(t, 'caption')
        }
    }


    ## check if emphasize parameters were passed
    emphasize.parameters <- c('emphasize.rows',
                              'emphasize.cols',
                              'emphasize.cells',
                              'emphasize.strong.rows',
                              'emphasize.strong.cols',
                              'emphasize.strong.cells',
                              'emphasize.italics.rows',
                              'emphasize.italics.cols',
                              'emphasize.italics.cells',
                              'emphasize.verbatim.rows',
                              'emphasize.verbatim.cols',
                              'emphasize.verbatim.cells')
    if (all(sapply(emphasize.parameters, function(p) is.null(mc[[p]]), USE.NAMES = FALSE))) {
        ## check if emphasize parameters were set in attributes
        if (all(sapply(emphasize.parameters, function(p) is.null(attr(t, p)), USE.NAMES = FALSE))) {
            t <- get.emphasize(t)
        }
        ## set emphasize parameters at last
        for (p in emphasize.parameters) {
            assign(p, attr(t, p))
        }
    } else {
        ## some emphasize parameters passed, other should be set to NULL
        for (p in emphasize.parameters) {
            if (is.null(mc[[p]])) {
                assign(p, NULL)
            }
        }
    }
    res <- ''

    ## store missing values
    wm <- which(is.na(t), arr.ind = TRUE)

    ## #########################################################################
    ## update numeric values
    ## #########################################################################

    ## round numbers & cut digits & apply decimal mark & optionally remove trailing zeros
    digits <- check_digits(digits, 'digits', ncol(t))

    ## Temporary conversion of matrix to data.frame,
    ## because matrix columns can't be formatted separately:
    ## as soon as first column is formatted all others are fomatted too.
    ## Formatting each column separately is needed
    ## to support digits and round params as vectors with values for each column.
    rn <- rownames(t)
    cln <- colnames(t)
    if (inherits(t, 'matrix') & !inherits(t, 'table')) {
      rownames(t) <- NULL
      temp.t <- t <- as.data.frame(t)
    } else {
      temp.t <- t
    }
    t.n <- which(sapply(1:ncol(t), function(x) is.numeric(t[, x])))
    if (length(t.n) > 0) {
        ## make sure it's numeric (eg convert integer64 first as sapply having issues)
        ## see eg: sapply(bit64::as.integer64(1:5), format)
        for (j in t.n) {
            t[, j] <- as.numeric(t[, j])
        }
        ## round digits as needed
        round <- check_digits(round, 'round', ncol(t))
        ## for-loop is needed to preserve row/col names and use index to get appropriate value from round vector
        for (j in 1:ncol(temp.t)) {
            if (j %in% t.n) {
                t[, j] <- round(t[, j], digits = round[j])
            }
        }
        if (!keep.trailing.zeros) {
            ## for-loop is needed to preserve row/col names and use index to get appropriate value from digits vector
            for (j in 1:ncol(temp.t)) {
                temp.t[, j] <- sapply(t[, j],
                                      format,
                                      trim         = TRUE,
                                      digits       = digits[j],
                                      decimal.mark = decimal.mark,
                                      big.mark     = big.mark,
                                      quote        = FALSE)
            }
        }
    }

    ## drop unexpected classes and revert back to a common format
    if (keep.trailing.zeros) {
        ## for-loop is needed to preserve row/col names and use index to get appropriate value from digits vector
        for (j in 1:ncol(t)) {
            temp.t[, j] <- format(t[, j],
                                  trim         = TRUE,
                                  digits       = digits[j],
                                  decimal.mark = decimal.mark,
                                  big.mark     = big.mark,
                                  quote        = FALSE)
        }
    } else {
        ## here adds unneeded zero's
        temp.t <- format(temp.t, trim = TRUE, quote = FALSE)
    }

    ## transform to matrix with proper row/colnames
    t <- as.matrix(temp.t)
    colnames(t) <- cln
    rownames(t) <- rn

    ## force all non-numeric cells to character vectors
    wf <- which(sapply(t, is.factor))
    if (length(wf) > 0) {
        t[, wf] <- apply(t[wf], 2, as.character)
    }

    ## replace missing values
    if (length(wm) > 0) {
        t[wm] <- missing
    }

    ## #########################################################################
    ## adding formatting (emphasis, strong etc.)
    ## #########################################################################

    if (is.null(emphasize.italics.rows)) {
        emphasize.italics.rows <- emphasize.rows
    }
    if (is.null(emphasize.italics.cols)) {
        emphasize.italics.cols <- emphasize.cols
    }
    if (is.null(emphasize.italics.cells)) {
        emphasize.italics.cells <- emphasize.cells
    }
    if (!is.null(emphasize.verbatim.rows) && !plain.ascii) {
        check.highlight.parameters(emphasize.verbatim.rows, nrow(t))
        t[emphasize.verbatim.rows, ] <- apply(t[emphasize.verbatim.rows, , drop = FALSE], #nolint
                                              c(1, 2),
                                              pandoc.verbatim.return)
    }
    if (!is.null(emphasize.verbatim.cols) && !plain.ascii) {
        check.highlight.parameters(emphasize.verbatim.cols, ncol(t))
        t[, emphasize.verbatim.cols] <- apply(t[, emphasize.verbatim.cols, drop = FALSE],
                                              c(1, 2), pandoc.verbatim.return)
    }
    if (!is.null(emphasize.verbatim.cells) && !plain.ascii) {
        t <- as.matrix(t)
        check.highlight.parameters(emphasize.verbatim.cells, nrow(t), ncol(t))
        t[emphasize.verbatim.cells] <- sapply(t[emphasize.verbatim.cells], pandoc.verbatim.return)
    }
    if (!is.null(emphasize.italics.rows) && !plain.ascii) {
        check.highlight.parameters(emphasize.italics.rows, nrow(t))
        t[emphasize.italics.rows, ] <- base::t(apply(t[emphasize.italics.rows, , drop = FALSE], #nolint
                                             c(1),
                                             pandoc.emphasis.return))
    }
    if (!is.null(emphasize.strong.rows) && !plain.ascii) {
        check.highlight.parameters(emphasize.strong.rows, nrow(t))
        t[emphasize.strong.rows, ] <- base::t(apply(t[emphasize.strong.rows, , drop = FALSE], #nolint
                                                    c(1),
                                                    pandoc.strong.return))
    }
    if (!is.null(emphasize.italics.cols) && !plain.ascii) {
        check.highlight.parameters(emphasize.italics.cols, ncol(t))
        t[, emphasize.italics.cols] <- apply(t[, emphasize.italics.cols, drop = FALSE], c(2), pandoc.emphasis.return)
    }
    if (!is.null(emphasize.strong.cols) && !plain.ascii) {
        check.highlight.parameters(emphasize.strong.cols, ncol(t))
        t[, emphasize.strong.cols] <- apply(t[, emphasize.strong.cols, drop = FALSE], c(2), pandoc.strong.return)
    }
    if (!is.null(emphasize.italics.cells) && !plain.ascii) {
        t <- as.matrix(t)
        check.highlight.parameters(emphasize.italics.cells, nrow(t), ncol(t))
        t[emphasize.italics.cells] <- pandoc.emphasis.return(t[emphasize.italics.cells])
    }
    if (!is.null(emphasize.strong.cells) && !plain.ascii) {
        t <- as.matrix(t)
        check.highlight.parameters(emphasize.strong.cells, nrow(t), ncol(t))
        t[emphasize.strong.cells] <- pandoc.strong.return(t[emphasize.strong.cells])
    }

    ## #########################################################################
    ## (re)set row and column names
    ## #########################################################################

    ## get (col/row)names if any
    t.colnames <- tryCatch(colnames(t), error = function(e) NULL)
    t.rownames <- tryCatch(rownames(t), error = function(e) NULL)

    ## fixed for incorrect pipelining with rmarkdown (#186)
    if (style == 'rmarkdown') {
        t <- apply(t, c(1,2), function(x) gsub('\\|', '\\\\|', x)) #nolint
        t.rownames <- sapply(t.rownames, function(x) gsub('\\|', '\\\\|', x)) #nolint
    }

    t <- split.large.cells(t)

    ## re-set col/rownames to be passed to split tables
    if (!is.null(t.rownames)) {
        rownames(t) <- t.rownames
    }
    if (!is.null(t.colnames)) {
        colnames(t) <- t.colnames
    }

    ## #########################################################################
    ## compute column width
    ## #########################################################################

    ## header width
    if (!is.null(t.colnames)) {
        t.colnames <- replace(t.colnames, which(t.colnames == ''), '&nbsp;')
        t.colnames <- split.large.cells(t.colnames)
        t.colnames.width <- sapply(t.colnames,
                                   function(x) max(nchar(strsplit(x, '\n')[[1]], type = 'width'), 0),
                                   USE.NAMES = FALSE)
    } else {
        t.colnames.width <- 0
    }

    ## remove traling spaces, because they affect formatting negatively
    t <- apply(t, c(1, 2), function(x) gsub('[[:space:]]*$', '', x))

    ## also dealing with cells split by newlines
    t.width <-  as.numeric(apply(cbind(t.colnames.width, apply(t, 2, function(x) max(sapply(strsplit(x,'\n'), function(x) max(nchar(x, type = 'width'), 0))))), 1, max)) #nolint

    ## remove obvious row.names
    if ((!row.names.provided && (all(t.rownames == 1:nrow(t)) | all(t.rownames == ''))) |
        row.names.provided && row.names == FALSE) {
        t.rownames <- NULL
    }

    if (!is.null(t.rownames) && emphasize.rownames) {
        t.rownames <- pandoc.strong.return(t.rownames)
    }

    if (length(t.rownames) != 0) {

        if ((length(split.cells) <= dim(t)[2]) && (length(split.cells) > 1)) {
            split.cells <- c(panderOptions('table.split.cells'), split.cells)
        }
        t.rownames <- split.large.cells(t.rownames, TRUE)

        if (!is.null(t.colnames)) {
            t.colnames <- c('&nbsp;', t.colnames)
        }
        t.width <- c(max(sapply(strsplit(t.rownames, '\n'), function(x) max(nchar(x, type = 'width'), 0))), t.width)
        t.width[1] <- t.width[1]

        ## if we have a non-breaking space in the header
        if (!is.null(t.colnames)) {
            t.width[1] <- max(t.width[1], 6)
        }

    }

    if (length(justify) != 1) {
        if (length(justify) != length(t.width)) {
            stop(sprintf('Wrong number of parameters (%s instead of *%s*) passed: justify',
                         length(justify), length(t.width)))
        }
    } else {
        if (all (strsplit(justify, '')[[1]] %in% c('c', 'l', 'r') )) {
          if (nchar(justify) != length(t.width)) {
            stop(sprintf('Wrong number of parameters (%s instead of *%s*) passed: justify',
                         nchar(justify), length(t.width)))
          }
          justify <- c(l = 'left', c = 'centre', r = 'right')[ strsplit(justify, '')[[1]] ]
        } else {
          justify <- rep(justify, length(t.width))
        }
    }
    justify <- sub('^center$', 'centre', justify)
    if (!all(justify %in% c('left', 'right', 'centre'))) {
        stop('Invalid values passed for `justify` that can be "left", "right" or "centre/center".')
    }

    ## #########################################################################
    ## Jira format is simple, let's print early
    ## #########################################################################

    if (style == 'jira') {

        if (length(t.rownames) != 0) {
            t <- cbind(t.rownames, t)
        }

        if (length(t.colnames) != 0) {
            res <- paste0('|', paste(wrap(t.colnames, '|'), collapse = ''), '|')
        } else {
            res <- ''
        }

        for (i in seq_len(nrow(t))) {
            res <- paste0(res, '\n|')
            for (j in seq_len(ncol(t))) {
                res <- paste0(res, t[i, j], '|')
            }
        }

        return(paste0(res, '\n\n'))

    }

    ## #########################################################################
    ## split too wide tables
    ## #########################################################################

    ## determine weather separator influences column's width (#164)
    extra.spaces.width <- switch(style,
                                 ## 3 because 2 spaces + 1 separator
                                 'grid' =, 'rmarkdown' = 3, #nolint
                                 'multiline' = , 'simple' = 0) #nolint

    ## add extra 2 spaces for alignment
    if (style %in% c('simple', 'multiline')) {
        t.width <- t.width + 2
    }

    ## +1 for the middle separator
    if (sum(t.width + extra.spaces.width) + 1 > split.tables
        & length(t.width) > 1 + (length(t.rownames) != 0)) {

        t.split <- max(which(cumsum(t.width + extra.spaces.width + 1) > split.tables + 1)[1] - 1, 1)
        if (t.split == 1 & length(t.rownames) != 0) {
            t.split <- 2
        }

        ## update caption
        if (!is.null(caption)) {
            caption <- paste(caption, panderOptions('table.continues.affix'))
        } else {
            caption <- panderOptions('table.continues')
        }

        ## split
        if (length(t.rownames) != 0) {
            justify <- list(justify[1:t.split], justify[c(1, (t.split + 1):length(t.width))])
            t.split <- t.split - 1
        } else {
            justify <- list(justify[1 : (t.split)], justify[c( (t.split + 1):length(t.width) )])
        }
        res <- list(t[, 1 : (t.split), drop = FALSE], t[, (t.split + 1):ncol(t), drop = FALSE])

        ## recursive call
        res <- paste(
            pandoc.table.return(res[[1]], caption = caption, digits = digits, decimal.mark = decimal.mark, round = round, missing = missing, justify = justify[[1]], style = style, split.tables = split.tables, split.cells = split.cells,  keep.trailing.zeros = keep.trailing.zeros, keep.line.breaks = keep.line.breaks, plain.ascii = plain.ascii, use.hyphening = use.hyphening, emphasize.rownames = emphasize.rownames), #nolint
            pandoc.table.return(res[[2]], caption = NULL, digits = digits, decimal.mark = decimal.mark, round = round, justify = justify[[2]], style = style, split.tables = split.tables, split.cells = split.cells,  keep.trailing.zeros = keep.trailing.zeros, keep.line.breaks = keep.line.breaks, plain.ascii = plain.ascii, use.hyphening = use.hyphening, emphasize.rownames = emphasize.rownames)) #nolint

        return(res)

    } else {

        ## #########################################################################
        ## define markdown dialects
        ## #########################################################################

        switch(style,
               'grid'      = {
                   sep.row <- paste0('\n+', paste(sapply(t.width + 2, function(x) repChar('-', x)), collapse = '+'), '+') #nolint
                   sep.top <- sep.row
                   sep.btn <- sep.row
                   sep.hdr <- paste0('+', paste(sapply(t.width + 2, function(x) repChar('=', x)), collapse = '+'), '+')
                   sep.col <- c('| ', ' | ', ' |')
               },
               'multiline' = {
                   sep.row <- '\n'
                   sep.hdr <- paste(sapply(t.width, function(x) repChar('-', x)), collapse = ' ')
                   if (length(t.colnames) != 0) {
                       sep.top <- gsub(' ', '-', sep.hdr)
                   } else {
                       sep.top <- paste(sapply(t.width, function(x) repChar('-', x)), collapse = ' ')
                   }
                   sep.btn <- sep.top
                   sep.col <- c('', ' ', '')
               },
               'simple'   = {
                   sep.row <- ''
                   if (length(t.colnames) == 0) {
                       sep.top <- paste(sapply(t.width, function(x) repChar('-', x)), collapse = ' ')
                       sep.btn <- paste0('\n', sep.top)
                   } else {
                       sep.top <- sep.btn <- ''
                   }
                   sep.hdr <- paste(sapply(t.width, function(x) repChar('-', x)), collapse = ' ')
                   sep.col <- c('', ' ', '')
               },
               'rmarkdown' = {
                   sep.row <- ''
                   sep.top <- ''
                   sep.btn <- ''
                   sep.hdr <- align.hdr(t.width, justify)
                   sep.col <- c('| ', ' | ', ' |')
               })

        if (plain.ascii){
            t <- apply(t, c(1, 2), to.plain.ascii)
            t.rownames <- sapply(t.rownames, to.plain.ascii)
            t.colnames <- sapply(t.colnames, to.plain.ascii)
        }

        ## #########################################################################
        ## Actual printing starts here
        ## #########################################################################

        ## header
        if (length(t.colnames) != 0) {
            res <- paste(res, sep.top,
                         table.expand(t.colnames, t.width, justify, sep.col), sep.hdr, sep = '\n')
        } else {
            if (style == 'rmarkdown') {
                blank.hdr <- paste0('| ', paste(sapply(t.width, function(x) repChar(' ', x)), collapse = ' | '), ' |')
                res <- paste(res, blank.hdr, sep.hdr, sep = '\n')
            } else {
                res <- paste(res, sep.top, sep = '\n')
            }
        }

        ## body
        res <- paste0(res, '\n')

        if (length(t.rownames) != 0) {
            t <- cbind(t.rownames, t)
        }

        res <- paste0(res, paste(apply(t, 1, function(x) paste0(table.expand(x, t.width, justify, sep.col), sep.row)), collapse = '\n')) #nolint

        ## It is possible for a multiline table to have just one row,
        ## but the row should be followed by a blank line (and then
        ## the row of dashes that ends the table), or the table may
        ## be interpreted as a simple table.
        if (style == 'multiline' & nrow(t) == 1 & length(t.colnames) == 0) {
            res <- paste0(res, '\n')
        }

        ## footer
        if (style != 'grid') {
            res <- paste0(res, sep.btn, '\n\n')
        } else {
            res <- paste0(res, '\n\n')
        }

        ## (optional) caption
        check_caption(caption)
        if (!is.null(caption) && caption != '') {
            res <- paste0(res, panderOptions('table.caption.prefix'), caption, '\n\n')
        }

        return(res)

    }
}

#' @export
pandoc.table <- function(...)
    cat(pandoc.table.return(...))

#' Formulas
#'
#' Pandoc's mardown formula.
#' @param x formula
#' @param text text to be written before result in the same line. Typically used by calls from other functions in the package
#' @param max.width maximum width in characters per line
#' @param caption caption (string) to be shown under the formula
#' @param add.line.breaks if to add 2 line breaks after formula
#' @param ... extra arguments passed by from parent call, disregarded
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.formula
#' @examples
#' pandoc.formula(y ~ x)
#' pandoc.formula(formula(paste('y ~ ', paste0('x', 1:12, collapse = ' + '))))
pandoc.formula.return <- function(x, text = NULL, max.width = 80, caption, add.line.breaks = FALSE, ...){
    mc  <- match.call()
    if (is.null(mc$caption)) {
        if (is.null(attr(t, 'caption'))) {
            caption <- get.caption()
        } else {
            caption <- attr(t, 'caption')
        }
    }
    res <- paste(sub('^[ ]*', '', deparse(x, width.cutoff = max.width)), collapse = '')
    if (!is.null(text)) {
        res <- paste(text, res, sep = ' ')
    }
    if (add.line.breaks) {
        res <- paste(res, '\n\n')
    }
    ## (optional) caption
    if (!is.null(caption) && caption != '' && check_caption(caption)) {
        res <- paste0(res, '\n\n', panderOptions('formula.caption.prefix'), caption, '\n\n')
    }
    return(res)
}

#' @export
pandoc.formula <- function(...)
    cat(pandoc.formula.return(...))


#' Dates
#'
#' Pandoc's mardown date.
#' @param x date or vector of dates
#' @param inline if to render vector of dates as inline paragraph or not (as list)
#' @param simplified if just add date formatting to vector of dates
#' @param ... extra arguments passed by from parent call, disregarded
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.date
#' @examples
#' pandoc.date(Sys.Date())
#' pandoc.date(Sys.Date() - 1:10)
#' pandoc.date(Sys.Date() - 1:10, inline = FALSE)
pandoc.date.return <- function(x, inline = TRUE, simplified = FALSE, ...){
    if (length(x) == 1 || simplified){
        format(x, format = panderOptions('date'))
    } else {
        if (inline) {
            p(as.character(format(x, format = panderOptions('date'))))
        } else {
            pandoc.list.return(as.list(format(x, format = panderOptions('date'))), add.end.of.list = FALSE)
        }
    }
}

#' @export
pandoc.date <- function(...){
    cat(pandoc.date.return(...))
}
