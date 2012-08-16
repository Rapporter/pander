#' Add trailing and leading blank line
#'
#' Adds a line break before \emph{and} after the character string(s).
#' @param x character vector
#' @export
add.blank.lines <- function(x)
    sprintf('\n%s\n', x)


#' Remove more then two joined newlines
#' @param x character vector
#' @export
#' @examples
#' remove.extra.newlines(c('\n\n\n', '\n\n', '\n'))
remove.extra.newlines <- function(x)
    gsub('[\n][\n]+', '\n\n', x)


#' Trim leading and trailing spaces
#' @param x character vector
#' @return character vector
#' @export
#' @seealso \code{trim.space} in \code{rapport} package
trim.spaces <- function(x)
    gsub('^[[:space:]]+|[[:space:]]+$', '', x)

#' Repeating chars
#'
#' Repeating a string \code{n} times and returning a concatenated character vector.
#' @param x string to repeat
#' @param n integer
#' @param sep separator between repetitions
#' @return character vector
#' @export
repChar <- function(x, n, sep = '')
    paste(rep.int(x, n), collapse = sep)


#' Inline Printing
#'
#' \code{\link{p}} merges elements of a vector in one string for the sake of pretty inline printing. Default parameters are read from appropriate \code{option} values (see argument description for details). This function allows you to put the results of an expression that yields a variable \emph{inline}, by wrapping the vector elements with the string provided in \code{wrap}, and separating elements by main and ending separator (\code{sep} and \code{copula}). In case of a two-length vector, value specified in \code{copula} will be used as a separator. You can also control the length of provided vector by altering an integer value specified in \code{limit} argument (defaults to \code{Inf}).
#' @param x an atomic vector to get merged for inline printing
#' @param wrap a string to wrap vector elements (uses value set in \code{p.wrap} option: \code{"_"} by default, which is a markdown-friendly wrapper and it puts the string in \emph{italic})
#' @param sep a string with the main separator, i.e. the one that separates all vector elements but the last two (uses the value set in \code{p.sep} option - \code{","} by default)
#' @param copula a string with ending separator - the one that separates the last two vector elements (uses the value set in \code{p.copula} option, \code{"and"} by default)
#' @param limit maximum character length (defaults to \code{Inf}initive  elements)
#' @return a string with concatenated vector contents
#' @examples
#' p(c("fee", "fi", "foo", "fam"))
#' ## [1] "_fee_, _fi_, _foo_ and _fam_"
#' p(1:3, wrap = "")
#' ## [1] "1, 2 and 3"
#' p(LETTERS[1:5], copula = "and the letter")
#' ## [1] "_A_, _B_, _C_, _D_ and the letter _E_"
#' p(c("Thelma", "Louise"), wrap = "", copula = "&")
#' ## [1] "Thelma & Louise"
#' @export
#' @author Aleksandar Blagotić
#' @references This function was moved from \code{rapport} package: \url{http://rapport-package.info/}.
p <- function(x, wrap = panderOptions('p.wrap'), sep = panderOptions('p.sep'), copula = panderOptions('p.copula'), limit = Inf){

    stopifnot(is.vector(x))
    stopifnot(all(sapply(list(wrap, sep, copula), function(x) is.character(x) && length(x) == 1)))
    x.len <- length(x)
    stopifnot(x.len > 0)
    stopifnot(x.len <= limit)

    ## prettify numbers
    if (is.numeric(x)) {
        x <- round(x, panderOptions('round'))
        x <- format(x, trim = TRUE, digits = panderOptions('digits'), decimal.mark = panderOptions('decimal.mark'))
    }

    if (x.len == 1)
        wrap(x, wrap)
    else if (x.len == 2)
        paste(wrap(x, wrap), collapse = sprintf(' %s ', copula))
    else
        paste(paste(wrap(head(x, -1), wrap), collapse = sep), copula, wrap(tail(x, 1), wrap))
}


#' Wrap Vector Elements
#'
#' Wraps vector elements with string provided in \code{wrap} argument.
#' @param x a vector to wrap
#' @param wrap a string to wrap around vector elements
#' @return a string with wrapped elements
#' @examples \dontrun{
#' wrap("foobar")
#' wrap(c("fee", "fi", "foo", "fam"), "_")
#' }
#' @export
#' @author Aleksandar Blagotić
#' @references This function was moved from \code{rapport} package: \url{http://rapport-package.info/}.
wrap <- function(x, wrap = '"'){
    stopifnot(is.vector(x))
    sprintf('%s%s%s', wrap, x, wrap)
}


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

    if (!is.character(x))
        stop('Only character strings are allowed.')
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

    if (!is.vector(x))
        stop('Sorry, vectors only!')

    f.e  <- gsub('*', '\\*', f, fixed = TRUE)
    x    <- trim.spaces(x)
    w    <- which(!grepl(sprintf('^%s.*%s$', f.e, f.e), x))
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
    pandoc.add.formatting(x, '~~')

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
#' ## different styles/formats
#' pandoc.verbatim('FOO')
#'
#' src <- c('FOO', 'indent', 'BAR' )
#' pandoc.verbatim(src)
#' pandoc.verbatim.return(src)
#' pandoc.verbatim(c('FOOO\nBAR  ', ' I do R'), 'indent')
#' pandoc.verbatim(c('FOOO\nBAR  ', ' I do R'), 'delim')
#'
#' ## add highlighting and HTML/LaTeX ID and classes (even custom attribute)
#' pandoc.verbatim(c('cat("FOO")', 'mean(bar)'), 'delim', '.R #MyCode custom_var="10"')
pandoc.verbatim.return <- function(x, style = c('inline', 'indent', 'delim'), attrs = '') {

    style <- match.arg(style)
    if (style != 'delim' & !missing(attrs))
        warning('Providing attrs is only meaningful with delimited blocks.')

    switch(style,
           'inline' = paste0('`', trim.spaces(paste(x, collapse = ' ')), '`'),
           'indent' = sprintf('\n%s\n', paste(paste0(repChar(' ', 4), unlist(strsplit(trim.spaces(x), '\n')), collapse = '\n'))),
           'delim'  = paste0('\n', repChar('`', 7), ifelse(attrs == '', '', sprintf('{%s}', attrs)), '\n', paste(trim.spaces(x), collapse = '\n'), '\n', repChar('`', 7), '\n')
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
#' @examples
#' pandoc.image('foo.png')
#' pandoc.image('foo.png', 'Nice image, huh?')
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
pandoc.image.return <- function(img, caption = NULL) {
    if (is.null(caption))
        caption <- ''
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

    if (missing(style))
        style <- panderOptions('header.style')
    else
        style <- match.arg(style)

    if (!is.numeric(level))
        stop('Wrong level provided!')
    if (any((style == 'atx' & level > 6), (style == 'setext' & level > 2)))
        stop('Too high level provided!')
    if (level < 1)
        stop('Too low level provided!')

    res <- switch(style,
           'atx'    = paste(repChar('#', level), x),
           'setext' = paste(x, repChar(ifelse(level == 1, '=', '-'), nchar(x)), sep = '\n')
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
#' pandoc.title('Gergely Daróczi', 'Render pandoc in R', '2012-05-16')
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

    if ((author == '') & (title == '') & (date != ''))
        stop('You cannot create a title with only date specified!')

    ## updating title tags
    if (author != '')
        author <- paste('%', paste(author, collapse = '; '))
    if (title != '')
        title  <- paste0('% ', gsub('[\t ][\t ]*', '  ', gsub('\n', '\n  ', paste(title, collapse = '\n'))))

    ## formatting result
    if (title == '') {               # author
        res <- paste0('%\n', author)
    } else {
        if (date == '') {
            if (author == '')        # title
                res <- title
            else                        # author & title
                res <- paste(title, author, sep = '\n')
        } else {
            date <- paste0('% ', gsub('\n', ' ', date)[1])
            if (author == '')        # title & date
                res <- paste(title, '%', date, sep = '\n')
            else                        # author & title & date
                res <- paste(title, author, date, sep = '\n')

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
#' l <- list("First list element", rep.int('sub element', 5), "Second element", list('F', 'B', 'I', c('phone', 'pad', 'talics')))
#' pandoc.list(l)
#' pandoc.list(l, loose = TRUE)
#' pandoc.list(l, 'roman')
#' @importFrom utils as.roman
pandoc.list.return <- function(elements, style = c('bullet', 'ordered', 'roman'), loose = FALSE, add.line.breaks = TRUE, add.end.of.list = TRUE, indent.level = 0) {

    if (!is.logical(loose))
        stop('Wrong argument provided: loose')

    if (missing(style))
        style <- panderOptions('list.style')
    else
        style <- match.arg(style)

    elements.l <- length(elements)
    marker     <- switch(style,
                         'bullet'  = rep('* ', elements.l),
                         'ordered' = paste0(1:elements.l, '. '),
                         'roman'   = paste0(as.roman(1:elements.l), '. '))

    i.lag <- 0
    res <- ifelse(add.line.breaks, '\n', '')
    res <- paste(sapply(1:elements.l, function(i) {
        if (length(elements[[i]]) == 1) {
            paste0(paste(rep(' ', indent.level * 4), collapse = ''), marker[i-i.lag], elements[i])
        } else {
            i.lag <<- i.lag + 1
            pandoc.list.return(elements[[i]], style, loose, FALSE, FALSE, indent.level + 1)
        }
    }), collapse = '\n', ifelse(loose, '\n', ''))

    if (add.end.of.list)
        res <- paste0(res, ifelse(loose, '', '\n\n'), '<!-- end of list -->\n')
    if (add.line.breaks)
        res <- add.blank.lines(res)

    return(res)

}

#' @export
pandoc.list <- function(...)
    cat(pandoc.list.return(...))


#' Create a table
#'
#' Creates a Pandoc's markdown style table with optional caption.
#'
#' This function will try to make pretty the provided R object's content like: rounding numbers, auto-recognizing if row names should be included etc.
#'
#' And also tries to split cells with line breaks or even the whole table to separate parts on demand. See the parameters above.
#' @param t data frame, matrix or table
#' @param caption string
#' @param digits passed to \code{format}
#' @param decimal.mark passed to \code{format}
#' @param round passed to \code{round}
#' @param justify see \code{prettyNum}
#' @param style which Pandoc style to use: \code{simple}, \code{multiline} or grid
#' @param split.tables where to split wide tables to separate tables. The default value (\code{80}) suggests the conventional number of characters used in a line, feel free to change (e.g. to \code{Inf} to disable this feature) if you are not using a VT100 terminal any more :)
#' @param split.cells where to split cells' text with line breaks. Default to \code{30}, to disable set to \code{Inf}.
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.table
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.table(mtcars)
#'
#' ## caption
#' pandoc.table(mtcars, 'Motor Trend Car Road Tests')
#'
#' ## other input/output formats
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
#' m <- data.frame(a=c(1, -500, 10320, 23, 77), b=runif(5), c=c('a', 'bb', 'ccc', 'dddd', 'eeeee'))
#' pandoc.table(m)
#' pandoc.table(m, justify = c('right', 'left', 'centre'))
#'
#' ## splitting up too wide tables
#' pandoc.table(mtcars)
#' pandoc.table(mtcars, caption = 'Only once after the first part!')
#'
#' ## tables with line breaks in cells
#' ## Note: line breaks are removed from table content and added automatically based on "split.cells" parameter!
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
#' pandoc.table(t, style = "simple")
#' tryCatch(pandoc.table(t, style = "simple", split.cells = 5), error = function(e) 'Yeah, no newline support in simple tables')
pandoc.table.return <- function(t, caption = NULL, digits = panderOptions('digits'), decimal.mark = panderOptions('decimal.mark'), round = panderOptions('round'), justify = 'left', style = c('multiline', 'grid', 'simple'), split.tables = panderOptions('table.split.table'), split.cells = panderOptions('table.split.cells')) {

    ## helper functions
    table.expand <- function(cells, cols.width, justify, sep.cols) {

        df  <- data.frame(txt = cells, width = cols.width, justify = justify)

        if (any(grepl('\n', df$txt))) {

            if (style == 'simple')
                stop('Pandoc does not support newlines in simple table format!')

            res <- lapply(as.character(df$txt), function(x) strsplit(x, '\n')[[1]])
            res.lines <- max(sapply(res, length))
            res <- paste(sapply(1:res.lines, function(i) table.expand(sapply(res, function(x) ifelse(is.na(x[i]), '  ', x[i])), cols.width, justify, sep.cols)), collapse = '\n')
            return(res)

        } else {

            res <- apply(df, 1, function(x) format(x[1], justify = x[3], width = x[2]))
            return(paste0(sep.cols[1], paste(res, collapse = sep.cols[2]), sep.cols[3]))

        }

    }
    split.large.cells <- function(cells)
        sapply(cells, function(x) {
            x <- paste(strwrap(x, width = split.cells), collapse = '\n')
            if (x == 'NA')
                ''
            else
                x
        }, USE.NAMES = FALSE)

    ## initializing
    res <- ''
    if (missing(style))
        style <- panderOptions('table.style')
    else
        style <- match.arg(style)

    ## format numeric & convert to string
    if (length(dim(t)) == 0) {  # named char
        ## just numbers
        t.n <- as.numeric(which(sapply(t, is.numeric)))
        if (length(t.n) > 0)
            t[t.n] <- round(t[t.n], round)
    }
    if (length(dim(t)) == 1) {
        ## just numbers
        t.n <- as.numeric(which(apply(t, 1, is.numeric)))
        if (length(t.n) > 0)
            t[t.n] <- round(t[t.n], round)
    }
    if (length(dim(t)) == 2) {
        ## just numbers (not just column-wise to make it general)
        t.n <- as.numeric(which(apply(t, 2, is.numeric)))
        if (length(t.n) > 0)
            t[, t.n] <- round(t[, t.n], round)
    }
    t <- format(t, trim = TRUE, digits = digits, decimal.mark = decimal.mark)

    ## TODO: adding formatting (emphasis, strong etc.)

    ## helper variables & split too long (30+ chars) cells
    if (length(dim(t)) < 2) {

        if (length(dim(t)) == 0)
            t[1:length(t)] <- split.large.cells(t)
        else
            t[1:dim(t)] <- split.large.cells(t)

        t.rownames  <- NULL
        t.colnames  <- names(t)
        if (!is.null(t.colnames)) {
            t.colnames       <- split.large.cells(t.colnames)
            t.colnames.width <- sapply(t.colnames, function(x) max(nchar(strsplit(x, '\n')[[1]]), 0), USE.NAMES = FALSE) + 2
        } else {
            t.colnames.width <- 0
        }

        if (length(dim(t)) == 0)
            t.width <- as.numeric(apply(cbind(t.colnames.width, as.numeric(sapply(t, nchar))), 1, max))
        else
            t.width <- as.numeric(apply(cbind(t.colnames.width, as.numeric(apply(t, 1, nchar))), 1, max))

    } else {

        t <- apply(t, c(1,2), split.large.cells)

        t.rownames  <- rownames(t)
        t.colnames  <- colnames(t)
        if (!is.null(t.colnames)) {
            t.colnames  <- split.large.cells(t.colnames)
            t.colnames.width <- sapply(t.colnames, function(x) max(nchar(strsplit(x, '\n')[[1]]), 0), USE.NAMES = FALSE) + 2
        } else {
            t.colnames.width <- 0
        }

        ## also dealing with cells split by newlines
        t.width     <-  as.numeric(apply(cbind(t.colnames.width, apply(t, 2, function(x) max(sapply(strsplit(x,'\n'), function(x) max(nchar(x), 0))))), 1, max))

        ## remove obvious row.names
        if (all(rownames(t) == 1:nrow(t)))
            t.rownames <- NULL

        if (!is.null(t.rownames))
            t.rownames <- pandoc.strong.return(t.rownames)

    }

    if (length(t.rownames) != 0) {

        t.rownames <- split.large.cells(t.rownames)
        t.colnames <- c('&nbsp;', t.colnames)
        t.width <- c(max(sapply(strsplit(t.rownames, '\n'), function(x) max(nchar(x), 0))), t.width)
        t.width[1] <- t.width[1] + 2

    }

    if (length(justify) != 1) {
        if (length(t.rownames) != 0)
            if (length(justify) != length(t.width))
                stop(sprintf('Wrong number of parameters (%s instead of *%s*) passed: justify', length(justify), length(t.width)))
    } else {
        justify <- rep(justify, length(t.width))
    }

    ## split too wide tables
    if (sum(t.width + 4) > split.tables) {

        t.split <- which(cumsum(t.width + 4) > split.tables)[1]
        t.col.n <- ifelse(length(dim(t)) > 1, ncol(t), length(t))

        ## do not make one column tables
        if (t.split >= t.col.n)
            t.split <- t.split - 1

        ## update caption
        if (!is.null(caption))
            caption <- paste(caption, '(continued below)')
        else
            caption <- 'Table continues below'

        ## split
        if (length(t.rownames) != 0) {
            t.split <- t.split - 1
            justify <- list(justify[1:t.split], justify[c(1, (t.split + 1):length(t.width))])
        } else {
            justify <- list(justify[1:(t.split - 1)], justify[c(t.split:length(t.width))])
        }

        if (length(dim(t)) > 1)
            res <- list(t[, 1:(t.split-1)], t[, t.split:ncol(t)])
        else
            res <- list(t[1:(t.split-1)], t[t.split:length(t)])

        ## recursive call
        res <- paste(pandoc.table.return(res[[1]], caption = caption, digits = digits, decimal.mark = decimal.mark, round = round, justify = justify[[1]], style = style), pandoc.table.return(res[[2]], caption = NULL, digits = digits, decimal.mark = decimal.mark, round = round, justify = justify[[2]], style = style))

        return(res)

    } else {

        switch(style,
               'grid'      = {
                   sep.row <- paste0('\n+', paste(sapply(t.width + 2, function(x) repChar('-', x)), collapse = '+'), '+')
                   sep.top <- sep.row
                   sep.btn <- sep.row
                   sep.hdr <- paste0('+', paste(sapply(t.width + 2, function(x) repChar('=', x)), collapse = '+'), '+')
                   sep.col <- c('| ', ' | ', ' |')
               },
               'multiline' = {
                   sep.row <- '\n'
                   sep.hdr <- paste(sapply(t.width, function(x) repChar('-', x)), collapse = ' ')
                   sep.top <- gsub(' ', '-', sep.hdr)
                   sep.btn <- sep.top
                   sep.col <- c('', ' ', '')
               },
               'simple'   = {
                   sep.row <- ''
                   sep.top <- ''
                   sep.btn <- ''
                   sep.hdr <- paste(sapply(t.width, function(x) repChar('-', x)), collapse = ' ')
                   sep.col <- c('', ' ', '')
               })

        ## header
        if (length(t.colnames) != 0) {
            res <- paste(res, sep.top, table.expand(t.colnames, t.width, justify, sep.col), sep.hdr, sep = '\n')
        } else {
            res <- paste(res, sep.top, sep = '\n')
        }

        ## body
        res <- paste0(res, '\n')
        b   <- t

        if (length(t.rownames) != 0)
            b <- cbind(t.rownames, b)

        if (length(dim(t)) > 1)
            res <- paste0(res, paste(apply(b, 1, function(x) paste0(table.expand(x, t.width, justify, sep.col), sep.row)), collapse = '\n'))
        else
            res <- paste0(res, paste0(table.expand(b, t.width, justify, sep.col), sep.row), collapse = '\n')

        ## footer
        if (style != 'grid')
            res <- paste0(res, sep.btn, '\n\n')
        else
            res <- paste0(res, '\n\n')

        ## (optional) caption
        if (!is.null(caption))
            res <- sprintf('%sTable: %s\n\n', res, caption)

        return(res)

    }
}

#' @export
pandoc.table <- function(...)
    cat(pandoc.table.return(...))

#' Adds caption in current block
#'
#' This is a helper function to be used \emph{inside brew blocks} to add a caption to the returning image/table.
#' @param x string
#' @export
set.caption <- function(x)
    assign('caption', x , envir = pander:::storage)


#' Sets alignment for tables
#'
#' This is a helper function to be used \emph{inside brew blocks} to update the alignment (\code{justify} parameter of \code{pandoc.table}) of the returning table. Possible values are: \code{centre}, \code{right}, \code{left}
#' @param align character vector which length equals to one (would be repeated \code{n} times) ot \code{n} - where \code{n} equals to the number of columns in the following table
#' @param row.names string holding the alignment of the (optional) row names
#' @export
set.alignment <- function(align = 'centre', row.names = 'left')
    assign('alignment', list(align = align, row.names = row.names) , envir = pander:::storage)


#' Add significance stars
#'
#' This function adds significance stars to passed \code{p} value(s) as: one star for value below \code{0.05}, two for \code{0.01} and three for \code{0.001}.
#' @param p numeric vector or tabular data
#' @return character vector
#' @export
add.significance.stars <- function(p) {

    if (inherits(p, c("matrix", "data.frame")) && length(dim(p)) == 2) {
        apply(p, c(1,2), add.significance.stars)
    } else {
        if (length(p) > 1) {
            sapply(p, add.significance.stars)
        } else {
            s <- ifelse(p > 0.05, '', ifelse(p > 0.01, ' *', ifelse(p > 0.001, ' * *', ' * * *')))
            paste0(p(p), s)
        }
    }
}
