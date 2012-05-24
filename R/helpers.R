## TODO: add more functionality to pandoc.table (see below)

#' Add trailing and leading blank line
#'
#' Adds a line break before *and* after the character string(s).
#' @param x character vector
#' @export
add.blank.lines <- function(x)
    sprintf('\n%s\n', x)


#' Trim leading and trailing spaces
#' @param x character vector
#' @return character vector
#' @export
#' @seealso \code{\link{trim.space}}
trim.spaces <- function(x)
    gsub('^[[:space:]]+|[[:space:]]+$', '', x)

#' Repeating chars
#'
#' Repeating a string \code{n} times and returning a concatenated character vector.
#' @param x string to repeat
#' @param n integer
#' @param sep separator between repeatitions
#' @return character vector
#' @export
rep.char <- function(x, n, sep = '')
    paste(rep.int(x, n), collapse = sep)


#' Pretty print vectors
#' @param x vector
#' @return character string
#' @references Credit goes to Aleksandar Blagotić for the idea and implementation in rapport package. \url{http://github.com/aL3xa/rapport/blob/master/R/rp_helpers.R}
#' @export
#' @examples
#' p(1:5)
p <- function(x)
    sprintf('%s and *%s*', paste(sprintf('*%s*', head(x, -1)), collapse = ', '), tail(x, 1))


#' Indent text
#'
#' Indent all (optionally concatenated) lines of provided text with given level.
#' @param x character vector
#' @param level integer
#' @author Gergely Daróczi
#' @export
#' @examples
#' pandoc.indent('FOO', 1)
#' pandoc.indent(pandoc.table.return(table(mtcars$gear)), 2)
#' cat(pandoc.indent(pandoc.table.return(table(mtcars$gear)), 3))
pandoc.indent <- function(x, level = 0) {

    if (!is.character(x))
        stop('Only character strings are allowed.')
    indent <- rep.char(' ', level*4)
    res <- paste0(indent, gsub('\n', paste0('\n', indent), x))

    ## remove wasted space
    res <- gsub(' *\n', '\n', res)
    res <- sub(' *$', '', res)

    res

}

#' Paragraphs
#'
#' Pandoc style paragraph.
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
    add.blank.lines(paste(x, collapse = '\n'))

#' @export
pandoc.p <- function(...)
    cat(pandoc.p.return(...))


#' Strong emphasis
#'
#' Pandoc style strong emphasis format (e.g. \code{**FOO**}) is added to character string.
#' @param x character vector
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.strong
#' @seealso \code{\link{pandoc.emphasis}} \code{\link{pandoc.strikeout}} \code{\link{pandoc.verbatim}}
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.strong('FOO')
#' pandoc.strong.return('FOO')
pandoc.strong.return <- function(x)
    paste0('**', trim.spaces(x), '**')

#' @export
pandoc.strong <- function(...)
    cat(pandoc.strong.return(...))

#' Emphasis
#'
#' Pandoc style emphasis format (e.g. \code{*FOO*}) is added to character string.
#' @param x character vector
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.empasis
#' @seealso \code{\link{pandoc.strong}} \code{\link{pandoc.strikeout}} \code{\link{pandoc.verbatim}}
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.emphasis('FOO')
#' pandoc.emphasis.return('FOO')
pandoc.emphasis.return <- function(x)
    paste0('*', trim.spaces(x), '*')

#' @export
pandoc.emphasis <- function(...)
    cat(pandoc.emphasis.return(...))


#' Add strikeout
#'
#' Pandoc style strikeout format (e.g. \code{~~FOO~~}) is added to character string.
#' @param x character vector
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.strikeout
#' @seealso \code{\link{pandoc.emphasis}} \code{\link{pandoc.strong}} \code{\link{pandoc.verbatim}}
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.strikeout('FOO')
#' pandoc.strikeout.return('FOO')
pandoc.strikeout.return <- function(x)
    paste0('~~', trim.spaces(x), '~~')

#' @export
pandoc.strikeout <- function(...)
    cat(pandoc.strikeout.return(...))


#' Add verbatim
#'
#' Pandoc style verbatim format (e.g. \code{`FOO`}) is added to character string.
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
           'indent' = sprintf('\n%s\n', paste(paste0(rep.char(' ', 4), unlist(strsplit(trim.spaces(x)), '\n'))), collapse = '\n'),
           'delim'  = paste0('\n', rep.string('`', 7), ifelse(attrs == '', '', sprintf('{%s}', attrs)), '\n', paste(trim.spaces(x), collapse = '\n'), '\n', rep.string('`', 7), '\n')
           )

}

#' @export
pandoc.verbatim <- function(...)
    cat(pandoc.verbatim.return(...))


#' Create pandoc link
#' @param url hyperlink
#' @param text link text
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.link.return
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
#' Creates a pandoc style image hyperlink.
#' @param img image path
#' @param caption text
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.image
#' @examples
#' pandoc.image('foo.png')
#' pandoc.image('foo.png', 'Nice image, huh?')
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
pandoc.image.return <- function(img, caption = '')
    sprintf('![%s](%s)', caption, img)

#' @export
pandoc.image <- function(...)
    cat(pandoc.image.return(...))


#' Footnote
#'
#' Creates a pandoc style footnote.
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
#' Creates a pandoc style horizontal line with trailing and leading newlines.
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
#' Creates a pandoc/markdown style header with given level.
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

    style <- match.arg(style)
    if (!is.numeric(level))
        stop('Wrong level provided!')
    if (any((style == 'atx' & level > 6), (style == 'setext' & level > 2)))
        stop('Too hight level provided!')
    if (level < 1)
        stop('Too low level provided!')

    res <- switch(style,
           'atx'    = paste(rep.char('#', level), x),
           'setext' = paste(x, rep.char(ifelse(level == 1, '=', '-'), nchar(x)), sep = '\n')
           )

    add.blank.lines(res)

}

#' @export
pandoc.header <- function(...)
    cat(pandoc.header.return(...))


#' Create title block
#'
#' Creates a pandoc style title block with optional author, title and date fields.
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
#' pandoc.title(, 'Render pandoc in R', '2012-05-16')
pandoc.title.return <- function(author, title, date) {

    if (missing(author) & missing(title) & !missing(date))
        stop('You cannot create a title with only date specified!')

    ## updating title tags
    if (!missing(author))
        author <- paste('%', paste(author, collapse = '; '))
    if (!missing(title))
        title  <- paste0('% ', gsub('[\t ][\t ]*', '  ', gsub('\n', '\n  ', paste(title, collapse = '\n'))))

    ## formatting result
    if (missing(title)) {               # author
        res <- paste0('%\n', author)
    } else {
        date <- paste0('% ', gsub('\n', ' ', date)[1])
        if (missing(date)) {
            if (missing(author))        # title
                res <- title
            else                        # author & title
                res <- paste(title, author, sep = '\n')
        } else {
            if (missing(author))        # title & date
                res <- paste(title, '%', date, sep = '\n')
            else                        # author & title & date
                res <- paste(title, author, date, sep = '\n')

        }
    }

    add.blank.lines(res)

}

#' @export
pandoc.title <- function(...)
    cat(pandoc.title.return(...))


#' Create a list
#'
#' Creates a pandoc style list from provided character vector/list.
#' @param elements character vector of strings
#' @param style the required style of the list
#' @param loose adding a newline between elements
#' @param add.line.breaks adding a leading and trailing newline before/after the list
#' @param add.end.of.list adding a separator comment after the list
#' @param indent.level the level of ident
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
#' l <- list("First list element", paste0(1:5, '. subelement'), "Second element", list('F', 'B', 'I', c('phone', 'pad', 'talics')))
#' pandoc.list(l)
#' pandoc.list(l, loose = TRUE)
#' pandoc.list(l, 'roman')
#' @importFrom utils as.roman
pandoc.list.return <- function(elements, style = c('bullet', 'ordered', 'roman'), loose = FALSE, add.line.breaks = TRUE, add.end.of.list = TRUE, indent.level = 0) {

    if (!is.logical(loose))
        stop('Wrong argument provided: loose')

    elements.l <- length(elements)
    style      <- match.arg(style)
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
#' Creates a pandoc style "grid" table with optional caption.
#'
#' This function will try to make pretty the provided R object's content like: rounding numbers, auto-recognizing if row names should be included etc.
#' @param t data frame, matrix or table
#' @param caption string
#' @param digits see \code{prettyNum}
#' @param decimal.mark see \code{prettyNum}
#' @param justify see \code{prettyNum}
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
#' @aliases pandoc.table
#' @note Pandoc does not support justify parameter for grid tables ATM. ## TODO: multiline?
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
#' @examples
#' pandoc.table(mtcars)
#'
#' ## caption
#' pandoc.table(mtcars, 'Motor Trend Car Road Tests')
#'
#' ## other input/output formats
#' pandoc.table(mtcars, decimal.mark = ',')
#' pandoc.table(mtcars, decimal.mark = ',', justify = 'right')
#' pandoc.table(matrix(sample(1:1000, 25), 5, 5))
#' pandoc.table(matrix(runif(25), 5, 5))
#' pandoc.table(matrix(runif(25), 5, 5), digits = 5)
#' pandoc.table.return(table(mtcars$am))
#' pandoc.table(table(mtcars$am, mtcars$gear))
#' pandoc.table(table(state.division, state.region))
#' pandoc.table(table(state.division, state.region), justify = 'centre')
#'
#' m <- data.frame(a=c(1, -500, 10320, 23, 77), b=runif(5), c=c('a', 'bb', 'ccc', 'dddd', 'eeeee'))
#' pandoc.table(m)
#' pandoc.table(m, justify = c('right', 'left', 'centre'))
pandoc.table.return <- function(t, caption, digits = 2, decimal.mark = '.', justify = 'left') {

    ## helper functions
    table.sep  <- function(cols.width, sep = '+')
        paste0(sep, paste(sapply(cols.width+2, function(x) rep.char('-', x)), collapse = sep), sep)

    table.expand <- function(cells, cols.width, justify) {

        df  <- data.frame(txt = cells, width = cols.width, justify = justify)
        res <- apply(df, 1, function(x) format(x[1], justify = x[3], width = x[2]))
        paste0('| ', paste(res, collapse = ' | '), ' |')

    }

    ## intializing result
    res <- ''

    ## format numerics & convert to string
    t <- format(t, trim = TRUE, digits = digits, decimal.mark = decimal.mark)

    ## TODO: adding formatting (emphasis, strong etc.)

    ## helper variables
    if (length(dim(t)) == 1) {

        t.colnames  <- names(t)
        t.rownames  <- NULL
        t.width     <- as.numeric(apply(t, 1, nchar))

    } else {

        t.colnames  <- colnames(t)
        t.rownames  <- rownames(t)
        t.width     <- as.numeric(apply(cbind(nchar(t.colnames), apply(t, 2, function(x) max(nchar(x)))), 1, max))

        ## remove obvoius row.names
        if (all(rownames(t) == 1:nrow(t)))
        t.rownames <- NULL

    }


    if (length(t.rownames) != 0) {

        t.colnames <- c('', t.colnames)
        t.width <- c(max(nchar(t.rownames)), t.width)

    }

    if (length(justify) != 1) {
        if (length(t.rownames) != 0)
            if (length(justify) != length(t.width))
                stop(sprintf('Wrong number of parameters (%s instead of *%s*) passed: justify', length(justify), length(t.width)))
    } else {
        justify <- rep(justify, length(t.width))
    }

    t.sep <- table.sep(t.width)

    ## header
    if (length(t.colnames) != 0) {
        res <- paste(res, t.sep, table.expand(t.colnames, t.width, justify[1]), gsub('-', '=', t.sep), sep = '\n')
    } else {
        res <- paste(res, t.sep, sep = '\n')
    }

    ## body
    res <- paste0(res, '\n')
    b   <- t

    if (length(t.rownames) != 0)
        b <- cbind(t.rownames, b)

    if (length(dim(t)) > 1)
        res <- paste0(res, paste(apply(b, 1, function(x) paste(table.expand(x, t.width, justify), t.sep, sep = '\n')), collapse = '\n'))
    else
        res <- paste0(res, paste(table.expand(b, t.width, justify), t.sep, sep = '\n'), collapse = '\n')

    res <- paste0(res, '\n\n')

    ## (optional) caption
    if (!missing(caption))
        res <- sprintf('%s    Table: %s\n\n', res, caption)

    return(res)

}

#' @export
pandoc.table <- function(...)
    cat(pandoc.table.return(...))
