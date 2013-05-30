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

    attributes(x) <- NULL
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
#' @seealso \code{\link{set.caption}}
#' @note The \code{caption} text is read from an internal buffer which defaults to \code{NULL}. To update that, call \code{link{set.caption}} before.
#' @examples
#' pandoc.image('foo.png')
#' pandoc.image('foo.png', 'Nice image, huh?')
#' @references John MacFarlane (2012): _Pandoc User's Guide_. \url{http://johnmacfarlane.net/pandoc/README.html}
pandoc.image.return <- function(img, caption = storage$caption) {

    if (is.null(caption))
        caption <- ''

    ## truncating caption buffer if needed
    if (!is.null(storage$caption))
        storage$caption <- NULL

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
        if (length(elements[[i]]) == 1 && !is.list(elements[[i]])) {
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
#' Creates a Pandoc's markdown style table with optional caption and some other tweaks. See 'Details' below.
#'
#' This function takes any tabular data as its first argument and will try to make it pretty like: rounding and applying \code{digits} and custom \code{decimal.mark} to numbers, auto-recognizing if row names should be included, setting alignment of cells and dropping trailing zeros by default.
#'
#' \code{pandoc.table} also tries to split large cells with line breaks or even the whole table to separate parts on demand. Other arguments lets the use to highlight some rows/cells/cells in the table with italic or bold text style.
#'
#' For more details please see the parameters above and passed arguments of \code{\link{panderOptions}}.
#' @param t data frame, matrix or table
#' @param caption caption (string) to be shown under the table
#' @param digits passed to \code{format}
#' @param decimal.mark passed to \code{format}
#' @param round passed to \code{round}
#' @param justify defines alignment in cells passed to \code{format}. Can be \code{left}, \code{right} or \code{centre}, which latter can be also spelled as \code{center}. Defaults to \code{centre}.
#' @param style which Pandoc style to use: \code{simple}, \code{multiline}, \code{grid} or \code{rmarkdown}
#' @param split.tables where to split wide tables to separate tables. The default value (\code{80}) suggests the conventional number of characters used in a line, feel free to change (e.g. to \code{Inf} to disable this feature) if you are not using a VT100 terminal any more :)
#' @param split.cells where to split cells' text with line breaks. Default to \code{30}, to disable set to \code{Inf}.
#' @param keep.trailing.zeros to show or remove trailing zeros in numbers on a column basis width
#' @param emphasize.rows a vector for a two dimensional table specifying which rows to emphasize
#' @param emphasize.cols a vector for a two dimensional table specifying which cols to emphasize
#' @param emphasize.cells a vector for one-dimensional tables or a matrix like structure with two columns for row and column indexes to be emphasized in two dimensional tables. See e.g. \code{which(..., arr.ind = TRUE)}
#' @param emphasize.strong.rows see \code{emphasize.rows} but in bold
#' @param emphasize.strong.cols see \code{emphasize.cols} but in bold
#' @param emphasize.strong.cells see \code{emphasize.cells} but in bold
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call \code{pandoc.table.return} instead.
#' @export
#' @aliases pandoc.table
#' @seealso \code{\link{set.caption}}, \code{\link{set.alignment}}
#' @note If \code{caption} is missing, then the value is first checked in \code{t} object's \code{caption} attribute and if not found in an internal buffer set by \code{link{set.caption}}. \code{justify} parameter works similarly, see \code{\link{set.alignment}} for details.
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
#' m <- data.frame(a = c(1, -500, 10320, 23, 77),
#'   b = runif(5),
#'   c = c('a', 'bb', 'ccc', 'dddd', 'eeeee'))
#' pandoc.table(m)
#' pandoc.table(m, justify = c('right', 'left', 'centre'))
#'
#' ## splitting up too wide tables
#' pandoc.table(mtcars)
#' pandoc.table(mtcars, caption = 'Only once after the first part!')
#'
#' ## tables with line breaks in cells
#' ## NOTE: line breaks are removed from table content
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
#' pandoc.table(t, style = "simple")
#' tryCatch(pandoc.table(t, style = "simple", split.cells = 5),
#'   error = function(e) 'Yeah, no newline support in simple tables')
#' pandoc.table(t, style = "rmarkdown")
#'
#' ## highlight cells
#' t <- mtcars[1:3, 1:5]
#' pandoc.table(t$mpg, emphasize.cells = 1)
#' pandoc.table(t$mpg, emphasize.strong.cells = 1)
#' pandoc.table(t$mpg, emphasize.cells = 1, emphasize.strong.cells = 1)
#' pandoc.table(t$mpg, emphasize.cells = 1:2)
#' pandoc.table(t$mpg, emphasize.strong.cells = 1:2)
#' pandoc.table(t, emphasize.cells = which(t > 20, arr.ind = TRUE))
#' pandoc.table(t, emphasize.cells = which(t == 6, arr.ind = TRUE))
#' ## with helpers
#' emphasize.cols(1)
#' emphasize.rows(1)
#' pandoc.table(t)
#'
#' emphasize.strong.cells(which(t > 20, arr.ind = TRUE))
#' pandoc.table(t)
pandoc.table.return <- function(t, caption, digits = panderOptions('digits'), decimal.mark = panderOptions('decimal.mark'), round = panderOptions('round'), justify, style = c('multiline', 'grid', 'simple', 'rmarkdown'), split.tables = panderOptions('table.split.table'), split.cells = panderOptions('table.split.cells'), keep.trailing.zeros = panderOptions('keep.trailing.zeros'), emphasize.rows, emphasize.cols, emphasize.cells, emphasize.strong.rows, emphasize.strong.cols, emphasize.strong.cells) {

    ## helper functions
    table.expand <- function(cells, cols.width, justify, sep.cols) {

        df  <- data.frame(txt = cells, width = cols.width, justify = justify)

        if (any(grepl('\n', df$txt))) {

            if (style %in% c('simple', 'rmarkdown'))
                stop('Pandoc does not support newlines in simple or Rmarkdown table format!')

            res <- lapply(strsplit(as.character(df$txt), '\n'), unlist)
            res.lines <- max(sapply(res, length))
            res <- paste(sapply(1:res.lines, function(i) table.expand(sapply(res, function(x) ifelse(is.na(x[i]), '  ', x[i])), cols.width, justify, sep.cols)), collapse = '\n')
            return(res)

        } else {

            res <- apply(df, 1, function(x) format(x[1], justify = x[3], width = as.numeric(x[2]) + length(which(gregexpr("\\\\", x[1])[[1]] > 0))))
            return(paste0(sep.cols[1], paste(res, collapse = sep.cols[2]), sep.cols[3]))

        }

    }
    split.large.cells <- function(cells)
        sapply(cells, function(x) {

            ## split
            if (nchar(x) == nchar(x, type = 'width')) {

                x <- paste(strwrap(x, width = split.cells), collapse = '\n')

            } else {

                # dealing with CJK chars
                split <- strsplit(x, '\\s')[[1]]
                n <- nchar(split[1], type = 'width')
                x <- split[1]
                for (s in tail(split, -1)) {
                    nc <- nchar(s, type = 'width')
                    n  <- n + nc + 1
                    if (n > split.cells) {
                        n <- nc
                        x <- paste(x, s, sep = '\n')
                    } else {
                        x <- paste(x, s, sep = ' ')
                    }
                }

            }

            ## return
            if (x == 'NA')
                ''
            else
                x
        }, USE.NAMES = FALSE)
    align.hdr <- function(t.width, justify) {
        justify.vec <- rep(justify, length.out = length(t.width))
        dashes <- mapply(function(justify, width)
                         switch(
                             justify,
                             left = paste0(":", repChar("-", width + 1)),
                             right = paste0(repChar("-", width + 1), ":"),
                             centre = paste0(":", repChar("-", width), ":")
                             ),
                         justify.vec, t.width)
        hdr <- paste0("|", paste(dashes, collapse = "|"), "|")
        return(hdr)
    }
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
        abs(x - round(x)) < tol
    check.highlight.parameters <- function(x, num, num2) {
        if (missing(num2)) {
            if (!is.vector(x))
                stop('Only a vector or NULL can be passed to highlight table cell(s), row(s) or column(s).')
        } else {
            if (length(dim(t)) != 2)
                stop('A matrix like structure can be passed to highlight cells in a table with two columns for row and column indexes.')
        }
        if (!all(is.wholenumber(x)))
            stop('Only integers (whole numbers) can be passed to highlight table cell(s), row(s) or column(s).')
        if (!all(x > 0))
            stop(('Only positive numbers can be passed to highlight table cell(s), row(s) or column(s).'))
        if (missing(num2)) {
            if (!all(x < (num + 1)))
                stop(paste('Too high number passed that should be kept below', num + 1))
        } else {
            if (ncol(x) != 2)
                stop('A matrix like structure should be passed to highlight cells of a table with two columns for row and column indexes - just like what is returned by `which(..., arr.ind = TRUE)`.')
            if (!all(x[, 1] < (num + 1)))
                stop(paste('Too high number passed for row indexes that should be kept below', num + 1))
            if (!all(x[, 2] < (num2 + 1)))
                stop(paste('Too high number passed for column indexes that should be kept below', num + 1))
        }
    }

    ## initializing
    mc  <- match.call()
    if (is.null(mc$style))
        style <- panderOptions('table.style')
    else
        style <- match.arg(style)
    if (is.null(mc$justify)) {
        if (is.null(attr(t, 'alignment')))
            justify <- get.alignment(t)
        else
            justify <- attr(t, 'alignment')
    }
    if (is.null(mc$caption)) {
        if (is.null(attr(t, 'caption'))) {
            caption <- get.caption()
        } else {
            caption <- attr(t, 'caption')
        }
    }
    emphasize.parameters <- c('emphasize.rows', 'emphasize.cols', 'emphasize.cells', 'emphasize.strong.rows', 'emphasize.strong.cols', 'emphasize.strong.cells')
    ## check if emphasize parameters were passed
    if (all(sapply(emphasize.parameters, function(p) is.null(mc[[p]]), USE.NAMES = FALSE))) {
        ## check if emphasize parameters were set in attributes
        if (all(sapply(emphasize.parameters, function(p) is.null(attr(t, p)), USE.NAMES = FALSE)))
            t <- get.emphasize(t)
        ## set emphasize parameters at last
        for (p in emphasize.parameters)
            assign(p, attr(t, p))
    } else {
        ## some emphasize parameters passed, other should be set to NULL
        for (p in emphasize.parameters)
            if (is.null(mc[[p]]))
                assign(p, NULL)
    }
    res <- ''

    ## round numbers & cut digits & apply decimal mark & optionally remove trailing zeros
    if (length(dim(t)) < 2 | !is.null(dim(t)) && length(dim(t)) == 2 && is.data.frame(t))
        t.n <- as.numeric(which(sapply(t, is.numeric)))
    else
        t.n <- as.numeric(which(apply(t, 2, is.numeric)))
    if (length(t.n) > 0) {
        t[t.n] <- round(t[t.n], round)
        if (!keep.trailing.zeros) {
            if (length(dim(t)) == 0) # named char
                t[t.n] <- sapply(t[t.n], format, trim = TRUE, digits = digits, decimal.mark = decimal.mark)
            if (length(dim(t)) == 1)
                t[t.n] <- apply(t[t.n, drop = FALSE], 1, format, trim = TRUE, digits = digits, decimal.mark = decimal.mark)
            if (length(dim(t)) == 2)
                t[, t.n] <- apply(t[, t.n, drop = FALSE], c(1,2), format, trim = TRUE, digits = digits, decimal.mark = decimal.mark)
        }
    }

    ## drop unexpected classes and revert back to a common format
    if (keep.trailing.zeros)
        t <- format(t, trim = TRUE, digits = digits, decimal.mark = decimal.mark)
    else
        t <- format(t, trim = TRUE)

    ## adding formatting (emphasis, strong etc.)
    if (length(dim(t)) < 2) {
        if (!is.null(emphasize.rows) | !is.null(emphasize.cols) | !is.null(emphasize.strong.rows) | !is.null(emphasize.strong.cols))
            stop('There is no sense in highlighting rows/columns in 1 dimensional tables. Hint: highlight cells instead. ')
        if (!is.null(emphasize.cells)) {
            check.highlight.parameters(emphasize.cells, length(t))
            t[emphasize.cells] <- pandoc.emphasis.return(t[emphasize.cells])
        }
        if (!is.null(emphasize.strong.cells)) {
            check.highlight.parameters(emphasize.strong.cells, length(t))
            t[emphasize.strong.cells] <- pandoc.strong.return(t[emphasize.strong.cells])
        }
    } else {
        if (!is.null(emphasize.rows)) {
            check.highlight.parameters(emphasize.rows, nrow(t))
            t[emphasize.rows, ] <- apply(t[emphasize.rows, , drop = FALSE], c(1, 2), pandoc.emphasis.return)
        }
        if (!is.null(emphasize.strong.rows)) {
            check.highlight.parameters(emphasize.strong.rows, nrow(t))
            t[emphasize.strong.rows, ] <- apply(t[emphasize.strong.rows, , drop = FALSE], c(1, 2), pandoc.strong.return)
        }
        if (!is.null(emphasize.cols)) {
            check.highlight.parameters(emphasize.cols, ncol(t))
            t[, emphasize.cols] <- apply(t[, emphasize.cols, drop = FALSE], c(1, 2), pandoc.emphasis.return)
        }
        if (!is.null(emphasize.strong.cols)) {
            check.highlight.parameters(emphasize.strong.cols, ncol(t))
            t[, emphasize.strong.cols] <- apply(t[, emphasize.strong.cols, drop = FALSE], c(1, 2), pandoc.strong.return)
        }
        if (!is.null(emphasize.cells)) {
            t <- as.matrix(t)
            check.highlight.parameters(emphasize.cells, nrow(t), ncol(t))
            t[emphasize.cells] <- pandoc.emphasis.return(t[emphasize.cells])
        }
        if (!is.null(emphasize.strong.cells)) {
            t <- as.matrix(t)
            check.highlight.parameters(emphasize.strong.cells, nrow(t), ncol(t))
            t[emphasize.strong.cells] <- pandoc.strong.return(t[emphasize.strong.cells])
        }
    }

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
            t.colnames.width <- sapply(t.colnames, function(x) max(nchar(strsplit(x, '\n')[[1]], type = 'width'), 0), USE.NAMES = FALSE) + 2
        } else {
            t.colnames.width <- 0
        }

        if (length(dim(t)) == 0)
            t.width <- as.numeric(apply(cbind(t.colnames.width, as.numeric(sapply(t, nchar, type = 'width'))), 1, max))
        else
            t.width <- as.numeric(apply(cbind(t.colnames.width, as.numeric(apply(t, 1, nchar, type = 'width'))), 1, max))

    } else {

        ## checking for empty data frames
        if (dim(t)[1] == 0)
            t[1, ] <- NA

        t <- apply(t, c(1,2), split.large.cells)

        t.rownames  <- rownames(t)
        t.colnames  <- colnames(t)
        if (!is.null(t.colnames)) {
            t.colnames  <- split.large.cells(t.colnames)
            t.colnames.width <- sapply(t.colnames, function(x) max(nchar(strsplit(x, '\n')[[1]], type = 'width'), 0), USE.NAMES = FALSE) + 2
        } else {
            t.colnames.width <- 0
        }

        ## also dealing with cells split by newlines
        t.width     <-  as.numeric(apply(cbind(t.colnames.width, apply(t, 2, function(x) max(sapply(strsplit(x,'\n'), function(x) max(nchar(x, type = 'width'), 0))))), 1, max))

        ## remove obvious row.names
        if (all(rownames(t) == 1:nrow(t)))
            t.rownames <- NULL

        if (!is.null(t.rownames))
            t.rownames <- pandoc.strong.return(t.rownames)

    }

    if (length(t.rownames) != 0) {

        t.rownames <- split.large.cells(t.rownames)
        if (!is.null(t.colnames))
            t.colnames <- c('&nbsp;', t.colnames)
        t.width <- c(max(sapply(strsplit(t.rownames, '\n'), function(x) max(nchar(x, type = 'width'), 0))), t.width)
        t.width[1] <- t.width[1] + 2

    }

    if (length(justify) != 1) {
        if (length(t.rownames) != 0)
            if (length(justify) != length(t.width))
                stop(sprintf('Wrong number of parameters (%s instead of *%s*) passed: justify', length(justify), length(t.width)))
    } else {
        justify <- rep(justify, length(t.width))
    }
    justify <- sub('^center$', 'centre', justify)
    if (!all(justify %in% c('left', 'right', 'centre')))
        stop('Invalid values passed for `justify` that can be "left", "right" or "centre/center".')

    ## split too wide tables
    if (sum(t.width + 4) > split.tables & length(t.width) > 1 + (length(t.rownames) != 0)) {

        t.split <- max(which(cumsum(t.width + 4) > split.tables)[1] - 1, 1)
        if (t.split == 1 & length(t.rownames) != 0)
            t.split <- 2

        ## update caption
        if (!is.null(caption))
            caption <- paste(caption, panderOptions('table.continues.affix'))
        else
            caption <- panderOptions('table.continues')

        ## split
        if (length(t.rownames) != 0) {
            justify <- list(justify[1:t.split], justify[c(1, (t.split + 1):length(t.width))])
            t.split <- t.split - 1
        } else {
            justify <- list(justify[1:(t.split)], justify[c((t.split + 1):length(t.width))])
        }

        if (length(dim(t)) > 1)
            res <- list(t[, 1:(t.split), drop = FALSE], t[, (t.split + 1):ncol(t), drop = FALSE])
        else
            res <- list(t[1:t.split, drop = FALSE], t[(t.split + 1):length(t), drop = FALSE])

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
                   if (length(t.colnames) != 0)
                       sep.top <- gsub(' ', '-', sep.hdr)
                   else
                       sep.top <- paste(sapply(t.width, function(x) repChar('-', x)), collapse = ' ')
                   sep.btn <- sep.top
                   sep.col <- c('', ' ', '')
               },
               'simple'   = {
                   sep.row <- ''
                   if (length(t.colnames) == 0) {
                       sep.top <- paste(sapply(t.width, function(x) repChar('-', x)), collapse = ' ')
                       sep.btn <- paste0('\n', sep.top)
                   } else
                       sep.top <- sep.btn <- ''
                   sep.hdr <- paste(sapply(t.width, function(x) repChar('-', x)), collapse = ' ')
                   sep.col <- c('', ' ', '')
               },
               'rmarkdown'= {
                   sep.row <- ''
                   sep.top <- ''
                   sep.btn <- ''
                   sep.hdr <- align.hdr(t.width, justify)
                   sep.col <- c('| ', ' | ', ' |')
               })

        ## header
        if (length(t.colnames) != 0) {
            res <- paste(res, sep.top, table.expand(t.colnames, t.width, justify, sep.col), sep.hdr, sep = '\n')
        } else {
            if (style == "rmarkdown") {
                blank.hdr <- paste0('| ', paste(sapply(t.width, function(x) repChar(' ', x)), collapse = ' | '), ' |')
                res <- paste(res, blank.hdr, sep.hdr, sep='\n')
            } else {
                    res <- paste(res, sep.top, sep = '\n')
            }
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
        if (!is.null(caption) && caption != '')
            res <- paste0(res, panderOptions('table.caption.prefix'), caption, '\n\n')

        return(res)

    }
}

#' @export
pandoc.table <- function(...)
    cat(pandoc.table.return(...))
