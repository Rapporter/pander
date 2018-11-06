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
#' @param wrap a string to wrap vector elements (uses value set in \code{p.wrap} option: \code{'_'} by default, which is a markdown-friendly wrapper and it puts the string in \emph{italic})
#' @param sep a string with the main separator, i.e. the one that separates all vector elements but the last two (uses the value set in \code{p.sep} option - \code{','} by default)
#' @param copula a string with ending separator - the one that separates the last two vector elements (uses the value set in \code{p.copula} option, \code{'and'} by default)
#' @param limit maximum character length (defaults to \code{Inf}initive  elements)
#' @param keep.trailing.zeros to show or remove trailing zeros in numbers
#' @param missing string to replace missing values
#' @param digits numeric (default: 2) passed to format
#' @param round numeric (default: Inf) passed to round
#' @return a string with concatenated vector contents
#' @examples
#' p(c('fee', 'fi', 'foo', 'fam'))
#' # [1] '_fee_, _fi_, _foo_ and _fam_'
#' p(1:3, wrap = '')
#' # [1] '1, 2 and 3'
#' p(LETTERS[1:5], copula = 'and the letter')
#' # [1] '_A_, _B_, _C_, _D_ and the letter _E_'
#' p(c('Thelma', 'Louise'), wrap = '', copula = '&')
#' # [1] 'Thelma & Louise'
#' @export
#' @author Aleksandar Blagotic
#' @references This function was moved from \code{rapport} package: \url{http://rapport-package.info}.
p <- function(x, wrap = panderOptions('p.wrap'), sep = panderOptions('p.sep'), copula = panderOptions('p.copula'), limit = Inf, keep.trailing.zeros = panderOptions('keep.trailing.zeros'), missing = panderOptions('missing'), digits = panderOptions('digits'), round = panderOptions('round')){ #nolint

    attributes(x) <- NULL
    stopifnot(is.vector(x))
    stopifnot(all(sapply(list(wrap, sep, copula), function(x) is.character(x) && length(x) == 1)))
    x.len <- length(x)
    if (x.len == 0) {
        return('')
    }
    stopifnot(x.len <= limit)

    ## store missing values
    w <- which(is.na(x))

    ## prettify numbers
    if (is.numeric(x)) {

        x <- round(x, digits = round)

        ## optionally remove trailing zeros by running format separately on each element of the vector
        if (!keep.trailing.zeros) {
            x <- sapply(x, format, trim = TRUE, digits = digits, decimal.mark = panderOptions('decimal.mark')) #nolint
        } else {
            ## otherwise force using the same number format for all vector elements
            x <- format(x, trim = TRUE, digits = digits, decimal.mark = panderOptions('decimal.mark'))
        }

    }

    ## replace missing values
    if (length(w) > 0) {
        x[w] <- missing
    }

    if (x.len == 1) {
        wrap(x, wrap)
    } else if (x.len == 2) {
        paste(wrap(x, wrap), collapse = copula)
    } else {
        paste0(paste(wrap(head(x, -1), wrap), collapse = sep), copula, wrap(tail(x, 1), wrap))
    }
}


#' Wrap Vector Elements
#'
#' Wraps vector elements with string provided in \code{wrap} argument.
#' @param x a vector to wrap
#' @param wrap a string to wrap around vector elements
#' @return a string with wrapped elements
#' @examples \dontrun{
#' wrap('foobar')
#' wrap(c('fee', 'fi', 'foo', 'fam'), '_')
#' }
#' @export
#' @author Aleksandar Blagotic
#' @references This function was moved from \code{rapport} package: \url{http://rapport-package.info}.
wrap <- function(x, wrap = '"'){
    attributes(x) <- NULL
    stopifnot(is.vector(x))
    sprintf('%s%s%s', wrap, x, wrap)
}

#' Check if rownames are available
#'
#' Dummy helper to check if the R object has real rownames or not.
#' @param x a tabular-like R object
#' @return \code{TRUE} OR \code{FALSE}
#' @export
has.rownames <- function(x) {
    length(dim(x)) != 1 && length(rownames(x)) > 0 && !all(rownames(x) == 1:nrow(x)) && !all(rownames(x) == '')
}

#' Adds caption in current block
#'
#' This is a helper function to add a caption to the returning image/table.
#' @param x string
#' @param permanent (default \code{FALSE}) if caption is permanent (for all future tables) or not
#' @export
set.caption <- function(x, permanent = FALSE){
    assign('caption', x, envir = storage)
    if (!is.null(x)) {
        attr(storage$caption, 'permanent') <- permanent
    }
}


#' Get caption
#'
#' Get caption from temporary environment and truncates that
#' @return stored caption as string
#' @keywords internal
get.caption <- function()
    get.storage('caption')


#' Sets alignment for tables
#'
#' This is a helper function to update the alignment (\code{justify} parameter in \code{pandoc.table}) of the next returning table. Possible values are: \code{centre} or \code{center}, \code{right}, \code{left}.
#' @param default character vector which length equals to one (would be repeated \code{n} times) ot \code{n} - where \code{n} equals to the number of columns in the following table
#' @param row.names string holding the alignment of the (optional) row names
#' @param permanent (default \code{FALSE}) if alignment is permanent (for all future tables) or not. It's cleaner to use \code{panderOptions} instead.
#' @export
set.alignment <- function(default = panderOptions('table.alignment.default'), row.names = panderOptions('table.alignment.rownames'), permanent = FALSE) { #nolint
    assign('alignment', list(default = default, row.names = row.names), envir = storage)
    attr(storage$alignment, 'permanent') <- permanent
}


#' Get alignment
#'
#' Get alignment from temporary environment, truncating that and applying rownames and other columns alignment to passed \code{df}.
#' @return vector of alignment parameters
#' @keywords internal
#' @param df data.frame
#' @param remove.obious.rownames if this rule should be applied or not
get.alignment <- function(df, remove.obious.rownames = TRUE) {

    if (is.null(attr(df, 'alignment'))) {

        a <- get.storage('alignment')

        if (is.null(a)) {
            ad <- panderOptions('table.alignment.default')
            ar <- panderOptions('table.alignment.rownames')
            if (remove.obious.rownames && !has.rownames(df)) {
                ar <- NULL
            }
            if (is.function(ar)) {
                ar <- ar()
            }
            if (is.function(ad)) {
                return(c(ar, ad(df)))
            }
            a <- list(default = ad, row.names = ar)
        }

        if (length(a) == 1) {
            a <- list(default = as.character(a), row.names = as.character(a))
        }

        if (length(dim(df)) < 2) {
            w <- length(df)
            n <- NULL
        } else {
            w <- ncol(df)
            n <- rownames(df)
            if (isTRUE(remove.obious.rownames) && all(n == 1:nrow(df))) {
                n <- NULL
            }
        }

        if (is.null(n)) {
            return(rep(a$default, length.out = w))
        } else {
            return(c(a$row.names, rep(a$default, length.out = w)))
        }

    }

    attr(df, 'alignment')
}


#' Emphasize rows/columns/cells
#'
#' Storing indexes of cells to be (strong) emphasized of a tabular data in an internal buffer that can be released and applied by \code{\link{pandoc.table}}, \code{\link{pander}} or \code{\link{evals}} later.
#' @param x vector of row/columns indexes or an array like returned by \code{which(..., arr.ind = TRUE)}
#' @aliases emphasize.rows emphasize.cols emphasize.cells emphasize.strong.rows emphasize.strong.cols emphasize.strong.cells emphasize.italics.rows emphasize.italics.cols emphasize.italics.cells emphasize.verbatim.rows emphasize.verbatim.cols emphasize.verbatim.cells
#' @usage
#' emphasize.rows(x)
#'
#' emphasize.cols(x)
#'
#' emphasize.cells(x)
#'
#' emphasize.strong.rows(x)
#'
#' emphasize.strong.cols(x)
#'
#' emphasize.strong.cells(x)
#'
#' emphasize.italics.rows(x)
#'
#' emphasize.italics.cols(x)
#'
#' emphasize.italics.cells(x)
#'
#' emphasize.verbatim.rows(x)
#'
#' emphasize.verbatim.cols(x)
#'
#' emphasize.verbatim.cells(x)
#' @export
#' @examples \dontrun{
#' n <- data.frame(x = c(1,1,1,1,1), y = c(0,1,0,1,0))
#' emphasize.cols(1)
#' emphasize.rows(1)
#' pandoc.table(n)
#'
#' emphasize.strong.cells(which(n == 1, arr.ind = TRUE))
#' pander(n)
#' }
emphasize.rows <- function(x)
    assign(deparse(match.call()[[1]]), x, envir = storage)
#' @export
emphasize.strong.rows <- emphasize.rows
#' @export
emphasize.italics.rows <- emphasize.rows
#' @export
emphasize.verbatim.rows <- emphasize.rows
#' @export
emphasize.cols <- emphasize.rows
#' @export
emphasize.strong.cols <- emphasize.rows
#' @export
emphasize.italics.cols <- emphasize.rows
#' @export
emphasize.verbatim.cols <- emphasize.rows
#' @export
emphasize.cells <- emphasize.rows
#' @export
emphasize.strong.cells <- emphasize.rows
#' @export
emphasize.italics.cells <- emphasize.rows
#' @export
emphasize.verbatim.cells <- emphasize.rows


#' Get emphasize params from internal buffer
#'
#' And truncate content.
#' @param df tabular data
#' @return R object passed as \code{df} with possibly added \code{attr}s captured from internal buffer
#' @keywords internal
get.emphasize <- function(df) {
    for (v in c('emphasize.rows',
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
                'emphasize.verbatim.cells')) {
        if (is.null(attr(df, v))) {
            attr(df, v) <- get.storage(v)
        }
    }
    return(df)
}


#' Get a value from internal buffer
#'
#' And truncate content.
#' @param what string
#' @keywords internal
get.storage <- function(what) {
    res <- tryCatch(get(what, envir = storage, inherits = FALSE), error = function(e) NULL)
    if (is.null(attr(res, 'permanent')) || !attr(res, 'permanent')) {
        assign(what, NULL, envir = storage)
    }
    return(res)
}


#' Add significance stars
#'
#' This function adds significance stars to passed \code{p} value(s) as: one star for value below \code{0.05}, two for \code{0.01} and three for \code{0.001}.
#' @param p numeric vector or tabular data
#' @param cutoffs the cutoffs for the 1/2/3 significance stars
#' @return character vector
#' @export
add.significance.stars <- function(p, cutoffs = c(0.05, 0.01, 0.001)) {

    stopifnot(length(cutoffs) == 3)

    if (inherits(p, c('matrix', 'data.frame')) && length(dim(p)) == 2) {
        apply(p, c(1,2), add.significance.stars, cutoffs = cutoffs)
    } else {
        if (length(p) > 1) {
            sapply(p, add.significance.stars, cutoffs = cutoffs)
        } else {
            ifelse(p > cutoffs[1], '',
                   ifelse(p > cutoffs[2], ' *',
                          ifelse(p > cutoffs[3], ' * *', ' * * *')))
        }
    }
}


#' Toggle cache
#'
#' This function is just a wrapper around \code{\link{evalsOptions}} to switch pander's cache on or off easily, which might be handy in some brew documents to prevent repetitive strain injury :)
#' @aliases cache.on
#' @usage
#' cache.on()
#'
#' cache.off()
#' @export
cache.off <- function()
    evalsOptions('cache', FALSE)

#' @export
cache.on <- function()
    evalsOptions('cache', TRUE)

#' Split line with line breaks depending on max.width
#'
#' This is a helper function to insert line breaks depending on (\code{split.cells} parameter of \code{pandoc.table}) of the returning table.
#' @param x string to be split. Works only with one string. Non-string arguments and multi-dimensional arguments are returned unchaged
#' @param max.width default integer value specyfing max number of characters between line breaks
#' @param use.hyphening (default: \code{FALSE}) if try to use hyphening when splitting large cells according to table.split.cells. Requires \pkg{sylly}.
#' @return character string with line breaks
#' @export
#' @examples
#' splitLine('foo bar', 6)
#' splitLine('foo bar', 7)
#' splitLine('Pandoc Package', 3, TRUE)
splitLine <- function(x, max.width = panderOptions('table.split.cells'), use.hyphening = FALSE) {
    if (any(is.na(x))) {
        return(x)
    }
    if (!is.character(x) || !is.null(dim(x)) || length(x) != 1 || x == '') {
        return(x)
    }
    if (suppressWarnings(!is.na(as.numeric(x)))) {
        return(x)
    }
    if (is.infinite(max.width)) {
        max.width <- .Machine$integer.max
    }
    if (use.hyphening) {
        if (requireNamespace('sylly', quietly = TRUE) && requireNamespace('sylly.en', quietly = TRUE)) {
            sylly.en::hyph.support.en()
        } else {
            use.hyphening <- FALSE
        }
    }
    hyphen_f <- function(s)
        sylly::hyphen(s, hyph.pattern = 'en', quiet = TRUE)@hyphen[1, 2]
    .Call('pander_splitLine_cpp', PACKAGE = 'pander', x, max.width, use.hyphening, hyphen_f)
}


#' Check if caption is valid
#' @param caption R object to check
#' @return boolean
#' @keywords internal
check_caption <- function(caption) {

    if (length(caption) > 1) {
        stop('The caption should be exactly one string.')
    }

    if (!(is.character(caption) | is.null(caption))){
        stop('The caption should be string (character class) or NULL.')
    }
    invisible(TRUE)
}


#' Check if vector parameter for round/digits and adjust accodingly
#' @param param vector to be checked
#' @param name parameter name
#' @param n needed size of vector
#' @return original vector if size is good, vector of default values otherwise
#' @keywords internal
check_digits <- function(param, name, n) {
    if (length(param) == 1) {
        param <- rep(param, n)
    } else if (length(param) < n) {
        warning(sprintf('%s is not equal to ncol(t), reverting to default value', name))
        param <- rep(panderOptions(name), n)
    }
    param
}

#' Create a multitable used for rendering objects from rms package
#'
#' When ols/lrm/orm from rms package get rendered, main statistics are group in table of tables.
#' Since pandoc doesn't support row or col-span,
#' we chose to group those statistics in a column each.
#' This function takes care of that
#' @param v list of vectors/lists to be merge in the table
#' @return data.frame in specified format
#' @examples
#' pander:::multitable(list(list(a=1, b=2),list(c=3, d=4)))
#' @keywords internal
multitable <- function(v) {
    ml <- max(sapply(v, length))
    mod <- lapply(1:length(v),
                  function(i)  {
                      uv <- unlist(c(rbind(pandoc.strong.return(names(v[[i]])),
                                           sapply(v[[i]], p, wrap = ''))))
                      if (length(v[[i]]) < ml)
                          uv <- c(uv, rep('', 2 * (ml - length(v[[i]]))))
                      uv
                  })
    do.call(cbind, mod)
}

#' Calculate coef matrix for models from rms package
#' Forked from prModFit from rms
#'
#' @param obj object list
#' @param coefs numeric value if to print only the first n regression coefficients in the model.
#' @return coeficients matrix
#' @importFrom stats pt pchisq
coef_mat <- function(obj, coefs) {
    errordf <- obj$errordf
    beta <- obj$coef
    se <- obj$se
    Z <- beta / se
    if (length(errordf)) {
        P <- 2 * (1 - pt(abs(Z), errordf))
    } else {
        P <- 1 - pchisq(Z ^ 2, 1)
    }
    U <- cbind(beta, se, Z, P)
    colnames(U) <- c('Coef', 'S.E.', 'Wald Z', 'Pr(>|Z|)')
    if (length(errordf)) {
        colnames(U)[3:4] <- c('t', 'Pr(>|t|)')
    }
    rownames(U) <- names(beta)
    if (length(obj$aux)) {
        U <- cbind(U, obj$aux)
        colnames(U)[ncol(U)] <- obj$auxname
    }
    if (is.numeric(coefs)) {
        U <- U[1:coefs, , drop = FALSE] #nolint
        U <- rbind(U, rep('', ncol(U)))
        rownames(U)[nrow(U)] <- '. . .'
    }
    U
}
