library(testthat)
library(pander)

## reset later
original_contrasts_options <- getOption('contrasts')

context('pandoc.table')

tables <- list(
    mtcars,
    mtcars$am,
    mtcars[1:2, ],
    mtcars[1:2, 5],
    summary(mtcars$am),
    table(mtcars$am) + 0.1,
    table(mtcars$am, mtcars$gear) + 0.1,
    summary(lm(mtcars$hp~1))$coeff,
    table(mtcars$am, mtcars$gear, mtcars$carb),
    addmargins(table(mtcars$gear, mtcars$carb)),
    summary(Sys.Date() + 0:5)
)

test_that('no error: multiline', {
    for (t in tables)
        expect_that(pandoc.table.return(t, style = 'multiline'), is_a('character'))
})
test_that('no error: simple', {
    for (t in tables)
        expect_that(pandoc.table.return(t, style = 'simple'), is_a('character'))
})
test_that('no error: grid', {
    for (t in tables)
        expect_that(pandoc.table.return(t, style = 'grid'), is_a('character'))
})
test_that('no error: rmarkdown', {
    for (t in tables)
        expect_that(pandoc.table.return(t, style = 'rmarkdown'), is_a('character'))
})

tables <- list(
    mtcars,
    mtcars$am,
    mtcars[1:2, ],
    mtcars[1:2, 5],
    summary(mtcars$am),
    table(mtcars$am) + 0.1,
    table(mtcars$am, mtcars$gear) + 0.1,
    lm(mtcars$hp~1),
    t.test(extra ~ group, data = sleep),
    prcomp(USArrests),
    density(mtcars$hp),
    table(mtcars$am, mtcars$gear, mtcars$carb) + 0.1
    )

test_that('split.tables', {
    # test exact values (#164)
    t <- data.frame(a = '7 chars', b = paste(rep('Δ', 5), collapse = ' '))
    expect_equal(nchar('Δ Δ Δ Δ Δ', type = 'width'), 9)
    res_grid <- pander_return(t, style = 'grid', split.tables = 23)
    expect_equal(length(res_grid), 8)
    expect_false(any(grepl('Table', res_grid)))
    res_grid <- pander_return(t, style = 'grid', split.tables = 22)
    expect_equal(length(res_grid), 18)
    expect_true(any(grepl('Table', res_grid)))
    res_simple <- pander_return(t, style = 'simple', split.tables = 21)
    expect_equal(length(res_simple), 6)
    expect_false(any(grepl('Table', res_simple)))
    res_simple <- pander_return(t, style = 'simple', split.tables = 20)
    expect_equal(length(res_simple), 14)
    expect_true(any(grepl('Table', res_simple)))
})

test_that('rmarkdown pipe-delimited table is correct (#186)', {
    d <- data.frame(a = 'foo|bar', b = 'my missing cell')
    res <- pander_return(d, style = 'rmarkdown')
    expect_true(any(grep('foo', res)))
    expect_true(any(grep('bar', res)))
    expect_true(any(grep('my missing cell', res)))
    rownames(d) <- 'x|y'
    res <- pander_return(d, style = 'rmarkdown')
    expect_true(any(grep('x', res)))
    expect_true(any(grep('y', res)))
    expect_true(any(grep('foo', res)))
    expect_true(any(grep('bar', res)))
    expect_true(any(grep('my missing cell', res)))
})

test_that('pandoc.table.return behaves correctly', {
    expect_warning(pander_return(mtcars[, 1:2], split.tables = Inf, split.cells = c('50%', '50%')))
    t <- mtcars[1:3, 1:2]
    attr(t, 'alignment') <- 'left'
    attr(t, 'caption') <- 'simplified mtcars'
    res <- pander_return(t, emphasize.rownames = FALSE)
    expect_false(any(grep('^[[:space:]]', res))) # because of left alignment
    expect_true(any(grep('simplified mtcars', res)))
    expect_error(pander_return(t, justify = c('left', 'right'))) # needs 3 because of rownames
    expect_error(pander_return(t, justify = 'lr')) # needs 3 because of rownames
    res <- pander_return(t, justify = 'lrr')
    expect_false(any(grep('[[:space:]]$', res)))
    expect_error(pander_return(t, justify = 'laft'))
    res <- pander_return(t, split.tables = 1)
    expect_equal(length(res), 24)
    res <- pander_return(t, split.cells = c(10, 10, 10))
    expect_equal(length(res), 16)
    expect_warning(pander_return(t, split.cells = vector()))
})

dm <- panderOptions('decimal.mark')
panderOptions('decimal.mark', ',')
test_that('decimal mark', {
    for (t in tables)
        expect_true(grepl(',', paste(pander_return(t), collapse = '\n')))
})

panderOptions('decimal.mark', dm)

test_that('Date is preserved', {
    expect_equal(grep('2015', pander_return(summary(as.Date('2015-01-01') + 0:5))), 5)
})

context('highlight tables')

t <- mtcars[1:3, 1:5]
test_that('highlight 1D: no error', {
    expect_that(pandoc.table.return(t$mpg, emphasize.italics.cells = 1), is_a('character'))
    expect_that(pandoc.table.return(t$mpg, emphasize.italics.cells = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t$mpg, emphasize.strong.cells = 1), is_a('character'))
    expect_that(pandoc.table.return(t$mpg, emphasize.strong.cells = 1:2), is_a('character'))
})

t <- table(mtcars$am, mtcars$gear)
test_that('emphasize 2D: no error', {
    expect_that(pandoc.table.return(t, emphasize.italics.rows = 1), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.italics.rows = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.italics.cols = 1), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.italics.cols = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.italics.cells = which(t > 10, arr.ind = TRUE)), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.italics.cells = which(t > 20, arr.ind = TRUE)), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.rows = 1), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.rows = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.cols = 1), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.cols = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.cells = which(t > 10, arr.ind = TRUE)), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.cells = which(t > 20, arr.ind = TRUE)), is_a('character'))
})

test_that('emphasize: error', {
    expect_that(pandoc.table(t, emphasize.italics.cols = 1:5), throws_error())
    expect_that(pandoc.table(t, emphasize.italics.cols = 1.5), throws_error())
    expect_that(pandoc.table(t, emphasize.strong.cols = 1:5), throws_error())
    expect_that(pandoc.table(t, emphasize.strong.cols = 1.5), throws_error())
})

test_that('no warning for highlight NA/empty strings', {
    expect_warning(pandoc.table(data.frame(x = 1:2, y = c(1, NA)), emphasize.italics.cols = 2), regexp = NA)
})

test_that('emphasize.italics.rows works correctly', {
    # test for issue 176
    df <- data.frame(a=1:3, b=1:3, c=1:3)
    res <- pander_return(df, emphasize.italics.rows = c(1,2), style = 'simple')
    expect_equal(res[5], ' *1*   *1*   *1* ')
    expect_equal(res[6], ' *2*   *2*   *2* ')
    res <- pander_return(df, emphasize.strong.rows = c(1,2), style = 'simple')
    expect_equal(res[5], ' **1**   **1**   **1** ')
    expect_equal(res[6], ' **2**   **2**   **2** ')
})

test_that('emphasize.verbatim works correctly', {
    df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
    res <- pander_return(df, emphasize.verbatim.rows = c(1, 2),
                         style = 'simple')
    expect_equal(res[5], ' `1`   `4`   `7` ')
    expect_equal(res[6], ' `2`   `5`   `8` ')
    res <- pander_return(df, emphasize.verbatim.cols = c(1,2), style = 'simple')
    expect_equal(res[5], ' `1`   `4`   7 ')
    expect_equal(res[6], ' `2`   `5`   8 ')
    df <- data.frame(a = 1:3, b = 1:3, c = 1:3)
    res <- pander_return(df, emphasize.italics.rows = c(1, 2),
                         style = 'simple')
    expect_equal(res[5], ' *1*   *1*   *1* ')
    expect_equal(res[6], ' *2*   *2*   *2* ')
    res <- pander_return(df, emphasize.strong.rows = c(1, 2),
                         style = 'simple')
    expect_equal(res[5], ' **1**   **1**   **1** ')
    expect_equal(res[6], ' **2**   **2**   **2** ')
})

test_that('emphasize.verbatim works correctly', {
    df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
    res <- pander_return(df, emphasize.verbatim.rows = c(1, 2),
                         style = 'simple')
    expect_equal(res[5], ' `1`   `4`   `7` ')
    expect_equal(res[6], ' `2`   `5`   `8` ')
    res <- pander_return(df, emphasize.verbatim.cols = c(1, 2),
                         style = 'simple')
    expect_equal(res[5], ' `1`   `4`   7 ')
    expect_equal(res[6], ' `2`   `5`   8 ')
    res <- pander_return(df, emphasize.verbatim.cells =
                                 which(df > 5, arr.ind = TRUE),
                         style = 'simple')
    expect_equal(res[5], ' 1    4    `7` ')
    expect_equal(res[6], ' 2    5    `8` ')
    res <- pander_return(df, emphasize.verbatim.cells =
                                 which(df > 5, arr.ind = TRUE),
                         emphasize.strong.rows = c(1, 2),
                         style = 'simple')
    expect_equal(res[5], ' **1**   **4**   **`7`** ')
    expect_equal(res[7], '   3      `6`      `9`   ')
})

test_that('emphasize.strong.rows works correctly with no rows to emphasize #354', {
    df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
    res <- pander_return(df, emphasize.strong.rows = integer(0), style = 'simple')
    expect_equal(res[5], ' 1   4   7 ')
    expect_equal(res[6], ' 2   5   8 ')
    expect_equal(res[7], ' 3   6   9 ')
})

context('captions')

tables <- list(
    mtcars,
    mtcars[1:2, ],
    summary(mtcars$am),
    table(mtcars$am) + 0.1,
    table(mtcars$am, mtcars$gear) + 0.1,
    lm(mtcars$hp~1),
    t.test(extra ~ group, data = sleep),
    prcomp(USArrests),
    density(mtcars$hp)
    )

has.caption <- function(ttt, evals = FALSE) {
    set.caption('foo')
    if (!evals)
        any(grepl('Table:', pander_return(ttt)))
    else
        !is.null(attr(evals('get("ttt")', env = parent.frame())[[1]]$result, 'caption'))
}

test_that('direct call', {
    for (ttt in tables)
        expect_that(has.caption(ttt), is_true())
})

cache.dir <- evalsOptions('cache.dir')
graph.dir <- evalsOptions('graph.dir')
wd <- getwd()
setwd(tempdir())
evalsOptions('cache.dir',  file.path(tempdir(), '.cache'))
evalsOptions('graph.dir',  file.path(tempdir(), 'plots'))
test_that('evals', {
    for (ttt in tables)
        expect_that(has.caption(ttt, evals = TRUE), is_true())
})
evalsOptions('cache.dir',  cache.dir)
evalsOptions('graph.dir',  graph.dir)
setwd(wd)

context('row and column names')

library(testthat)
library(pander)

tables <- list(
  mtcars[1:2, 1:2],
  data.frame(x = 1:2, y = 2:3),
  data.frame(x = 1:3, y = 2:4)[c(1, 3), ]
  )

t <- tables[[1]]

test_that('row names can be suppressed', {
  for (t in tables) {
    expect_false(grepl("&nbsp;", pandoc.table.return(t, row.names = FALSE)))
  }
})

test_that('row names can be set', {
  for (t in tables) {
    expect_true(grepl("\\n \\*\\*a\\*\\*", pandoc.table.return(t, row.names = c("a", "b"))))
  }
})

test_that('obvious row names can be preserved', {
    expect_true(grepl(".*1.*2.*3.*", pandoc.table.return(data.frame(a = 7:9), row.names = 1:3)))
})

test_that('column names can be set', {
  for (t in tables) {
    res <- capture.output(pandoc.table(t, col.names = c("a", "b")))
    expect_true(grepl("a *b", res[3]))
  }
})

context('default alignments')

tables <- list(
    mtcars,
    mtcars$am,
    mtcars[1:2, ],
    mtcars[1:2, 5],
    summary(mtcars$am),
    table(mtcars$am) + 0.1,
    table(mtcars$am, mtcars$gear) + 0.1,
    summary(lm(mtcars$hp~1))$coeff,
    table(mtcars$am, mtcars$gear, mtcars$carb)
    )

tad <- panderOptions('table.alignment.default')
tar <- panderOptions('table.alignment.rownames')

panderOptions('table.alignment.default', 'centre')
panderOptions('table.alignment.rownames', 'right')
test_that('no error: allright', {
    for (t in tables)
        expect_that(pandoc.table.return(t, style = 'multiline'), is_a('character'))
})

f <- function(df) {
        if (class(df) != 'matrix' && !is.table(df) || length(dim(df)) < 2)
            ifelse(sapply(df, is.numeric), 'right', 'left')
        else
            ifelse(apply(df, 2, is.numeric), 'right', 'left')
}
panderOptions('table.alignment.default', f)
test_that('no error: functions', {
    for (t in tables)
        expect_that(pandoc.table.return(t, style = 'multiline'), is_a('character'))
})
panderOptions('table.alignment.default', tad)
panderOptions('table.alignment.rownames', tar)

test_that('digits param', {
    m <- matrix(rep(0.111111, 6), nrow = 2)
    res <- pander_return(m, digits = c(2, 4, 1))
    expect_equal(res[3], ' 0.11   0.1111   0.1 ')
    expect_warning(pander_return(m, digits = c(1, 2)))
    res <- pander_return(m, digits = 2)
    expect_equal(res[3],' 0.11   0.11   0.11 ')
    mt <- mtcars[1:4, 5:8]
    res <- pander_return(mt, digits = c(1, 4, 3, 4),
                         keep.trailing.zeros = TRUE)
    expect_equal(res[5],
                 '   **Mazda RX4**       4     2.620   16.5   0  ')
})

test_that('round param', {
    m <- matrix(rep(0.111111, 6), nrow = 2)
    res <- pander_return(m, round = c(2, 4, 1))
    expect_equal(res[3], ' 0.11   0.1111   0.1 ')
    expect_warning(pander_return(m, round = c(1, 2)))
    res <- pander_return(m, round = 2)
    expect_equal(res[3],' 0.11   0.11   0.11 ')
})

context('keep.trailing.zeros')
test_that('digits param', {
    expect_true(grepl('3.90', pander_return(mtcars[2:3, 1:5], keep.trailing.zeros = TRUE)[5]))
    expect_false(grepl('3.90', pander_return(mtcars[2:3, 1:5], keep.trailing.zeros = FALSE)[5]))
})

context('keep.line.breaks')
test_that('keep.line.breaks works correctly', {
  # keeping line breaks in a simple data.frame with one line breaks differs lines amount by one
  x <- data.frame(a = 'Pander\nPackage')
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE), '\n')[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), '\n')[[1]])
  expect_equal(lines.x.no.breaks + 1, lines.x.keep.breaks)

  # keeping line breaks in a simple data.frame with 2 rows with 1 line breaks differst lines amount by 2
  x <- data.frame(a = c('Pander\nPackage', 'Pander\nPackage'))
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE), '\n')[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), '\n')[[1]])
  expect_equal(lines.x.no.breaks + 2, lines.x.keep.breaks)

  #if there are no line breaks originally, they do not get introduced
  x <- data.frame(a = c('Pander Package', 'Pander Package'))
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE), '\n')[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), '\n')[[1]])
  expect_equal(lines.x.no.breaks, lines.x.keep.breaks)

  # works with random number of rows added
  rows <- sample(1:100, 1)
  x <- data.frame(a = rep('Pander\nPackage', rows))
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE), '\n')[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), '\n')[[1]])
  expect_equal(lines.x.no.breaks + rows, lines.x.keep.breaks)

  # random number of line breaks in one cell
  n <- sample(1:10, 1)
  x <- data.frame(a = paste(rep('pander', n), collapse = '\n'))
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE, split.cells = Inf), '\n')[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), '\n')[[1]])
  expect_equal(lines.x.no.breaks + n - 1, lines.x.keep.breaks)

  # random number of line breaks in cells, 3 columns
  n <- sample(1:10, 3)
  x <- data.frame(a = paste(rep('pander', n[1]), collapse = '\n'),
                  b = paste(rep('pander', n[2]), collapse = '\n'),
                  c = paste(rep('pander', n[3]), collapse = '\n'))
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE, split.cells = Inf, split.tables = Inf), '\n')[[1]]) #nolint
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), '\n')[[1]])
  expect_equal(lines.x.no.breaks + max(n) - 1, lines.x.keep.breaks)

  # 3 columns, 2 rows
  x <- rbind(x, x)
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE, split.cells = Inf, split.tables = Inf), '\n')[[1]]) #nolint
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), '\n')[[1]])
  expect_equal(lines.x.no.breaks + 2 * max(n) - 2, lines.x.keep.breaks)
})

context('split.cells')
test_that('split.cells works correctly', {
  x <- data.frame(a = 'foo bar\nfo bar')
  # single line break behaves correctly combines with keep line breaks
  expect_equal(pandoc.table.return(x, keep.line.breaks = TRUE, split.cells = 7),
               '\n---------\n    a    \n---------\n foo bar \n fo bar  \n---------\n\n')
  expect_equal(pandoc.table.return(x, keep.line.breaks = TRUE, split.cells = 6),
               '\n--------\n   a    \n--------\n  foo   \n  bar   \n fo bar \n--------\n\n')
  # Corner values of split.cells
  x <- data.frame(a = 'foo bar', b = 'foo bar')
  expect_equal(length(strsplit(pandoc.table.return(x, split.cells = c(6, Inf)), '\n')[[1]]), 8)
  expect_equal(pandoc.table.return(x, split.cells = c(6, Inf)),
               '\n---------------\n  a       b    \n----- ---------\n foo   foo bar \n bar           \n---------------\n\n')
  expect_equal(pandoc.table.return(x, split.cells = c(Inf, 6)),
               '\n---------------\n    a       b  \n--------- -----\n foo bar   foo \n           bar \n---------------\n\n')
  expect_equal(pandoc.table.return(x, split.cells = c(7, 7)),
               '\n-------------------\n    a         b    \n--------- ---------\n foo bar   foo bar \n-------------------\n\n')
  expect_equal(pandoc.table.return(x, split.cells = c(7, 7)), pandoc.table.return(x, split.cells = Inf))
  expect_equal(pandoc.table.return(x, split.cells = c(6, 6)), pandoc.table.return(x, split.cells = 6))
  expect_equal(pandoc.table.return(x, split.cells = c(7, 7)), pandoc.table.return(x, split.cells = 7))
  expect_equal(pandoc.table.return(x, split.cells = c(7, 7)), pandoc.table.return(x, split.cells = 7))
  # relative split.cells
  expect_equal(pandoc.table.return(x, split.cells = c('50%', '50%')),
               pandoc.table.return(x, split.cells = c(7)))
  expect_equal(pandoc.table.return(x, split.cells = c('20%', '80%'), split.tables = 30),
               pandoc.table.return(x, split.cells = c(6, Inf)))
  expect_equal(pandoc.table.return(x, split.cells = c('80%', '20%'), split.tables = 30),
               pandoc.table.return(x, split.cells = c(Inf, 6)))
})

test_that('split.cells param produces expected warnings', {
  mt <- mtcars[1:2, 1:4]
  expect_warning(pander(mt, split.cells = c(1, 2)))
  expect_warning(pander(mt, split.cells = c(1, 2, 3)))
  expect_warning(pander(mt, split.cells = c('10%', '10%', '10%')))
  expect_warning(pander(mt, split.cells = c('30%', '30%', '40%')))
})

context('tableExpand')

test_that('produces.errors', {
  x <- data.frame(a = 'Pander\nPackage')
  expect_error(pander(x, style = 'simple', keep.line.breaks = TRUE))
  expect_error(pander(x, style = 'rmarkdown', keep.line.breaks = TRUE))
})

test_that('tableExpand behaves correctly', {
  ## multiline style check
  argv <-  structure(list(txt = structure(c(1L, 4L, 2L, 3L), .Label = c('&nbsp;',  'cyl', 'disp', 'mpg'), class = 'factor'), width = c(19, 5, 5,  6), justify = structure(c(1L, 1L, 1L, 1L), .Label = 'centre', class = 'factor')), .Names = c('txt',  'width', 'justify'), row.names = c(NA, -4L), class = 'data.frame') #nolint
  sep.cols <-  c('', ' ', '')
  style <-  'multiline'
  res <- tableExpand_cpp(argv[, 1], argv[, 2], argv[, 3], sep.cols, style)
  # max number of line breaks equals number of lines in the result
  expect_equal(max(sapply(strsplit(as.character(argv[, 1]), '\n'), length)),
               length(strsplit(res, '\n')[[1]]))
  expect_equal(nchar(res),
               nchar(sep.cols)[1] + (length(argv[, 2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[, 2]))
  expect_equal(res, '      &nbsp;         mpg   cyl   disp ');

  ## grid style check
  argv <-  structure(list(txt = structure(c(1L, 5L, 2L, 3L, 4L), .Label = c('&nbsp;',  'cyl', 'disp', 'hp', 'mpg'), class = 'factor'), width = c(20,  5, 5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = 'centre', class = 'factor')), .Names = c('txt',  'width', 'justify'), row.names = c(NA, -5L), class = 'data.frame') #nolint
  sep.cols <-  c('| ', ' | ', ' |')
  res <- tableExpand_cpp(argv[, 1], argv[, 2], argv[, 3], sep.cols, style)
  # max number of line breaks equals number of lines in the result
  expect_equal(max(sapply(strsplit(as.character(argv[, 1]), '\n'), length)), length(strsplit(res, '\n')[[1]]))
  expect_equal(nchar(res), nchar(sep.cols)[1] + (length(argv[, 2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[, 2])) #nolint
  expect_equal(res, '|        &nbsp;        |  mpg  |  cyl  |  disp  |  hp  |' )

  ## rmarkdown style check
  argv <-  structure(list(txt = structure(c(1L, 4L, 5L, 3L, 2L), .Label = c('**Mazda RX4**',  '110', '160', '21', '6'), class = 'factor'), width = c(20, 5,  5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = 'centre', class = 'factor')), .Names = c('txt',  'width', 'justify'), row.names = c('t.rownames', 'mpg', 'cyl',  'disp', 'hp'), class = 'data.frame') #nolint
  sep.cols <-  c('| ', ' | ', ' |')
  style <-  'rmarkdown'
  res <- tableExpand_cpp(argv[, 1], argv[, 2], argv[, 3], sep.cols, style)
  # max number of line breaks equals number of lines in the result
  expect_equal(max(sapply(strsplit(as.character(argv[, 1]), '\n'), length)), length(strsplit(res, '\n')[[1]]))
  expect_equal(nchar(res),
               nchar(sep.cols)[1] + (length(argv[, 2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[, 2]))
  expect_equal(res, '|    **Mazda RX4**     |  21   |   6   |  160   | 110  |')

  ## simple style check
  argv <-  structure(list(txt = structure(c(1L, 3L, 4L, 2L, 5L), .Label = c('**Datsun 710**',  '108', '22.8', '4', '93'), class = 'factor'), width = c(20, 5,  5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = 'centre', class = 'factor')), .Names = c('txt',  'width', 'justify'), row.names = c('t.rownames', 'mpg', 'cyl',  'disp', 'hp'), class = 'data.frame') #nolint
  sep.cols <-  c('', ' ', '')
  style <-  'simple'
  res <- tableExpand_cpp(argv[, 1], argv[, 2], argv[, 3], sep.cols, style)
  # max number of line breaks equals number of lines in the result
  expect_equal(max(sapply(strsplit(as.character(argv[, 1]), '\n'), length)), length(strsplit(res, '\n')[[1]]))
  expect_equal(nchar(res),
               nchar(sep.cols)[1] + (length(argv[, 2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[, 2]))
  expect_equal(res, '   **Datsun 710**    22.8    4    108    93 ')

  ## left justification
  argv <-  structure(list(txt = structure(c(1L, 4L, 5L, 3L, 2L), .Label = c('**Mazda',  '110', '160', '21', '6'), class = 'factor'), width = c(10, 5,  5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = 'left', class = 'factor')), .Names = c('txt',  'width', 'justify'), row.names = c(NA, -5L), class = 'data.frame') #nolint
  sep.cols <-  c('', ' ', '')
  style <-  'multiline'
  res <- tableExpand_cpp(argv[, 1], argv[, 2], argv[, 3], sep.cols, style)
  # max number of line breaks equals number of lines in the result
  expect_equal(max(sapply(strsplit(as.character(argv[, 1]), '\n'), length)), length(strsplit(res, '\n')[[1]]))
  expect_equal(nchar(res),
               nchar(sep.cols)[1] + (length(argv[, 2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[, 2]))
  expect_equal(res, '**Mazda    21    6     160    110 ');

  ## right justification
  argv <-  structure(list(txt = structure(c(1L, 3L, 5L, 4L, 2L), .Label = c('**Hornet 4 Drive**',  '110', '21.4', '258', '6'), class = 'factor'), width = c(20,  5, 5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = 'right', class = 'factor')), .Names = c('txt',  'width', 'justify'), row.names = c('t.rownames', 'mpg', 'cyl',  'disp', 'hp'), class = 'data.frame') #nolint
  sep.cols <-  c('', ' ', '')
  style <-  'simple'
  res <- tableExpand_cpp(argv[, 1], argv[, 2], argv[, 3], sep.cols, style)
  # max number of line breaks equals number of lines in the result
  expect_equal(max(sapply(strsplit(as.character(argv[, 1]), '\n'), length)), length(strsplit(res, '\n')[[1]]))
  expect_equal(nchar(res),
               nchar(sep.cols)[1] + (length(argv[, 2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[, 2]))
  expect_equal(res, '  **Hornet 4 Drive**  21.4     6    258  110' );

  ## multiple lines
  argv <-  structure(list(txt = structure(c(1L, 3L, 5L, 4L, 2L), .Label = c('**Hornet\n4\nDrive**',  '110', '21.4', '258', '6'), class = 'factor'), width = c(10,  5, 5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = 'left', class = 'factor')), .Names = c('txt',  'width', 'justify'), row.names = c('t.rownames', 'mpg', 'cyl',  'disp', 'hp'), class = 'data.frame') #nolint
  sep.cols <-  c('', ' ', '')
  style <-  'multiline'
  res <- tableExpand_cpp(argv[, 1], argv[, 2], argv[, 3], sep.cols, style)
  # max number of line breaks equals number of lines in the result
  expect_equal(max(sapply(strsplit(as.character(argv[, 1]), '\n'), length)),
               length(strsplit(res, '\n')[[1]]))
  # max number of line breaks equals number of lines in the result
  expect_equal(res, '**Hornet   21.4  6     258    110 \n4                                 \nDrive**                           '); #nolint


  argv <-  structure(list(txt = structure(c(1L, 3L, 4L, 2L, 5L), .Label = c('**Datsun\n710**',  '108', '22.8', '4', '93'), class = 'factor'), width = c(10, 5,  5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = 'left', class = 'factor')), .Names = c('txt',  'width', 'justify'), row.names = c('t.rownames', 'mpg', 'cyl',  'disp', 'hp'), class = 'data.frame') #nolint
  sep.cols <-  c('', ' ', '')
  style <-  'multiline'
  res <- tableExpand_cpp(argv[, 1], argv[, 2], argv[, 3], sep.cols, style)
  # max number of line breaks equals number of lines in the result
  expect_equal(max(sapply(strsplit(as.character(argv[, 1]), '\n'), length)),
               length(strsplit(res, '\n')[[1]]))
  expect_equal(res, '**Datsun   22.8  4     108    93  \n710**                             ');

  argv <-  structure(list(txt = structure(c(1L, 4L, 5L, 3L, 2L), .Label = c('**Mazda\nRX4\nWag**',  '110', '160', '21', '6'), class = 'factor'), width = c(10, 5,  5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = 'left', class = 'factor')), .Names = c('txt',  'width', 'justify'), row.names = c('t.rownames', 'mpg', 'cyl',  'disp', 'hp'), class = 'data.frame') #nolint
  sep.cols <-  c('', ' ', '')
  style <-  'multiline'
  res <- tableExpand_cpp(argv[, 1], argv[, 2], argv[, 3], sep.cols, style)
  # max number of line breaks equals number of lines in the result
  expect_equal(max(sapply(strsplit(as.character(argv[, 1]), '\n'), length)), length(strsplit(res, '\n')[[1]]))
  expect_equal(res, '**Mazda    21    6     160    110 \nRX4                               \nWag**                             '); #nolint

  # empty cells
  cells <- c('', '', '')
  cols.width <- c(2, 2, 2)
  justify <- c('centre', 'centre', 'centre')
  sep.cols <- c('', ' ', '')
  res <- tableExpand_cpp(cells, cols.width, justify, sep.cols, 'multiline')
  # when max.width param is small, every word is a line
  expect_equal(max(sapply(strsplit(as.character(cells), '\n'), length)), 0)
  expect_equal(nchar(res),
               nchar(sep.cols)[1] + (length(cells) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(cols.width))
  expect_equal(res, '        ')

  # backslashes issue (#22)
  expect_equal(pandoc.table.return(
      data.frame(a = '\\1 \\ 32', b ='23')),
      '\n--------------\n    a      b  \n--------- ----\n \\1 \\ 32   23 \n--------------\n\n') #nolint
  expect_equal(pandoc.table.return(
      data.frame(a = '\\1 \\ 32', b = '23'),
      justify = 'right'),
      '\n--------------\n        a    b\n--------- ----\n  \\1 \\ 32   23\n--------------\n\n') #nolint
  expect_equal(pandoc.table.return(data.frame(a = '\\1', b = '23')),
               '\n---------\n a    b  \n---- ----\n \\1   23 \n---------\n\n') #nolint

  # unicode string issue
  expect_equal(pandoc.table.return(data.frame(a = 'ßß')),
               '\n----\n a  \n----\n ßß \n----\n\n')
})

context('Empty objects')

test_that('Behavior for empty objects is correct', {
    mt <- mtcars[mtcars$mpg < 0, 1:4]
    res <- pander_return(mt)
    expect_equal(res[3], ' **mpg**   **cyl**   **disp**   **hp** ')
    expect_equal(length(res), 6)
    colnames(mt) <- NULL
    res <- suppressWarnings(pander_return(mt))
    expect_equal(length(res), 0)
    expect_warning(pander_return(mt))
    mt <- matrix(0, nrow = 0, ncol = 5)
    res <- suppressWarnings(pander_return(mt))
    expect_equal(length(res), 0)
    expect_warning(pander_return(mt))
    colnames(mt) <- 1:5
    res <- pander_return(mt)
    expect_equal(res[3], ' **1**   **2**   **3**   **4**   **5** ')
    expect_equal(length(res), 6)
    expect_equal(length(suppressWarnings(pander_return(data.frame()))), 0)
    expect_warning(pander_return(data.frame()))
})

context('plain.ascii')

test_that('plain.ascii option works correctly', {
    # dim is NULL
    x <- 1:10
    res <- pandoc.table.return(x, emphasize.cells = c(3, 4), plain.ascii = TRUE)
    res <- strsplit(res, '\n')[[1]]
    expect_false(any(grepl('\\*', res)))
    expect_equal(res[3], ' 1   2   3   4   5   6   7   8   9   10 ')
    # length(dim) == 1
    x <- array(1:10)
    res <- pandoc.table.return(x, emphasize.cells = c(3, 4), plain.ascii = TRUE)
    res <- strsplit(res, '\n')[[1]]
    expect_false(any(grepl('\\*', res)))
    expect_equal(res[3], ' 1   2   3   4   5   6   7   8   9   10 ')
    # length(dim) > 1
    x <- mtcars[1:3, 1:4]
    res <- pandoc.table.return(x, emphasize.rows = 2, plain.ascii = TRUE)
    expect_false(grepl('&nbsp;', res))
    expect_false(grepl('\\*', res))
})

context('S3 methods')

test_that('pander.tabular behaves correctly', {
    suppressMessages(require(tables))
    tab <- pander_return(tables::tabular(as.factor(am) ~ (mpg + hp + qsec) * (mean + median),
                                         data = mtcars),
                         emphasize.rownames = FALSE,
                         split.tables = Inf)
    expect_equal(length(tab), 10)
    tab <- pander_return(tables::tabular( (Species + 1) ~ (n = 1) + Format(digits = 2) * (Sepal.Length + Sepal.Width) * (mean + sd), data = iris ), #nolint
                         split.tables = Inf)
    expect_equal(length(tab), 14)
})

test_that('pander.CrossTable behaves correctly', {
    suppressMessages(require(descr))
    # issue https://github.com/Rapporter/pander/issues/163
    x <- CrossTable(mtcars$cyl, mtcars$gear, prop.c = FALSE, prop.t = FALSE, chisq = FALSE, prop.chisq = FALSE)
    res <- pander_return(x)
    expect_true(any(grepl(gsub('\\$', '\\\\$', x$ColData), res))) #nolint
    expect_true(any(grepl(gsub('\\$', '\\\\$', x$RowData), res))) #nolint
    # expected N, residual, std residual, adj std residual rownames was not included
    x <- suppressWarnings(CrossTable(mtcars$cyl, mtcars$gear, expected = TRUE,
                                     resid = TRUE, sresid = TRUE, asresid = TRUE))
    res <- pander_return(x)
    expect_true(any(grepl('Expected N', res)))
    expect_true(any(grepl('Residual', res)))
    expect_true(any(grepl('Std Residual', res)))
    expect_true(any(grepl('Adj Std Resid', res)))
    # issue 211 support for total.r total.c
    x <- suppressWarnings(CrossTable(mtcars$cyl, mtcars$gear))
    res <- pander_return(x, total.c = FALSE)
    expect_equal(length(res), 27)
    expect_equal(length(grep('Total', res)), 4)
    res <- pander_return(x, total.r = FALSE)
    expect_equal(length(res), 30)
    expect_equal(length(grep('Total', res)), 4)
    res <- pander_return(x)
    expect_equal(length(res), 30)
    expect_equal(length(grep('Total', res)), 5)
})

test_that('pander.NULL behaves correctly', {
    expect_equal(length(pander_return(NULL)), 0)
    expect_equal(length(pander_return(c(NULL, NULL))), 0)
})

test_that('pander.cast_df behaves correctly', {
    df <- data.frame(type = c(1, 1, 2, 2, 3, 3), variable = 'n', value = c(71, 72, 68, 80, 21, 20))
    df.cast <- reshape::cast(df, type~., sum)
    expect_equal(pander_return(df.cast, style = 'simple'),
                 c('', '', ' type   (all) ', '------ -------',
                   '  1      143  ', '  2      148  ', '  3      41   ', ''))
})

test_that('pander.lm/pander.summary.lm behaves correctly', {
    ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
    trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
    group <- gl(2, 10, 20, labels = c('Ctl', 'Trt'))
    weight <- c(ctl, trt)
    lm.D9 <- lm(weight ~ group)
    res1 <- pander_return(lm.D9)
    expect_equal(length(res1), 11)
    expect_equal(max(nchar(res1)), 63)
    res <- pander_return(summary(lm.D9))
    expect_true(any(grepl('Fitting linear model', res)))
    expect_true(any(grepl('Observations', res)))
    expect_equal(length(res), 18)
})

test_that('pander.glm/pander.summary.glm behaves correctly', {
    clotting <- data.frame(
        u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
        lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
        lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12))
    glmm <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
    pglmm <- pander_return(glmm)
    expect_equal(length(pglmm), 11)
    expect_equal(max(nchar(pglmm)), 70)
    res <- pander_return(summary(glmm))
    expect_true(any(grepl('Null deviance', res)))
    expect_equal(length(res), 21)
})

test_that('pander.aov/pander.summary.aov behaves correctly', {
    npk.aovE <- aov(yield ~  N * P * K + Error(block), npk)
    paov <- pander_return(npk.aovE)
    psaov <- pander_return(summary(npk.aovE))
    expect_equal(paov, psaov) # choice of similar result for summary and standard
    expect_equal(length(psaov), 23)
})

test_that('pander.anova behaves correctly', {
    fit <- lm(sr ~ ., data = LifeCycleSavings)
    a <- anova(fit)
    pa <- pander_return(a, style = 'simple')
    expect_true(all(sapply(names(a)[-5], grepl, pa[3])))
    #more complicated run
    fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
    fit1 <- update(fit0, . ~ . + pop15)
    fit2 <- update(fit1, . ~ . + pop75)
    fit3 <- update(fit2, . ~ . + dpi)
    fit4 <- update(fit3, . ~ . + ddpi)
    a <- anova(fit0, fit1, fit2, fit3, fit4, test = 'F')
    pa <- pander_return(a, style = 'simple')
    expect_true(all(sapply(names(a)[-6], grepl, pa[3])))
})

test_that('pander.aovlist/pander.summary.aovlist behaves correctly', {
    options(contrasts = c('contr.helmert', 'contr.poly'))
    N <- c(0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
    P <- c(1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0)
    K <- c(1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0)
    yield <- c(49.5, 62.8, 46.8, 57.0, 59.8, 58.5, 55.5, 56.0, 62.8, 55.8, 69.5,
               55.0, 62.0, 48.8, 45.5, 44.2, 52.0, 51.5, 49.8, 48.8, 57.2, 59.0, 53.2, 56.0)
    npk <- data.frame(block = gl(6, 4), N = factor(N), P = factor(P),
                      K = factor(K), yield = yield)
    a <- aov(yield ~  N * P * K + Error(block), npk)
    pa <- pander_return(a, style = 'simple')
    expect_equal(length(pa), 14)
})

test_that('pander.ts behaves correctly', {
    # ncol NULL
    res <- pander_return(ts(1:10, frequency = 4, start = c(1959, 2)), style = 'simple')
    expect_equal(res[3], '  &nbsp;    Q1   Q2   Q3   Q4 ')
    expect_equal(length(res), 8)
    # ncol not NULL
    res <- pander_return(ts(matrix(rnorm(30), 10, 3), start = c(1961, 1), frequency = 12), style = 'simple')
    expect_equal(res[3], '    &nbsp;      Series.1   Series.2   Series.3 ')
    expect_equal(length(res), 15)
})

test_that('pander.coxph behaves correctly', {
    suppressMessages(require(survival))
    test1 <- list(time = c(4, 3, 1, 1, 2, 2, 3),
                  status = c(1, 1, 1, 0, 1, 1, 0),
                  x = c(0, 2, 1, 1, 1, 0, 0),
                  sex = c(0, 0, 0, 0, 1, 1, 1))
    # Fit a stratified model
    res <- pander_return(coxph(Surv(time, status) ~ x + strata(sex), test1))
    expect_equal(res[3], ' &nbsp;    coef    exp(coef)   se(coef)     z        p    ')
    expect_equal(length(res), 10)
})

test_that('pander.clogit works correctly', {
    resp <- levels(logan$occupation)
    n <- nrow(logan)
    indx <- rep(1:n, length(resp))
    logan2 <- data.frame(logan[indx, ],
                         id = indx,
                         tocc = factor(rep(resp, each = n)))
    logan2$case <- (logan2$occupation == logan2$tocc)
    res <- pander_return(suppressWarnings(clogit(case ~ tocc + tocc:education + strata(id), logan2)), split.table = Inf)
    expect_true(grepl('Fitting Conditional logistic regression', res[24]))
    expect_equal(length(res), 26)
})

test_that('pander.zoo works correctly', {
    suppressMessages(require(zoo))
    x.Date <- as.Date('2003-02-01') + c(1, 3, 7, 9, 14) - 1
    res <- pander_return(zoo(rnorm(5), x.Date), style = 'simple')
    expect_true(grepl('Value', res[3]))
    expect_equal(length(res), 10)
    # more complex example with colnames
    res <- pander_return(zoo(cbind(foo = rnorm(5), bar = rnorm(5))), style = 'simple')
    expect_true(grepl('foo', res[3]))
    expect_equal(length(res), 10)
})

test_that('pander.lme/pander.summary.lme behaves correctly', {
    suppressMessages(require(nlme))
    l1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
    sl <- summary(l1)
    pl <- pander_return(l1)
    spl <- pander_return(sl)
    expect_equal(length(pl), 11)
    expect_equal(length(grep('Table', pl)), 1)
    expect_equal(length(spl), 29)
    expect_equal(length(grep('Table', spl)), 3)
})

# test_that('pander.describe works correctly', {
#     suppressMessages(require(psych))
#     x <- data.frame(a = rnorm(10), b = rnorm(10, 2, 2), c = rnorm(10, 3, 4))
#     res <- pander_return(describe(x))
#     expect_equal(length(res), 37)
#     expect_equal(length(grep('Table', res)), 2)
#     res <- pander_return(describe(x), split.tables = Inf)
#     expect_equal(length(res), 11)
# })

test_that('pander.survdiff works correctly', {
    suppressMessages(require(survival))
    res <- pander_return(survdiff(Surv(futime, fustat) ~ rx, data = ovarian))
    expect_equal(length(res), 12)
    expect_equal(res[3], '  &nbsp;    N    Observed   Expected   (O-E)^2/E   (O-E)^2/V ')
    # length(x$n) == 1
    expect <- survexp(futime ~ ratetable(age = (accept.dt - birth.dt),
                                         sex = 1,
                                         year = accept.dt,
                                         race = 'white'),
                      jasa, cohort = FALSE, ratetable = survexp.usr)
    res <- pander_return(survdiff(Surv(jasa$futime, jasa$fustat) ~ offset(expect)))
    expect_equal(res[3], ' Observed   Expected     Z      p ')
    expect_equal(length(res), 9)
})

test_that('pander.survfit works correctly', {
    suppressMessages(require(survival))
    res <- pander_return(survfit(Surv(time, status) ~ x, data = aml))
    expect_equal(length(res), 20)
    expect_true(any(grepl('Table', res)))
    # using additional options
    res <- pander_return(survfit(Surv(time, status) ~ x, data = aml), print.rmean = TRUE)
    expect_equal(length(res), 21)
    expect_equal(res[21], '* restricted mean with upper limit =  161')
})

test_that('pander.sessionInfo works correctly', {
    suppressMessages(require(utils))
    res <- pander_return(sessionInfo())
    expect_true(any(grepl('locale', res)))
    expect_true(any(grepl('attached base package', res)))
    res <- pander_return(sessionInfo(), locale = FALSE, compact = FALSE)
    expect_false(any(grepl('locale', res)))
    expect_true(any(grepl('utils', res)))
})

test_that('pander.stat.table works correctly', {
    suppressMessages(require(Epi))
    res <- pander_return(stat.table(
        tension,
        list(count(), mean(breaks)),
        data = warpbreaks))
    expect_equal(length(res), 11)
    expect_equal(res[3], ' &nbsp;   count()   mean(breaks) ')
    res <- pander_return(stat.table(index = list(tension, wool),
                                    mean(breaks), data = warpbreaks))
    expect_equal(length(res), 13)
    # here add test
})

test_that('pander.microbenchmark works correctly', {
    suppressMessages(require(microbenchmark))
    res <- pander_return(microbenchmark(paste(1:10), paste0(1:10)))
    expect_true(any(grepl('Unit', res)))
    expect_equal(length(res), 11)
    res <- pander_return(microbenchmark(paste(1:10), paste0(1:10)), split.tables = Inf, expr.labels = c('A'))
    expect_true(any(grepl('A', res)))
    expect_true(any(grepl('paste0\\(1:10\\)', res)))
    res <- pander_return(microbenchmark(paste(1:10), paste0(1:10)), split.tables = Inf, expr.labels = c('A', 'B'))
    expect_true(any(grepl('A', res)))
    expect_true(any(grepl('B', res)))
    expect_false(any(grepl('paste0\\(1:10\\)', res)))
    res <- pander_return(microbenchmark(paste(1:10), paste0(1:10)), split.tables = Inf, expr.labels = c('A', 'B', 'C'))
    expect_true(any(grepl('A', res)))
    expect_true(any(grepl('B', res)))
    expect_false(any(grepl('paste0\\(1:10\\)', res)))
})

test_that('pander.function works correctly', {
    testf <- function(x) {
        y <- x + 1
        y
    }
    res <- pander_return(testf)
    expect_true(all(grepl('\t', res)))
    expect_equal(length(res), 5)
    res <- pander_return(testf, syntax.highlighting = TRUE, add.name = TRUE)
    expect_equal(length(res), 7)
    expect_equal(res[1], '```r')
    expect_true(grepl('testf', res[2]))
})

test_that('pander.rlm works correctly', {
    suppressMessages(require(MASS))
    res <- pander_return(rlm(stack.loss ~ ., stackloss))
    expect_equal(res[4], ' (Intercept)   Air.Flow   Water.Temp   Acid.Conc. ')
    expect_equal(length(res), 13)
    res <- pander_return(rlm(stack.loss ~ ., stackloss, psi = psi.hampel, init = 'lts'))
    expect_equal(res[11], 'Degrees of freedom: 21 total; 17 residual')
    expect_equal(length(res), 13)
})

test_that('pander.summary.table works correctly', {
    ts <- summary(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph))
    res <- pander_return(ts)
    expect_equal(res[6], ' Chisq   df     p.value  ')
    expect_equal(length(res), 13)
    res <- pander_return(ts, print.call = FALSE)
    expect_false(any(grepl('Calls', res)))
    expect_equal(length(res), 12)
    res <- pander_return(ts, caption = 'Factor')
    expect_equal(res[11], 'Table: Factor')
    res <- pander_return(summary(table(1:3)))
    expect_false(any(grepl('-', res)))
    expect_equal(length(res), 2)
})

test_that('pander.randomForest works correctly', {
    suppressMessages(require(randomForest))
    iris.rf <- randomForest(Species ~ ., data = iris, importance = TRUE,
                            proximity = TRUE)
    res <- pander_return(iris.rf)
    expect_equal(length(res), 20)
    expect_equal(res[19], 'Table: Confusion Matrix')
    ozone.rf <- randomForest(Ozone ~ ., data = airquality, mtry = 3,
                             importance = TRUE, na.action = na.omit)
    res <- pander_return(ozone.rf)
    expect_equal(length(res), 8)
    expect_false(any(grep('-', res)))
    # with test
    index <- 1:nrow(iris)
    trainindex <- sample(index, trunc(length(index) / 2))
    trainset <- iris[trainindex, ]
    testset <- iris[-trainindex, ]
    res <- pander_return(randomForest(x = trainset[, -1],
                                      y = trainset[, 1],
                                      xtest = testset[, -1],
                                      ytest = testset[, 1]))
    expect_equal(length(res), 10)
    expect_true(any(grepl('Test set MSE', res)))
    res <- pander_return(randomForest(x = trainset[, -ncol(trainset)],
                                      y = trainset[, ncol(trainset)],
                                      xtest = testset[, -ncol(testset)],
                                      ytest = testset[, ncol(testset)]))
    expect_equal(length(res), 34)
    expect_equal(res[33], 'Table: Test Confusion Matrix')
})

test_that('pander.irts works correctly', {
    suppressMessages(require(tseries))
    n <- 10
    t <- cumsum(rexp(n, rate = 0.1))
    v <- rnorm(n)
    res <- pander_return(as.irts(cbind(t, v)))
    expect_equal(length(res), 23)
    u <- rnorm(n)
    res <- pander_return(irts(t, cbind(u, v)))
    expect_equal(length(res), 23)
})

test_that('pander.manova/summary.manova works correctly', {
    npk2 <- within(npk, foo <- rnorm(24))
    x <- manova(cbind(yield, foo) ~ block + N * P * K, npk2)
    res1 <- pander_return(x)
    xs <- summary(x)
    res2 <- pander_return(xs)
    expect_equal(res1, res2)
    expect_equal(length(res1), 21)
})

test_that('pander.gtable works correctly', {
    suppressMessages(require(gtable))
    suppressMessages(require(grid))
    a <- gtable(unit(1:3, c('cm')), unit(5, 'cm'))
    rect <- rectGrob(gp = gpar(fill = 'black'))
    a <- gtable_add_grob(a, rect, 1, 1)
    res <- pander_return(a)
    expect_equal(length(res), 7)
})

test_that('pander.nls/pander.summary.nls works correctly', {
    utils::data(muscle, package = 'MASS')
    with(muscle, table(Strip))
    musc.1 <- nls(Length ~ cbind(1, exp(-Conc / th)), muscle,
                start = list(th = 1), algorithm = 'plinear')
    res <- pander_return(musc.1, show.convergence = TRUE)
    expect_equal(length(res), 17)
    res <- pander_return(musc.1)
    expect_equal(length(res), 14)
    musc.1.s <- summary(musc.1)
    res <- pander_return(musc.1.s)
    expect_equal(length(res), 18)
    res <- pander_return(musc.1.s, show.convergence = TRUE)
    expect_equal(length(res), 21)
    musc.1.s <- summary(musc.1, correlation = TRUE)
    res <- pander_return(musc.1.s)
    expect_true(any(grepl('Correlation', res)))
    expect_equal(length(res), 31)
})

test_that('pander.arima works correctly', {
    res <- pander_return(arima(lh, order = c(1, 0, 0)))
    expect_equal(length(res), 17)
    expect_true(any(grep('s\\.e', res)))
    res <- pander_return(arima(lh, order = c(3, 0, 0)))
    expect_equal(length(res), 17)
    expect_true(any(grep('s\\.e', res)))
    res <- pander_return(arima(lh, order = c(3, 0, 0), method = 'CSS'), se = FALSE)
    expect_equal(length(res), 15)
    expect_false(any(grep('s\\.e', res)))
})

test_that('pander.polr/summary.polr works correctly', {
    house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
    res <- pander_return(house.plr)
    expect_equal(length(res), 25)
    expect_equal(length(grep('Table', res)), 2)
    res <- pander_return(suppressMessages(summary(house.plr)))
    expect_equal(length(res), 39)
    expect_equal(length(grep('Table', res)), 2)
    res <- pander_return(suppressMessages(summary(house.plr, correlation = TRUE)), split.table = Inf)
    expect_equal(length(res), 60)
    expect_equal(length(grep('Table', res)), 3)
})

test_that('pander.survreg/summary.survreg works correctly', {
    suppressMessages(require(survival))
    x <- survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian, dist = 'exponential')
    res <- pander_return(x)
    expect_equal(length(res), 27)
    expect_equal(length(grep('Table', res)), 1)
    res <- pander_return(suppressMessages(summary(x)))
    expect_equal(length(res), 33)
    expect_equal(length(grep('Table', res)), 1)
    res <- pander_return(suppressMessages(summary(x, correlation = TRUE)), split.table = Inf)
    expect_equal(length(res), 44)
    expect_equal(length(grep('Table', res)), 2)
})

test_that('pander.ols works correctly', {
    suppressMessages(require(rms))
    set.seed(1)
    x1 <- runif(200)
    x2 <- sample(0:3, 200, TRUE)
    distance <- (x1 + x2 / 3 + rnorm(200)) ^ 2
    d <- datadist(x1, x2)
    f <- ols(sqrt(distance) ~ rcs(x1, 4) + scored(x2), x = TRUE)
    res <- pander_return(f)
    expect_equal(length(res), 50)
    expect_equal(length(grep('Table', res)), 3)
})

test_that('pander.lrm works correctly', {
    suppressMessages(require(rms))
    x    <- 1:5
    y    <- c(0, 1, 0, 1, 0)
    reps <- c(1, 2, 3, 2, 1)
    res <- pander_return(lrm(y ~ x))
    expect_equal(length(res), 39)
    expect_true(any(grep('y ~ x', res)))
    expect_equal(length(grep('Table', res)), 2)

    x <- rep(x, reps)
    y <- rep(y, reps)
    res <- pander_return(lrm(y ~ x), coefs = FALSE)
    expect_equal(length(res), 28)
    expect_true(any(grep('y ~ x', res)))
    expect_equal(length(grep('Table', res)), 1)

    res <- pander_return(lrm(y ~ x, penalty = 0.1), coefs = TRUE)
    expect_true(any(grep('y ~ x', res)))
    expect_equal(length(grep('Table', res)), 3)
})

test_that('pander.orm works correctly', {
    suppressMessages(require(rms))
    n <- 100
    y <- round(runif(n), 2)
    x1 <- sample(c(-1, 0, 1), n, TRUE)
    x2 <- sample(c(-1, 0, 1), n, TRUE)
    res <- pander_return(orm(y ~ x1 + x2, eps = 1e-5))
    expect_equal(length(res), 39)
    expect_true(any(grep('y ~ x1 \\+ x2', res)))
    expect_equal(length(grep('Table', res)), 2)

    res <- pander_return(orm(y ~ x1 + x2, eps = 1e-5), coefs = FALSE)
    expect_equal(length(res), 28)
    expect_true(any(grep('y ~ x1 \\+ x2', res)))
    expect_equal(length(grep('Table', res)), 1)
})

test_that('pander.Glm works correctly', {
    suppressMessages(require(rms))
    clotting <- data.frame(
        u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
        lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
        lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12))
    f <- Glm(lot1 ~ log(u), data = clotting, family = Gamma)
    res <- pander_return(f)
    expect_equal(length(res), 31)
    expect_true(any(grep('lot1 ~ log\\(u\\)', res)))
    expect_equal(length(grep('Table', res)), 2)
    res <- pander_return(f, coefs = FALSE)
    expect_equal(length(res), 20)
    expect_true(any(grep('ot1 ~ log\\(u\\)', res)))
    expect_equal(length(grep('Table', res)), 1)
})

test_that('pander.cph', {
    options(contrasts = original_contrasts_options)
    suppressMessages(require(rms))
    n <- 1000
    set.seed(731)
    age <- 50 + 12 * rnorm(n)
    label(age) <- 'Age'
    sex <- factor(sample(c('Male', 'Female'), n, rep = TRUE, prob = c(.6, .4)))
    cens <- 15 * runif(n)
    h <- .02 * exp(.04 * (age - 50) + .8 * (sex == 'Female'))
    dt <- -log(runif(n)) / h
    label(dt) <- 'Follow-up Time'
    e <- ifelse(dt <= cens, 1, 0)
    dt <- pmin(dt, cens)
    units(dt) <- 'Year'
    dd <- datadist(age, sex)
    S <- Surv(dt, e)
    f <- cph(S ~ rcs(age, 4) + sex, x = TRUE, y = TRUE)
    res <- pander_return(f)
    expect_equal(length(res), 43)
    expect_true(any(grep('S ~ rcs\\(age, 4\\) \\+ sex', res)))
    expect_equal(length(grep('Table', res)), 2)
    res <- pander_return(f, conf.int = 0.95)
    expect_equal(length(res), 58)
    expect_true(any(grep('S ~ rcs\\(age, 4\\) \\+ sex', res)))
    expect_equal(length(grep('Table', res)), 3)
})

test_that('pander.ets works correctly', {
  suppressMessages(require(forecast))
  res <- pander_return(ets(lh, model = 'AAN', lambda = .5))
  expect_equal(length(res), 27)
  expect_true(any(grep('Box-Cox', res)))
  res <- pander_return(ets(lh, model = 'ANN'))
  expect_equal(length(res), 25)
  expect_false(any(grep('Box-Cox', res)))
  res <- pander_return(ets(lh, model = 'AAN', damped = TRUE))
  expect_equal(length(res), 25)
  expect_true(any(grep('phi', res)))
})

test_that('pander.list works correctly', {
  res <- pander_return(list(1:10))
  expect_equal(length(res), 7)
  expect_true(any(grep('end of list', res)))
  res <- pander_return(list(a = 1:10, b = list()))
  expect_equal(length(res), 8)
  expect_true(any(grep('end of list', res)))
  res <- pander_return(list(a = list(b = list(c = 5))))
  expect_equal(length(res), 13)
  expect_true(any(grep('end of list', res)))
})
