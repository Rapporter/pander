library(testthat)
library(pander)

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
    table(mtcars$am, mtcars$gear, mtcars$carb)
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

dm <- panderOptions('decimal.mark')
panderOptions('decimal.mark', ',')
test_that('decimal mark', {
    for (t in tables)
        expect_true(grepl(',', paste(pander.return(t), collapse = '\n')))
})

## ## manual test
## for (t in tables)
##     pander(t)

panderOptions('decimal.mark', dm)

context('highlight tables')

t <- mtcars[1:3, 1:5]
test_that('highlight 1D: no error', {
    expect_that(pandoc.table.return(t$mpg, emphasize.cells = 1), is_a('character'))
    expect_that(pandoc.table.return(t$mpg, emphasize.cells = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t$mpg, emphasize.strong.cells = 1), is_a('character'))
    expect_that(pandoc.table.return(t$mpg, emphasize.strong.cells = 1:2), is_a('character'))
})

t <- table(mtcars$am, mtcars$gear)
test_that('emphasize 2D: no error', {
    expect_that(pandoc.table.return(t, emphasize.rows = 1), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.rows = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.cols = 1), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.cols = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.cells = which(t > 10, arr.ind = TRUE)), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.cells = which(t > 20, arr.ind = TRUE)), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.rows = 1), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.rows = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.cols = 1), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.cols = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.cells = which(t > 10, arr.ind = TRUE)), is_a('character'))
    expect_that(pandoc.table.return(t, emphasize.strong.cells = which(t > 20, arr.ind = TRUE)), is_a('character'))
})

test_that('emphasize: error', {
    expect_that(pandoc.table(t, emphasize.cols = 1:5), throws_error())
    expect_that(pandoc.table(t, emphasize.cols = 1.5), throws_error())
    expect_that(pandoc.table(t, emphasize.strong.cols = 1:5), throws_error())
    expect_that(pandoc.table(t, emphasize.strong.cols = 1.5), throws_error())
})

test_that('no warning for highlight NA/empty strings', {
    expect_that(pandoc.table(data.frame(x = 1:2, y = c(1,NA)), emphasize.cols = 2), not(gives_warning()))
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
        any(grepl('Table:', pander.return(ttt)))
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

context("keep.line.breaks")
test_that('keep.line.breaks works correctly', {
  # keeping line breaks in a simple data.frame with one line breaks differs lines amount by one
  x <- data.frame(a="Pander\nPackage")
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE), "\n")[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), "\n")[[1]])
  expect_equal(lines.x.no.breaks + 1, lines.x.keep.breaks)

  # keeping line breaks in a simple data.frame with 2 rows with 1 line breaks differst lines amount by 2
  x <- data.frame(a=c("Pander\nPackage","Pander\nPackage"))
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE), "\n")[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), "\n")[[1]])
  expect_equal(lines.x.no.breaks + 2, lines.x.keep.breaks)

  #if there are no line breaks originally, they do not get introduced
  x <- data.frame(a=c("Pander Package","Pander Package"))
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE), "\n")[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), "\n")[[1]])
  expect_equal(lines.x.no.breaks, lines.x.keep.breaks)

  # works with random number of rows added
  rows <- sample(1:100, 1)
  x <- data.frame(a=rep("Pander\nPackage", rows))
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE), "\n")[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), "\n")[[1]])
  expect_equal(lines.x.no.breaks + rows, lines.x.keep.breaks)

  # random number of line breaks in one cell
  n <- sample(1:10, 1)
  x <- data.frame(a=paste(rep("pander", n), collapse="\n"))
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE, split.cells = Inf), "\n")[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), "\n")[[1]])
  expect_equal(lines.x.no.breaks + n - 1, lines.x.keep.breaks)

  # random number of line breaks in cells, 3 columns
  n <- sample(1:10, 3)
  x <- data.frame(a=paste(rep("pander", n[1]), collapse="\n"),
                  b = paste(rep("pander", n[2]), collapse="\n"),
                  c = paste(rep("pander", n[3]), collapse="\n"))
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE, split.cells = Inf, split.tables = Inf), "\n")[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), "\n")[[1]])
  expect_equal(lines.x.no.breaks + max(n) - 1, lines.x.keep.breaks)

  # 3 columns, 2 rows
  x <- rbind(x,x)
  lines.x.no.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = FALSE, split.cells = Inf, split.tables = Inf), "\n")[[1]])
  lines.x.keep.breaks <- length(strsplit(pandoc.table.return(x, keep.line.breaks = TRUE), "\n")[[1]])
  expect_equal(lines.x.no.breaks + 2 * max(n) - 2, lines.x.keep.breaks)
})

context("split.cells")
test_that('split.cells works correctly',{
  x <- data.frame(a = "foo bar\nfo bar")
  # single line break behaves correctly combines with keep line breaks
  expect_equal(pandoc.table.return(x, keep.line.breaks = T, split.cells = 7), "\n-------\n   a   \n-------\nfoo bar\nfo bar \n-------\n\n")
  expect_equal(pandoc.table.return(x, keep.line.breaks = T, split.cells = 6), "\n------\n  a   \n------\n foo  \n bar  \nfo bar\n------\n\n")
  # Corner values of split.cells
  x <- data.frame(a = "foo bar", b = "foo bar")
  expect_equal(length(strsplit(pandoc.table.return(x, split.cells = c(6, Inf)), "\n")[[1]]), 8)
  expect_equal(pandoc.table.return(x, split.cells = c(6, Inf)), "\n-----------\n a     b   \n--- -------\nfoo foo bar\nbar        \n-----------\n\n")
  expect_equal(pandoc.table.return(x, split.cells = c(Inf, 6)), "\n-----------\n   a     b \n------- ---\nfoo bar foo\n        bar\n-----------\n\n")
  expect_equal(pandoc.table.return(x, split.cells = c(7, 7)), "\n---------------\n   a       b   \n------- -------\nfoo bar foo bar\n---------------\n\n")
  expect_equal(pandoc.table.return(x, split.cells = c(7, 7)), pandoc.table.return(x, split.cells = Inf))
  expect_equal(pandoc.table.return(x, split.cells = c(6 ,6)), pandoc.table.return(x, split.cells = 6))
  expect_equal(pandoc.table.return(x, split.cells = c(7 ,7)), pandoc.table.return(x, split.cells = 7))
  expect_equal(pandoc.table.return(x, split.cells = c(7 ,7)), pandoc.table.return(x, split.cells = 7))
  # relative split.cells
  expect_equal(pandoc.table.return(x, split.cells = c("50%", "50%")), pandoc.table.return(x, split.cells = c(7)))
  expect_equal(pandoc.table.return(x, split.cells = c("20%", "80%"), split.tables = 30), pandoc.table.return(x, split.cells = c(6, Inf)))
  expect_equal(pandoc.table.return(x, split.cells = c("80%", "20%"), split.tables = 30), pandoc.table.return(x, split.cells = c(Inf, 6)))
})

test_that('split.cells param produces expected warnings',{
  mt <- mtcars[1:2, 1:4]
  expect_warning(pander(mt, split.cells = c(1,2)))
  expect_warning(pander(mt, split.cells = c(1,2,3)))
  expect_warning(pander(mt, split.cells = c("10%","10%","10%")))
  expect_warning(pander(mt, split.cells = c("30%","30%","40%")))
})

context("table.expand")

test_that('produces.errors',{
  x <- data.frame(a="Pander\nPackage")
  expect_error(pander(x, style='simple', keep.line.breaks = T))
  expect_error(pander(x, style='rmarkdown', keep.line.breaks = T))
})

table.expand <- function(cells, cols.width, justify, sep.cols, style) {
  .Call('pander_tableExpand_cpp', PACKAGE = 'pander', cells, cols.width, justify, sep.cols, style)
}

test_that('table.expand behaves correctly',{
  # Saved results of original table expand written in R and now as a safe check that new table.expand returns the same results for different styles and justify
  ## multiline style check
  argv <-  structure(list(txt = structure(c(1L, 4L, 2L, 3L), .Label = c("&nbsp;",  "cyl", "disp", "mpg"), class = "factor"), width = c(19, 5, 5,  6), justify = structure(c(1L, 1L, 1L, 1L), .Label = "centre", class = "factor")), .Names = c("txt",  "width", "justify"), row.names = c(NA, -4L), class = "data.frame")
  sep.cols <-  c("", " ", "")
  style <-  "multiline"
  res <- table.expand(argv[,1], argv[,2], argv[,3], sep.cols, style)
  expect_equal(max(sapply(strsplit(as.character(argv[,1]), "\n"), length)), length(strsplit(res, "\n")[[1]])) # max number of line breaks equals number of lines in the result
  expect_equal(nchar(res), nchar(sep.cols)[1] + (length(argv[,2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[,2]))
  expect_equal(res, "      &nbsp;         mpg   cyl   disp ");

  ## grid style check
  argv <-  structure(list(txt = structure(c(1L, 5L, 2L, 3L, 4L), .Label = c("&nbsp;",  "cyl", "disp", "hp", "mpg"), class = "factor"), width = c(20,  5, 5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = "centre", class = "factor")), .Names = c("txt",  "width", "justify"), row.names = c(NA, -5L), class = "data.frame")
  sep.cols <-  c("| ", " | ", " |")
  res <- table.expand(argv[,1], argv[,2], argv[,3], sep.cols, style)
  expect_equal(max(sapply(strsplit(as.character(argv[,1]), "\n"), length)), length(strsplit(res, "\n")[[1]])) # max number of line breaks equals number of lines in the result
  expect_equal(nchar(res), nchar(sep.cols)[1] + (length(argv[,2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[,2]))
  expect_equal(res, "|        &nbsp;        |  mpg  |  cyl  |  disp  |  hp  |" )

  ## rmarkdown style check
  argv <-  structure(list(txt = structure(c(1L, 4L, 5L, 3L, 2L), .Label = c("**Mazda RX4**",  "110", "160", "21", "6"), class = "factor"), width = c(20, 5,  5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = "centre", class = "factor")), .Names = c("txt",  "width", "justify"), row.names = c("t.rownames", "mpg", "cyl",  "disp", "hp"), class = "data.frame")
  sep.cols <-  c("| ", " | ", " |")
  style <-  "rmarkdown"
  res <- table.expand(argv[,1], argv[,2], argv[,3], sep.cols, style)
  expect_equal(max(sapply(strsplit(as.character(argv[,1]), "\n"), length)), length(strsplit(res, "\n")[[1]])) # max number of line breaks equals number of lines in the result
  expect_equal(nchar(res), nchar(sep.cols)[1] + (length(argv[,2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[,2]))
  expect_equal(res, "|    **Mazda RX4**     |  21   |   6   |  160   | 110  |")

  ## simple style check
  argv <-  structure(list(txt = structure(c(1L, 3L, 4L, 2L, 5L), .Label = c("**Datsun 710**",  "108", "22.8", "4", "93"), class = "factor"), width = c(20, 5,  5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = "centre", class = "factor")), .Names = c("txt",  "width", "justify"), row.names = c("t.rownames", "mpg", "cyl",  "disp", "hp"), class = "data.frame")
  sep.cols <-  c("", " ", "")
  style <-  "simple"
  res <- table.expand(argv[,1], argv[,2], argv[,3], sep.cols, style)
  expect_equal(max(sapply(strsplit(as.character(argv[,1]), "\n"), length)), length(strsplit(res, "\n")[[1]])) # max number of line breaks equals number of lines in the result
  expect_equal(nchar(res), nchar(sep.cols)[1] + (length(argv[,2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[,2]))
  expect_equal(res, "   **Datsun 710**    22.8    4    108    93 ")

  ## left justification
  argv <-  structure(list(txt = structure(c(1L, 4L, 5L, 3L, 2L), .Label = c("**Mazda",  "110", "160", "21", "6"), class = "factor"), width = c(10, 5,  5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = "left", class = "factor")), .Names = c("txt",  "width", "justify"), row.names = c(NA, -5L), class = "data.frame")
  sep.cols <-  c("", " ", "")
  style <-  "multiline"
  res <- table.expand(argv[,1], argv[,2], argv[,3], sep.cols, style)
  expect_equal(max(sapply(strsplit(as.character(argv[,1]), "\n"), length)), length(strsplit(res, "\n")[[1]])) # max number of line breaks equals number of lines in the result
  expect_equal(nchar(res), nchar(sep.cols)[1] + (length(argv[,2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[,2]))
  expect_equal(res, "**Mazda    21    6     160    110 ");

  ## right justification
  argv <-  structure(list(txt = structure(c(1L, 3L, 5L, 4L, 2L), .Label = c("**Hornet 4 Drive**",  "110", "21.4", "258", "6"), class = "factor"), width = c(20,  5, 5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = "right", class = "factor")), .Names = c("txt",  "width", "justify"), row.names = c("t.rownames", "mpg", "cyl",  "disp", "hp"), class = "data.frame")
  sep.cols <-  c("", " ", "")
  style <-  "simple"
  res <- table.expand(argv[,1], argv[,2], argv[,3], sep.cols, style)
  expect_equal(max(sapply(strsplit(as.character(argv[,1]), "\n"), length)), length(strsplit(res, "\n")[[1]])) # max number of line breaks equals number of lines in the result
  expect_equal(nchar(res), nchar(sep.cols)[1] + (length(argv[,2]) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(argv[,2]))
  expect_equal(res, "  **Hornet 4 Drive**  21.4     6    258  110" );

  ## multiple lines
  argv <-  structure(list(txt = structure(c(1L, 3L, 5L, 4L, 2L), .Label = c("**Hornet\n4\nDrive**",  "110", "21.4", "258", "6"), class = "factor"), width = c(10,  5, 5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = "left", class = "factor")), .Names = c("txt",  "width", "justify"), row.names = c("t.rownames", "mpg", "cyl",  "disp", "hp"), class = "data.frame")
  sep.cols <-  c("", " ", "")
  style <-  "multiline"
  res <- table.expand(argv[,1], argv[,2], argv[,3], sep.cols, style)
  expect_equal(max(sapply(strsplit(as.character(argv[,1]), "\n"), length)), length(strsplit(res, "\n")[[1]])) # max number of line breaks equals number of lines in the result
  expect_equal(res, "**Hornet   21.4  6     258    110 \n4                                 \nDrive**                           ");

  argv <-  structure(list(txt = structure(c(1L, 3L, 4L, 2L, 5L), .Label = c("**Datsun\n710**",  "108", "22.8", "4", "93"), class = "factor"), width = c(10, 5,  5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = "left", class = "factor")), .Names = c("txt",  "width", "justify"), row.names = c("t.rownames", "mpg", "cyl",  "disp", "hp"), class = "data.frame")
  sep.cols <-  c("", " ", "")
  style <-  "multiline"
  res <- table.expand(argv[,1], argv[,2], argv[,3], sep.cols, style)
  expect_equal(max(sapply(strsplit(as.character(argv[,1]), "\n"), length)), length(strsplit(res, "\n")[[1]])) # max number of line breaks equals number of lines in the result
  expect_equal(res, "**Datsun   22.8  4     108    93  \n710**                             ");

  argv <-  structure(list(txt = structure(c(1L, 4L, 5L, 3L, 2L), .Label = c("**Mazda\nRX4\nWag**",  "110", "160", "21", "6"), class = "factor"), width = c(10, 5,  5, 6, 4), justify = structure(c(1L, 1L, 1L, 1L, 1L), .Label = "left", class = "factor")), .Names = c("txt",  "width", "justify"), row.names = c("t.rownames", "mpg", "cyl",  "disp", "hp"), class = "data.frame")
  sep.cols <-  c("", " ", "")
  style <-  "multiline"
  res <- table.expand(argv[,1], argv[,2], argv[,3], sep.cols, style)
  expect_equal(max(sapply(strsplit(as.character(argv[,1]), "\n"), length)), length(strsplit(res, "\n")[[1]])) # max number of line breaks equals number of lines in the result
  expect_equal(res, "**Mazda    21    6     160    110 \nRX4                               \nWag**                             ");

  # empty cells
  cells <- c("","","")
  cols.width <- c(2, 2, 2)
  justify <- c("centre", "centre", "centre")
  sep.cols <- c("", " ", "")
  res <- table.expand(cells, cols.width, justify, sep.cols, "multiline")
  expect_equal(max(sapply(strsplit(as.character(cells), "\n"), length)), 0) # max number of line breaks equals number of lines in the result
  expect_equal(nchar(res), nchar(sep.cols)[1] + (length(cells) - 1) * nchar(sep.cols)[2] + nchar(sep.cols)[3] + sum(cols.width))
  expect_equal(res, "        ")

  # backslashes issue (#22)
  expect_equal(pandoc.table.return(data.frame(a="\\1 \\ 32",b="23")),"\n-----------\n   a     b \n------- ---\n\\1 \\ 32 23 \n-----------\n\n")
  expect_equal(pandoc.table.return(data.frame(a="\\1 \\ 32",b="23"), justify = 'right'), "\n-----------\n      a   b\n------- ---\n\\1 \\ 32  23\n-----------\n\n")
  expect_equal(pandoc.table.return(data.frame(a="\\1",b="23")), "\n-------\n a   b \n--- ---\n\\1  23 \n-------\n\n")

  # unicode string issue
  expect_equal(pandoc.table.return(data.frame(a = 'ßß')), "\n---\n a \n---\nßß \n---\n\n")
})

context("S3 methods")

test_that('pander.tabular behaves correctly', {
    suppressMessages(require(tables))
    tab <- pander.return(tables::tabular(as.factor(am) ~ (mpg+hp+qsec) * (mean+median), data = mtcars), emphasize.rownames = FALSE, split.tables = Inf)
    expect_equal(length(tab), 10)
    tab <- pander.return(tables::tabular( (Species + 1) ~ (n=1) + Format(digits=2)* (Sepal.Length + Sepal.Width)*(mean + sd), data=iris ), split.tables = Inf)
    expect_equal(length(tab), 14)
})

test_that('pander.CrossTable behaves correctly', {
    suppressMessages(require(descr))
    # issue https://github.com/Rapporter/pander/issues/163
    x <- CrossTable(mtcars$cyl, mtcars$gear, prop.c = FALSE, prop.t = FALSE, chisq = FALSE, prop.chisq = FALSE)
    res <- pander.return(x)
    expect_true(any(grepl(gsub("\\$", "\\\\$", x$ColData), res)))
    expect_true(any(grepl(gsub("\\$", "\\\\$", x$RowData), res)))
})
