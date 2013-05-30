context('pandoc.table')

tables <- list(
    mtcars,
    mtcars$am,
    mtcars[1:2, ],
    mtcars[1:2, 5],
    summary(mtcars$am),
    table(mtcars$am) + 0.1,
    table(mtcars$am, mtcars$gear) + 0.1,
    summary(lm(mtcars$hp~1))$coeff
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
    density(mtcars$hp)
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
    summary(lm(mtcars$hp~1))$coeff
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
        if (!is.table(df) || length(dim(df)) < 2)
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
