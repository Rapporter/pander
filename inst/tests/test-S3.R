context('pandoc.table')

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

test_that('no error', {
    for (t in tables)
        expect_that(pander.return(t), is_a('character'))
})

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
    expect_that(pandoc.table.return(t$mpg, highlight.cells = 1), is_a('character'))
    expect_that(pandoc.table.return(t$mpg, highlight.cells = 1:2), is_a('character'))
})

t <- table(mtcars$am, mtcars$gear)
test_that('highlight 2D: no error', {
    expect_that(pandoc.table.return(t, highlight.rows = 1), is_a('character'))
    expect_that(pandoc.table.return(t, highlight.rows = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t, highlight.cols = 1), is_a('character'))
    expect_that(pandoc.table.return(t, highlight.cols = 1:2), is_a('character'))
    expect_that(pandoc.table.return(t, highlight.cells = which(t > 10, arr.ind = TRUE)), is_a('character'))
    expect_that(pandoc.table.return(t, highlight.cells = which(t > 20, arr.ind = TRUE)), is_a('character'))
})

test_that('highlight: error', {
    expect_that(pandoc.table(t, highlight.cols = 1:5), throws_error())
    expect_that(pandoc.table(t, highlight.cols = 1.5), throws_error())
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
test_that('evals', {
    for (ttt in tables)
        expect_that(has.caption(ttt, evals = TRUE), is_true())
})
