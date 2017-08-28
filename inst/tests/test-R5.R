library(testthat)
library(pander)
context('Pandoc R5 reference class')

graph.dir <- evalsOptions('graph.dir')
evalsOptions('graph.dir',  file.path(tempdir(), 'plots'))

test_that('quick demo on R5 refclass', {
    myReport <- Pandoc$new()
    myReport$add.paragraph('Hello there!')
    myReport$add(head(mtcars))
    myReport$add(chisq.test(table(mtcars$am, mtcars$gear)))
    expect_true(class(myReport) == 'Pandoc')
    expect_equal(length(capture.output(myReport)), 63)
    t <- tempfile()
    myReport$format <- 'html'
    expect_equal(myReport$export(t, open = FALSE), paste0(t, '.html'))
    unlink(t)
})

evalsOptions('graph.dir',  graph.dir)
