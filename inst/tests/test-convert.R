library(testthat)
library(pander)
context('Pandoc.convert')

test_that('openFileInOS works correctly', {
    tf <- paste(tempfile(), '.txt', sep='')
    sink(file = tf)
    cat('Some info')
    sink()
#    expect_equal(openFileInOS(tf), 0)
    expect_error(openFileInOS('NEWS2'))
})

test_that('Pandoc.convert works correctly', {
    tf <- tempfile()
    sink(tf)
    pander(mtcars[1:3, 1:4])
    sink()
    res1 <- Pandoc.convert(tf, open = FALSE)
    expect_equal(dirname(tf), dirname(res1))
    res2 <- Pandoc.convert(text = pandoc.table.return(mtcars[1:3, 1:4]), open = FALSE)
    expect_equal(readLines(res1), readLines(res2)) # text or file gives same result
})
