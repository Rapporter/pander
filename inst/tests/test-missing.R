library(testthat)
library(pander)

context('replacing missing values')

pom <- panderOptions('missing')

panderOptions('missing', '<missing>')
test_that('replacing missing values works with string', {
    expect_equal(pander_return(c(1, NA)), '_1_ and _<missing>_')
    expect_equal(paste(pander_return(list(1, NA)), collapse = '\n'),
                 '\n\n  * _1_\n  * <missing>\n\n<!-- end of list -->\n\n')
    expect_equal(paste(pander_return(
        data.frame(a = 1:2, b = c(1, NA), c = c(NA, 'z'))),
        collapse = '\n'),
                 '\n---------------------------\n a       b           c     \n--- ----------- -----------\n 1       1       <missing> \n\n 2   <missing>       z     \n---------------------------\n') #nolint
})

panderOptions('missing', '')
test_that('suppressing missing values works', {
    expect_equal(pander_return(c(1, NA)), '_1_ and __')
    expect_equal(paste(pander_return(list(1, NA)), collapse = '\n'),
                 '\n\n  * _1_\n  *\n\n<!-- end of list -->\n\n')
    expect_equal(paste(pander_return(data.frame(a = 1:2, b = c(1, NA), c = c(NA, 'z'))), collapse = '\n'),
                 '\n-----------\n a   b   c \n--- --- ---\n 1   1     \n\n 2       z \n-----------\n')
})

panderOptions('missing', pom)
