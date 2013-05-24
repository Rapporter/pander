context('has.rownames')

test_that('has.rownames: TRUE', {
    expect_true(has.rownames(mtcars))
    expect_true(has.rownames(mtcars[1:2, ]))
    expect_true(has.rownames(mtcars[1, ]))
    expect_true(has.rownames(table(mtcars$am, mtcars$gear)))
    expect_true(has.rownames(table(mtcars$am, mtcars$gear) + 0.1))
    expect_true(has.rownames(summary(lm(mtcars$hp~1))$coeff))
})

test_that('has.rownames: FALSE', {
    expect_false(has.rownames(mtcars$am))
    expect_false(has.rownames(mtcars[1:2, 5]))
    expect_false(has.rownames(summary(mtcars$am)))
    expect_false(has.rownames(table(mtcars$am)))
    expect_false(has.rownames(table(mtcars$am) + 0.1))
})

