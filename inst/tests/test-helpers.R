library(testthat)
library(pander)
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

context('splitLine')

test_that('splitLine leaves non-string arguments and empty string unchanged', {
  expect_equal(1, splitLine(1))
  expect_equal(1:10, splitLine(1:10))
  expect_equal(c('string 1', 'string 2', 'string 3'), splitLine(c('string 1', 'string 2', 'string 3')))
  expect_equal(mtcars, splitLine(mtcars))
  expect_equal(summary(lm(mtcars$hp~1)), splitLine(summary(lm(mtcars$hp~1))))
  expect_equal(splitLine('', 10), '')
  expect_equal(splitLine('', 0), '')
  expect_equal(splitLine('', Inf), '')
})

test_that('splitLine without hyphening behaves correctly', {
  widthSpaced <- function(x, max.width, type='max'){
    if (type == 'max')
      max(sapply(strsplit(splitLine(x, max.width), '\n'), function(x) nchar(x) * grepl(' ', x)))
    else if (type == 'min')
      min(sapply(strsplit(splitLine(x, max.width), '\n'), nchar))
  }
  x <- 'foo bar'
  expect_equal(splitLine(x, 7), 'foo bar')
  expect_equal(splitLine(x, 6), 'foo\nbar')
  x <- 'Each character string in the input is first split into paragraphs (or lines containing whitespace only)'
  expect_equal(splitLine(x, 10),
               'Each\ncharacter\nstring in\nthe input\nis first\nsplit into\nparagraphs\n(or lines\ncontaining\nwhitespace\nonly)') #nolint
  expect_equal(splitLine(x, 20),
               'Each character\nstring in the input\nis first split into\nparagraphs (or lines\ncontaining\nwhitespace only)') #nolint
  expect_equal(splitLine(x, 0), paste(strwrap(x, 0), collapse = '\n')) # zero width replaces whitespaces by line breaks
  expect_equal(splitLine(x, Inf), x) # Infinite does not change
  x <- 'Really do clean up and test your code and think twice before you even start contemplating optimizing the code'
  expect_true(widthSpaced(x, 5, 'max') <= 5) # max width of line is less than 5 (in case it has spaces)
  expect_true(widthSpaced(x, 10, 'max') <= 10) # max width of line is less than 10 (in case it has spaces)
  expect_true(widthSpaced(x, 10, 'min') >= 1) # every line has at least one character
  expect_equal('character', splitLine('character', 2)) # string without spaces remains unchanged
  expect_equal('', splitLine('', 2)) # for empty string, returns empty string
  expect_equal(length(strsplit(splitLine(x, 2), '\n')[[1]]), 19) # when max.width param is small, every word is a line
})

test_that('splitLine with hyphening behaves correctly', {
  x <- 'Really do clean up and test your code and think twice before you even start contemplating optimizing the code'
  widthSpaced <- function(x, max.width, type='max'){
    if (type == 'max')
      max(sapply(strsplit(splitLine(x, max.width, TRUE), '\n'), function(x) nchar(x) * grepl(' ', x)))
    else if (type == 'min')
      min(sapply(strsplit(splitLine(x, max.width, TRUE), '\n'), nchar))
  }
  expect_true(widthSpaced(x, 5, 'max') <= 5) # max width of line is less than 5 (in case it has spaces)
  expect_true(widthSpaced(x, 10, 'max') <= 10) # max width of line is less than 10 (in case it has spaces)
  expect_true(widthSpaced(x, 10, 'min') >= 1) # every line has at least one character
  # check correctness for predifined words/phrases
  expect_equal('char-\nac-\nter', splitLine('character', 2, TRUE))
  expect_equal(splitLine('start testing', 5, T), 'start\ntest-\ning')
  expect_equal(splitLine('Pander Package', 5, T), 'Pan-\nder\nPack-\nage')
  expect_equal(splitLine('Pander Package', 6, T), 'Pander\nPack-\nage')
  expect_equal(splitLine('Pander Package', 7, T), 'Pander\nPackage')
  # predifined example
  # when max.width param is small, every word is a line
  expect_equal(length(strsplit(splitLine(x, 10, TRUE), '\n')[[1]]), 12)
  expect_false(grepl('- ', splitLine(x, 10, TRUE))) # no space after hyphen
  expect_false(grepl(' -', splitLine(x, 10, TRUE))) # no space before hyphen
  expect_false(grepl('\n ', splitLine(x, 20, TRUE))) # line does not start with a space
  # infinite width
  expect_equal(splitLine(x, Inf, TRUE), x)
})

test_that('options', {
    expect_error(panderOptions('foobar'))
    expect_error(evalsOptions('foobar'))
})

context('p function')
test_that('p', {
    expect_equal(p(character(0)), '')
    expect_equal(p(1), '_1_')
    expect_equal(p(1:3), '_1_, _2_ and _3_')
    expect_equal(p(1, wrap = ''), '1')
    expect_equal(p(1:3, wrap = '', sep = '|', copula = '|'), '1|2|3')
    expect_equal(p(seq(1, 1.2, 0.1)), '_1_, _1.1_ and _1.2_')
    expect_equal(p(seq(1, 1.2, 0.1), keep.trailing.zeros = TRUE),
                 '_1.0_, _1.1_ and _1.2_')
    expect_error(p(1:10, limit = 3))
})
