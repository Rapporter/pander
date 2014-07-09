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

context('split.line')

test_that('split.line leaves non-string arguments unchanged',{
  expect_equal(1, split.line(1))
  expect_equal(1:10, split.line(1:10))
  expect_equal(c("string 1", "string 2", "string 3"), split.line(c("string 1", "string 2", "string 3")))
  expect_equal(mtcars, split.line(mtcars))
  expect_equal(summary(lm(mtcars$hp~1)), split.line(summary(lm(mtcars$hp~1))))
})

test_that('split.line without hyphening behaves correctly',{
  x <- "Really do clean up and test your code and think twice before you even start contemplating optimizing the code"
  width.spaced <- function(x, max.width, type="max"){
    if (type=="max")
      max(sapply(strsplit(split.line(x, max.width), "\n"), function(x) nchar(x) * grepl(" ", x)))
    else if (type=="min")
      min(sapply(strsplit(split.line(x, max.width), "\n"), nchar))
  }
  expect_true(width.spaced(x, 5, "max") <= 5) # max width of line is less than 5 (in case it has spaces)
  expect_true(width.spaced(x, 10, "max") <= 10) # max width of line is less than 10 (in case it has spaces)
  expect_true(width.spaced(x, 10, "min") >= 1) # every line has at least one character
  expect_equal("character", split.line("character", 2)) # string without spaces remains unchanged
  expect_equal("", split.line("", 2)) # for empty string, returns empty string
  expect_equal(length(strsplit(split.line(x, 2), "\n")[[1]]), 19) # when max.width param is small, every word is a line
})

test_that('split.line with hyphening behaves correctly',{
  x <- "Really do clean up and test your code and think twice before you even start contemplating optimizing the code"
  width.spaced <- function(x, max.width, type="max"){
    if (type=="max")
      max(sapply(strsplit(split.line(x, max.width, TRUE), "\n"), function(x) nchar(x) * grepl(" ", x)))
    else if (type=="min")
      min(sapply(strsplit(split.line(x, max.width, TRUE), "\n"), nchar))
  }
  expect_true(width.spaced(x, 5, "max") <= 5) # max width of line is less than 5 (in case it has spaces)
  expect_true(width.spaced(x, 10, "max") <= 10) # max width of line is less than 10 (in case it has spaces)
  expect_true(width.spaced(x, 10, "min") >= 1) # every line has at least one character
  expect_equal("char-\nac-\nter\n", split.line("character", 2, TRUE)) # check correctness for predifined word
  expect_equal("", split.line("", 2, TRUE)) # for empty string, returns empty string
  expect_equal(length(strsplit(split.line(x, 10, TRUE), "\n")[[1]]), 13) # when max.width param is small, every word is a line
  expect_false(grepl("- ", split.line(x, 10, TRUE))) # no space after hyphen
  expect_false(grepl(" -", split.line(x, 10, TRUE))) # no space before hyphen
  expect_false(grepl("\n ", split.line(x, 20, TRUE))) # line does not start with a space
})