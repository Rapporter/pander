## test_file('test-evals.R')

cache.dir <- evalsOptions('cache.dir')
graph.dir <- evalsOptions('graph.dir')
wd <- getwd()
setwd(tempdir())
evalsOptions('cache.dir',  file.path(tempdir(), '.cache'))
evalsOptions('graph.dir',  file.path(tempdir(), 'plots'))

context('eval.msgs')

test_that('returns', {
    expect_that(eval.msgs('1:5')$result, equals(1:5))
    expect_that(eval.msgs('mtcars')$result, equals(mtcars))
    expect_that(eval.msgs('x <- mtcars')$result, equals(NULL))
    expect_that(evals('plot(mtcars)')$result, equals(NULL))
    expect_that(eval.msgs('cat(1:5);1:5')$result, equals(1:5))
})

test_that('messages', {
    expect_that(eval.msgs('1:5')$msg$messages, equals(NULL))
    expect_that(eval.msgs('mtcars')$msg$messages, equals(NULL))
    expect_that(eval.msgs('x <- mtcars')$msg$messages, equals(NULL))
    expect_that(evals('plot(mtcars)')$msg$messages, equals(NULL))
    expect_that(eval.msgs('warning("d");warning("f");1')$msg$messages, equals(NULL))
    expect_that(eval.msgs('message("FOO")')$msg$messages, equals('FOO'))
    expect_that(eval.msgs(c('message("FOO")', '1:2'))$msg$messages, equals('FOO'))
    expect_that(eval.msgs(c('message("FOO");message("FOO");warning("FOO")', '1:2'))$msg$messages, equals(c('FOO', 'FOO')))
})

test_that('warnings', {
    expect_that(eval.msgs('1:5')$msg$warnings, equals(NULL))
    expect_that(eval.msgs('mtcars')$msg$warnings, equals(NULL))
    expect_that(eval.msgs('x <- mtcars')$msg$warnings, equals(NULL))
    expect_that(evals('plot(mtcars)')$msg$warnings, equals(NULL))
    expect_that(eval.msgs('warning("d");warning("f");1')$msg$warnings, equals(c('d', 'f')))
    expect_that(eval.msgs('message("FOO")')$msg$warnings, equals(NULL))
    expect_that(eval.msgs(c('message("FOO")', '1:2'))$msg$warnings, equals(NULL))
    expect_that(eval.msgs(c('warning("FOO")', '1:2'))$msg$warnings, equals('FOO'))
    expect_that(eval.msgs(c('warning("FOO");message("FOO");warning("FOO")', '1:2'))$msg$warnings, equals(c('FOO', 'FOO')))
})

test_that('errors', {
    expect_that(eval.msgs('1:5')$msg$errors, equals(NULL))
    expect_that(eval.msgs('mtcars')$msg$errors, equals(NULL))
    expect_that(eval.msgs('x <- mtcars')$msg$errors, equals(NULL))
    expect_that(evals('plot(mtcars)')$msg$errors, equals(NULL))
    expect_that(eval.msgs('warning("d");warning("f");1')$msg$errors, equals(NULL))
    expect_that(eval.msgs('message("FOO")')$msg$errors, equals(NULL))
    expect_that(eval.msgs(c('message("FOO")', '1:2'))$msg$errors, equals(NULL))
    expect_that(eval.msgs(c('warning("FOO")', '1:2'))$msg$errors, equals(NULL))
    expect_that(eval.msgs(c('warning("FOO");message("FOO");warning("FOO")', '1:2'))$msg$errors, equals(NULL))
    expect_that(eval.msgs('runiff(2)')$msg$errors, is_a('character'))
    expect_that(eval.msgs('runif would be nice to run')$msg$errors, is_a('character'))
    expect_that(eval.msgs('no.R.object.like.that')$msg$errors, is_a('character'))
})

test_that('output', {
    expect_that(eval.msgs('1:5')$output, is_a('character'))
    expect_that(eval.msgs('mtcars')$output, is_a('character'))
    expect_that(eval.msgs('x <- mtcars')$output, equals(NULL))
    expect_that(evals('plot(mtcars)')$output, equals(NULL))
    expect_that(eval.msgs('cat(1:5)')$output, equals(NULL))
    expect_that(eval.msgs('cat(1:5);1:5')$output, is_a('character'))
})

test_that('stdout', {
    expect_that(eval.msgs('1:5')$stdout, equals(NULL))
    expect_that(eval.msgs('mtcars')$stdout, equals(NULL))
    expect_that(eval.msgs('x <- mtcars')$stdout, equals(NULL))
    expect_that(evals('plot(mtcars)')$stdout, equals(NULL))
    expect_that(eval.msgs('cat(1:5)')$stdout, equals("1 2 3 4 5"))
    expect_that(eval.msgs('cat(1:5);1:5')$stdout, equals("1 2 3 4 5"))
})


context('evals')

test_that('Variable assignement', {
    expect_that(evals('x <- rnorm(100)')[[1]]$result, equals(NULL))
})

test_that('Multiple variable assignement', {
            expect_that(evals(list(c('x <- rnorm(100)', 'y <- rnorm(10)')), parse = F)[[1]]$result, equals(NULL))
        })

## test_that('Comment', {
##             expect_that(evals('## comment')[[1]]$result, equals(NULL))
##         })

test_that('Comment & variable assignement', {
            expect_that(evals(list(c('## comment', 'y <- rnorm(10)')), parse = F)[[1]]$result, equals(NULL))
        })

test_that('Basic: numerics', {
            expect_that(evals('rnorm(100)')[[1]]$result, is_a('numeric'))
        })

test_that('Basic: character', {
            expect_that(evals('"Hello world!"')[[1]]$result, equals("Hello world!"))
        })

test_that('Basic: list', {
            expect_that(evals('lapply(rnorm(10), round)')[[1]]$result, is_a('list'))
        })

test_that('Basic: data.frame', {
            expect_that(evals('mtcars')[[1]]$result, is_a('data.frame'))
        })

test_that('Plot: graphics package', {
            expect_that(evals('plot(1:10)')[[1]]$result, is_a('image'))
        })

test_that('Plot: lattice package', {
            expect_that(evals('require(lattice);+histogram(mtcars$hp)')[[1]]$result, is_a('image'))
            expect_that(evals('require(lattice);histogram(mtcars$hp)', parse = F)[[1]]$result, is_a('image'))
        })

test_that('Plot: multiline code', {
            expect_that(evals(list(c('plot(cars, main="Stopping Distance versus Speed")', 'plot(cars, main="Stopping Distance versus Speed")')), parse = F)[[1]]$result, is_a('image'))
            expect_that(evals('plot(cars, main="Stopping Distance versus Speed"); plot(cars, main="Stopping Distance versus Speed")')[[2]]$result, is_a('image'))
        })

test_that('Plot: pdf', {
            expect_that(evals('plot(1:10)', graph.output = 'pdf')[[1]]$result, is_a('image'))
        })

test_that('Plot: pdf without plotting', {
            expect_that(evals('runif(10)', graph.output = 'pdf')[[1]]$result, is_a('numeric'))
        })

test_that('NULL', {
            expect_that(evals('rnorm(100); NULL')[[1]]$result, is_a('numeric'))
            expect_that(evals('rnorm(100); NULL')[[2]]$result, equals(NULL))
            expect_that(evals('rnorm(100); NULL', parse = FALSE)[[1]]$result, equals(NULL))
        })

test_that('## comment', {
            expect_that(evals('rnorm(100);## comment')[[1]]$result, is_a('numeric'))
            expect_that(evals('rnorm(100);## comment', parse = FALSE)[[1]]$result, is_a('numeric'))
        })

test_that('var assignement', {
            expect_that(evals('rnorm(100);x <- 1')[[1]]$result, is_a('numeric'))
            expect_that(evals('rnorm(100);x <- 1', parse = F)[[1]]$result, equals(NULL))
        })

test_that('creating function', {
            expect_that(evals('rnorm(100);f <- function(x) mean(x)')[[1]]$result, is_a('numeric'))
            expect_that(evals('rnorm(100);f <- function(x) mean(x)')[[2]]$result, equals(NULL))
            expect_that(evals('rnorm(100);f <- function(x) mean(x)', parse = F)[[1]]$result, equals(NULL))
        })


test_that('string+num', {
            expect_that(evals('"X";rnorm(100)')[[2]]$result, is_a('numeric'))
            expect_that(evals('"X";rnorm(100)')[[1]]$result, is_a('character'))
            expect_that(evals('"X";rnorm(100)', parse = FALSE)[[1]]$result, is_a('numeric'))
        })

test_that('data.frame+num', {
            expect_that(evals(list(c('mtcars', 'rnorm(100)')), parse = FALSE)[[1]]$result, is_a('numeric'))
        })

test_that('simple error', {
            expect_that(evals('dadaffaSFA+A')[[1]]$msg$error, is_a('character'))
        })

test_that('simple error', {
            expect_that(evals('runiff(10)')[[1]]$msg$error, is_a('character'))
        })

test_that('multiple errors', {
            expect_that(evals(list(c('runiff(20)', 'Old MacDonald had a farm?')), parse = F)[[1]]$msg$error, is_a('character'))
        })

test_that('multiple errors', {
            expect_that(evals('no.R.object;no.R.function();very.mixed.up(stuff)')[[1]]$msg$error, is_a('character'))
            expect_that(evals('no.R.object;no.R.function();very.mixed.up(stuff)')[[2]]$msg$error, is_a('character'))
            expect_that(evals('no.R.object;no.R.function();very.mixed.up(stuff)')[[3]]$msg$error, is_a('character'))
            expect_that(evals('no.R.object;no.R.function();very.mixed.up(stuff)', parse = F)[[1]]$msg$error, is_a('character'))
        })

test_that('output+errors', {
            expect_that(evals(list(c('mean(1:10)', 'no.R.function()')), parse = F)[[1]]$msg$error, is_a('character'))
        })

test_that('output+multiple errors', {
            expect_that(evals(list(c('no.R.object', 'Old MacDonald had a farm\\dots', 'pi')), parse = F)[[1]]$msg$error, is_a('character'))
        })

test_that('simple warning', {
            expect_that(evals('chisq.test(mtcars$gear, mtcars$hp)')[[1]]$msg$warning, is_a('character'))
        })

test_that('multiple warnings+output', {
            expect_that(evals(list(c('chisq.test(mtcars$gear, mtcars$am)', 'pi', 'chisq.test(mtcars$gear, mtcars$hp)')), parse = F)[[1]]$msg$warnings, is_a('character'))
            expect_that(evals('chisq.test(mtcars$gear, mtcars$am);pi;chisq.test(mtcars$gear, mtcars$hp)')[[1]]$msg$warnings, is_a('character'))
        })

test_that('long lines with line breaks', {
    expect_that(evals('if (TRUE) {\nset.caption("FOO")\nplot(c(1,2,3,4,5,6,7,8,9,10,11,12))\n}')[[1]]$result, is_a('image'))
    expect_that(length(evals('x <- "this is a very-very looooooooooong line which might be split on `parse`, but that is not a big deal at all"\nx\n1')), equals(3))
})

evalsOptions('cache.dir',  cache.dir)
evalsOptions('graph.dir',  graph.dir)
setwd(wd)
