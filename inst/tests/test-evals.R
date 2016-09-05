library(testthat)
library(pander)

cache.dir <- evalsOptions('cache.dir')
graph.dir <- evalsOptions('graph.dir')
wd <- getwd()
setwd(tempdir())
evalsOptions('cache.dir',  file.path(tempdir(), '.cache'))
evalsOptions('graph.dir',  file.path(tempdir(), 'plots'))

context('eval.msgs')

test_that('returns', {
    expect_equal(eval.msgs('1:5')$result, 1:5)
    expect_equal(eval.msgs('mtcars')$result, mtcars)
    expect_null(eval.msgs('x <- mtcars')$result)
    expect_null(evals('plot(mtcars)')$result)
    expect_equal(eval.msgs('cat(1:5);1:5')$result, 1:5)
    expect_null(eval.msgs('#comment')$result)
    expect_null(eval.msgs('')$result)
})

test_that('messages', {
    expect_null(eval.msgs('1:5')$msg$messages)
    expect_null(eval.msgs('mtcars')$msg$messages)
    expect_null(eval.msgs('x <- mtcars')$msg$messages)
    expect_null(evals('plot(mtcars)')$msg$messages)
    expect_null(eval.msgs('warning("d");warning("f");1')$msg$messages)
    expect_equal(eval.msgs('message("FOO")')$msg$messages, 'FOO')
    expect_equal(eval.msgs(c('message("FOO")', '1:2'))$msg$messages, 'FOO')
    expect_equal(eval.msgs(c('message("FOO");message("FOO");warning("FOO")', '1:2'))$msg$messages, c('FOO', 'FOO'))
})

test_that('warnings', {
    expect_null(eval.msgs('1:5')$msg$warnings)
    expect_null(eval.msgs('mtcars')$msg$warnings)
    expect_null(eval.msgs('x <- mtcars')$msg$warnings)
    expect_null(evals('plot(mtcars)')$msg$warnings)
    expect_null(eval.msgs('message("FOO")')$msg$warnings)
    expect_null(eval.msgs(c('message("FOO")', '1:2'))$msg$warnings)
    expect_equal(eval.msgs('warning("d");warning("f");1')$msg$warnings, c('d', 'f'))
    expect_equal(eval.msgs(c('warning("FOO")', '1:2'))$msg$warnings, 'FOO')
    expect_equal(eval.msgs(c('warning("FOO");message("FOO");warning("FOO")', '1:2'))$msg$warnings, c('FOO', 'FOO'))
})

test_that('errors', {
    expect_null(eval.msgs('1:5')$msg$errors)
    expect_null(eval.msgs('mtcars')$msg$errors)
    expect_null(eval.msgs('x <- mtcars')$msg$errors)
    expect_null(evals('plot(mtcars)')$msg$errors)
    expect_null(eval.msgs('warning("d");warning("f");1')$msg$errors)
    expect_null(eval.msgs('message("FOO")')$msg$errors)
    expect_null(eval.msgs(c('message("FOO")', '1:2'))$msg$errors)
    expect_null(eval.msgs(c('warning("FOO")', '1:2'))$msg$errors)
    expect_null(eval.msgs(c('warning("FOO");message("FOO");warning("FOO")', '1:2'))$msg$errors)
    expect_is(eval.msgs('runiff(2)')$msg$errors, 'character')
    expect_is(eval.msgs('runif would be nice to run')$msg$errors, 'character')
    expect_is(eval.msgs('no.R.object.like.that')$msg$errors, 'character')
    expect_error(evals())
    expect_error(evals('1:5', hooks = 1:10))
    expect_error(evals('plot(mtcars)', graph.name = 1))
    env <- new.env()
    env$plot <- 1
    expect_error(evals(1:10, env = env))
})

test_that('output', {
    expect_is(eval.msgs('1:5')$output, 'character')
    expect_is(eval.msgs('mtcars')$output, 'character')
    expect_is(eval.msgs('cat(1:5);1:5')$output, 'character')
    expect_null(eval.msgs('x <- mtcars')$output)
    expect_null(evals('plot(mtcars)')$output)
    expect_null(eval.msgs('cat(1:5)')$output)
})

test_that('stdout', {
    expect_null(eval.msgs('1:5')$stdout)
    expect_null(eval.msgs('mtcars')$stdout)
    expect_null(eval.msgs('x <- mtcars')$stdout)
    expect_null(evals('plot(mtcars)')$stdout)
    expect_equal(eval.msgs('cat(1:5)')$stdout, '1 2 3 4 5')
    expect_equal(eval.msgs('cat(1:5);1:5')$stdout, '1 2 3 4 5')
})

context('evals')

test_that('Variable assignement', {
    expect_null(evals('x <- rnorm(100)')[[1]]$result)
})

test_that('Multiple variable assignement', {
    expect_null(evals(list(c('x <- rnorm(100)', 'y <- rnorm(10)')), parse = F)[[1]]$result)
})

test_that('Comment', {
    expect_null(evals('## comment')[[1]]$result)
})

test_that('Comment & variable assignement', {
    expect_null(evals(list(c('## comment', 'y <- rnorm(10)')), parse = F)[[1]]$result)
})

test_that('Basic: numerics', {
    expect_is(evals('rnorm(100)')[[1]]$result, 'numeric')
})

test_that('Basic: character', {
    expect_equal(evals('"Hello world!"')[[1]]$result, 'Hello world!')
})

test_that('Basic: list', {
    expect_is(evals('lapply(rnorm(10), round)')[[1]]$result, 'list')
})

test_that('Basic: data.frame', {
    expect_is(evals('mtcars')[[1]]$result, 'data.frame')
})

test_that('Plot: graphics package', {
    expect_is(evals('plot(1:10)')[[1]]$result, 'image')
})

test_that('Plot: lattice package', {
    expect_is(evals('require(lattice);+histogram(mtcars$hp)')[[1]]$result, 'image')
    expect_is(evals('require(lattice);histogram(mtcars$hp)', parse = F)[[1]]$result, 'image')
})

test_that('Plot: multiline code', {
    expect_is(evals(list(c('plot(cars, main="Stopping Distance versus Speed")',
                           'plot(cars, main="Stopping Distance versus Speed")')),
                    parse = F)[[1]]$result,
              'image')
    expect_is(evals('plot(cars, main="Stopping Distance versus Speed"); plot(cars, main="Stopping Distance versus Speed")')[[2]]$result, 'image') #nolint
})

test_that('Plot: pdf', {
    expect_is(evals('plot(1:10)', graph.output = 'pdf')[[1]]$result, 'image')
})

test_that('Plot: pdf without plotting', {
    expect_is(evals('runif(10)', graph.output = 'pdf')[[1]]$result, 'numeric')
})

test_that('NULL', {
    expect_is(evals('rnorm(100); NULL')[[1]]$result, 'numeric')
    expect_null(evals('rnorm(100); NULL')[[2]]$result)
    expect_null(evals('rnorm(100); NULL', parse = FALSE)[[1]]$result)
})

test_that('## comment', {
    expect_is(evals('rnorm(100);## comment')[[1]]$result, 'numeric')
    expect_is(evals('rnorm(100);## comment', parse = FALSE)[[1]]$result, 'numeric')
})

test_that('var assignement', {
    expect_is(evals('rnorm(100);x <- 1')[[1]]$result, 'numeric')
    expect_null(evals('rnorm(100);x <- 1', parse = F)[[1]]$result)
})

test_that('creating function', {
    expect_is(evals('rnorm(100);f <- function(x) mean(x)')[[1]]$result, 'numeric')
    expect_null(evals('rnorm(100);f <- function(x) mean(x)')[[2]]$result)
    expect_null(evals('rnorm(100);f <- function(x) mean(x)', parse = F)[[1]]$result)
})


test_that('string+num', {
    expect_is(evals('"X";rnorm(100)')[[2]]$result, 'numeric')
    expect_is(evals('"X";rnorm(100)')[[1]]$result, 'character')
    expect_is(evals('"X";rnorm(100)', parse = FALSE)[[1]]$result, 'numeric')
})

test_that('data.frame+num', {
    expect_is(evals(list(c('mtcars', 'rnorm(100)')), parse = FALSE)[[1]]$result, 'numeric')
})

test_that('simple error', {
    expect_is(evals('dadaffaSFA+A')[[1]]$msg$error, 'character')
})

test_that('simple error', {
    expect_is(evals('runiff(10)')[[1]]$msg$error, 'character')
})

test_that('multiple errors', {
    expect_is(evals(list(c('runiff(20)', 'Old MacDonald had a farm?')), parse = F)[[1]]$msg$error, 'character')
})

test_that('multiple errors', {
    expect_is(evals('no.R.object;no.R.function();very.mixed.up(stuff)')[[1]]$msg$error, 'character')
    expect_is(evals('no.R.object;no.R.function();very.mixed.up(stuff)')[[2]]$msg$error, 'character')
    expect_is(evals('no.R.object;no.R.function();very.mixed.up(stuff)')[[3]]$msg$error, 'character')
    expect_is(evals('no.R.object;no.R.function();very.mixed.up(stuff)', parse = F)[[1]]$msg$error, 'character')
})

test_that('output+errors', {
    expect_is(evals(list(c('mean(1:10)', 'no.R.function()')), parse = F)[[1]]$msg$error, 'character')
})

test_that('output+multiple errors', {
    expect_is(evals(list(c('no.R.object', 'Old MacDonald had a farm\\dots', 'pi')),
                    parse = F)[[1]]$msg$error,
              'character')
})

test_that('simple warning', {
    expect_is(evals('chisq.test(mtcars$gear, mtcars$hp)')[[1]]$msg$warning, 'character')
})

test_that('multiple warnings+output', {
    expect_is(evals(list(c('chisq.test(mtcars$gear, mtcars$am)', 'pi', 'chisq.test(mtcars$gear, mtcars$hp)')),
                    parse = F)[[1]]$msg$warnings,
              'character')
    expect_is(evals('chisq.test(mtcars$gear, mtcars$am);pi;chisq.test(mtcars$gear, mtcars$hp)')[[1]]$msg$warnings,
              'character')
})

test_that('long lines with line breaks', {
    expect_is(evals('if (TRUE) {\nset.caption("FOO")\nplot(c(1,2,3,4,5,6,7,8,9,10,11,12))\n}')[[1]]$result, 'image')
    expect_equal(length(evals('x <- "this is a very-very looooooooooong line which might be split on `parse`, but that is not a big deal at all"\nx\n1')), 3) #nolint
})


test_that('cache.dir option works correctly', {
    dir <- tempdir()
    suppressWarnings(file.remove(list.files(dir)))
    t1 <- system.time(evals('1:1e5', cache.mode = 'disk', cache.dir = dir, cache.time = 0))
    lf <- list.files(dir)
    expect_true(length(lf) > 0)
    t2 <- system.time(evals('1:1e5', cache.mode = 'disk', cache.dir = dir, cache.time = 0))
    expect_equal(list.files(dir), lf)
    # plots
    suppressWarnings(file.remove(list.files(dir)))
    t1 <- system.time(evals('plot(mtcars)',
            cache.mode = 'disk',
            cache.dir = dir,
            cache.time = 0,
            cache.copy.images = T))
    lf <- list.files(dir)
    expect_true(length(lf) > 0)
    t2 <- system.time(evals('plot(mtcars)',
            cache.mode = 'disk',
            cache.dir = dir,
            cache.time = 0,
            cache.copy.images = TRUE))
    expect_equal(list.files(dir), lf)
})

test_that('caching works correctly', {
    env <- new.env()
    evals('x <- 1:10', env = env, cache.time = 0)
    expect_true(length(ls(pander:::cached.results)) > 0)
    evals('x <- 1:100', env = env, cache.time = 0)
    res <- evals('x', env = env)
    expect_equal(res[[1]]$result, 1:100)
    t1 <- system.time(evals('plot(mtcars)', hi.res = T))
    lf <- list.files('plots')
    t2 <- system.time(evals('plot(mtcars)', hi.res = T))
    expect_equal(list.files('plots'), lf)
})

test_that('plots', {
    r1 <- evals('ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_smooth(aes(wt,mpg), method = lm, se=FALSE)',
        graph.unify = T)
    r2 <- evals('xyplot(wt ~ mpg, mtcars, type = c("p","smooth"))', graph.unify = T)
    expect_equal(r1$type, r2$type)
    dir <- 'temp'
    r1 <- evals('plot(mtcars)',
        graph.recordplot = TRUE,
        cache.time = 0,
        cache.copy.images = TRUE,
        cache.mode = 'disk',
        cache.dir = dir)
    lf <- list.files(dir)
    expect_true(length(list.files(dir, pattern = 'recordedplot$')) > 0)
    png(filename = file.path(dir, 'recorded.png'))
    redraw.recordedplot(list.files(dir, pattern = 'recordedplot$', full.names = T)[1])
    dev.off()
})

test_that('hooks work correctly', {
    set.seed(1)
    txt <- 'runif(1:4); matrix(runif(25), 5, 5); 1:5'
    hooks <- list('numeric' = round, 'matrix' = pander_return)
    res <- evals(txt, hooks = hooks)
    set.seed(1)
    expect_equal(res[[1]]$result, round(runif(1:4)))
    expect_equal(res[[2]]$result, pander_return(matrix(runif(25), 5, 5)))
    ## using pander's default hook
    set.seed(1)
    res <- evals(txt, hooks = list('default' = pander_return))
    set.seed(1)
    expect_equal(res[[1]]$result, pander_return(runif(1:4)))
    expect_equal(res[[2]]$result, pander_return(matrix(runif(25), 5, 5)))
    expect_equal(res[[3]]$result, pander_return(1:5))
})

test_that('output param works correctly', {
    res <- evals('1:10', output = 'all')
    expect_equal(names(res[[1]]), c('src', 'result', 'output', 'type', 'msg', 'stdout'))
    res <- evals('1:10', output = c('src', 'result'))
    expect_equal(names(res[[1]]), c('src', 'result'))
    expect_error(evals('1:10', output = c('src', 'result', '123')))
    res <- evals('matrix(5,5,5)',
          hooks = list('matrix' = list(pander_return, 'Cap')))[[1]]$result
    expect_equal(res[14], 'Table: Cap')
})

evalsOptions('cache.dir',  cache.dir)
evalsOptions('graph.dir',  graph.dir)
setwd(wd)

test_that('switching cache works', {
    co <- evalsOptions('cache')
    evalsOptions('cache', FALSE)
    expect_false(evalsOptions('cache'))
    evalsOptions('cache', TRUE)
    expect_true(evalsOptions('cache'))
    evalsOptions('cache', co)
})
