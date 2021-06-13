## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(pander)
library(logger)
evalsOptions('graph.name', 'test')
evalsOptions('graph.dir', 'my_plots')
evalsOptions('graph.output', 'jpg')

## -----------------------------------------------------------------------------
evals('1:10')

## -----------------------------------------------------------------------------
evals('1:10', output = c('result', 'output'))

## -----------------------------------------------------------------------------
evals('x')[[1]]$msg
evals('as.numeric("1.1a")')[[1]]$msg

## -----------------------------------------------------------------------------
evals('plot(mtcars)')[[1]]$result

## -----------------------------------------------------------------------------
evals('plot(mtcars)', graph.dir = 'my_plots', graph.output = 'jpg')[[1]]$result

## -----------------------------------------------------------------------------
## generating dataset
set.seed(1)
df <- mtcars[, c('hp', 'wt')]
df$factor <- sample(c('Foo', 'Bar', 'Foo bar'), size = nrow(df), replace = TRUE)
df$factor2 <- sample(c('Foo', 'Bar', 'Foo bar'), size = nrow(df), replace = TRUE)
df$time <- 1:nrow(df)

## ----echo=FALSE---------------------------------------------------------------
## loading packages
require(ggplot2, quietly = TRUE)
require(lattice, quietly = TRUE)

## -----------------------------------------------------------------------------
evalsOptions('graph.unify', TRUE)
evals('histogram(df$hp, main = "Histogram with lattice")')[[1]]$result
evals('ggplot(df) + geom_histogram(aes(x = hp), binwidth = 50) + ggtitle("Histogram with ggplot2")')[[1]]$result
evalsOptions('graph.unify', FALSE)

## -----------------------------------------------------------------------------
x <- evals('1:10', log = 'foo')

## -----------------------------------------------------------------------------
evalsOptions('log', 'evals')
log_threshold(TRACE, namespace = 'evals')
x <- evals('1:10', cache.time = 0)

## -----------------------------------------------------------------------------
t <- tempfile()
log_appender(appender_file(t), namespace = 'evals')
x <- evals('1:10', log = 'evals')
readLines(t)
# revert back to console
log_appender(appender_stdout, namespace = 'evals')

## -----------------------------------------------------------------------------
evalsOptions('cache.time', 0)
evalsOptions('log', 'evals')
log_threshold(TRACE, 'evals')

## -----------------------------------------------------------------------------
system.time(evals('1:1e5'))
system.time(evals('1:1e5'))

## -----------------------------------------------------------------------------
res <- evals('1:1e5', cache.mode = 'disk', cache.dir = 'cachedir')
list.files('cachedir')

## -----------------------------------------------------------------------------
x <- mtcars$hp
y <- 1e3
system.time(evals('sapply(rep(x, y), mean)'))

## -----------------------------------------------------------------------------
f <- sapply
g <- rep
h <- mean
X <- mtcars$hp * 1
Y <- 1000
system.time(evals('f(g(X, Y), h)'))

## -----------------------------------------------------------------------------
x <- 1
res <- evals('x <- 1:10;')

## -----------------------------------------------------------------------------
evals('x <- 1:10; x[3]')[[2]]$result

## -----------------------------------------------------------------------------
system.time(evals('plot(mtcars)'))
system.time(evals('plot(mtcars)'))

## ---- echo = FALSE, message = FALSE-------------------------------------------
unlink('cachedir', recursive = TRUE, force = TRUE)
unlink('my_plots', recursive = TRUE, force = TRUE)

