## test_file('test-evals.R')
context('evals: no output generated')

test_that('Variable assignement', {
    expect_that(evals('x <- rnorm(100)')[[1]]$output, equals(NULL))
})

test_that('Multiple variable assignement', {
            expect_that(evals(list(c('x <- rnorm(100)', 'y <- rnorm(10)')))[[1]]$output, equals(NULL))
        })

test_that('Comment', {
            expect_that(evals('## comment')[[1]]$output, equals(NULL))
        })

test_that('Comment & variable assignement', {
            expect_that(evals(list(c('## comment', 'y <- rnorm(10)')))[[1]]$output, equals(NULL))
        })

context('evals: basic R object output generated')

test_that('Basic: numerics', {
            expect_that(evals('rnorm(100)')[[1]]$output, is_a('numeric'))
        })

test_that('Basic: character', {
            expect_that(evals('"Hello world!"')[[1]]$output, equals("Hello world!"))
        })

test_that('Basic: list', {
            expect_that(evals('lapply(rnorm(10), round)')[[1]]$output, is_a('list'))
        })

test_that('Basic: data.frame', {
            expect_that(evals('mtcars')[[1]]$output, is_a('data.frame'))
        })

context('evals: plots/images generated')

test_that('Plot: graphics package', {
            expect_that(evals('plot(1:10)')[[1]]$output, is_a('image'))
        })

test_that('Plot: lattice package', {
            expect_that(evals('require(lattice); histogram(mtcars$hp)')[[1]]$output, is_a('image'))
        })

test_that('Plot: multiline code', {
            expect_that(evals(list(c('plot(cars, main="Stopping Distance versus Speed")', 'plot(cars, main="Stopping Distance versus Speed")')))[[1]]$output, is_a('image'))
        })

test_that('Plot: pdf', {
            expect_that(evals('plot(1:10)', graph.output = 'pdf')[[1]]$output, is_a('image'))
        })

test_that('Plot: pdf without plotting', {
            expect_that(evals('runif(10)', graph.output = 'pdf')[[1]]$output, is_a('numeric'))
        })

context('evals: no output after output')

test_that('NULL', {
            expect_that(evals(list(c('rnorm(100)', 'NULL')))[[1]]$output, is_a('numeric'))
        })

test_that('## comment', {
            expect_that(evals(list(c('rnorm(100)', '## comment')))[[1]]$output, is_a('numeric'))
        })

test_that('var assignement', {
            expect_that(evals(list(c('rnorm(100)', 'x <- 1')))[[1]]$output, is_a('numeric'))
        })

test_that('creating function', {
            expect_that(evals(list(c('rnorm(100)', 'f <- function(x) mean(x)')))[[1]]$output, is_a('numeric'))
        })

context('evals: multiple outputs (last is preserved)')

test_that('string+num', {
            expect_that(evals(list(c('"X"', 'rnorm(100)')))[[1]]$output, is_a('numeric'))
        })

test_that('data.frame+num', {
            expect_that(evals(list(c('mtcars', 'rnorm(100)')))[[1]]$output, is_a('numeric'))
        })

context('evals: error handling')

test_that('simple error', {
            expect_that(evals('dadaffaSFA+A')[[1]]$msg$error, is_a('character'))
        })

test_that('simple error', {
            expect_that(evals('runiff(10)')[[1]]$msg$error, is_a('character'))
        })

test_that('multiple errors', {
            expect_that(evals(list(c('runiff(20)', 'Old MacDonald had a farm?')))[[1]]$msg$error, is_a('character'))
        })

test_that('multiple errors', {
            expect_that(evals(list(c('no.R.object', 'no.R.function()', 'very.mixed.up(stuff)')))[[1]]$msg$error, is_a('character'))
        })

test_that('output+errors', {
            expect_that(evals(list(c('mean(1:10)', 'no.R.function()')))[[1]]$msg$error, is_a('character'))
        })

test_that('output+multiple errors', {
            expect_that(evals(list(c('no.R.object', 'Old MacDonald had a farm\\dots', 'pi')))[[1]]$msg$error, is_a('character'))
        })

context('evals: warnings handling')

test_that('simple warning', {
            expect_that(evals('chisq.test(mtcars$gear, mtcars$hp)')[[1]]$msg$warning, is_a('character'))
        })

test_that('multiple warnings+output', {
            expect_that(evals(list(c('chisq.test(mtcars$gear, mtcars$am)', 'pi', 'chisq.test(mtcars$gear, mtcars$hp)')))[[1]]$msg$warnings, is_a('character'))
        })

context('evals(check.output=FALSE): no output generated, but it does return if the text is not closed with NULL')

test_that('Variable assignement', {
            expect_that(evals('x <- 1:10', check.output = FALSE)[[1]]$output, equals(1:10))
        })

test_that('Multiple variable assignement', {
            expect_that(evals(list(c('x <- rnorm(100)', 'y <- 1:10')), check.output = FALSE)[[1]]$output, equals(1:10))
        })

test_that('Comment', {
            expect_that(evals('## comment', check.output = FALSE)[[1]]$output, equals(NULL))
        })

test_that('Comment & variable assignement', {
            expect_that(evals(list(c('## comment', 'y <- 1:10')), check.output = FALSE)[[1]]$output, equals(1:10))
        })

context('evals(check.output=FALSE): no output generated as the text is closed with NULL')

test_that('Variable assignement', {
            expect_that(evals(list(c('x <- 1:10', 'NULL')), check.output = FALSE)[[1]]$output, equals(NULL))
        })

test_that('Multiple variable assignement', {
            expect_that(evals(list(c('x <- rnorm(100)', 'y <- 1:10', 'NULL')), check.output = FALSE)[[1]]$output, equals(NULL))
        })

test_that('Comment & variable assignement', {
            expect_that(evals(list(c('## comment', 'y <- 1:10', 'NULL')), check.output = FALSE)[[1]]$output, equals(NULL))
        })

context('evals(check.output=FALSE): basic R object output generated')

test_that('Basic: numerics', {
            expect_that(evals('rnorm(100)', check.output = FALSE)[[1]]$output, is_a('numeric'))
        })

test_that('Basic: character', {
            expect_that(evals('"Hello world!"', check.output = FALSE)[[1]]$output, equals("Hello world!"))
        })

test_that('Basic: list', {
            expect_that(evals('lapply(rnorm(10), round)', check.output = FALSE)[[1]]$output, is_a('list'))
        })

test_that('Basic: data.frame', {
            expect_that(evals('mtcars', check.output = FALSE)[[1]]$output, is_a('data.frame'))
        })

context('evals(check.output=FALSE): plots/images generated')

test_that('Plot: graphics package', {
            expect_that(evals('plot(1:10)', check.output = FALSE)[[1]]$output, is_a('image'))
        })

test_that('Plot: lattice package', {
            expect_that(evals('require(lattice); print(histogram(mtcars$hp))', check.output = FALSE)[[1]]$output, is_a('image'))
        })

test_that('Plot: multiline code', {
            expect_that(evals(list(c('plot(cars, main="Stopping Distance versus Speed")', 'plot(cars, main="Stopping Distance versus Speed")')), check.output = FALSE)[[1]]$output, is_a('image'))
        })

context('evals(check.output=FALSE): no output after output resuts in NULL')

test_that('NULL', {
            expect_that(evals(list(c('rnorm(100)', 'NULL')), check.output = FALSE)[[1]]$output, equals(NULL))
        })

test_that('## comment', {
            expect_that(evals(list(c('rnorm(100)', '## comment')), check.output = FALSE)[[1]]$output, is_a('numeric'))
        })

test_that('var assignement', {
            expect_that(evals(list(c('rnorm(100)', 'x <- "string"')), check.output = FALSE)[[1]]$output, is_a('character'))
        })

test_that('creating function', {
            expect_that(evals(list(c('rnorm(100)', 'f <- function(x) mean(x)')), check.output = FALSE)[[1]]$output, is_a('function'))
        })

context('evals(check.output=FALSE): multiple outputs (last is preserved)')

test_that('string+num', {
            expect_that(evals(list(c('"X"', 'rnorm(100)')), check.output = FALSE)[[1]]$output, is_a('numeric'))
        })

test_that('data.frame+num', {
            expect_that(evals(list(c('mtcars', 'rnorm(100)')), check.output = FALSE)[[1]]$output, is_a('numeric'))
        })











context('evals(check.output=FALSE): error handling')

test_that('simple error', {
            expect_that(evals('dadaffaSFA+A', check.output = FALSE)[[1]]$msg$error, is_a('character'))
        })

test_that('simple error', {
            expect_that(evals('runiff(10)', check.output = FALSE)[[1]]$msg$error, is_a('character'))
        })

test_that('multiple errors', {
            expect_that(evals(list(c('runiff(20)', 'Old MacDonald had a farm?')), check.output = FALSE)[[1]]$msg$error, is_a('character'))
        })

test_that('multiple errors', {
            expect_that(evals(list(c('no.R.object', 'no.R.function()', 'very.mixed.up(stuff)')), check.output = FALSE)[[1]]$msg$error, is_a('character'))
        })

test_that('output+errors', {
            expect_that(evals(list(c('mean(1:10)', 'no.R.function()')), check.output = FALSE)[[1]]$msg$error, is_a('character'))
        })

test_that('output+multiple errors', {
            expect_that(evals(list(c('no.R.object', 'Old MacDonald had a farm\\dots', 'pi')), check.output = FALSE)[[1]]$msg$error, is_a('character'))
        })

context('evals(check.output=FALSE): warnings handling')

test_that('simple warning', {
            expect_that(evals('chisq.test(mtcars$gear, mtcars$hp)', check.output = FALSE)[[1]]$msg$warning, is_a('character'))
        })

test_that('multiple warnings+output', {
            expect_that(evals(list(c('chisq.test(mtcars$gear, mtcars$am)', 'pi', 'chisq.test(mtcars$gear, mtcars$hp)')), check.output = FALSE)[[1]]$msg$warnings, is_a('character'))
        })
