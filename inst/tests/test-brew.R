library(testthat)
library(pander)
context('Pandoc.brew')

test_that('expected result', {
    expect_equal(capture.output(Pandoc.brew(text = '## foo')), '## foo ')
    expect_equal(capture.output(Pandoc.brew(text = '## <%=1%>')), '## _1_ ')
    expect_equal(capture.output(Pandoc.brew(text = '<%=1%>')), '_1_ ')
    expect_equal(capture.output(Pandoc.brew(text = '<%for (i in 1:3)%><%=i%>')), '_1__2__3_ ')
})

output <- '/tmp/dev.null' #nolint

test_that('correct number of list elements', {
    expect_equal(length(Pandoc.brew(text = '## foo', output = output)), 1)
    expect_equal(length(Pandoc.brew(text = '## foo <%=mtcars[1:2,]%>', output = output)), 3)
    expect_equal(length(Pandoc.brew(text = '## foo <%=mtcars[1:2,]%> bar <%=plot(1:10)%>', output = output)), 5)
    expect_equal(length(Pandoc.brew(text = '## foo <%=1%> bar <%=pi%>', output = output)), 1)
    expect_equal(length(Pandoc.brew(text = '<%for (i in 1:5){%>## foo<%}%>', output = output)), 5)
    expect_equal(length(Pandoc.brew(text = '# FOO <%=pi%> bar \n# sad', output = output)), 2)
    expect_equal(length(Pandoc.brew(text = '# FOO\n## bar<%=pi%> bar \n# sad', output = output)), 3)
    expect_equal(length(Pandoc.brew(text = '# FOO ## bar<%=pi%> barn# sad', output = output)), 1)
})

getChunkTypes <- function(x)
    sapply(x, function(x) x$type)

test_that('correct returned list element types', {
    expect_equal(getChunkTypes(Pandoc.brew(text = 'foo <%=1:10%>', output = output)), 'text')
    expect_equal(getChunkTypes(Pandoc.brew(text = '## foo <%=1:10%>', output = output)), 'heading')
    expect_equal(getChunkTypes(Pandoc.brew(text = 'foo\n<%=1:10%>', output = output)), c('text', 'block', 'text'))
    expect_equal(getChunkTypes(Pandoc.brew(text = '## foo\n<%=1:10%>', output = output)), c('heading', 'block', 'text'))
    expect_equal(getChunkTypes(Pandoc.brew(text = 'foo <%=list(1:10)%>', output = output)), c('text', 'block', 'text'))
    expect_equal(getChunkTypes(Pandoc.brew(text = '## foo <%=mtcars%>', output = output)),
                 c('heading', 'block', 'text'))
    expect_equal(getChunkTypes(Pandoc.brew(text = 'foo has <%=1%> apple and <%=3%> pears', output = output)), 'text')
    expect_equal(getChunkTypes(Pandoc.brew(text = 'foo has \nsome neat apples. Exactly <%=5%>', output = output)),
                  'text')
    expect_equal(getChunkTypes(Pandoc.brew(text = '<%=5%> apple', output = output)), 'text')
    expect_equal(getChunkTypes(Pandoc.brew(text = '## header\n<%=5%> apple', output = output)), c('heading', 'text'))
    expect_equal(getChunkTypes(Pandoc.brew(text = '<%=plot(1:10)%> ', output = output)), c('block', 'text'))

})
