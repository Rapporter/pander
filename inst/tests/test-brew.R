context('Pandoc.brew')

test_that('expected result', {
    expect_that(capture.output(Pandoc.brew(text='## foo')), equals('## foo '))
    expect_that(capture.output(Pandoc.brew(text='## <%=1%>')), equals('## _1_ '))
    expect_that(capture.output(Pandoc.brew(text='<%=1%>')), equals('_1_ '))
    expect_that(capture.output(Pandoc.brew(text='<%for (i in 1:3)%><%=i%>')), equals('_1__2__3_ '))
})

test_that('correct number of list elements', {
    expect_that(length(Pandoc.brew(text='## foo', output = '/tmp/dev.null')), equals(1))
    expect_that(length(Pandoc.brew(text='## foo <%=mtcars[1:2,]%>', output = '/tmp/dev.null')), equals(3))
    expect_that(length(Pandoc.brew(text='## foo <%=mtcars[1:2,]%> bar <%=plot(1:10)%>', output = '/tmp/dev.null')), equals(5))
    expect_that(length(Pandoc.brew(text='## foo <%=1%> bar <%=pi%>', output = '/tmp/dev.null')), equals(1))
    expect_that(length(Pandoc.brew(text='<%for (i in 1:5){%>## foo<%}%>', output = '/tmp/dev.null')), equals(5))
    expect_that(length(Pandoc.brew(text='# FOO <%=pi%> bar \n# sad', output = '/tmp/dev.null')), equals(2))
    expect_that(length(Pandoc.brew(text='# FOO\n## bar<%=pi%> bar \n# sad', output = '/tmp/dev.null')), equals(3))
    expect_that(length(Pandoc.brew(text='# FOO ## bar<%=pi%> barn# sad', output = '/tmp/dev.null')), equals(1))
})

getChunkTypes <- function(x)
    sapply(x, function(x) x$type)

test_that('correct returned list element types', {
    expect_that(getChunkTypes(Pandoc.brew(text='foo <%=1:10%>', output = '/tmp/dev.null')), equals('text'))
    expect_that(getChunkTypes(Pandoc.brew(text='## foo <%=1:10%>', output = '/tmp/dev.null')), equals('heading'))
    expect_that(getChunkTypes(Pandoc.brew(text='foo\n<%=1:10%>', output = '/tmp/dev.null')), equals(c('text', 'block', 'text')))
    expect_that(getChunkTypes(Pandoc.brew(text='## foo\n<%=1:10%>', output = '/tmp/dev.null')), equals(c('heading', 'block', 'text')))
    expect_that(getChunkTypes(Pandoc.brew(text='foo <%=list(1:10)%>', output = '/tmp/dev.null')), equals(c('text', 'block', 'text')))
    expect_that(getChunkTypes(Pandoc.brew(text='## foo <%=mtcars%>', output = '/tmp/dev.null')), equals(c('heading', 'block', 'text')))
    expect_that(getChunkTypes(Pandoc.brew(text='foo has <%=1%> apple and <%=3%> pears', output = '/tmp/dev.null')), equals('text'))
    expect_that(getChunkTypes(Pandoc.brew(text='foo has \nsome neat apples. Exactly <%=5%>', output = '/tmp/dev.null')), equals('text'))
    expect_that(getChunkTypes(Pandoc.brew(text='<%=5%> apple', output = '/tmp/dev.null')), equals('text'))
    expect_that(getChunkTypes(Pandoc.brew(text='## header\n<%=5%> apple', output = '/tmp/dev.null')), equals(c('heading', 'text')))


})
