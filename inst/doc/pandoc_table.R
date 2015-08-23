## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(pander)

## ------------------------------------------------------------------------
m <- data.frame('Value\n1', 'Value\n2')
colnames(m) <- c('Multiline\nCol1', 'Multiline\nCol2')
pandoc.table(m, keep.line.breaks = TRUE)
m <- mtcars[1:3, 1:4]
pandoc.table(m)

## ---- error=TRUE---------------------------------------------------------
m <- mtcars[1:3, 1:4]
pandoc.table(m, style = 'simple')
m <- data.frame('Value\n1', 'Value\n2')
colnames(m) <- c('Multiline\nCol1', 'Multiline\nCol2')
pandoc.table(m, keep.line.breaks = TRUE, style='simple')

## ---- error=TRUE---------------------------------------------------------
m <- mtcars[1:3, 1:4]
pandoc.table(m, style = 'grid')
m <- data.frame('Value\n1', 'Value\n2')
colnames(m) <- c('Multiline\nCol1', 'Multiline\nCol2')
pandoc.table(m, keep.line.breaks = TRUE, style='grid')

## ---- error=TRUE---------------------------------------------------------
m <- mtcars[1:3, 1:4]
pandoc.table(m, style = 'rmarkdown')
m <- data.frame('Value\n1', 'Value\n2')
colnames(m) <- c('Multiline\nCol1', 'Multiline\nCol2')
pandoc.table(m, keep.line.breaks = TRUE, style='rmarkdown')

## ------------------------------------------------------------------------
pandoc.table(head(iris[,1:3], 2), justify = 'right')
pandoc.table(head(iris[,1:3], 2), justify = c('right', 'center', 'left'))

## ------------------------------------------------------------------------
set.alignment('left', row.names = 'right') # set only for next table since permanent parameter is falce
pandoc.table(mtcars[1:2,  1:5])

## ------------------------------------------------------------------------
panderOptions('table.alignment.default',
    function(df)
        ifelse(sapply(df, mean) > 2, 'left', 'right'))
pandoc.table(head(iris[,1:3], 2))
panderOptions('table.alignment.default', 'center')

## ------------------------------------------------------------------------
t <- mtcars[1:3, 1:5]
emphasize.italics.cols(1)
emphasize.italics.rows(1)
emphasize.strong.cells(which(t > 20, arr.ind = TRUE))
pandoc.table(t)
pandoc.table(t, emphasize.verbatim.rows = 1, emphasize.strong.cells = which(t > 20, arr.ind = TRUE))

## ------------------------------------------------------------------------
pandoc.table(mtcars[1:2, ], style = "grid", caption = "Wide table to be split!")

## ------------------------------------------------------------------------
pandoc.table(mtcars[1:2, ], style = "grid",
             caption = "Wide table to be split!", split.table = Inf)

## ---- error=TRUE---------------------------------------------------------
df <- data.frame(a = 'Lorem ipsum', b = 'dolor sit', c = 'amet')
pandoc.table(df, split.cells = 5)
pandoc.table(df, split.cells = c(5, 20, 5))
pandoc.table(df, split.cells = c("80%", "10%", "10%"))
pandoc.table(df, split.cells = 5, style = 'simple')

## ------------------------------------------------------------------------
pandoc.table(data.frame(baz = 'foobar', foo='accoutrements'),
             use.hyphening = TRUE, split.cells = 3)

## ------------------------------------------------------------------------
r <- matrix(c(283764.97430, 29.12345678901, -7.1234, -100.1), ncol = 2)
pandoc.table(r, round = 2)
pandoc.table(r, round = c(4,2)) # vector for each column
pandoc.table(r, digits = 2)
pandoc.table(r, digits = c(0, 5)) # vector for each column
pandoc.table(r, big.mark = ',')
pandoc.table(r, decimal.mark = ',')

## ------------------------------------------------------------------------
pandoc.table(mtcars[1:3, 1:4])
pandoc.table(mtcars[1:3, 1:4], plain.ascii = TRUE)

## ------------------------------------------------------------------------
pandoc.table(mtcars[1:3, 1:5], style = "grid", caption = "My caption!")

## ------------------------------------------------------------------------
m <- mtcars[1:3, 1:5]
m$mpg <- NA
pandoc.table(m, missing = '?')

