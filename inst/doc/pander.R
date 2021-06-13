## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(pander)
library(tables)
panderOptions('knitr.auto.asis', FALSE)
panderOptions('plain.ascii', TRUE)

## -----------------------------------------------------------------------------
pander(head(iris))
pander(head(mtcars[1:5]))
pander(tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris ))

## -----------------------------------------------------------------------------
methods(pander)

## -----------------------------------------------------------------------------
evals('1:10')

## -----------------------------------------------------------------------------
str(Pandoc.brew(text ='Pi equals to `<%= pi %>`. And here are some random data: `<%= runif(10)%>`'))

## -----------------------------------------------------------------------------
pots <- panderOptions("table.style")
panderOptions("table.style", "simple")
pander(mtcars[1:3, 1:4])
pander(head(iris))
panderOptions("table.style", "grid")
pander(head(iris))
panderOptions("table.style", pots)

