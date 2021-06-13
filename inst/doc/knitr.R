## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(pander)
library(knitr)

## -----------------------------------------------------------------------------
head(iris)
knitr::kable(head(iris))

## ---- error = TRUE------------------------------------------------------------
library(descr, quietly = TRUE)
ct <- CrossTable(mtcars$gear, mtcars$cyl)
knitr::kable(ct)
library(tables, quietly = TRUE)
tab <- tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
knitr::kable(tab)

## -----------------------------------------------------------------------------
methods(pander)

## ---- error = TRUE------------------------------------------------------------
library(descr, quietly = TRUE)
pander(CrossTable(mtcars$gear, mtcars$cyl))
library(tables, quietly = TRUE)
tab <- tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
pander(tab)

## -----------------------------------------------------------------------------
panderOptions('knitr.auto.asis', FALSE)
pander(head(iris))

## -----------------------------------------------------------------------------
panderOptions('knitr.auto.asis', TRUE)

## -----------------------------------------------------------------------------
dfs <- list(mtcars[1:3, 1:4], mtcars[4:6, 1:4], mtcars[7:9, 1:4])
lapply(dfs, pander)

## ----results='asis'-----------------------------------------------------------
panderOptions('knitr.auto.asis', FALSE)
dfs <- list(mtcars[1:3, 1:4], mtcars[4:6, 1:4], mtcars[7:9, 1:4])
invisible(lapply(dfs, pander))

