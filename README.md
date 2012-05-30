**pander** is an [R](http://r-project.org) package containing [helpers](#helper-functions) to return [Pandoc](http://johnmacfarlane.net/pandoc/) markdown of user specified format or *automatically* of several type of [**R objects**](#generic-pander-method).

The package is also capable of exporting/converting complex Pandoc documents (reports) in two ways ATM:

  * users might write some reports in [brew](http://cran.r-project.org/web/packages/brew/index.html) syntax resulting in a pretty Pandoc document (where `cat`ed R object are transformed to table, list etc.).

    *Example*: this `README` is cooked with `Pandoc.brew` based on [`inst/README.brew`](https://github.com/daroczig/pander/blob/master/inst/README.brew). Details can be found [below](#brew-to-pandoc).

<!-- endlist -->

 * or users might create a report in a live R session by adding some R objects and paragraphs to a `Pandoc` reference class object. Details can be found [below](#live-report-generation).

# Helper functions

There are a bunch of helper functions in `pander` which return user specified inputs in Pandoc format. You could find these functions starting with `pandoc.`. For example `pandoc.strong` would return the passed characters with strong emphasis. E.g.:

```rout
> pandoc.strong('FOO')
**FOO**>
> pandoc.strong.return('FOO')
[1] "**FOO**"
```

As it can be seen here `pandoc` functions generally prints to console and do not return anything. If you want the opposite (get the Pandoc format as a string): call each function ending in `.return` - like the second call in the above example. For details please check documentation, e.g. `?pandoc.strong`.

Of course there are more complex functions too. Besides verbatim texts, (image) links or footnotes (among others) there are a helper e.g. for **lists**:

```rout
> l <- list("First list element", paste0(1:5, '. subelement'), "Second element", list('F', 'B', 'I', c('phone', 'pad', 'talics')))
> pandoc.list(l, 'roman')
```

Which returns:

```

* First list element 
    * 1. subelement 
    * 2. subelement 
    * 3. subelement 
    * 4. subelement 
    * 5. subelement  
* Second element 
    * F 
    * B 
    * I 
        * phone 
        * pad 
        * talics   

<!-- end of list -->

```

`pandoc` can return **tables** in [three formats](http://johnmacfarlane.net/pandoc/README.html#tables):

  * The default style is the `multiline` format as most features (e.g. multi-line cells and alignment) are available in there.

```rout
> m <- mtcars[1:2, 1:3]
> pandoc.table(m)

--------------------------
              mpg cyl disp
------------- --- --- ----
Mazda RX4     21  6   160 

Mazda RX4 Wag 21  6   160 

```

  * `simple` tables do not support line breaks in cells:

```rout
> pandoc.table(m, style = "simple")

              mpg cyl disp
------------- --- --- ----
Mazda RX4     21  6   160 
Mazda RX4 Wag 21  6   160 

```

  * `grid` format are really handy for [emacs](http://emacswiki.org/emacs/TableMode) users but do support line breaks inside cells, but do not tolerate cell alignment:

```rout
> pandoc.table(m, style = "grid")

+---------------+-----+-----+------+
|               | mpg | cyl | disp |
+===============+=====+=====+======+
| Mazda RX4     | 21  | 6   | 160  |
+---------------+-----+-----+------+
| Mazda RX4 Wag | 21  | 6   | 160  |
+---------------+-----+-----+------+

```

Besides the `style` parameter there are several other ways to tweak the output like `decimal.mark` or `digits`. And of course it's really easy to add a **caption**:

```rout
> pandoc.table(m, style = "grid", caption = "Hello caption!")

+---------------+-----+-----+------+
|               | mpg | cyl | disp |
+===============+=====+=====+======+
| Mazda RX4     | 21  | 6   | 160  |
+---------------+-----+-----+------+
| Mazda RX4 Wag | 21  | 6   | 160  |
+---------------+-----+-----+------+

    Table: Hello caption!

```

`pandoc.table`˙can deal with the problem of really **wide tables**. Ever had an issue in LaTeX or MS Word when tried to print a correlation matrix of 40 variables? Not a problem any more as `pandoc.table` splits up the table if wider then 80 characters and handles caption too:

```rout
> pandoc.table(mtcars[1:2, ], style = "grid", caption = "Hello caption!")

+---------------+-----+-----+------+-----+------+-----+------+----+
|               | mpg | cyl | disp | hp  | drat | wt  | qsec | vs |
+===============+=====+=====+======+=====+======+=====+======+====+
| Mazda RX4     | 21  | 6   | 160  | 110 | 3.9  | 2.6 | 16   | 0  |
+---------------+-----+-----+------+-----+------+-----+------+----+
| Mazda RX4 Wag | 21  | 6   | 160  | 110 | 3.9  | 2.9 | 17   | 0  |
+---------------+-----+-----+------+-----+------+-----+------+----+

    Table: Hello caption! (continued below)

 

+---------------+----+------+------+
|               | am | gear | carb |
+===============+====+======+======+
| Mazda RX4     | 1  | 4    | 4    |
+---------------+----+------+------+
| Mazda RX4 Wag | 1  | 4    | 4    |
+---------------+----+------+------+

```

# Generic pander method

`pander` or `pandoc` (call as you wish) can deal with a bunch of R object types as being a pandocized `S3` method with a variety of classes.

Besides simple types (vectors, matrices, tables or data frames) lists might be interesting for you:

```rout
> pander(list(a=1, b=2, c=table(mtcars$am), x=list(myname=1,2), 56))

  * **a**: *1*
  * **b**: *2*
  * **c**:

    -----
    0  1
    -- --
    19 13

  * **x**:

      * **myname**: *1*
      * *2*

  * *56*

<!-- end of list -->

```

A nested list can be seen above with a table and all (optional) list names inside. As a matter of fact `pander.list` is the default method of `pander` too, see:

```rout
> x <- chisq.test(table(mtcars$am, mtcars$gear))
> class(x) <- 'I\'ve never heard of!'
> pander(x)

  * **statistic**: *20.944669365722*
  * **parameter**: *2*
  * **p.value**: *2.83088895897563e-05*
  * **method**: Pearson's Chi-squared test
  * **data.name**: table(mtcars$am, mtcars$gear)
  * **observed**:

    --------
      3  4 5
    - -- - -
    0 15 4 0

    1 0  8 5

  * **expected**:

    -------------
      3   4   5
    - --- --- ---
    0 8.9 7.1 3.0

    1 6.1 4.9 2.0

  * **residuals**:

    ----------------
      3    4    5
    - ---- ---- ----
    0 2.0  -1.2 -1.7

    1 -2.5 1.4  2.1

  * **stdres**:

    ----------------
      3    4    5
    - ---- ---- ----
    0 4.4  -2.3 -2.9

    1 -4.4 2.3  2.9

<!-- end of list -->

 **WARNING**^[Chi-squared approximation may be incorrect + No pander method for "I've never heard of!", reverting to default.]
```

So `pander` showed a not known class in an (almost) user-friendly way. And we got some warnings too styled with [Pandoc **footnote**](http://johnmacfarlane.net/pandoc/README.html#footnotes)! If that document is exported to e.g. `HTML` or `pdf`, then the error/warning message could be found on the bottom of the page with a link. *Note*: there were two warnings in the above call - both captured and returned! Well, this is the feature of `Pandoc.brew`, see [below](#brew-to-pandoc).

The output of different **statistical methods** are tried to be prettyfied. Some examples:

```rout
> pander(ks.test(runif(50), runif(50)))

---------------------------------------------
Test statistic P value Alternative hypothesis
-------------- ------- ----------------------
     0.18        0.4         two-sided       

    Table: Two-sample Kolmogorov-Smirnov test

> pander(chisq.test(table(mtcars$am, mtcars$gear)))

-------------------------
Test statistic df P value
-------------- -- -------
      21       2  2.8e-05

    Table: Pearson's Chi-squared test
 **WARNING**^[Chi-squared approximation may be incorrect]

> pander(t.test(extra ~ group, data = sleep))

------------------------------------------------
Test statistic df P value Alternative hypothesis
-------------- -- ------- ----------------------
     -1.9      18  0.079        two.sided       

    Table: Welch Two Sample t-test

> ## Dobson (1990) Page 93: Randomized Controlled Trial (examples from: ?glm)
> counts <- c(18,17,15,20,10,20,25,13,12)
> outcome <- gl(3,1,9)
> treatment <- gl(3,3)
> m <- glm(counts ~ outcome + treatment, family=poisson())
> pander(m)

-------------------------------------------------
            Estimate Std. Error  z value Pr(>|z|)
----------- -------- ---------- -------- --------
(Intercept) 3.0e+00   1.7e-01   1.8e+01  5.4e-71 

   outcome2 -4.5e-01  2.0e-01   -2.2e+00 2.5e-02 

   outcome3 -2.9e-01  1.9e-01   -1.5e+00 1.3e-01 

 treatment2 1.3e-15   2.0e-01   6.7e-15  1.0e+00 

 treatment3 1.4e-15   2.0e-01   7.1e-15  1.0e+00 

    Table: Fitting generalized (poisson/log) linear model: counts ~ outcome + treatment

> pander(anova(m))

------------------------------------------
          Df Deviance Resid. Df Resid. Dev
--------- -- -------- --------- ----------
     NULL NA    NA        8        10.6   

  outcome 2  5.5e+00      6        5.1    

treatment 2  2.7e-15      4        5.1    

    Table: Analysis of Deviance Table

> pander(aov(m))

---------------------------------------------
            Df  Sum Sq Mean Sq F value Pr(>F)
----------- -- ------- ------- ------- ------
outcome     2  9.3e+01 4.6e+01 2.2e+00  0.22 

treatment   2  8.4e-31 4.2e-31 2.0e-32  1.00 

Residuals   4  8.3e+01 2.1e+01   NA      NA  

    Table: Analysis of Variance Model

> pander(prcomp(USArrests))

-----------------------------------
         PC1   PC2    PC3    PC4   
-------- ----- ------ ------ ------
Murder   0.042 -0.045 0.080  -0.995

Assault  0.995 -0.059 -0.068 0.039 

UrbanPop 0.046 0.977  -0.201 -0.058

Rape     0.075 0.201  0.974  0.072 

    Table: Principal Components Analysis

------------------------------------------------------
                       PC1     PC2     PC3     PC4    
---------------------- ------- ------- ------- -------
Standard deviation     8.4e+01 1.4e+01 6.5e+00 2.5e+00

Proportion of Variance 9.7e-01 2.8e-02 5.8e-03 8.5e-04

Cumulative Proportion  9.7e-01 9.9e-01 1.0e+00 1.0e+00

> pander(density(mtcars$hp))

----------------------------------
        Coordinates Density values
------- ----------- --------------
   Min.     -32        5.0e-06    

1st Qu.     81         4.1e-04    

 Median     194        1.7e-03    

   Mean     194        2.2e-03    

3rd Qu.     306        4.1e-03    

   Max.     419        6.1e-03    

    Table: Kernel density of *mtcars$hp* (bandwidth: 28.04104)

```

# Brew to Pandoc

Everyone knows and uses [brew](http://cran.r-project.org/web/packages/brew/index.html) but if you would need some smack, the following links really worth visiting:

  * TODO

*In short*: a `brew` document is a simple text file with some special tags. `Pandoc.brew` uses only two of them (sorry, no brew templates here):

  * `<\% ... \%>` (without the backslash) stand for running R calls
  * `<\%= ... \%>` (without the backslash) does pretty the same but applies `pander` to the returning R object (instead of `cat` like the original `brew` function does). So putting there any R object would return is a nice Pandoc markdown format.

This latter tries to be smart in some ways:

  * a block (R commands between the tags) could return a value in the middle of the block and do something else without any output in the rest (but only one returned value per block!)
  * plots and images are grabbed in the document, rendered to a `png` file and `pander` method would result in a Pandoc markdown formatted image link (so the image would be shown/included in the exported document).
  * all warnings/messages and errors are recorded in the blocks and returned in the document as a footnote

This document was generated by `Pandoc.brew` based on [`inst/README.brew`](https://github.com/daroczig/pander/blob/master/inst/README.brew) so the above examples were generated automatically - which could be handy while writing some nifty statistical reports :)

`Pandoc.brew` could cook a file (default) or work with a character vector provided in the `text` argument. The output is set to `stdout` by default, it could be tweaked to write result to a text file and run Pandoc on that to create a `HTML`, `odt`, `docx` or other document.

# Live report generation

`pander` package has a special reference class called `Pandoc` which could collect some blocks in a live R session and export the whole document to Pandoc/pdf/HTML etc.

Without any serious comments, please check out the below (self-commenting) example:

```r
## Initialize a new Pandoc object
myReport <- Pandoc$new()

## Add author, title and date of document
myReport$author <- 'Gergely Daróczi'
myReport$title  <- 'Demo'

## Or it could be done while initializing
myReport <- Pandoc$new('Gergely Daróczi', 'Demo')

## Add some free text
myReport$add.paragraph('Hello there, this is a really short tutorial!')

## Add maybe a header for later stuff
myReport$add.paragraph('# Showing some raw R objects below')

## Adding a short matrix
myReport$add(matrix(5,5,5))

## Or a table with even
myReport$add.paragraph('Hello table:')
myReport$add(table(mtcars$am, mtcars$gear))

## Or a "large" dataframe which barely fits on a page
myReport$add(mtcars)

## And a simple linear model with Anova tables
ml <- with(lm(mpg ~ hp + wt), data = mtcars)
myReport$add(ml)
myReport$add(anova(ml))
myReport$add(aov(ml))

## And do some principal component analysis at last
myReport$add(prcomp(USArrests))

## Sorry, I did not show how Pandoc deals with plots:
myReport$add(plot(1:10))

## Want to see the report? Just print it:
myReport

## Exporting to pdf (default)
myReport$export()

## Or to docx in tempdir():
myReport$format <- 'docx'
myReport$export(tempfile())

## You do not want to see the generated report after generation?
myReport$export(open = FALSE)
```