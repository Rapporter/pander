% [pander: An R Pandoc writer](https://github.com/daroczig/pander)

**pander** is an [R](http://r-project.org) package containing [helpers](#helper-functions) to return [Pandoc](http://johnmacfarlane.net/pandoc/)'s markdown of user specified format or *automatically* of several type of [**R objects**](#generic-pander-method).

The package is also capable of exporting/converting complex Pandoc documents (reports) in three ways ATM:

  * create somehow a markdown text file (e.g. with `brew`, `knitr` or any scripts of yours, maybe with `Pandoc.brew` - see just below) and transform that to other formats (like HTML, odt, pdf, docx etc.) with `Pandoc.convert`

  * users might write some reports in [brew](http://cran.r-project.org/web/packages/brew/index.html) syntax resulting in a pretty *Pandoc* document (where each R object are transformed to nicely formatted table, list etc.) and also in a **bunch of formats** (like HTML, odt, pdf, docx etc.) automatically.

    *Example*: this [`README.md`](https://github.com/daroczig/pander/blob/master/README.md) is cooked with `Pandoc.brew` based on [`inst/README.brew`](https://github.com/daroczig/pander/blob/master/inst/README.brew) and also exported to [HTML](http://daroczig.github.com/pander/). Details can be found [below](#brew-to-pandoc) or head directly to [examples](#examples).

<!-- endlist -->

 * or users might create a report in a live R session by adding some R objects and paragraphs to a `Pandoc` reference class object. Details can be found [below](#live-report-generation).

**How it is differ from Sweave, brew, knitr, R2HTML etc.?**

  * no need for calling `ascii`, `xtable`, `Hmisc`, `tables` etc. to transform an R object to `HTML`, `tex` etc. as `pander` results in Pandoc's *markdown* which can be converted to almost any text document format (like: pdf, HTML, odt, docx, textile, asciidoc, reStructuredText etc.). Conversion can be done automatically after calling `pander` reporting functions ([Pander.brew](#brew-to-pandoc) or [Pandoc](#live-report-generation)).
  * based on the above *no "traditional" R console output* is shown in the resulting document (nor in markdown, nor in exported docs) but **all R objects are transformed to tables, list etc**. Well, there is an option (`show.src`) to show the original R commands before the formatted output, and `pander`˛calls can be also easily tweaked (just file an issue) to return `print`ed R objects - if you would need that in some strange situation - like writing an R tutorial. But **I really think that nor R code, nor raw R results have anything to do with an exported report** :)
  * *graphs/plots* are recognized in blocks of R commands without any special setting or marks around code block and saved to disk in a `png` file linked in the resulting document. This means if you create a report (e.g. `brew` a text file) and export it to pdf/docx etc. all the plots/images would be there. There are some parameters to specify the resolution of the image and also the type (e.g. `jpg`, `svg` or `pdf`).
  * `pander`˛uses its build in (IMHO quite decent) **caching**. This means that if evaluation of some R commands take too much time (which can be set by option/parameter), then the results are saved in a file and returned from there on next exact R code's evaluation. This caching algorithm tries to be **smart** as checks not only the passed R sources, but *all variables* inside that and saves the hash of those. This is a quite secure way of caching, but if you would encounter any issues, just switch off the cache. I've not seen any issues :)
  * `knitr` *support* is coming too, for details see my [TODO list](https://github.com/daroczig/pander/blob/master/TODO.md) **Update**: just use `knitr` to generate markdown and pass that to `Pandoc.convert`

# Installation

Currently, this package is hosted only on [GitHub](https://github.com/daroczig/pander).

Until this gets on [CRAN](cran.r-project.org), you can install it via nifty function from `devtools` package:

```
library(devtools)
install_github('pander', 'daroczig')
```

Or download the [sources in a zip file](https://github.com/daroczig/pander/zipball/master) and build manually. If you're running R on Windows, you need to install [Rtools](http://cran.stat.ucla.edu/bin/windows/Rtools/).

## Depends

`pander` heavily builds on [Pandoc](http://johnmacfarlane.net/pandoc) which should be **pre-installed** before trying to convert your reports to [different formats](http://johnmacfarlane.net/pandoc/). Although **main functions work without Pandoc**, e.g. you can generate a markdown formatted report via [Pandoc.brew](#brew-to-pandoc) or the custom [reference class](#live-report-generation), but I would really suggest to install that really great piece of software!

~~And as `pander` and `rapport` are quite Siamese twins, you would need an **up-to-date** version of [rapport](http://rapport-package.info) most likely installed from [Github](https://github.com/aL3xa/rapport).~~ `pander` now can work independently from `rapport`.

Now you would only need a few cool packages from CRAN:

  * [brew](http://cran.r-project.org/web/packages/brew/index.html) for literate programming,
  * [digest](http://cran.r-project.org/web/packages/digest/index.html) to compute hashes while caching,
  * [parser](http://cran.r-project.org/web/packages/parser/index.html) to identify variables in passed R commands,
  * ~~[evaluate](http://cran.r-project.org/web/packages/evaluate/index.html)~~
  * besides [R](http://www.r-project.org/) of course!

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

I. First list element 
    I. 1. subelement 
    II. 2. subelement 
    III. 3. subelement 
    IV. 4. subelement 
    V. 5. subelement  
II. Second element 
    I. F 
    II. B 
    III. I 
        I. phone 
        II. pad 
        III. talics   

<!-- end of list -->

```

`pandoc` can return **tables** in [three formats](http://johnmacfarlane.net/pandoc/README.html#tables):

  * The default style is the `multiline` format as most features (e.g. multi-line cells and alignment) are available in there.

```rout
> m <- mtcars[1:2, 1:3]
> pandoc.table(m)

------------------------------------
                  mpg   cyl   disp  
----------------- ----- ----- ------
**Mazda RX4**     21    6     160   

**Mazda RX4 Wag** 21    6     160   
------------------------------------

```

  * `simple` tables do not support line breaks in cells:

```rout
> pandoc.table(m, style = "simple")

                  mpg   cyl   disp  
----------------- ----- ----- ------
**Mazda RX4**     21    6     160   
**Mazda RX4 Wag** 21    6     160   

```

  * `grid` format are really handy for [emacs](http://emacswiki.org/emacs/TableMode) users but do support line breaks inside cells, but do not tolerate cell alignment:

```rout
> pandoc.table(m, style = "grid")

+-------------------+-------+-------+--------+
|                   | mpg   | cyl   | disp   |
+===================+=======+=======+========+
| **Mazda RX4**     | 21    | 6     | 160    |
+-------------------+-------+-------+--------+
| **Mazda RX4 Wag** | 21    | 6     | 160    |
+-------------------+-------+-------+--------+

```

Besides the `style` parameter there are several other ways to tweak the output like `decimal.mark` or `digits`. And of course it's really easy to add a **caption**:

```rout
> pandoc.table(m, style = "grid", caption = "Hello caption!")

+-------------------+-------+-------+--------+
|                   | mpg   | cyl   | disp   |
+===================+=======+=======+========+
| **Mazda RX4**     | 21    | 6     | 160    |
+-------------------+-------+-------+--------+
| **Mazda RX4 Wag** | 21    | 6     | 160    |
+-------------------+-------+-------+--------+

Table: Hello caption!

```

`pandoc.table`˙can deal with the problem of really **wide tables**. Ever had an issue in LaTeX or MS Word when tried to print a correlation matrix of 40 variables? Not a problem any more as `pandoc.table` splits up the table if wider then 80 characters and handles caption too:

```rout
> pandoc.table(mtcars[1:2, ], style = "grid", caption = "Hello caption!")

+-------------------+-------+-------+--------+------+--------+------+
|                   | mpg   | cyl   | disp   | hp   | drat   | wt   |
+===================+=======+=======+========+======+========+======+
| **Mazda RX4**     | 21    | 6     | 160    | 110  | 3.9    | 2.6  |
+-------------------+-------+-------+--------+------+--------+------+
| **Mazda RX4 Wag** | 21    | 6     | 160    | 110  | 3.9    | 2.9  |
+-------------------+-------+-------+--------+------+--------+------+

Table: Hello caption! (continued below)

 

+-------------------+--------+------+------+--------+--------+
|                   | qsec   | vs   | am   | gear   | carb   |
+===================+========+======+======+========+========+
| **Mazda RX4**     | 16     | 0    | 1    | 4      | 4      |
+-------------------+--------+------+------+--------+--------+
| **Mazda RX4 Wag** | 17     | 0    | 1    | 4      | 4      |
+-------------------+--------+------+------+--------+--------+

```

And too wide cells are also split by line breaks. E.g.:

```rout
> pandoc.table(data.frame(id=1:2, value=c('FOO', 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.')))

----------------------------------
id   value                        
---- -----------------------------
1    FOO                          

2    Lorem ipsum dolor sit amet,  
     consectetur adipisicing elit,
     sed do eiusmod tempor        
     incididunt ut labore et      
     dolore magna aliqua. Ut enim 
     ad minim veniam, quis nostrud
     exercitation ullamco laboris 
     nisi ut aliquip ex ea commodo
     consequat. Duis aute irure   
     dolor in reprehenderit in    
     voluptate velit esse cillum  
     dolore eu fugiat nulla       
     pariatur. Excepteur sint     
     occaecat cupidatat non       
     proident, sunt in culpa qui  
     officia deserunt mollit anim 
     id est laborum.              
----------------------------------

```

# Generic pander method

`pander` or `pandoc` (call as you wish) can deal with a bunch of R object types as being a pandocized `S3` method with a variety of classes.

Besides simple types (vectors, matrices, tables or data frames) lists might be interesting for you:

```rout
> pander(list(a=1, b=2, c=table(mtcars$am), x=list(myname=1,2), 56))

  * **a**: *1*
  * **b**: *2*
  * **c**:

    -------
    0   1
    --- ---
    19  13
    -------

  * **x**:

      * **myname**: *1*
      * *2*

  * *56*

<!-- end of list -->

```

A nested list can be seen above with a table and all (optional) list names inside. As a matter of fact `pander.list` is the default method of `pander` too, see:

```rout
> x <- chisq.test(table(mtcars$am, mtcars$gear))
> class(x) <- "I've never heard of!"
> pander(x)
 **WARNING**^[Chi-squared approximation may be incorrect]

  * **statistic**: *21*
  * **parameter**: *2*
  * **p.value**: *2.8e-05*
  * **method**: Pearson's Chi-squared test
  * **data.name**: table(mtcars$am, mtcars$gear)
  * **observed**:

    -----------------
          3   4   5
    ----- --- --- ---
    **0** 15  4   0

    **1** 0   8   5
    -----------------

  * **expected**:

    -----------------
          3   4   5
    ----- --- --- ---
    **0** 8.9 7.1 3.0

    **1** 6.1 4.9 2.0
    -----------------

  * **residuals**:

    --------------------
          3    4    5
    ----- ---- ---- ----
    **0** 2.0  -1.2 -1.7

    **1** -2.5 1.4  2.1
    --------------------

  * **stdres**:

    --------------------
          3    4    5
    ----- ---- ---- ----
    **0** 4.4  -2.3 -2.9

    **1** -4.4 2.3  2.9
    --------------------

<!-- end of list -->

```

So `pander` showed a not known class in an (almost) user-friendly way. And we got some warnings too styled with [Pandoc **footnote**](http://johnmacfarlane.net/pandoc/README.html#footnotes)! If that document is exported to e.g. `HTML` or `pdf`, then the error/warning message could be found on the bottom of the page with a link. *Note*: there were two warnings in the above call - both captured and returned! Well, this is the feature of `Pandoc.brew`, see [below](#brew-to-pandoc).

The output of different **statistical methods** are tried to be prettyfied. Some examples:

```rout
> pander(ks.test(runif(50), runif(50)))

---------------------------------------------------
 Test statistic   P value   Alternative hypothesis 
---------------- --------- ------------------------
      0.14         0.72           two-sided        
---------------------------------------------------

Table: Two-sample Kolmogorov-Smirnov test: `runif(50)` and `runif(50)`

> pander(chisq.test(table(mtcars$am, mtcars$gear)))

-------------------------------
 Test statistic   df   P value 
---------------- ---- ---------
       21         2    2.8e-05 
-------------------------------

Table: Pearson's Chi-squared test: `table(mtcars$am, mtcars$gear)`

 **WARNING**^[Chi-squared approximation may be incorrect]

> pander(t.test(extra ~ group, data = sleep))

--------------------------------------------------------
 Test statistic   df   P value   Alternative hypothesis 
---------------- ---- --------- ------------------------
      -1.9        18    0.079          two.sided        
--------------------------------------------------------

Table: Welch Two Sample t-test: `extra` by `group`

> ## Dobson (1990) Page 93: Randomized Controlled Trial (examples from: ?glm)
> counts <- c(18,17,15,20,10,20,25,13,12)
> outcome <- gl(3,1,9)
> treatment <- gl(3,3)
> m <- glm(counts ~ outcome + treatment, family=poisson())
> pander(m)

------------------------------------------------------------
                 Estimate   Std. Error   z value   Pr(>|z|) 
--------------- ---------- ------------ --------- ----------
**(Intercept)**  3.0e+00     1.7e-01     1.8e+01   5.4e-71  

   **outcome2**  -4.5e-01    2.0e-01    -2.2e+00   2.5e-02  

   **outcome3**  -2.9e-01    1.9e-01    -1.5e+00   1.3e-01  

 **treatment2**  1.3e-15     2.0e-01     6.7e-15   1.0e+00  

 **treatment3**  1.4e-15     2.0e-01     7.1e-15   1.0e+00  
------------------------------------------------------------

Table: Fitting generalized (poisson/log) linear model: counts ~ outcome + treatment

> pander(anova(m))

------------------------------------------------------
               Df   Deviance   Resid. Df   Resid. Dev 
------------- ---- ---------- ----------- ------------
     **NULL**  NA      NA          8          10.6    

  **outcome**  2    5.5e+00        6          5.1     

**treatment**  2    2.7e-15        4          5.1     
------------------------------------------------------

Table: Analysis of Deviance Table

> pander(aov(m))

--------------------------------------------------------
               Df   Sum Sq   Mean Sq   F value   Pr(>F) 
------------- ---- -------- --------- --------- --------
  **outcome**  2   9.3e+01   4.6e+01   2.2e+00    0.22  

**treatment**  2   8.4e-31   4.2e-31   2.0e-32    1.00  

**Residuals**  4   8.3e+01   2.1e+01     NA        NA   
--------------------------------------------------------

Table: Analysis of Variance Model

> pander(prcomp(USArrests))

---------------------------------------
             PC1   PC2    PC3    PC4   
------------ ----- ------ ------ ------
**Murder**   0.042 -0.045 0.080  -0.995

**Assault**  0.995 -0.059 -0.068 0.039 

**UrbanPop** 0.046 0.977  -0.201 -0.058

**Rape**     0.075 0.201  0.974  0.072 
---------------------------------------

Table: Principal Components Analysis

----------------------------------------------------------
                           PC1     PC2     PC3     PC4    
-------------------------- ------- ------- ------- -------
**Standard deviation**     8.4e+01 1.4e+01 6.5e+00 2.5e+00

**Proportion of Variance** 9.7e-01 2.8e-02 5.8e-03 8.5e-04

**Cumulative Proportion**  9.7e-01 9.9e-01 1.0e+00 1.0e+00
----------------------------------------------------------

> pander(density(mtcars$hp))

------------------------------------------
             Coordinates   Density values 
----------- ------------- ----------------
   **Min.**      -32          5.0e-06     

**1st Qu.**      81           4.1e-04     

 **Median**      194          1.7e-03     

   **Mean**      194          2.2e-03     

**3rd Qu.**      306          4.1e-03     

   **Max.**      419          6.1e-03     
------------------------------------------

Table: Kernel density of *mtcars$hp* (bandwidth: 28.04104)

```

And of course tables are formatted (e.g. auto add of line breaks and splitting up tables) nicely:

```rout
> set.caption('Foo Bar')
> pander(data.frame(id=1:2, value=c('FOO', 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.')))

----------------------------------
id   value                        
---- -----------------------------
1    FOO                          

2    Lorem ipsum dolor sit amet,  
     consectetur adipisicing elit,
     sed do eiusmod tempor        
     incididunt ut labore et      
     dolore magna aliqua. Ut enim 
     ad minim veniam, quis nostrud
     exercitation ullamco laboris 
     nisi ut aliquip ex ea commodo
     consequat. Duis aute irure   
     dolor in reprehenderit in    
     voluptate velit esse cillum  
     dolore eu fugiat nulla       
     pariatur. Excepteur sint     
     occaecat cupidatat non       
     proident, sunt in culpa qui  
     officia deserunt mollit anim 
     id est laborum.              
----------------------------------

Table: Foo Bar

```

# Brew to Pandoc

Everyone knows and possibly uses [brew](http://cran.r-project.org/web/packages/brew/index.html) but if you would need some smack, the following links really worth visiting:

  * [slides on "Building a reporting sytem with BREW"](http://www.slideshare.net/xavierguardiola/building-a-reporting-sytem-with-brew)
  * [learnr blogpost on brew](http://learnr.wordpress.com/2009/09/09/brew-creating-repetitive-reports/)

**In short**: a `brew` document is a simple text file with some special tags. `Pandoc.brew` uses only two of them internally - but of course you could use `brew templates` (`<\%\%...\%\%>`) too:

  * `<\% ... \%>` (without the backslash) stand for running R calls
  * `<\%= ... \%>` (without the backslash) does pretty the same but applies `pander` to the returning R object (instead of `cat` like the original `brew` function does). So putting there any R object would return is a nice Pandoc markdown format with all possible messages etc.

This latter tries to be smart in some ways:

  * a block (R commands between the tags) could return values in any part of the block
  * plots and images are grabbed in the document, rendered to a `png` file and `pander` method would result in a Pandoc markdown formatted image link (so the image would be shown/included in the exported document).
  * all warnings/messages and errors are recorded in the blocks and returned in the document as a footnote
  * all heavy R commands (e.g. those taking more then 0.1 sec to evaluate) are **cached** so re`brew`ing a report would not result in a coffee break.

This document was generated by `Pandoc.brew` based on [`inst/README.brew`](https://github.com/daroczig/pander/blob/master/inst/README.brew) so the above examples were generated automatically - which could be handy while writing some nifty statistical reports :)

```
Pandoc.brew(system.file('README.brew', package='pander'))
```

`Pandoc.brew` could cook a file (default) or work with a character vector provided in the `text` argument. The output is set to `stdout` by default, it could be tweaked to write result to a text file and run Pandoc on that to create a `HTML`, `odt`, `docx` or other document.

To export a brewed file to other then Pandoc's markdown, please use the `convert` parameter. For example (please disregard the backslash in front of the percent sign):

```r
text <- paste('# Header', '', 'What a lovely list:\n<\%=as.list(runif(10))\%>', 'A wide table:\n<\%=mtcars[1:3, ]\%>', 'And a nice chart:\n\n<\%=plot(1:10)\%>', sep = '\n')
Pandoc.brew(text = text, output = tempfile(), convert = 'html')
Pandoc.brew(text = text, output = tempfile(), convert = 'pdf')
```

Of course a text file could work as input (by default) the above example uses `text` parameter as a reproducible example. For example brewing this README with all R chunks and converted  to html, please run:

```r
Pandoc.brew(system.file('README.brew', package='pander'), output = tempfile(), convert = 'html')
```

And there are some package bundled examples too.

## Examples

The package comes bundled with some examples for `Pandoc.brew` to let users check out its features pretty fast. These are:

  * [minimal.brew](https://github.com/daroczig/pander/blob/master/inst/examples/minimal.brew)
  * [short-code-long-report.brew](https://github.com/daroczig/pander/blob/master/inst/examples/short-code-long-report.brew)

To `brew` these examples on your machine try to run the followings.:

```r
Pandoc.brew(system.file('examples/minimal.brew', package='pander'))
Pandoc.brew(system.file('examples/minimal.brew', package='pander'), output = tempfile(), convert = 'html')

Pandoc.brew(system.file('examples/short-code-long-report.brew', package='pander'))
Pandoc.brew(system.file('examples/short-code-long-report.brew', package='pander'), output = tempfile(), convert = 'html')
```

For easy access I have uploaded some exported documents of the above examples:

  * minimal.brew: [markdown](minimal.md) [html](http://daroczig.github.com/pander/minimal.html) [pdf](http://daroczig.github.com/pander/minimal.pdf) [odt](http://daroczig.github.com/pander/minimal.odt) [docx](http://daroczig.github.com/pander/minimal.docx)
  * short-code-long-report.brew: [markdown](http://daroczig.github.com/pander/short-code-long-report.md) [html](http://daroczig.github.com/pander/short-code-long-report.html) [pdf](http://daroczig.github.com/pander/short-code-long-report.pdf) [odt](http://daroczig.github.com/pander/short-code-long-report.odt) [docx](http://daroczig.github.com/pander/short-code-long-report.docx)

Please check out `pdf`, `docx`, `odt` and other formats (changing the above `convert` option) on your machine too and do not forget to [give some feedback](https://github.com/daroczig/pander/issues)!

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

## Or a "large" data frame which barely fits on a page
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

# ESS

I have created some simple LISP functions which would be handy if you are using the best damn IDE for R. These functions and default key-bindings are [shipped with the package](https://github.com/daroczig/pander/tree/master/inst/pander.el), feel free to personalize:

  * `pander-brew` (`C-c p b`): Run `Pandoc.brew` on current buffer and show results in *ess-output* while setting working directory to `tempdir()` temporary.
  * `pander-brew-to-HTML` (`C-c p B`): Like the above function but exporting results to HTML too (and opening that automatically in default browser).
  * `pander-evals-region` (`C-c p e`): Run `evals` on region and show results in *ess-output* while setting working directory to `tempdir()` temporary.
  * `pander-region` (`C-c p r`): Run `pander` (after `eval`) on region and show results in *ess-output* while setting working directory to `tempdir()` temporary.
  * `pander-chunk` (`C-c p c`): Run `pander` (after `eval`) on current chunk (in which the pointer is) and show results in *ess-output* while setting working directory to `tempdir()` temporary. Chunk is **recognized** by opening and closing `brew` tags.
  * `pander-region-or-chunk` (`C-c p p`): IMHO the most useful function :)

	Run `pander` (after `eval`) on region **or** current chunk (if marker is not set) and show results in *ess-output* while setting working directory to `tempdir()` temporary. Chunk is recognized by opening and closing `brew` tags.

  * `pander-region-export` (`C-c p R`): Run `pander` (after `eval`) on region and convert results specified format in minibuffer with auto-complete.
  * `pander-chunk-export` (`C-c p C`): Run `pander` (after `eval`) on current chunk (in which the pointer is) and convert results specified format in minibuffer. Chunk is **recognized** by opening and closing `brew` tags with auto-complete.
  * `pander-region-or-chunk-export` (`C-c p P`): Run `pander` (after `eval`) on region **or** current chunk (if marker is not set) and convert results specified format in minibuffer with auto-complete. Chunk is recognized by opening and closing `brew` tags.

These functions are rather a POC of what could be done with `pander` to speed up writing reports, but having great potentials - and besides that: doing a great job right now too (for me).

## pander-mode

The above functions are to be removed from `pander.el` as I am currently working on `pander-mode` with options and much useful functions. ATM there are only three of them:

  * `pander-brew` (`C-c p b`): Run `Pandoc.brew` on current buffer or region (if mark is active), show results in *ess-output* and (optionally) copy results to clipboard while setting working directory to tempdir() temporary.
  * `pander-brew-export` (`C-c p B`): Run `Pandoc.brew` on current buffer or region (if mark is active) and export results to specified (auto-complete in minibuffer) format. Also tries to open exported document.
  * `pander-eval` (`C-c p e`): Run `pander` on (automatically evaluated) region *or* current chunk (if marker is not set), show results (of last returned R object) in `*ess-output*` and (optionally) copy those to clipboard while setting working directory to `tempdir()` temporary.

And a few options: `M-x customize-group pander`

  * `pander-clipboard`: If non-nil then the result of `pander-*` functions would be copied to clipboard.
  * `pander-show-source`: If non-nil then the source of R commands would also show up in generated documents while running 'pander-eval'. This would not affect `brew` functions ATM.


-------
This report was generated with [R](http://www.r-project.org/) (2.15.0) and [pander](https://github.com/daroczig/pander) (0.1) in 0.538 sec on x86_64-unknown-linux-gnu platform.