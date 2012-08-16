% [pander: An R Pandoc writer](https://github.com/Rapporter/pander)

**pander** is an [R](http://r-project.org) package containing [helpers](#helper-functions) to return [Pandoc](http://johnmacfarlane.net/pandoc/)'s markdown of user specified format or *automatically* of several type of [**R objects**](#generic-pander-method).

The package is also capable of exporting/converting complex Pandoc documents (reports) in three ways ATM:

  * create somehow a markdown text file (e.g. with `brew`, `knitr` or any scripts of yours, maybe with `Pandoc.brew` - see just below) and transform that to other formats (like HTML, odt, pdf, docx etc.) with `Pandoc.convert`

  * users might write some reports in [brew](http://cran.r-project.org/web/packages/brew/index.html) syntax resulting in a pretty *Pandoc* document (where each R object are transformed to nicely formatted table, list etc.) and also in a **bunch of formats** (like HTML, odt, pdf, docx etc.) automatically.

    *Example*: this [`README.md`](https://github.com/Rapporter/pander/blob/master/README.md) is cooked with `Pandoc.brew` based on [`inst/README.brew`](https://github.com/Rapporter/pander/blob/master/inst/README.brew) and also exported to [HTML](http://rapporter.github.com/pander/). Details can be found [below](#brew-to-pandoc) or head directly to [examples](#examples).

<!-- endlist -->

 * or users might create a report in a live R session by adding some R objects and paragraphs to a `Pandoc` reference class object. Details can be found [below](#live-report-generation).

**How it is differ from Sweave, brew, knitr, R2HTML etc.?**

  * no need for calling `ascii`, `xtable`, `Hmisc`, `tables` etc. to transform an R object to `HTML`, `tex` etc. as `pander` results in Pandoc's *markdown* which can (automatically) be converted to almost any text document format (like: pdf, HTML, odt, docx, textile, asciidoc, reStructuredText etc.). Conversion can be done automatically after calling `pander` reporting functions ([Pander.brew](#brew-to-pandoc) or [Pandoc](#live-report-generation)).
  * based on the above *no "traditional" R console output* is shown in the resulting document (nor in markdown, nor in exported docs) but **all R objects are transformed to tables, list etc**. Well, there is an option (`show.src`) to show the original R commands before the formatted output, and `pander`˛calls can be also easily tweaked (just file an issue) to return `print`ed R objects - if you would need that in some strange situation - like writing an R tutorial. But **I really think that nor R code, nor raw R results have anything to do with an exported report** :)
  * *graphs/plots* are recognized in blocks of R commands without any special setting or marks around code block and saved to disk in a `png` file linked in the resulting document. This means if you create a report (e.g. `brew` a text file) and export it to pdf/docx etc. all the plots/images would be there. There are some parameters to specify the resolution of the image and also the type (e.g. `jpg`, `svg` or `pdf`) besides a **wide variety of [theme options](#pander-options)**. About the latter, please check out `graphs.brew` [below](#examples).
  * `pander`˛uses its build in (IMHO quite decent) [**caching**](#caching). This means that if evaluation of some R commands take too much time (which can be set by option/parameter), then the results are saved in a file and returned from there on next exact R code's evaluation. This caching algorithm tries to be **smart** as checks not only the passed R sources, but *all variables and functions* inside that and saves the hash of those. This is a quite secure way of caching (see details [below](#caching)), but if you would encounter any issues, just switch off the cache. I've not seen any issues :)
  * `knitr` *support* is coming too, for details see my [TODO list](https://github.com/Rapporter/pander/blob/master/TODO.md) **Update**: just use `knitr` to generate markdown and pass that to `Pandoc.convert`

# Installation

Currently, this package is hosted only on [GitHub](https://github.com/Rapporter/pander).

Until this gets on [CRAN](cran.r-project.org), you can install it via nifty function from `devtools` package:

```
library(devtools)
install_github('pander', 'Rapporter')
```

Or download the [sources in a zip file](https://github.com/Rapporter/pander/zipball/master) and build manually. If you're running R on Windows, you need to install [Rtools](http://cran.stat.ucla.edu/bin/windows/Rtools/).

## Depends

`pander` heavily builds on [Pandoc](http://johnmacfarlane.net/pandoc) which should be **pre-installed** before trying to convert your reports to [different formats](http://johnmacfarlane.net/pandoc/). Although **main functions work without Pandoc**, e.g. you can generate a markdown formatted report via [Pandoc.brew](#brew-to-pandoc) or the custom [reference class](#live-report-generation), but I would really suggest to install that really great piece of software!

The [installation process of Pandoc](http://johnmacfarlane.net/pandoc/installing.html) is quite straightforward on most operating systems: just download and run the binary (a few megabytes), and get a full-blown document converted in a few seconds/minutes. On different Linux distributions it might be a bit more complicated (as repositories tend to provide out-dated versions of Pandoc, so you would need `cabal-install` to [install from sources](https://github.com/jgm/pandoc/wiki/Installing-the-development-version-of-pandoc-1.9)). *Please do not forget to restart your R session to update your `PATH` after installation!*

An alternative method (bypassing Pandoc dependency) would be to call the awesome [`markdown`](http://cran.r-project.org/web/packages/markdown/index.html) package to transform markdown (as far as I know: exclusively) to HTML.

~~And as `pander` and `rapport` are quite Siamese twins, you would need an **up-to-date** version of [rapport](http://rapport-package.info) most likely installed from [Github](https://github.com/aL3xa/rapport).~~ `pander` now can work independently from `rapport`.

Now you would only need a few cool packages from CRAN:

  * [digest](http://cran.r-project.org/web/packages/digest/index.html) to compute hashes while caching
  * ~~[brew](http://cran.r-project.org/web/packages/brew/index.html) for literate programming~~
  * ~~[parser](http://cran.r-project.org/web/packages/parser/index.html) to identify variables in passed R commands~~
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

--------------------------------------
&nbsp;              mpg   cyl   disp  
------------------- ----- ----- ------
**Mazda RX4**       21    6     160   

**Mazda RX4 Wag**   21    6     160   
--------------------------------------

```

  * `simple` tables do not support line breaks in cells:

```rout
> pandoc.table(m, style = "simple")

&nbsp;              mpg   cyl   disp  
------------------- ----- ----- ------
**Mazda RX4**       21    6     160   
**Mazda RX4 Wag**   21    6     160   

```

  * `grid` format are really handy for [emacs](http://emacswiki.org/emacs/TableMode) users but do support line breaks inside cells, but do not tolerate cell alignment:

```rout
> pandoc.table(m, style = "grid")

+---------------------+-------+-------+--------+
| &nbsp;              | mpg   | cyl   | disp   |
+=====================+=======+=======+========+
| **Mazda RX4**       | 21    | 6     | 160    |
+---------------------+-------+-------+--------+
| **Mazda RX4 Wag**   | 21    | 6     | 160    |
+---------------------+-------+-------+--------+

```

Besides the `style` parameter there are several other ways to tweak the output like `decimal.mark` or `digits`. And of course it's really easy to add a **caption**:

```rout
> pandoc.table(m, style = "grid", caption = "Hello caption!")

+---------------------+-------+-------+--------+
| &nbsp;              | mpg   | cyl   | disp   |
+=====================+=======+=======+========+
| **Mazda RX4**       | 21    | 6     | 160    |
+---------------------+-------+-------+--------+
| **Mazda RX4 Wag**   | 21    | 6     | 160    |
+---------------------+-------+-------+--------+

Table: Hello caption!

```

`pandoc.table`˙can deal with the problem of really **wide tables**. Ever had an issue in LaTeX or MS Word when tried to print a correlation matrix of 40 variables? Not a problem any more as `pandoc.table` splits up the table if wider then 80 characters and handles caption too:

```rout
> pandoc.table(mtcars[1:2, ], style = "grid", caption = "Hello caption!")

+---------------------+-------+-------+--------+------+--------+-------+
| &nbsp;              | mpg   | cyl   | disp   | hp   | drat   | wt    |
+=====================+=======+=======+========+======+========+=======+
| **Mazda RX4**       | 21    | 6     | 160    | 110  | 3.9    | 2.620 |
+---------------------+-------+-------+--------+------+--------+-------+
| **Mazda RX4 Wag**   | 21    | 6     | 160    | 110  | 3.9    | 2.875 |
+---------------------+-------+-------+--------+------+--------+-------+

Table: Hello caption! (continued below)

 

+---------------------+--------+------+------+--------+--------+
| &nbsp;              | qsec   | vs   | am   | gear   | carb   |
+=====================+========+======+======+========+========+
| **Mazda RX4**       | 16.46  | 0    | 1    | 4      | 4      |
+---------------------+--------+------+------+--------+--------+
| **Mazda RX4 Wag**   | 17.02  | 0    | 1    | 4      | 4      |
+---------------------+--------+------+------+--------+--------+

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

  * **a**: _1_
  * **b**: _2_
  * **c**:

    -------
    0   1
    --- ---
    19  13
    -------

  * **x**:

      * **myname**: _1_
      * _2_

  * _56_

<!-- end of list -->

```

A nested list can be seen above with a table and all (optional) list names inside. As a matter of fact `pander.list` is the default method of `pander` too, see:

```rout
> x <- chisq.test(table(mtcars$am, mtcars$gear))
> class(x) <- "I've never heard of!"
> pander(x)
 **WARNING**^[Chi-squared approximation may be incorrect]

  * **statistic**: _20.94_
  * **parameter**: _2_
  * **p.value**: _2.831e-05_
  * **method**: Pearson's Chi-squared test
  * **data.name**: table(mtcars$am, mtcars$gear)
  * **observed**:

    -------------------
    &nbsp;  3   4   5
    ------- --- --- ---
    **0**   15  4   0

    **1**   0   8   5
    -------------------

  * **expected**:

    -------------------------
    &nbsp;  3     4     5
    ------- ----- ----- -----
    **0**   8.906 7.125 2.969

    **1**   6.094 4.875 2.031
    -------------------------

  * **residuals**:

    ----------------------------
    &nbsp;  3      4      5
    ------- ------ ------ ------
    **0**   2.042  -1.171 -1.723

    **1**   -2.469 1.415  2.083
    ----------------------------

  * **stdres**:

    ----------------------------
    &nbsp;  3      4      5
    ------- ------ ------ ------
    **0**   4.395  -2.323 -2.943

    **1**   -4.395 2.323  2.943
    ----------------------------

<!-- end of list -->

```

So `pander` showed a not known class in an (almost) user-friendly way. And we got some warnings too styled with [Pandoc **footnote**](http://johnmacfarlane.net/pandoc/README.html#footnotes)! If that document is exported to e.g. `HTML` or `pdf`, then the error/warning message could be found on the bottom of the page with a link. *Note*: there were two warnings in the above call - both captured and returned! Well, this is the feature of `Pandoc.brew`, see [below](#brew-to-pandoc).

The output of different **statistical methods** are tried to be prettyfied. Some examples:

```rout
> pander(ks.test(runif(50), runif(50)))

---------------------------------------------------
 Test statistic   P value   Alternative hypothesis 
---------------- --------- ------------------------
      0.14        0.7166          two-sided        
---------------------------------------------------

Table: Two-sample Kolmogorov-Smirnov test: `runif(50)` and `runif(50)`

> pander(chisq.test(table(mtcars$am, mtcars$gear)))

-------------------------------
 Test statistic   df   P value 
---------------- ---- ---------
     20.94        2   2.831e-05
-------------------------------

Table: Pearson's Chi-squared test: `table(mtcars$am, mtcars$gear)`

 **WARNING**^[Chi-squared approximation may be incorrect]

> pander(t.test(extra ~ group, data = sleep))

---------------------------------------------------------
 Test statistic   df    P value   Alternative hypothesis 
---------------- ----- --------- ------------------------
     -1.861      17.78  0.07939         two.sided        
---------------------------------------------------------

Table: Welch Two Sample t-test: `extra` by `group`

> ## Dobson (1990) Page 93: Randomized Controlled Trial (examples from: ?glm)
> counts <- c(18,17,15,20,10,20,25,13,12)
> outcome <- gl(3,1,9)
> treatment <- gl(3,3)
> m <- glm(counts ~ outcome + treatment, family=poisson())
> pander(m)

---------------------------------------------------------------
           &nbsp;  Estimate   Std. Error   z value    Pr(>|z|) 
----------------- ---------- ------------ ---------- ----------
  **(Intercept)** 3.045e+00   1.709e-01   1.781e+01  5.427e-71 

     **outcome2** -4.543e-01  2.022e-01   -2.247e+00 2.465e-02 

     **outcome3** -2.930e-01  1.927e-01   -1.520e+00 1.285e-01 

   **treatment2** 1.338e-15   2.000e-01   6.690e-15  1.000e+00 

   **treatment3** 1.421e-15   2.000e-01   7.105e-15  1.000e+00 
---------------------------------------------------------------

Table: Fitting generalized (poisson/log) linear model: counts ~ outcome + treatment

> pander(anova(m))

--------------------------------------------------------
         &nbsp;  Df   Deviance   Resid. Df   Resid. Dev 
--------------- ---- ---------- ----------- ------------
       **NULL**  NA      NA          8         10.581   

    **outcome**  2   5.452e+00       6         5.129    

  **treatment**  2   2.665e-15       4         5.129    
--------------------------------------------------------

Table: Analysis of Deviance Table

> pander(aov(m))

-----------------------------------------------------------
         &nbsp;  Df   Sum Sq    Mean Sq   F value   Pr(>F) 
--------------- ---- --------- --------- --------- --------
    **outcome**  2   9.267e+01 4.633e+01 2.224e+00  0.2242 

  **treatment**  2   8.382e-31 4.191e-31 2.012e-32  1.0000 

  **Residuals**  4   8.333e+01 2.083e+01    NA        NA   
-----------------------------------------------------------

Table: Analysis of Variance Model

> pander(prcomp(USArrests))

-------------------------------------------------
&nbsp;         PC1     PC2      PC3      PC4     
-------------- ------- -------- -------- --------
**Murder**     0.04170 -0.04482 0.07989  -0.99492

**Assault**    0.99522 -0.05876 -0.06757 0.03894 

**UrbanPop**   0.04634 0.97686  -0.20055 -0.05817

**Rape**       0.07516 0.20072  0.97408  0.07233 
-------------------------------------------------

Table: Principal Components Analysis

--------------------------------------------------------------
&nbsp;                       PC1      PC2      PC3     PC4    
---------------------------- -------- -------- ------- -------
**Standard deviation**       83.73240 14.21240 6.48943 2.48279

**Proportion of Variance**   0.96553  0.02782  0.00580 0.00085

**Cumulative Proportion**    0.96553  0.99335  0.99915 1.00000
--------------------------------------------------------------

> pander(density(mtcars$hp))

--------------------------------------------
       &nbsp;  Coordinates   Density values 
------------- ------------- ----------------
     **Min.**    -32.12        0.0000050    

  **1st Qu.**     80.69        0.0004068    

   **Median**    193.50        0.0016650    

     **Mean**    193.50        0.0022140    

  **3rd Qu.**    306.30        0.0040900    

     **Max.**    419.10        0.0060510    
--------------------------------------------

Table: Kernel density of *mtcars$hp* (bandwidth: 28.04104)

> ## Don't like scientific notation?
> panderOptions('round', 2)
> pander(density(mtcars$hp))

--------------------------------------------
       &nbsp;  Coordinates   Density values 
------------- ------------- ----------------
     **Min.**    -32.12           0.00      

  **1st Qu.**     80.69           0.00      

   **Median**    193.50           0.00      

     **Mean**    193.50           0.00      

  **3rd Qu.**    306.30           0.00      

     **Max.**    419.10           0.01      
--------------------------------------------

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

**In short**: a `brew` document is a simple text file with some special tags. `Pandoc.brew` uses only two of them (as building on a personalized version of Jeff's really great `brew` function):

  * `<\% ... \%>` (without the backslash) stand for running R calls
  * `<\%= ... \%>` (without the backslash) does pretty the same but applies `pander` to the returning R object (instead of `cat` like the original `brew` function does). So putting there any R object would return is a nice Pandoc's markdown format with all possible messages etc.

This latter tries to be smart in some ways:

  * a block (R commands between the tags) could return values in any part of the block
  * plots and images are grabbed in the document, rendered to a `png` file and `pander` method would result in a Pandoc markdown formatted image link (so the image would be shown/included in the exported document).
  * all warnings/messages and errors are recorded in the blocks and returned in the document as a footnote
  * all heavy R commands (e.g. those taking more then 0.1 sec to evaluate) are [**cached**](#caching) so re`brew`ing a report would not result in a coffee break.

Besides this, the custom `brew` function can do more and also less compared to the original [`brew` package](http://cran.r-project.org/web/packages/brew/index.html). First of all the internal caching mechanism (and other, from `pander` package POV needless features) of `brew` is removed for some extra profits:

  * multiple R expressions can be passed between `<\%= ... \%>` tags,
  * the text of the file and also **the evaluated R objects are (invisibly) returned in a structured list**, which can be really useful while post-processing the results of `brew` (just try: `str(Pandoc.brew(text='Pi equals to <\%=pi\%>.\nAnd here are some random data:\n<\%=runif(10)\%>'))` - without the backslash in front of the percent signs).

This document was generated by `Pandoc.brew` based on [`inst/README.brew`](https://github.com/Rapporter/pander/blob/master/inst/README.brew) so the above examples were generated automatically - which could be handy while writing some nifty statistical reports :)

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

  * [minimal.brew](https://github.com/Rapporter/pander/blob/master/inst/examples/minimal.brew)
  * [short-code-long-report.brew](https://github.com/Rapporter/pander/blob/master/inst/examples/short-code-long-report.brew)
  * [graphs.brew](https://github.com/Rapporter/pander/blob/master/inst/examples/graphs.brew)

To `brew` these examples on your machine try to run the followings.:

```r
Pandoc.brew(system.file('examples/minimal.brew', package='pander'))
Pandoc.brew(system.file('examples/minimal.brew', package='pander'), output = tempfile(), convert = 'html')

Pandoc.brew(system.file('examples/short-code-long-report.brew', package='pander'))
Pandoc.brew(system.file('examples/short-code-long-report.brew', package='pander'), output = tempfile(), convert = 'html')

Pandoc.brew(system.file('examples/graphs.brew', package='pander'))
Pandoc.brew(system.file('examples/graphs.brew', package='pander'), output = tempfile(), convert = 'html')
```

For easy access I have uploaded some exported documents of the above examples:

  * minimal.brew: [markdown](http://rapporter.github.com/pander/minimal.md) [html](http://rapporter.github.com/pander/minimal.html) [pdf](http://rapporter.github.com/pander/minimal.pdf) [odt](http://rapporter.github.com/pander/minimal.odt) [docx](http://rapporter.github.com/pander/minimal.docx)
  * short-code-long-report.brew: [markdown](http://rapporter.github.com/pander/short-code-long-report.md) [html](http://rapporter.github.com/pander/short-code-long-report.html) [pdf](http://rapporter.github.com/pander/short-code-long-report.pdf) [odt](http://rapporter.github.com/pander/short-code-long-report.odt) [docx](http://rapporter.github.com/pander/short-code-long-report.docx)
  * graphs.brew: [markdown](http://rapporter.github.com/pander/graphs.md) [html](http://rapporter.github.com/pander/graphs.html) [pdf](http://rapporter.github.com/pander/graphs.pdf) [odt](http://rapporter.github.com/pander/graphs.odt) [docx](http://rapporter.github.com/pander/graphs.docx)

Please check out `pdf`, `docx`, `odt` and other formats (changing the above `convert` option) on your machine too and do not forget to [give some feedback](https://github.com/Rapporter/pander/issues)!

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

# Pander options

`pander` comes with some globally adjustable options which would have an effect on the result of your reports. You can query and update these options with `panderOptions()`:

  * `digits`: numeric (default: `2`) passed to `format`
  * `decimal.mark`: numeric (default: `.`) passed to `format`
  * `round`: numeric (default: `Inf`) passed to `round`
  * `date`: string (default: `'%Y/%m/%d %X'`) passed to `format` when printing dates (`POSIXct` or `POSIXt`)
  * `header.style`: `'atx'` or `'setext'` passed to `pandoc.header`
  * `list.style`: `'bullet'` (default), `'ordered'` or `'roman'` passed to `pandoc.list`. Please not that this has no effect on `pander` methods.
  * `table.style`: `'multiline'`, `'grid'` or `'simple'` passed to `pandoc.table`
  * `table.split.table`: numeric passed to `pandoc.table` and also affects `pander` methods. This option tells `pander` where to split too wide tables. The default value (`80`) suggests the conventional number of characters used in a line, feel free to change (e.g. to `Inf` to disable this feature) if you are not using a VT100 terminal any more :)
  * `table.split.cells`: numeric (default: `30`) passed to `pandoc.table` and also affects `pander` methods. This option tells `pander` where to split too wide cells with line breaks. Set `Inf` to disable.
  * `evals.messages`: boolean (default: `TRUE`) passed to `evals`' `pander` method specifying if messages should be rendered
  * `p.wrap`: a string (default:`'_'`) to wrap vector elements passed to `p` function
  * `p.sep`: a string (default: `', '`) with the main separator passed to `p` function
  * `p.copula`: a string (default: `'and'`) a string with ending separator passed to `p` function
  * `graph.nomargin`: boolean (default: `TRUE`) if trying to keep plots' margins at minimal
  * `graph.fontfamily`: string (default: `'sans'`) specifying the font family to be used in images. Please note, that using a custom font on Windows requires `grDevices:::windowsFonts` first.
  * `graph.fontcolor`: string (default: `'black'`) specifying the default font color
  * `graph.fontsize`: numeric (default: `12`) specifying the *base* font size in pixels. Main title is rendered with `1.2` and labels with `0.8` multiplier.
  * `graph.grid`: boolean (default: `TRUE`) if a grid should be added to the plot
  * `graph.grid.minor`: boolean (default: `TRUE`) if a miner grid should be also rendered
  * `graph.grid.color`: string (default: `'grey'`) specifying the color of the rendered grid
  * `graph.grid.lty`: string (default: `'dashed'`) specifying the line type of grid
  * `graph.boxes`: boolean (default: `FALSE`) if to render a border around of plot (and e.g. around strip)
  * `graph.legend.position`: string (default: `'right'`) specifying the position of the legend: 'top', 'right', 'bottom' or 'left'
  * `graph.background`: string (default: `'white'`) specifying the plots main background's color
  * `graph.panel.background`: string (default: `'transparent'`) specifying the plot's main panel background. Please *note*, that this option is not supported with `base` graphics.
  * `graph.colors`: character vector of default color palette (defaults to a [colorblind theme](http://jfly.iam.u-tokyo.ac.jp/color/)). Please *note* that this update work with `base` plots by appending the `col` argument to the call if not set.
  * `graph.color.rnd`: boolean (dafault: `FALSE`) specifying if the palette should be reordered randomly before rendering each plot to get colorful images
  * `graph.axis.angle`: numeric (default: `1`) specifying the angle of axes' labels. The available options are based on `par(les)` and sets if the labels should be:

      *  `1`: parallel to the axis,
      *  `2`: horizontal,
      *  `3`: perpendicular to the axis or
      *  `4`: vertical.

  *  `graph.symbol`: numeric (default: `1`) specifying a symbol (see the `pch` parameter of `par`)

Besides localization of numeric formats, table/list's and plots' styles there are some technical options too which would effect e.g. [caching](#caching) or the format of rendered image files. You can query/update those with `evalsOptions()` as the main backend of `pander` calls is a custom evaluation function called [`evals`](#evals).

The list of possible options are:

  * `parse`: if `TRUE` the provided `txt` elements would be merged into one string and parsed to logical chunks. This is useful if you would want to get separate results of your code parts - not just the last returned value, but you are passing the whole script in one string. To manually lock lines to each other (e.g. calling a `plot` and on next line adding an `abline` or `text` to it), use a plus char (`+`) at the beginning of each line which should be evaluated with the previous one(s). If set to `FALSE`, [`evals`](#evals) would not try to parse R code, it would get evaluated in separate runs - as provided. Please see the documentation of [`evals`](#evals).
  * `cache`: [caching](#caching) the result of R calls if set to `TRUE`
  * `cache.mode`: cached results could be stored in an `environment` in _current_ R session or let it be permanent on `disk`.
  * `cache.dir`: path to a directory holding cache files if `cache.mode` set to `disk`. Default set to `.cache` in current working directory.
  * `cache.time`: number of seconds to limit caching based on `proc.time`. If set to `0`, all R commands, if set to `Inf`, none is cached (despite the `cache` parameter).
  * `cache.copy.images`: copy images to new file names if an image is returned from the *disk* cache? If set to `FALSE` (default), the cached path would be returned.
  * `classes`: a vector or list of classes which should be returned. If set to `NULL` (by default) all R objects will be returned.
  * `hooks`: list of hooks to be run for given classes in the form of `list(class = fn)`. If you would also specify some parameters of the function, a list should be provided in the form of `list(fn, param1, param2=NULL)` etc. So the hooks would become `list(class1=list(fn, param1, param2=NULL), ...)`. See example of [`evals`](#evals) for more details. A default hook can be specified too by setting the class to `'default'`. This can be handy if you do not want to define separate methods/functions to each possible class, but automatically apply the default hook to all classes not mentioned in the list. You may also specify only one element in the list like: `hooks=list('default' = pander.return)`. Please note, that nor error/warning messages, nor stdout is captured (so: updated) while running hooks!
  * `length`: any R object exceeding the specified length will not be returned. The default value (`Inf`) does not filter out any R objects.
  * `output`: a character vector of required returned values. This might be useful if you are only interested in the `result`, and do not want to save/see e.g. `messages` or `print`ed `output`. See examples of [`evals`](#evals).
  * `graph.unify`: should `evals` try to unify the style of (`base`, `lattice` and `ggplot2`) plots? If set to `TRUE`, some `panderOptions()` would apply. By default this is disabled not to freak out useRs :)
  * `graph.name`: set the file name of saved plots which is `%s` by default. A simple character string might be provided where `%d` would be replaced by the index of the generating `txt` source, `%n` with an incremented integer in `graph.dir` with similar file names and `%t` by some unique random characters. A function's name to be `eval`uated can be passed here too.
  * `graph.dir`: path to a directory where to place generated images. If the directory does not exist, [`evals`](#evals) try to create that. Default set to `plots` in current working directory.
  * `graph.output`: set the required file format of saved plots. Currently it could be any of  `grDevices`: `png`, `bmp`, `jpeg`, `jpg`, `tiff`, `svg` or `pdf`.
  * `width`: width of generated plot in pixels for even vector formats
  * `height`: height of generated plot in pixels for even vector formats
  * `res`: nominal resolution in `ppi`. The height and width of vector images will be calculated based in this.
  * `hi.res`: generate high resolution plots also? If set to `TRUE`, each R code parts resulting an image would be run twice.
  * `hi.res.width`: width of generated high resolution plot in pixels for even vector formats. The `height` and `res` of high resolution image is automatically computed based on the above options to preserve original plot aspect ratio.
  * `graph.env`: save the environments in which plots were generated to distinct files (based on `graph.name`) with `env` extension?
  * `graph.recordplot`: save the plot via `recordPlot` to distinct files (based on `graph.name`) with `recodplot` extension?
  * `graph.RDS` save the raw R object returned (usually with `lattice` or `ggplot2`) while generating the plots to distinct files (based on `graph.name`) with `RDS` extension?

# Caching

As `pander` is using a **custom caching algorithm**, it might be worthwhile to give a short summary of what is going on in the background.

All evaluation of provided R commands (while running [`brew`](#brew-to-pandoc) or ["live report generation"](#live-report-generation) is done by [`evals`](#evals) which is caching results (*besides* returned informative/warning/error messages, anything written to `stdout` etc. - see [below](#evals)) **line-by-line** (to be more accurate: by single R commands) instead of caching chunks **without any noticeable overhead**.

## Theoretical background

  * Each passed R chunk is `parse`d to single commands (`expressions`).
  * Each parsed expression's **part** (let it be a function, variable, constant etc.) `eval`uated (as a `name`) separately to a `list`. This list describes the unique structure and the content of the passed R expressions, and has some IMHO really great benefits (see below).
  * A **hash** if computed to each list element and *cached* too in `pander`'s local environments. This is useful if you are using large data frames, just imagine: the caching algorithm would have to compute the hash for the same data frame each time it's touched! This way the hash is recomputed only if the R object with the given name is changed.
  * The list is `serialize`d and an `SHA-1` hash is computed for that - which is unique and there is no real risk of collision.
  * If [`evals`](#evals) can find the cached results in an environment of `pander`'s namespace (if `cache.mode` set to `enviroment` - see [above](#pander-options)) or in a file named to the computed hash (if `ċache.mode` set to `disk`), then it is returned on the spot. *The objects modified/created by the cached code are also updated.*
  * Otherwise the call is evaluated and the results and the modified R objects of the environment are optionally saved to cache (e.g. if `cache` is active, if the `proc.time()` of the evaluation is higher then it is defined in `cache.time` etc. - see details in [evals' options](#pander-options)).

## In practice

As `pander` does not cache based on raw sources of chunks and there is no easy way of enabling/disabling caching on a chunk basis, the users have to live with some *great advantages* and some *minor tricky situations* - which latter cannot be solved theoretically in my opinion, but [I'd love to hear your feedback](https://github.com/Rapporter/pander/issues).

The caching hash is computed based on the structure and content of the R commands, so let us make some POC example to show the **greatest asset**:

```r
require(lattice)
evals('histogram(rep(mtcars$hp, 1e5))')
```

It took a while, huh? :)

Let us have some custom functions and variables:

```r
f <- histogram
g <- rep
A <- mtcars$hp
B <- 1e5
```

And now try to run something like:

```r
evals('f(g(A, B))')
```

Yes, it was returned from cache!

About the **kickback**:

As `pander` (or rather: `evals`) does not really deal with what is written in the provided sources but rather checks what is **inside that**, there might be some tricky situations where you would expect the cache to work, but it would not. Short example: we are computing and saving to a variable something heavy in a chunk (please run these in a clean R session to avoid conflicts):

```r
evals('x <- sapply(rep(mtcars$hp, 1e3), mean)')
```

It is cached, just run again, you will see.

But if you would create `x` in your *global environment* with any value (which has nothing to do with the special environment of the report!) **and** `x` was not defined in the report before this call (**and** you had no `x` value in your global environment before), then the content of `x` would result in a new hash for the cache - so caching would not work. E.g.:

```r
x <- 'foobar'
evals('x <- sapply(rep(mtcars$hp, 1e3), mean)')
```

I really think this is a minor issue (with very special coincidences) which cannot be addressed cleverly - but **could be avoided with some cautions** (e.g. run `Pandoc.brew` in a clean R session like with `Rscript` or [`littler`](http://dirk.eddelbuettel.com/code/littler.html) - if you are really afraid of this issue). And after all: you loose nothing, just the cache would not work for that only line and only once in most of the cases.

# Evals

Sorry, no online documentation ATM. Please check: `?evals`

# ESS

I have created some simple LISP functions which would be handy if you are using the best damn IDE for R. These functions and default key-bindings are [shipped with the package](https://github.com/Rapporter/pander/tree/master/inst/pander.el), feel free to personalize.

As time passed these small functions grew heavier (with my Emacs knowledge) so I ended up with a small library:

## pander-mode

I am currently working on `pander-mode` which is a small *minor-mode* for Emacs. There are a few (but useful) functions with default keybindings:

  * `pander-brew` (`C-c p b`): Run `Pandoc.brew` on current buffer or region (if mark is active), show results in *ess-output* and (optionally) copy results to clipboard while setting working directory to `tempdir()` temporary.
  * `pander-brew-export` (`C-c p B`): Run `Pandoc.brew` on current buffer or region (if mark is active) and export results to specified (auto-complete in minibuffer) format. Also tries to open exported document.
  * `pander-eval` (`C-c p e`): Run `pander` on (automatically evaluated) region *or* current chunk (if marker is not set), show results (of last returned R object) in `*ess-output*` and (optionally) copy those to clipboard while setting working directory to `tempdir()` temporary.

Few options of `pander-mode`: `M-x customize-group pander`

  * `pander-clipboard`: If non-nil then the result of `pander-*` functions would be copied to clipboard.
  * `pander-show-source`: If non-nil then the source of R commands would also show up in generated documents while running 'pander-eval'. This would not affect `brew` functions ATM.

To use this small lib, just type: `M-x pander-mode` on any document. It might be useful to add a hook to `markdown-mode` if you find this useful.

[![githalytics.com alpha](https://cruel-carlota.pagodabox.com/9dfac0c1da37bd83d1848289630631fd "githalytics.com")](http://githalytics.com/Rapporter/pander) 


-------
This report was generated with [R](http://www.r-project.org/) (2.15.1) and [pander](https://github.com/rapporter/pander) (0.1) in 1.282 sec on x86_64-unknown-linux-gnu platform.