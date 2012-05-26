**pander** is an [R](http://r-project.org) package containing helpers to return of user specified text elements (like: header, paragraph, table, image, lists etc.) in pandoc markdown or several type of R objects. Also capable of exporting/converting complex pandoc documents (reports).

# Short introduction

A full-blown vignette is under heavy development.

Till I finish that, please check out the below short examples.

## Helper functions

There are a bunch of helper functions in `pander` which return user specified pandoc format. You could find this functions starting with `pandoc`. For example `pandoc.strong` would return the passed characters with strong emphasis. E.g.:

```
> pandoc.strong('FOO')
**FOO**>
> pandoc.strong.return('FOO')
[1] "**FOO**"
```

As it can be seen here: `pandoc` functions generally prints to console and do not return anything. If you want the opposite: call each function ending in `.return`. For details please check documentation, e.g. `?pandoc.strong`.

Of course there are more complex functions in there too. Besides verbatim texts, (image) links or footnotes (among others) there are a helper e.g. for lists:

```
>  l <- list("First list element", rep(5, 'subelement'), "Second element", list('F', 'B', 'I', c('phone', 'pad', 'talics')))
> pandoc.list(l, 'roman')

I. First list element
    I. subelement
    II. subelement
    III. subelement
    IV. subelement
    V. subelement
II. Second element
    I. F
    II. B
    III. I
	I. phone
	II. pad
	III. talics

<!-- end of list -->
```

Or tables:

```
> pandoc.table(USArrests[1:4, ], decimal.mark = ',', justify = 'centre', caption = "FOO")

+----------+--------+---------+----------+------+
|          | Murder | Assault | UrbanPop | Rape |
+==========+========+=========+==========+======+
| Alabama  |  13,2  |   236   |    58    |  21  |
+----------+--------+---------+----------+------+
|  Alaska  |  10,0  |   263   |    48    |  44  |
+----------+--------+---------+----------+------+
| Arizona  |  8,1   |   294   |    80    |  31  |
+----------+--------+---------+----------+------+
| Arkansas |  8,8   |   190   |    50    |  20  |
+----------+--------+---------+----------+------+

    Table: FOO
```

## Generic pandoc (pander) method

`pander` or `pandoc` (call as you wish) can deal with a bunch of R object types.

Besides simple types (vectors, tables and e.g.  data frame), list might be interesting for you:

```
> pander(list(a=1, b=2, c=table(mtcars$am), x=list(myname=1,2), 56))


  * **a**: *1*
  * **b**: *2*
  * **c**:

    +----+----+
    | 0  | 1  |
    +====+====+
    | 19 | 13 |
    +----+----+

  * **x**:

      * **myname**: *1*
      * *2*

  * *56*

<!-- end of list -->
```

A nested list can be seen above with a table and all (optional) list names inside. As a matter of fact `pander.list` is the default method of `pander` too, see:

```
> pandoc(unclass(chisq.test(table(mtcars$am, mtcars$gear))))


  * **statistic**: *20.944669365722*
  * **parameter**: *2*
  * **p.value**: *2.83088895897563e-05*
  * **method**: Pearson's Chi-squared test
  * **data.name**: table(mtcars$am, mtcars$gear)
  * **observed**:

    +---+----+---+---+
    |   | 3  | 4 | 5 |
    +===+====+===+===+
    | 0 | 15 | 4 | 0 |
    +---+----+---+---+
    | 1 | 0  | 8 | 5 |
    +---+----+---+---+

  * **expected**:

    +---+-----+-----+-----+
    |   | 3   | 4   | 5   |
    +===+=====+=====+=====+
    | 0 | 8.9 | 7.1 | 3.0 |
    +---+-----+-----+-----+
    | 1 | 6.1 | 4.9 | 2.0 |
    +---+-----+-----+-----+

  * **residuals**:

    +---+------+------+------+
    |   | 3    | 4    | 5    |
    +===+======+======+======+
    | 0 | 2.0  | -1.2 | -1.7 |
    +---+------+------+------+
    | 1 | -2.5 | 1.4  | 2.1  |
    +---+------+------+------+

  * **stdres**:

    +---+------+------+------+
    |   | 3    | 4    | 5    |
    +===+======+======+======+
    | 0 | 4.4  | -2.3 | -2.9 |
    +---+------+------+------+
    | 1 | -4.4 | 2.3  | 2.9  |
    +---+------+------+------+


<!-- end of list -->
```

The output of different statistical methods are tried to be prettyfied. Some examples:

```
> pander(ks.test(runif(50), runif(50)))

+----------------+---------+------------------------+
| Test statistic | P value | Alternative hypothesis |
+================+=========+========================+
|      0.16      |  0.55   |       two-sided        |
+----------------+---------+------------------------+

    Table: Two-sample Kolmogorov-Smirnov test

> pander(chisq.test(table(mtcars$am, mtcars$gear)))

+----------------+----+---------+
| Test statistic | df | P value |
+================+====+=========+
|       21       | 2  | 2.8e-05 |
+----------------+----+---------+

    Table: Pearson's Chi-squared test

> pander(t.test(extra ~ group, data = sleep))

+----------------+----+---------+------------------------+
| Test statistic | df | P value | Alternative hypothesis |
+================+====+=========+========================+
|      -1.9      | 18 |  0.079  |       two.sided        |
+----------------+----+---------+------------------------+

    Table: Welch Two Sample t-test
```

And models:

```
> m <- glm(counts ~ outcome + treatment, family=poisson())
> pander(m)

+-------------+----------+------------+----------+----------+
|             | Estimate | Std. Error |  z value | Pr(>|z|) |
+=============+==========+============+==========+==========+
| (Intercept) | 3.0e+00  |  1.7e-01   | 1.8e+01  | 5.4e-71  |
+-------------+----------+------------+----------+----------+
|    outcome2 | -4.5e-01 |  2.0e-01   | -2.2e+00 | 2.5e-02  |
+-------------+----------+------------+----------+----------+
|    outcome3 | -2.9e-01 |  1.9e-01   | -1.5e+00 | 1.3e-01  |
+-------------+----------+------------+----------+----------+
|  treatment2 | 1.3e-15  |  2.0e-01   | 6.7e-15  | 1.0e+00  |
+-------------+----------+------------+----------+----------+
|  treatment3 | 1.4e-15  |  2.0e-01   | 7.1e-15  | 1.0e+00  |
+-------------+----------+------------+----------+----------+

    Table: Fitting generalized (poisson/log) linear model: counts ~ outcome + treatment

> pander(anova(m))

+-----------+----+----------+-----------+------------+
|           | Df | Deviance | Resid. Df | Resid. Dev |
+===========+====+==========+===========+============+
|      NULL | NA |    NA    |     8     |    10.6    |
+-----------+----+----------+-----------+------------+
|   outcome | 2  | 5.5e+00  |     6     |    5.1     |
+-----------+----+----------+-----------+------------+
| treatment | 2  | 2.7e-15  |     4     |    5.1     |
+-----------+----+----------+-----------+------------+

    Table: Analysis of Deviance Table

> pander(aov(m))

+-------------+----+---------+---------+---------+--------+
|             | Df |  Sum Sq | Mean Sq | F value | Pr(>F) |
+=============+====+=========+=========+=========+========+
| outcome     | 2  | 9.3e+01 | 4.6e+01 | 2.2e+00 |  0.22  |
+-------------+----+---------+---------+---------+--------+
| treatment   | 2  | 8.4e-31 | 4.2e-31 | 2.0e-32 |  1.00  |
+-------------+----+---------+---------+---------+--------+
| Residuals   | 4  | 8.3e+01 | 2.1e+01 |   NA    |   NA   |
+-------------+----+---------+---------+---------+--------+

    Table: Analysis of Variance Model
```

And for example for `prcomp`:

```
> pander(prcomp(USArrests))

+----------+-------+--------+--------+--------+
|          | PC1   | PC2    | PC3    | PC4    |
+==========+=======+========+========+========+
| Murder   | 0.042 | -0.045 | 0.080  | -0.995 |
+----------+-------+--------+--------+--------+
| Assault  | 0.995 | -0.059 | -0.068 | 0.039  |
+----------+-------+--------+--------+--------+
| UrbanPop | 0.046 | 0.977  | -0.201 | -0.058 |
+----------+-------+--------+--------+--------+
| Rape     | 0.075 | 0.201  | 0.974  | 0.072  |
+----------+-------+--------+--------+--------+

    Table: Principal Components Analysis


+------------------------+---------+---------+---------+---------+
|                        | PC1     | PC2     | PC3     | PC4     |
+========================+=========+=========+=========+=========+
| Standard deviation     | 8.4e+01 | 1.4e+01 | 6.5e+00 | 2.5e+00 |
+------------------------+---------+---------+---------+---------+
| Proportion of Variance | 9.7e-01 | 2.8e-02 | 5.8e-03 | 8.5e-04 |
+------------------------+---------+---------+---------+---------+
| Cumulative Proportion  | 9.7e-01 | 9.9e-01 | 1.0e+00 | 1.0e+00 |
+------------------------+---------+---------+---------+---------+
```

Or `density`:

```
> pander(density(mtcars$hp))

+---------+-------------+----------------+
|         | Coordinates | Density values |
+=========+=============+================+
|    Min. |     -32     |    5.0e-06     |
+---------+-------------+----------------+
| 1st Qu. |     81      |    4.1e-04     |
+---------+-------------+----------------+
|  Median |     194     |    1.7e-03     |
+---------+-------------+----------------+
|    Mean |     194     |    2.2e-03     |
+---------+-------------+----------------+
| 3rd Qu. |     306     |    4.1e-03     |
+---------+-------------+----------------+
|    Max. |     419     |    6.1e-03     |
+---------+-------------+----------------+

    Table: Kernel density of *mtcars$hp* (bandwidth: 28.04104)
```

## Exporting results

Short demo without any comments:

```
> x<-Pandoc$new('My name', 'Exciting title')
> x$add.paragraph('This data set contains statistics, in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973.  Also given is the percent of the population living in urban areas:')
> x$add(USArrests[1:5, ])
> x$add(pi)
> x

Exciting title
==============
 written by *My name* at *Sat May 26 00:28:33 2012*

  This report holds 3 block(s).

---


This data set contains statistics, in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973.  Also given is the percent of the population living in urban areas:


+------------+--------+---------+----------+------+
|            | Murder | Assault | UrbanPop | Rape |
+============+========+=========+==========+======+
| Alabama    | 13.2   | 236     | 58       | 21   |
+------------+--------+---------+----------+------+
| Alaska     | 10.0   | 263     | 48       | 44   |
+------------+--------+---------+----------+------+
| Arizona    | 8.1    | 294     | 80       | 31   |
+------------+--------+---------+----------+------+
| Arkansas   | 8.8    | 190     | 50       | 20   |
+------------+--------+---------+----------+------+
| California | 9.0    | 276     | 91       | 41   |
+------------+--------+---------+----------+------+

*3.14159265358979*
---

Proc. time:  0.024 seconds.

> x$export()

Exported to */tmp/pander-2ce13041fe1.[md|pdf]* under 0.434 seconds.

> x$format <- 'docx'
> x$export()

Exported to */tmp/pander-2ce16b70a27.[md|docx]* under 0.065 seconds.

```