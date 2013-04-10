# General issues

  * ~~should not depend on `rapport`, try to only import (against that: depending on rapport's options?)~~

# Documentation

## README.md

  * ~~add installation details + Pandoc dependency~~
  * ~~add example files and html/pdf/docx exports~~
  * ~~add absolute path to examples (as view from two locations)~~
  * ~~remove `rapport` dependency~~
  * ~~add [ESS](http://ess.r-project.org/) functions~~
  * ~~add: short documentation (or at least mention!): `evals`~~
  * ~~comparison with `knitr`~~
  * ~~add: options~~ + ~~`p` options~~ + ~~`evals.messages` option~~

## Github pages

  * ~~add absolute path to examples (as view from two locations)~~

## Examples

  * do not forget about switching `I.have.time` to `TRUE` before exporting to Github pages

## Vignette

  * ~~TODO~~ won't fix: we have a homepage

# Coding

## General tasks

  * ~~**add controllable, global options**~~ `pander.option`
  * ~~rename pander.options & eval.options not to conflict with S3 methods** `panderOptions` and `evalsOptions`~~
  * ~~update package functions to use new naming scheme for options~~
  * **add ~~caption~~/other tweaks (~~alignment~~, sign. stars, emphasis etc.) to images/tables using `attrs`**

## Evals

  * ~~cleanup code from repeating code (like: `suppressMessages(suppressWarnings(...)))`)~~
  * ~~remove `evaluate` dependency~~
  * ~~grab multiple returned object/images in one chunk~~
  * ~~add option: grab.images (render those to disk or return unprinted)~~ use `eval.msgs` instead
  * ~~caching~~
  * ~~add: global options~~
  * ~~add: option to copy images~~
  * ~~check if img file referenced in cached result really exists~~
  * ~~add: `evals.messages` option passed to `pander.evals` to possibly suppress messages~~
  * ~~add: optionally save img's R object (lattice & ggplot2) to RDA file~~
  *  ~~What if img file referenced in cached result was altered? Should not we try to check for existing recordedPlot and just rerender the img (without actual `eval`) to overcome this issue?~~: resolved with `cache.copy.images` and `cache.mode == "disk"`
  * ~~run twice with `evalsOptions('cache.time', 0)`:  `evals('x<-1:21;histogram(x)')`~~: resolved by updating objects (while returning from cache) changed by cached code~~
  * add option to run code in sandbox:

  	  * ~~RAppArmor: https://github.com/jeroenooms/RAppArmor~~: see `RAppArmor` branch
      * ~~sandboxR: https://github.com/rapporter/sandboxR~~: just provide a sandboxed environment

### Image options:

Global options for: `lattice`, `ggplot2` and `base` plots

  * ~~plot margins~~
  * ~~theme: font (family, base size, color)~~
  * ~~theme: plotting area background colors~~
  
      * known issues: not in `base` plots (just global `backgroung`)
  
  * ~~theme: foreground colors (discrete, continuous)~~
  
      * known issues: `base` plot solution is really ugly (overwriting calls' `col` parameter)
	  
  * ~~grid options:~~
  
    * ~~enabled/disabled~~
	* ~~split enable/disable: major/minor~~
	* ~~grid color~~
 	* ~~grid line type~~
	
  * axes:
 
    * ~~color~~
	* ~~angle~~
	* split long labels

  * title: split if too long (strwidth, strheight)
  * others:
      * ~~border/box around plot/strip~~
	  * ~~transparent border of histogram~~
	  * ~~symbols~~

  * ~~remove dependencies: `ggplot2`, `latticeExtra` and build only on `grid`~~

**Add demo to GH page!**: `inst/examples/graph.brew`

## Helper functions

  * ~~"p" from rapport: modify defaults, fork it~~
  * ~~check strings before applying formatting (to prevent e.g. "****foo****")~~
  * ~~table: implement multi-line syntax for tables (as [Pandoc](http://johnmacfarlane.net/pandoc) does not support alignment with the current grid approach)~~
  * ~~table: support cells with line breaks~~
  * ~~table: split too wide tables into multiple tables (global solution which can be handled in LaTeX, docx etc. separately)~~
  * ~~table: add strong emphasis to row names~~
  * ~~table: fails with one-column tables. E.g.: `pander(mtcars[1,])`~~
  * ~~table: **issue** with rounding numbers, just check: `pander(mtcars$mpg)` vs. `pander(mtcars$mpg)` -> this is based on low `digits` option~~
  * ~~table: add more styling options (e.g. (strong) emphasizing custom cells, ~~alignment~~)~~
  * table: add option to draw significance starts in cells
  * table: @idea wrap header of tables to minimal (even in words!) to keep the table's width minimal

### Required helper functions

  * ~~add significance stars~~
  * ~~indent concatenated strings~~: `pandoc.indent`

### Calling Pandoc (converting docs)

  * ~~update footer (currently shows: rapport)~~
  * ~~revert colorbox to slimbox2 with fullscreen images~~
  * add templates for different formats
  * add option to change rendering back-end's name (like: pander -> rapport with version number)
  * **Q**: include Pandoc somehow in the package not to ask users to install it?
  
    **A**: INSTALL file updated + if Pandoc is not found it's shown to the user. Might try to auto-download binary for Mac/Windows? Linux users would deal with that problem, right? :)
	**UPDATE**: see `installr` package

## Pandoc/pander methods

  * ~~logical~~
  * ~~default~~
  * ~~list~~
  * ~~density~~
  * CrossTable
  * *What to do with summary classes? Leaving out, implement in Pandoc methods or what?*

   **Leaving out!** As most Pandoc methods return a table and also trying to add a chatty caption, in most cases the full object information is needed which can (and mostly would) be truncated *inside* the Pandoc method with `summary`.


## Brew

  * ~~check out image directory (should be `getwd()/images`)~~
  * ~~remove image absolute path~~
  * ~~prettify image names to `output` + index no.~~
  * ~~exporting features?~~
  * option to convert document to multiple formats at once (although with caching it's no so bad even now)
  * ~~remove parse (to deal with syntax errors)~~

## R5

  * ~~migrate to `Pandoc.convert`~~
  * ~~remove multiple line breaks, see: `remove.extra.newlines()`~~
  * ~~open exported docs~~
  * tweak `evals` like in `Pandoc.brew`

## Add support for `knitr`

  * implement it: `Pandoc.knitr`
  * create `knitr` hooks which would apply `pander` to each R object

### Tests

  * ~~Do that at last!~~
  * ~~evals~~
  * ~~brew~~
  * helper functions

# HTML/JS/CSS issues

  * move errors (footnote) above of upper footer and add a header tag (if found)

# ESS

  * ~~integrate (add some Lisp functions/optional key-bindings) `evals`/`pander`/`Pander.brew` - thanks for great ideas [*Michael Lawrence* @ [ESS] mail list](https://stat.ethz.ch/pipermail/ess-help/attachments/20120602/554dfb2f/attachment.pl)~~: http://rapporter.github.com/pander/#ess

# Great ideas

  * [*Michael Lawrence* @ [ESS] mail list](https://stat.ethz.ch/pipermail/ess-help/attachments/20120602/554dfb2f/attachment.pl): *Another thing I've been wanting is a way to output objects in multiple ways, separate from the report itself. Typical side effects would be: generation of an R data package with the result as a dataset, or storage of the result in some other database. It would be cool to be able to specify, on a per-block basis, the output driver (or a list of them). For example, your pander() function could be a dual-dispatch S4 generic, dispatching on both the object to export, and an object representing the target.* -> hook could depend on outer parameters in `evals`?
