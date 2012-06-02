# General issues

  * should not depend on `rapport`, try to only import (against that: depending on rapport's options?)

# Documentation

## README.md

  * ~~add installation details + Pandoc dependency~~
  * ~~add example files and html/pdf/docx exports~~
  * ~~add absolute path to examples (as view from two locations)~~
  * ~~remove `rapport` dependency~~

## Github pages

  * ~~add absolute path to examples (as view from two locations)~~

## Examples

  * do not forget about switching `I.have.time` to `TRUE` before exporting to Github pages

## Vignette

  * TODO

# Coding

## Helper functions

  * ~~"p" from rapport: modify defaults, fork it~~
  * check strings before applying formatting (to prevent e.g. "****foo****")
  * ~~table: implement multi-line syntax for tables (as [Pandoc](http://johnmacfarlane.net/pandoc) does not support alignment with the current grid approach)~~
  * ~~table: support cells with line breaks~~
  * ~~table: split too wide tables into multiple tables (global solution which can be handled in LaTeX, docx etc. separately)~~
  * table: add strong emphasis to row names
  * table: add more styling options (e.g. (strong) emphasizing custom cells)
  * table: add option to draw significance starts in cells

### Required helper functions

  * add significance stars
  * ~~indent concatenated strings~~ -> pandoc.indent

### Calling Pandoc (converting docs)

  * ~~update footer (currently shows: rapport)~~
  * add templates for different formats
  * add option to change rendering backend's name (like: pander -> rapport with version number)

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

## R5

  * ~~migrate to `Pandoc.convert`~~
  * ~~remove multiple line breaks, see: `remove.extra.newlines()`~~
  * ~~open exported docs~~
  * add caption to images/tables (using `attrs`?)
  * tweak `evals` like in `Pandoc.brew`

## Add support for `knitr`

  * implement it: `Pandoc.knitr`
  * create `knitr` hooks which would apply `pander` to each R object

### Tests

  * do that at last!
