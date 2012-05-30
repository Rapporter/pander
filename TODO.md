# README.md:

  * ~~add installation details + Pandoc dependency~~
  * add example files and html/pdf/docx exports

# Utils:

  * ~~"p" from rapport: modify defaults, fork it~~
  * check strings before applying formatting (to prevent e.g. "****foo****")
  * ~~table: implement multi-line syntax for tables (as [Pandoc](http://johnmacfarlane.net/pandoc) does not support alignment with the current grid approach)~~
  * ~~table: support cells with line breaks~~
  * ~~table: split too wide tables into multiple tables (global solution which can be handled in LaTeX, docx etc. separately)~~
  * table: add strong emphasis to row names
  * table: add more styling options (e.g. (strong) emphasizing custom cells)
  * table: add option to draw significance starts in cells

# Required util functions:

  * add significance stars
  * ~~indent concatenated strings~~ -> pandoc.indent

# Pandoc methods:

  * logical
  * ~~default~~
  * ~~list~~
  * ~~density~~
  * CrossTable

# Brew

  * ~~check out image directory (should be `getwd()/images`)~~
  * exporting features?

# Exporting:

  * remove multiple line breaks, see: `remove.extra.newlines()`
  * ~~open exported docs~~
  * add caption to images/tables (using `attrs`?)

# Calling Pandoc (converting docs)

  * add templates for different formats
  * add option to change rendering backend's name (like: pander -> rapport with version number)

# Brainstorming:

  * What to do with summary classes? Leaving out, implement in Pandoc methods or what?

   **Leaving out!** As most Pandoc methods return a table and also trying to add a chatty caption, in most cases the full object information is needed which can (and mostly would) be truncated *inside* the Pandoc method with `summary`.
