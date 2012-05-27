
# Utils:

  * ~~"p" from rapport: modify defaults, fork it~~
  * check strings before applying formatting (to prevent e.g. "****foo****")
  * table: implement multi-line syntax for tables (as [Pandoc](http://johnmacfarlane.net/pandoc) does not support alignment with the current grid approach)
  * table: add strong emphasis to rownames
  * table: add more styling options (e.g. (strong) emphasising custom cells)
  * table: add option to draw significance starts in cells

# Required util functions:

  * add significance stars
  * ~~indent concatenated strings~~ -> pandoc.indent

# Pandoc methods:

  * ~~default~~
  * ~~list~~
  * ~~density~~
  * CrossTable

# Exporting:

 * remove multiple '\n's, see: `remove.extra.newlines()`
 * open exported docs

# Brainstorming:

  * What to do with summary classes? Leaving out, implement in Pandoc methods or what?

   **Leaving out!** As most Pandoc methods return a table and also trying to add a chatty caption, in most cases the full object information is needed which can (and mostly would) be truncated *inside* the Pandoc method with `summary`.
