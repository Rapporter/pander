#' @importFrom grid unit
NULL

#' A ggplot theme for the pander package
#'
#' The \pkg{pander} ships with a default theme when the "unify plots" option is
#' enabled via \code{panderOptions}, which is now also available outside of \pkg{pander} internals,
#' like \code{evals}, \code{eval.msgs} or \code{Pandoc.brew}.
#' @inheritParams ggplot2::theme_bw
#' @param nomargin suppress the white space around the plot (boolean)
#' @param fc font color (name or hexa code)
#' @param gM major grid (boolean)
#' @param gm minor grid (boolean)
#' @param gc grid color (name or hexa code)
#' @param gl grid line type (\code{lty})
#' @param boxes to render a border around the plot or not
#' @param bc background color (name or hexa code)
#' @param pc panel background color (name or hexa code)
#' @param lp legend position
#' @param axis axis angle as defined in \code{par(les)}
#' @export
#' @examples
#' library("ggplot2")
#' p <- qplot(mpg, wt, data = mtcars)
#' p + theme_pander()
#'
#' panderOptions('graph.grid.color', 'red')
#' p + theme_pander()
#'
#' p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
#' p + theme_pander() + scale_color_pander()
#'
#' ## standard examples of the ggtheme package
#' qplot(carat, price, data = diamonds, colour = cut) +
#'   theme_pander() +
#'   scale_colour_pander()
#'
#' ggplot(diamonds, aes(clarity, fill = cut)) +
#'   geom_bar() +
#'   scale_fill_pander() +
#'   theme_pander()
#'
theme_pander <- function(base_size = panderOptions('graph.fontsize'),
                         base_family = panderOptions('graph.fontfamily'),
                         nomargin = panderOptions('graph.nomargin'),
                         fc = panderOptions('graph.fontcolor'),
                         gM = panderOptions('graph.grid'),
                         gm = panderOptions('graph.grid.minor'),
                         gc = panderOptions('graph.grid.color'),
                         gl = panderOptions('graph.grid.lty'),
                         boxes = panderOptions('graph.boxes'),
                         bc = panderOptions('graph.background'),
                         pc = panderOptions('graph.panel.background'),
                         lp = panderOptions('graph.legend.position'),
                         axis = panderOptions('graph.axis.angle')) {

    ## ggplot2 functions
    element_text <- getFromNamespace("element_text", "ggplot2")
    element_line <- getFromNamespace("element_line", "ggplot2")
    element_rect <- getFromNamespace("element_rect", "ggplot2")
    element_blank <- getFromNamespace("element_blank", "ggplot2")
    theme <- getFromNamespace("theme", "ggplot2")

    ## DRY
    tc <- ifelse(pc == 'transparent', bc, pc) # "transparent" color

    ## default colors, font and legend position
    res <- theme(
        text             = element_text(family = base_family),
        plot.background  = element_rect(fill = bc, colour = NA),
        panel.grid       = element_line(colour = gc, size = 0.2, linetype = gl),
        panel.grid.minor = element_line(size = 0.1),
        axis.ticks       = element_line(colour = gc, size = 0.2),
        plot.title       = element_text(colour = fc,
                                        face = "bold", size = base_size * 1.2),
        axis.text        = element_text(colour = fc,
                                        face = 'plain', size = base_size * 0.8),
        legend.text      = element_text(colour = fc,
                                        face = 'plain', size = base_size * 0.8),
        legend.title     = element_text(colour = fc,
                                        face = 'italic', size = base_size),
        axis.title.x     = element_text(colour = fc,
                                        face = 'plain', size = base_size),
        strip.text.x     = element_text(colour = fc,
                                        face = 'plain', size = base_size),
        axis.title.y     = element_text(colour = fc,
                                        face = 'plain', size = base_size,
                                        angle = 90),
        strip.text.y     = element_text(colour = fc,
                                        face = 'plain', size = base_size,
                                        angle = -90),
        legend.key       = element_rect(colour = gc, fill = 'transparent'),
        strip.background = element_rect(colour = gc, fill = 'transparent'),
        panel.border     = element_rect(fill = NA, colour = gc),
        panel.background = element_rect(fill = pc, colour = gc),
        legend.position  = lp
    )

    ## disable box(es) around the plot
    if (!isTRUE(boxes)) {
        res <- res + theme(
            legend.key       = element_rect(colour = 'transparent',
                                            fill = 'transparent'),
            strip.background = element_rect(colour = 'transparent',
                                            fill = 'transparent'),
            panel.border     = element_rect(fill = NA, colour = tc),
            panel.background = element_rect(fill = pc, colour = tc)
        )
    }

    ## disable grid
    if (!isTRUE(gM)) {
        res <- res + theme(
            panel.grid       = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )
    }
    ## disable minor grid
    if (!isTRUE(gm))
        res <- res + theme(
            panel.grid.minor = element_blank()
        )

    ## margin
    if (nomargin)
        res <- res + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0), "lines"))

    ## axis angle (TODO: DRY with ifelse in the default color etc. section)
    if (axis == 0)
        res <- res + theme(axis.text.y =
                             element_text(colour = fc, family = base_family,
                                          face = 'plain',
                                          size = base_size * 0.8,
                                          angle = 90))
    if (axis == 2)
        res <- res + theme(axis.text.x =
                             element_text(colour = fc,family = base_family,
                                          face = 'plain',
                                          size = base_size * 0.8, angle = 90,
                                          hjust = 1))
    if (axis == 3)
        res <- res + theme(
            axis.text.y = element_text(colour = fc, family = base_family,
                                       face = 'plain', size = base_size * 0.8,
                                       angle = 90),
            axis.text.x = element_text(colour = fc, family = base_family,
                                       face = 'plain', size = base_size * 0.8,
                                       angle = 90, hjust = 1)
        )

    res

}


#' Color palette from the pander package
#'
#' The \pkg{pander} ships with a default colorblind and printer-friendly
#' color palette borrowed from \url{http://jfly.iam.u-tokyo.ac.jp/color/}.
#' @param n number of colors
#' @param cols vector of colors to use
#' @param random_order if the palette should be reordered randomly before
#'  rendering each plot to get colorful images
#' @export
#' @family colour pander
#' @examples
#' palette_pander(TRUE)
palette_pander <- function(n, random_order = FALSE,
                           cols = panderOptions('graph.colors')) {

    if (isTRUE(random_order))
        cols <- sample(cols)

    if (length(cols) < n)
        cols <- rep(cols, length.out = n)

    cols[1:n]

}


#' Color scale from the pander package
#'
#' The \pkg{pander} ships with a default colorblind and printer-friendly color
#' palette borrowed from \url{http://jfly.iam.u-tokyo.ac.jp/color/}.
#' @param ... passed to \code{\link[ggplot2]{scale_colour_discrete}}
#' @inheritParams palette_pander
#' @rdname scale_pander
#' @seealso \code{\link{theme_pander}}
#' @export
scale_color_pander <- function(...)
    getFromNamespace("discrete_scale", "ggplot2")('colour', 'pander',
                                                  palette_pander, ...)


#' @rdname scale_pander
#' @export
scale_colour_pander <- scale_color_pander


#' @rdname scale_pander
#' @export
scale_fill_pander <- function(...)
  getFromNamespace("discrete_scale", "ggplot2")('fill', 'pander',
                                                palette_pander, ...)
