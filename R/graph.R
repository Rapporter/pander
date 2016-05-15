#' Minor tick marks
#'
#' Adds minor tick marks to an existing \code{base} plot.
#' @param nx intervals to split on x axis
#' @param ny intervals to split on x axis
#' @param grid show grid lines at minor ticks?
#' @keywords internal
#' @references This function is a forked version of \code{Hmisc::minor.tick}
#' @importFrom graphics par axis abline
add.minor.ticks <- function (nx = 4, ny = 4, grid = TRUE) {

    ax <- function(w, n, grid) {
        range <- par('usr')[ifelse(w == 'x', 1:2, 3:4)]
        tick.pos <- ifelse(w == 'x', par('xaxp'), par('yaxp'))
        distance.between.minor <- (tick.pos[2] - tick.pos[1]) / tick.pos[3] / n
        possible.minors <- tick.pos[1] - (0:100) * distance.between.minor
        low.minor <- min(possible.minors[possible.minors >= range[1]])
        if (is.na(low.minor)) {
            low.minor <- tick.pos[1]
        }
        possible.minors <- tick.pos[2] + (0:100) * distance.between.minor
        hi.minor <- max(possible.minors[possible.minors <= range[2]])
        if (is.na(hi.minor)) {
            hi.minor <- tick.pos[2]
        }
        ticks.at <- seq(low.minor, hi.minor, by = distance.between.minor)
        axis(if (w == 'x')
             1
        else 2, ticks.at,
             labels = FALSE, tcl = par('tcl'))
        if (w == 'x') {
            abline(v = ticks.at,
                   col = panderOptions('graph.grid.color'),
                   lwd = 0.2,
                   lty = panderOptions('graph.grid.lty'))
        } else {
            abline(h = ticks.at,
                   col = panderOptions('graph.grid.color'),
                   lwd = 0.2,
                   lty = panderOptions('graph.grid.lty'))
        }
    }

    if (nx > 1) {
        ax('x', nx, grid = grid)
    }
    if (ny > 1) {
        ax('y', ny, grid = grid)
    }

    invisible()

}


#' Add grids
#'
#' Exactly the same fn as  see \code{latticeExtra::axis.grid} but using \code{grid} colors instead.
#' @param side see \code{latticeExtra::axis.grid}
#' @param ... see \code{latticeExtra::axis.grid}
#' @param ticks see \code{latticeExtra::axis.grid}
#' @param scales see \code{latticeExtra::axis.grid}
#' @param components see \code{latticeExtra::axis.grid}
#' @param line.col see \code{latticeExtra::axis.grid}
#' @keywords internal
#' @references This function is a forked version of \code{latticeExtra::axis.grid}
add.lattice.grid <- function (side = c('top', 'bottom', 'left', 'right'), ..., ticks = c('default', 'yes', 'no'), scales, components, line.col) { #nolint

    side <- match.arg(side)
    ticks <- match.arg(ticks)
    scales.tck <- switch(side, left = , bottom = scales$tck[1], right = , top = scales$tck[2]) #nolint
    comps.major <- components
    mycomps <- components[[side]]

    if (is.list(mycomps)) {
        lab <- as.character(mycomps$labels$labels)
        if (any(lab != '')) {
            tck <- mycomps$ticks$tck
            if (any(tck * scales.tck != 0)) {
                tck <- rep(tck, length = length(lab))
                comps.major[[side]]$ticks$tck <- ifelse(lab == '', NA, tck)
            }
        }
    } else {
        ticks <- 'no'
    }

    lattice::axis.default(side,
                          scales = scales,
                          ticks = ticks,
                          components = comps.major,
                          ...,
                          line.col = panderOptions('graph.grid.color'))

    if (side %in% c('top', 'left')) {
        return()
    }
    if (scales$draw == FALSE) {
        return()
    }
    ref.line <- lattice::trellis.par.get('reference.line')

    if (side == 'bottom') {
        tck <- abs(mycomps$ticks$tck)
        lattice::panel.refline(v = mycomps$ticks$at,
                               lwd = ref.line$lwd * tck,
                               alpha = ref.line$alpha * tck / max(tck, na.rm = TRUE))
    }
    if (side == 'right') {
        if (!is.list(mycomps)) {
            mycomps <- components[['left']]
        }
        tck <- abs(mycomps$ticks$tck)
        lattice::panel.refline(h = mycomps$ticks$at,
                               lwd = ref.line$lwd * tck,
                               alpha = ref.line$alpha * tck / max(tck, na.rm = TRUE))
    }

}

#' Add subticks
#'
#' See \code{latticeExtra::xscale.components.subticks}.
#' @param lim see \code{latticeExtra::xscale.components.subticks}
#' @param ... see \code{latticeExtra::xscale.components.subticks}
#' @param n see \code{latticeExtra::xscale.components.subticks}
#' @keywords internal
#' @aliases add.lattice.ysubticks
#' @references This is a simplified verwion of \code{latticeExtra::xscale.components.subticks}.
add.lattice.xsubticks <- function (lim, ..., n = 2) {

    ans    <- lattice::xscale.components.default(lim = lim, ..., n = n)
    ticks  <- ans$bottom$ticks$at
    ticks2 <- head(ticks, -1) + diff(head(ticks, 2)) / 2
    ans$bottom$ticks$at <- c(ticks, ticks2)
    ans$bottom$ticks$tck <- c(rep(1, length(ticks)), rep(0.5, length(ticks2)))
    ans$bottom$labels$at <- ans$bottom$ticks$at
    ans$bottom$labels$labels <- c(ans$bottom$labels$labels, rep(' ', length(ticks2)))
    ans$bottom$labels$check.overlap <- FALSE
    ans

}

#' @keywords internal
add.lattice.ysubticks <- function (lim, ..., n = 2) {

    ans    <- lattice::yscale.components.default(lim = lim, ..., n = n)
    ticks  <- ans$left$ticks$at
    ticks2 <- head(ticks, -1) + diff(head(ticks, 2)) / 2
    ans$left$ticks$at <- c(ticks, ticks2)
    ans$left$ticks$tck <- c(rep(1, length(ticks)), rep(0.5, length(ticks2)))
    ans$left$labels$at <- ans$left$ticks$at
    ans$left$labels$labels <- c(ans$left$labels$labels, rep(' ', length(ticks2)))
    ans$left$labels$check.overlap <- FALSE
    ans

}
