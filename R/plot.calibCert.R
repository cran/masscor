#' S3 method for plotting objects of class \code{"calibCert"}
#'
#' The function plots the indication error or the conventional
#' mass correction for a balance whose
#' calibration data is in a object of class \code{"calibCert"}.
#' @param x object of class \code{"calibCert"}.
#' @param error logical. If \code{TRUE} (the default), the indication error is
#'   plotted. If \code{FALSE} the conventional mass correction is
#'   plotted instead.
#' @param y0line Logical. If \code{TRUE} (the default) a horizontal
#'   line is drawn at y = 0.
#' @param  ... Other graphical parameters used in \code{\link[graphics]{plot}}
#'
#' @return A base plot with calibration data of indication error or correction.
#' @examples
#' data(MT.XPE.204)
#' plot(MT.XPE.204)
#' @seealso [calibCert()], [print.calibCert()]
#' @export
#' @importFrom graphics arrows abline
plot.calibCert <- function(x, error = TRUE, y0line = TRUE, ...) {
  mass <- x$oldIndError[, 1]
  indError <- x$oldIndError[, 2]
  MCcorr <- - 1 * x$oldIndError[, 2]
  Uncert <- x$oldIndError[, 3]

  ylim1 <- c(1.2 * min(indError - Uncert), 1.2 * max(indError + Uncert))
  ylim2 <- c(1.2 * min(MCcorr - Uncert), 1.2 * max(MCcorr + Uncert))

  if(!error) {
    plot(x = mass, y = MCcorr,
         xlab = paste0('Load /[', x$orgIndErrorUnits[1], ']'),
         ylab = paste0('Mass correction /[', x$orgIndErrorUnits[2], ']'),
         pch = 18, ylim = ylim2, ...)
    arrows(x0 = mass, y0 = MCcorr - Uncert, x1 = mass,
           y1 = MCcorr + Uncert, code = 3,
           angle = 90, length = 0.1, col = 'steelblue')
  } else  {
    plot(x = mass, y = indError,
         xlab = paste0('Load /[', x$orgIndErrorUnits[1], ']'),
         ylab = paste0('Indication error /[', x$orgIndErrorUnits[2], ']'),
         pch = 18, ylim = ylim1, ...)
    arrows(x0 = mass, y0 = indError - Uncert, x1 = mass,
           y1 = indError + Uncert, code = 3,
           angle = 90, length = 0.1, col = 'steelblue')
  }
  if (y0line) abline(h = 0)
}
