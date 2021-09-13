#' S3 method for printing objects of class \code{"calibCert"}
#'
#' The function prints objects of class \code{"calibCert"}.
#' @inheritParams print.massStandard
#' @param complete logical default to \code{FALSE}. If \code{TRUE}, all the
#'   information contained in the object of class \code{calibCert} is shown.
#' @param nudeCertificate logical default to \code{FALSE}. If \code{TRUE},
#'   the object of class \code{calibCert} is shown as a list.
#' @param ... Further arguments passed to or from other methods.
#' @return No return value, called for side effects.
#' @seealso [calibCert()], [plot.calibCert()]
#' @examples
#' data(MT.XPE.204)
#' print(MT.XPE.204)
#' @export
print.calibCert <- function(x, complete = FALSE, nudeCertificate = FALSE, ...) {
  # In case complete nude information is asked
  if (nudeCertificate) {
    class(x) <- "list"
    print(x)
    return()
  }

  message('BALANCE CALIBRATION DATA: ', x$balanceID, '\n')
  cat(paste0('   Balance default units:  [', x$standardUnits, '] \n'))

  cat(paste0('   Balance division scale: ',
      convertMassUnitsSI(value = x$d, from = x$standardUnits, to = x$orgdUnits),
      ' [', x$orgdUnits, '] \n'))

  cat('\n   Repeatability results:\n')
  if (x$rep.natur == 'single') {
    cat(paste0('       Standard deviation for a balance load of ', x$oldRep[1], ' [', x$orgRepUnits[1], '] was ',
        x$oldRep[2], ' [', x$orgRepUnits[2], ']\n'))
  } else {
    colnames(x$oldRep) <- c(paste0('Load [', x$orgRepUnits[1], ']'), paste0('Std Dev [', x$orgRepUnits[2], ']'))
    x$oldRep <- cbind('.' = rep('     ', nrow(x$oldRep)), x$oldRep)
    print(x$oldRep, row.names = FALSE)
  }

  cat('\n   Eccentricity:\n')
  cat(paste0('       Maximal difference for a balance load of ', x$oldEccen[1], ' [', x$orgEccenUnits[1], '] was ',
             x$oldEccen[2], ' [', x$orgEccenUnits[2], ']\n'))

  cat('\n   Error of indication:\n')
  colnames(x$oldIndError) <- c(paste0('Load [', x$orgIndErrorUnits[1], ']'),
                               paste0('Error [', x$orgIndErrorUnits[2], ']'),
                               paste0('Uncertainty [', x$orgIndErrorUnits[3], ']'))
  x$oldIndError <- cbind('.' = rep('     ', nrow(x$oldIndError)), x$oldIndError)
  print(x$oldIndError, row.names = FALSE)
  cat(paste0('      (Uncertainties are expanded with a coverage factor k = ', x$k, ')'))

  if (complete) {
    message('\n Additional information')
    if (!is.null(x$institution)) {
      cat('   Calibration laboratory:', x$institution, '\n')
      if (!is.null(x$accreditation)) {
        cat('   Accreditation:', x$accreditation, '\n')
      }
    }
  } else {
    message("\n Use 'print(x, complete = TRUE)' to show additional information\n",
            "     contained in the object of class 'calibCert'")
  }
}
