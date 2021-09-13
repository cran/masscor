#' S3 method for printing objects of class \code{"massStandard"}
#'
#' The function prints objects of class \code{"massStandard"}.
#' @param x Object of class \code{"massStandard"}.
#' @param minimal logical default to \code{TRUE}.
#'   If \code{TRUE}, only minimal information
#'   regarding the calibration certificate is provided.
#' @param institution logical. If \code{TRUE} (the default)
#'   the calibrating institution information
#'   (including calibration traceability information)
#'   is printed. Ignored if \code{minimal = TRUE}.
#' @param description logical. If \code{TRUE} (the default)
#'   details of class, serial and manufacturer
#'   are printed. Ignored if \code{minimal = TRUE}.
#' @param density logical. If \code{TRUE} the density information is
#'   printed.
#' @param envConditions logical. If \code{TRUE} (the default)
#'   the environmental conditions at the place
#'   and the moment of calibration are printed.
#'   Ignored if \code{minimal = TRUE}.
#' @param addInfo logical. If \code{TRUE} (the default)
#'   additional information of
#'   the calibration is printed. Ignored if \code{minimal = TRUE}.
#' @param ... further arguments passed to or from other methods.
#' @seealso [massStandard()], [print.massStandardKit()]
#' @return No return value, called for side effects.
#' @examples
#' data(E2.MS.20g)
#' print(E2.MS.20g)
#' print(E2.MS.20g, minimal = FALSE)
#' @export
print.massStandard <- function(x, minimal = TRUE, description = TRUE,
                               institution = TRUE,
                               density = FALSE, envConditions = TRUE,
                               addInfo = TRUE, ...) {

  coreInfo <- data.frame(
    'Nominal mass' = paste0(x$nominal, ' ', x$standardUnits),
    'Conv mass correction' = paste0(x$convMassCor, ' ', x$standardUnits),
    'Conv mass' = paste0(x$convMass, ' ', x$standardUnits),
    'Uncertainty' = paste0(x$expandUncert, ' ', x$standardUnits), row.names = '')
  if (minimal) {
    cat('CALIBRATED MASS STANDARD:', x$nominal, x$standardUnits, '\n\n')
    print(coreInfo)
    cat('\nUncertainty is expanded uncertainty with a coverage factor of', x$k, '\n')
  } else {
    if (x$partofakit) {
      message('The mass standard is part of a mass standards kit.
      To see complete calibration data please print the object
      of class "massStandardKit" to which the object "massStandard" belongs to.')
      cat('\n')
      print(x, minimal = TRUE)
      return(cat(''))
    }
    cat('CALIBRATED MASS STANDARD:', x$nominal, x$standardUnits, '\n\n')

    if (description) {
      cat('Description')
      cat('\n         Class:', ifelse(is.null(x$class), 'Not provided', x$class))
      cat('\n        Serial:', ifelse(is.null(x$serial), 'Not provided', x$serial))
      cat('\n  Manufacturer:', ifelse(is.null(x$manufacturer), 'Not provided', x$manufacturer))
      cat('\n\n')
    }
    cat('Mass information:\n')
    print(coreInfo)
    cat('\nUncertainty is expanded uncertainty with a coverage factor of', x$k, '\n')
    cat('\n\n')

    if (institution) {
      cat('  Calibration perfomed by:',
          ifelse(!is.null(x$institution), x$institution, 'Not provided'))#, '\n')
      cat('\n                     Date:',
          ifelse(!is.null(x$date), x$date, 'Not provided'))#, '\n')
      cat('\n       Certificate number:',
          ifelse(!is.null(x$certificate), x$certificate, 'Not provided'))#, '\n')
      cat('\n Calibration traceability:',
          ifelse(!is.null(x$traceability), x$traceability, 'Not provided'))#, '\n')
      cat('\n\n')
    }

    if (envConditions) {
      cat('Environmental conditions')
      cat('\n                Temperature:',
          ifelse(is.null(x$Temp), 'Not provided',
                 paste0(paste0(x$Temp, collapse = ' - '), ' [',
                        x$unitsENV[1], ']', collapse = '')))
      cat('\n        Barometric pressure:',
          ifelse(is.null(x$p), 'Not provided',
                 paste0(paste0(x$p, collapse = ' - '), ' [',
                        x$unitsENV[2], ']', collapse = '')))
      cat('\n          Relative humidity:',
          ifelse(is.null(x$h), 'Not provided',
                 paste0(paste0(x$h, collapse = ' - '), ' [',
                        x$unitsENV[3], ']', collapse = '')))
      cat('\n\n')
    }

    if (density) {
      cat('Mass standard density: ', x$rho, '+-', x$u_rho,
          paste0(' [', x$unitsrho, ']', collapse = ''))
      cat('\n\n')
    }

    if (addInfo) {
      cat(ifelse(is.null(x$add.info),
                 'No additional information provided.',
                 c('Additional information:\n', x$add.info)))
    }
  }
}

