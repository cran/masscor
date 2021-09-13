#' S3 method for printing objects of class \code{"massStandardKit"}
#'
#' The function prints objects of class \code{"massStandardKit"}.
#' @inheritParams print.massStandard
#' @param x object of class \code{"massStandardKit"}.
#' @seealso [massStandardKit()], [print.massStandard()]
#' @return No return value, called for side effects.
#' @examples
#' data(Box.E2.MS.Kit)
#' print(Box.E2.MS.Kit, minimal = TRUE)
#' # We can print individual information of a single mass standard:
#' print(Box.E2.MS.Kit[['20']])
#' @export
print.massStandardKit <- function(x, minimal = FALSE, description = TRUE,
                                  institution = TRUE, density = FALSE,
                                  envConditions = TRUE, addInfo = TRUE, ...) {
  units <- paste0('[', x[[1]]$standardUnits, ']', collapse = '')

  coreInfo <- data.frame('Nominal mass' = paste0(x$merged$nominal,
                                                 x$merged$diff),
                         '.' = rep(units, length(x$merged$nominal)),
                         'Conv mass correction' = x$merged$convMassCor,
                         '..' = rep(units, length(x$merged$nominal)),
                         'Conv mass' = x$merged$nominal +
                           x$merged$convMassCor,
                         '...' = rep(units, length(x$merged$nominal)),
                         'Uncertainty' = x$merged$expandUncert,
                         '....' = rep(units, length(x$merged$nominal)))
  cat('CALIBRATED MASS STANDARDS KIT:', max(x$merged$nominal), '-',
      min(x$merged$nominal), '',
      x[[1]]$standardUnits,' \n\n')

  if (minimal) {
    print(coreInfo)
    cat('\nUncertainty is expanded uncertainty with a coverage factor of',
        x[[1]]$k, '\n')
  } else {
    if (description) {
      cat('Description')
      cat('\n         Class:', ifelse(is.null(x$class), 'Not provided', x$class))
      cat('\n        Serial:', ifelse(is.null(x$serial), 'Not provided', x$serial))
      cat('\n  Manufacturer:', ifelse(is.null(x$manufacturer), 'Not provided',
                                      x$manufacturer))
      cat('\n\n')
    }
    cat('Mass information:\n')
    print(coreInfo)
    cat('\nUncertainty is expanded uncertainty with a coverage factor of',
        x[[1]]$k, '\n')
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
                 paste0(paste0(x$Temp, collapse = ' - '), ' [', x$unitsENV[1], ']',
                        collapse = '')))
      cat('\n        Barometric pressure:',
          ifelse(is.null(x$p), 'Not provided',
                 paste0(paste0(x$p, collapse = ' - '), ' [', x$unitsENV[2], ']',
                        collapse = '')))
      cat('\n          Relative humidity:',
          ifelse(is.null(x$h), 'Not provided',
                 paste0(paste0(x$h, collapse = ' - '), ' [', x$unitsENV[3], ']',
                        collapse = '')))
      cat('\n')
    }

    if (density) {
      cat('Mass standards densities ',
          paste0(' [', x[[1]]$unitsrho, ']:', collapse = ''), '\n')
      print(data.frame('Nominal mass' = paste0(x$merged$nominal, x$merged$diff),
                       '.' = rep(units, length(x$merged$nominal)),
                       Value = x$merged$rho, Uncert = x$merged$u_rho))
      cat('\n\n')
    }

    if (addInfo) {
      cat(ifelse(is.null(x$add.info),
                 'No additional information provided.',
                 c('Additional information:\n', x$add.info)))
    }
  }
  # class(x) <- "list"
  # print(x, ...)
}
