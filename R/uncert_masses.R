#' Uncertainty due to mass correction using calibration certificate
#'
#' Given a balance reading indication and the calibration information
#' of the balance, the function
#' uses the conventional mass correction uncertainties of the two
#' closest calibration points to the balance reading to interpolate
#' the uncertainty due to the conventional mass correction.
#'
#' @inheritParams convMass
#'
#' @return A numeric value of uncertainty for a conventional mass correction.
#'
#' @examples
#'   data(minimalCert)
#'   uncertErrorCorr(reading = 12.4835, calibCert = minimalCert)
#' @export
#' @seealso [convMass()], [uncertReading()], [uncertConvMass()]

uncertErrorCorr <- function(calibCert,
                            reading,
                            units = NULL) {
  if(missing(units)) {
    fc <- 1
    units <- calibCert$standardUnits
  } else {
    fc <- convertMassUnitsSI(from = units, to = calibCert$standardUnits, value = 1)
  }

  reading <- reading * fc
  if (reading > max(calibCert$indError[, 1]) || reading < min(calibCert$indError[, 1])) {
    warning('Reading is outside calibration interval: ', min(calibCert$indError[, 1]),
            ' - ', max(calibCert$indError[, 1]), ' [', calibCert$standardUnits, ']')
  }

  p1 <- which.min(abs(calibCert$indError[, 1] - reading))
  p2prim <- min(abs(calibCert$indError[, 1][-p1] - reading))
  p2 <- which(abs(calibCert$indError[, 1] - reading) == p2prim)

  #u_E <- sqrt(calibCert$indError[, 3][p1]^2 + calibCert$indError[, 3][p2]^2)
  u_E <- calibCert$indError[, 3][c(p1, p2)]
  mp_i <- calibCert$indError[, 1][c(p1, p2)]
  uncert <- suppressWarnings(
    predict(lm(u_E ~ mp_i), newdata = data.frame(mp_i = reading)))

  return(as.numeric(uncert / fc))
}

#' Uncertainty of balance readings
#'
#' Uncertainty in a given balance reading considering the effects of
#' rounding error, lack of repeatability, eccentricity and balance taring.
#'
#' @inheritParams convMass
#' @inheritParams calibCert
#' @param d balance division scale. Useful when the balance is operated a a
#'   division scale different from that stated in the calibration certificate.
#'   This is the common case when the user is give up some readability
#'   in order to make faster mass measurements. If not provided.
#'   the functions uses
#'   the balance division scale stated in the calibration certificate.
#' @param d.units character with the units of parameter \code{d}.
#'   If not provided, the value stated at \code{units} or the balance
#'   standard units is used.
#' @param sd standard deviation when a balance reading is the result
#'   of averaging several individual measurements of same object. If not provided
#'   the information is taken from the calibration certificate of the balance
#' @param sd.units character with the units of standard deviation.
#'   If not provided, the value stated at \code{units} or the balance
#'   standard units is used.
#' @return A numeric value of uncertainty for a balance reading.
#' @examples
#' data(minimalCert)
#' uncertReading(calibCert = minimalCert, reading = 12.4835)
#' uncertReading(calibCert = minimalCert, reading = 12.484, d = 1, d.units = 'mg')
#' @export
#' @importFrom graphics barplot
#' @importFrom stats sd
#' @seealso [uncertErrorCorr()], [uncertConvMass()]
uncertReading <- function(calibCert, reading, units = NULL,
                          sd = NULL, sd.units = NULL,
                          d = NULL, d.units = NULL) {
  if(missing(units)) {
    fc <- 1
    units <- calibCert$standardUnits
  } else {
    fc <- convertMassUnitsSI(from = units, to = calibCert$standardUnits, value = 1)
  }
  reading <- reading * fc

  if (missing(d)) {
    if (!missing(d.units)) warning("Argument 'd.units' ignored because no value was provided to 'd'.")
    d <- calibCert$d
  } else {
    if (!missing(d.units)) {
      d <- convertMassUnitsSI(from = d.units, to = calibCert$standardUnits, value = d)
    } else {
      if (!missing(units)) {
        d <- convertMassUnitsSI(from = units, to = calibCert$standardUnits, value = d)
      } else {
        message('Provided division scale is assumed to be in the balance standard units: [',
                calibCert$standardUnits, ']')
      }
    }
  }
  u_d <- d/sqrt(12)

  if (missing(sd)) {
    if (!missing(sd.units)) warning("Argument 'sd.units' ignored because no value was provided to 'sd'.")
    if (calibCert$rep.natur == 'single') {
      sd <- calibCert$rep[2]
    } else {
      p1 <- which.min(abs(calibCert$rep[, 1] - reading))
      p2prim <- min(abs(calibCert$rep[, 1][-p1] - reading))
      p2 <- which(abs(calibCert$rep[, 1] - reading) == p2prim)

      sd <- max(calibCert$rep[c(p1, p2), 2])
    }
    } else {
      if (!missing(sd.units)) {
        sd <- convertMassUnitsSI(from = sd.units, to = calibCert$standardUnits, value = sd)
      } else {
        if (!missing(units)) {
          sd <- convertMassUnitsSI(from = units, to = calibCert$standardUnits, value = sd)
        } else {
          message('Provided standard deviation is assumed to be in the balance standard units: [',
                  calibCert$standardUnits, ']')
        }
      }
    }

  u_ecc <- reading * abs(calibCert$eccen[2]) /  (2 * calibCert$eccen[1] * sqrt(3))

  u_r <- sqrt(2 * u_d ^ 2 + sd ^ 2 + u_ecc ^ 2)
  return(u_r / fc)
}

#' Uncertainty in conventional mass value
#'
#' The function combines the uncertainty of the conventional mass correction
#' (as obtained by [uncertErrorCorr()])
#' and the uncertainty in the balance reading (as obtained by [uncertReading()]),
#' to produce the uncertainty of a conventional mass value.
#'
#' @inheritParams uncertReading
#' @return A numeric value of uncertainty for a conventional mass value.
#'
#' @examples
#' data(minimalCert)
#' uncertConvMass(reading = 12.4835, calibCert = minimalCert)
#' @export
#' @seealso [convMass()], [uncertReading()], [uncertErrorCorr()]

uncertConvMass <- function (calibCert, reading, units,
                            sd, sd.units,
                            d, d.units) {
  u_err <- uncertErrorCorr(calibCert, reading, units)
  u_read <- uncertReading(calibCert, reading, units,
                          sd, sd.units,
                          d, d.units)
  return(sqrt(u_err ^ 2 + u_read ^ 2))
}
