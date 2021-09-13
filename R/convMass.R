#' Corrects a balance reading using balance calibration data.
#'
#' Given a balance reading indication and the calibration information of
#' the balance, the function
#' interpolates error correction for the reading using the errors of indication
#' for the two closest calibration
#' points. The output is generally the mass measurement
#' result under the conditions of calibration. If densities from the object
#' and the local air are provided the conventional mass of the object can be
#' calculated. See Details.
#'
#' The conventional mass value of a body is equal to the mass \eqn{m_c}
#' of a mass standard
#' that balances this body under conventionally chosen conditions:
#' at a temperature \eqn{t_{ref} = 20^o}C,
#' with mass standards of density \eqn{\rho_c=8000} kg m\eqn{^{-3}},
#' in normal air of density
#' \eqn{\rho_0=1.2} kg m\eqn{^{-3}} (OIML, 2004).
#'
#' @references
#' OIML, (2004). ORGANISATION INTERNATIONALE DE MÉTROLOGIE LÉGALE.
#' International Document D 28: Conventional value of the result of weighing in air.
#'
#' Harris, G. (2019). Selected Laboratory and Measurement Practices and Procedures to Support Basic
#' Mass Calibrations. SOP 2 - Recommended Standard Operating Procedure for Applying Air Buoyancy
#' Corrections. National Institute of Standards and Technology (NIST). doi:10.6028/NIST.IR.6969-2019
#'
#' @param reading numeric with balance reading for the mass of the object.
#' @param units character with the units of \code{reading}. Must be a SI unit.
#'   If not provided, the balance standard units are assumed.
#'   See [calibCert()] for details.
#' @param calibCert object of class \code{"calibCert"} with the calibration
#'   data of the balance. See [calibCert()] for details.
#' @param rho density of the object in g cm\eqn{^{-3}}.
#' @param rho_air density of local air in g cm\eqn{^{-3}}.
#' @return Numeric value of conventional mass.
#' @seealso [uncertConvMass()]
#' @examples
#' data(minimalCert)
#' convMass(reading = 12.4835, calibCert = minimalCert)
#' @export
#' @importFrom stats predict lm

convMass <- function(calibCert, reading, units = NULL,
                     rho = NULL, rho_air = NULL) {
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
  p2 <- which.min(abs(calibCert$indError[, 1] - p2prim))

  Cp_i <- - calibCert$indError[, 2][c(p1, p2)]
  mp_i <- calibCert$indError[, 1][c(p1, p2)]
  correction <- suppressWarnings(
    predict(lm(Cp_i ~ mp_i), newdata = data.frame(mp_i = reading)))
  corrected <- reading + correction
  corrected <- as.numeric(corrected) / fc
  #if (print) cat(paste0('Masa corregida: ', corrected, ' [', units, ']\n'))

  if (missing(rho) || missing(rho_air)) {
    message('The result corresponds to the mass measurement',
            'result at the conditions of the calibration.')
    return(corrected)
  } else {
    corrected <- corrected * (1 + ((rho_air - 0.0012)*(1/rho - 1/8)))
    return(corrected)
  }
}
