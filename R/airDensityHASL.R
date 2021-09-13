#' Air density estimation using only height above sea level.
#'
#' Calculates the approximated density of local air using a model that relies
#' on height above sea level (HASL) information. More accurate alternatives are found
#' in [airDensity()] but those require data form environmental conditions
#' (temperature, barometric pressure and relative humidity).
#'
#' @param HASL height altitude above sea level in meters.
#' @seealso [airDensity()] for better models to predict air density.
#' @return Numeric value of an approximated air density in g cm\eqn{^{-3}}.
#' @examples
#' airDensityHASL(HASL = 0)    # [g/cm^3]
#' airDensityHASL(HASL = 1600) # [g/cm^3]
#' @export

airDensityHASL <- function(HASL) { # [g/cm^3]
  rho_airHASL <- 1.2 * exp(-(1.2 / 101325) * 9.8 * HASL) / 1000
  return(rho_airHASL)
}
