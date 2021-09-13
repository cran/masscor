#' Magnitude of the Air Buoyancy Correction
#'
#' Calculates the Magnitude of the Air Buoyancy Correction (MABC).
#' If no parameters are provided the
#' function returns MABC for weighing water at standard conditions.
#'
#' Comparing masses (weighing) in air produces results that are
#' influenced by the objects densities
#' due to their buoyancy in air. This air buoyancy effects are
#' usually small but must be taken in account when high accuracy is required.
#' The effect can be corrected by using the densities of the object,
#' the mass standard and the air filling the room where the measurement
#' process takes place (Harris, 2019).
#'
#' The uncertainty associated to MABC can be calculated by the function [uncertMABC()].
#'
#' @param rho density of the sample in \eqn{g~cm^{-3}}
#' @param rho_w density of the weigths in \eqn{g~cm^{-3}}
#' @param rho_air density of the air in \eqn{g~cm^{-3}}.
#'   If not provided, the value returned by the function [airDensity()] with no
#'   parameters is used. See [airDensity()] for details.
#'
#' @return Numeric value of the Magnitude of the Air Buoyancy Correction factor.
#' @references
#' Harris, G. (2019). Selected Laboratory and Measurement Practices and Procedures to Support Basic
#' Mass Calibrations. SOP 2 - Recommended Standard Operating Procedure for Applying Air Buoyancy
#' Corrections. National Institute of Standards and Technology (NIST). doi:10.6028/NIST.IR.6969-2019
#' @examples
#' ## Magnitude of the air buoyancy correction for some materials:
#' # Water
#' MABC()
#' # Zinc metal
#' MABC(rho = 7.133)
#' # Copper metal
#' MABC(rho = 8.96)
#' @export
#' @seealso [uncertMABC()], [airDensity()], [uncertMABC()]

MABC <- function(rho = 0.997, rho_w = 8, rho_air = airDensity()) {
  MABC <- (1 - rho_air / rho_w) / (1 - rho_air / rho) # NIST formula
  MABC1 <- (1 + rho_air * (1 / rho - 1 / rho_w))      # Euramet formula
  return(MABC)
}


