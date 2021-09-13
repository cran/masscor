#' Models for calculating air density based on environmental conditions
#'
#' The function uses environmental conditions information
#' (barometric pressure, temperature and
#' relative humidity.) to calculate a local air density value.
#' If no parameter is defined, the air
#' density at 20\eqn{^o}C, 1013.25 hPa and 50% relative humidity is returned.
#' The air density value value can later be used to calculate the
#' Magnitude of Air Buoyancy Correction ([MABC()]).
#' The uncertainty of the air density value can be calculated using
#' the function [uncertAirDensity()].
#'
#' Local air density can be estimated using one of several methods.
#' The most complete approach is the
#' CIMP complete formula (the default, \code{method = 'CIMP2007'})
#' as described in Picard et al (2008).
#' The CIMP approximated exponential formula (\code{method = 'CIMP.approx'})
#' and the method reported by Jones, (\code{method = 'Jones1978'}) (Harris, 2019)
#' are also included.
#'
#' @inheritSection calibCert unitsENV
#'
#' @references
#' Picard, A; Davis, R S; Gläser, M; Fujii, K  (2008). Revised formula for
#' the density of moist air (CIPM-2007).
#' Metrologia, 45(2), 149–155. doi:10.1088/0026-1394/45/2/004
#'
#' Harris, G. (2019). Selected Laboratory and Measurement Practices and Procedures to Support Basic
#' Mass Calibrations. SOP 2 - Recommended Standard Operating Procedure for Applying Air Buoyancy
#' Corrections. National Institute of Standards and Technology (NIST). doi:10.6028/NIST.IR.6969-2019
#'
#' Preguntar a andres referencia de la f'ormula simplificada exponencial
#'
#' @param Temp ambient temperature in weighing room.
#' @param p barometric pressure in weighing room.
#' @param h relative humidity in weighing room.
#' @param x_CO2 molar fraction of carbon dioxide in the air inside weighing room.
#' @param model model to use for air density calculation.
#'   Must be one of \code{'CIMP2007'} (default),
#'   \code{'CIMP.approx'} or \code{'Jones1978'}. See See Details for references.
#' @inheritParams calibCert
#'
#' @return Numeric value of air density in \eqn{g~cm^{-3}}, according to chosen model.
#' @seealso [MABC()] to calculate the Magnitude of Air Buoyancy Correction and
#'   [uncertAirDensity()] to estimate the uncertainty of the calculated air density.
#' @export
#'
#' @examples
#' airDensity(Temp = 23.4, p = 612.3, h = 23,
#'            unitsENV = c('deg.C', 'mmHg', '%')) # [g/cm^3]
#' airDensity(Temp = 23.4, p = 612.3, h = 23,
#'            unitsENV = c('deg.C', 'mmHg', '%'), model = 'CIMP.approx') # [g/cm^3]
#' airDensity(Temp = 23.4, p = 612.3, h = 23,
#'            unitsENV = c('deg.C', 'mmHg', '%'), model = 'Jones1978')   # [g/cm^3]

airDensity <- function(Temp = 20, p = 1013.25, h = 50,
                       unitsENV = c('deg.C', 'hPa', '%'),
                       x_CO2 = 0.0004, model = 'CIMP2007') { # [g/cm^3]

  if (!(model %in% c('Jones1978', 'CIMP.approx', 'CIMP2007'))) {
    stop("Parameter 'model' must be 'CIMP2007', 'CIMP.approx' or 'Jones1978'. See details.")
  }
  if (!(unitsENV[1] %in% c('deg.C', 'K'))) {
    stop("Temperature unitsENV must be 'deg.C' or 'K'.")
  }
  if (!(unitsENV[2] %in% c('Pa', 'hPa', 'kPa', 'mmHg'))) {
    stop("Pressure unitsENV must be 'Pa', 'hPa', 'kPa' or 'mmHg'.")
  }
  if (!(unitsENV[3] %in% c('%', 'frac'))) {
    stop("Relative humidity must be '%' or 'frac'.")
  }

  #if (unitsENV[1] == 'deg.C') Temp <- Temp + 273.15
  Temp <- convertTemperature(from = unitsENV[1], to = 'K', value = Temp)

  if (model == 'Jones1978') {
    p <- convertPressure(from = unitsENV[2], to = 'mmHg', value = p)
    h <- convertRelHum(from = unitsENV[3], to = '%', value = h)
    if (h < 0 || h > 100) {
      stop("Relative humidity must be between 0 and 1 (or 0%-100%).")
    }
    e_s <- 1.3146e9 * exp(-5315.56/Temp)
    rho_air_exp <- expression(((0.46460 * (p - 0.0037960 * h * e_s))/Temp)*10^-3)
    rho_air <- eval(rho_air_exp)
  }

  if (model == 'CIMP.approx') {
    p <- convertPressure(from = unitsENV[2], to = 'kPa', value = p)
    h <- convertRelHum(from = unitsENV[3], to = 'frac', value = h)
    if (h < 0.20 || h > 0.80) {
      stop("For CIMP.approx the relative humidity must be between 0.2 and 0.8 (or 20%-80%).")
    }
    if (Temp < (273.15 + 15) || Temp > (273.15 + 27)) {
      stop("For CIMP.approx the temperatures must be between 15 and 27 deg.C.")
    }
    if (p < 60 || p > 110) {
      stop("For CIMP.approx the barometric pressure must be between 60 and 110 hPa.")
    }
    rho_air <- (3.4848 * p - (0.9024 * h * exp(0.0612 * (Temp - 273.15))))/ (Temp) / 1000
  }

  if (model == 'CIMP2007') {
    p <- convertPressure(from = unitsENV[2], to = 'Pa', value = p)
    h <- convertRelHum(from = unitsENV[3], to = 'frac', value = h)
    if (h < 0 || h > 1) {
      stop("Relative humidity must be between 0 and 1 (or 0%-100%).")
    }

    rho_air_exp <- expression(((p*M_a)/(Z*R*Temp)) * (1 - x_v * (1 - M_v/M_a)))
    x_v_exp <- expression(h * f * p_sv / p)
    Z_exp <- expression(1 - ((p/Temp)*(a0 + a1*t + a2*t^2 + (b0 + b1*t)*x_v +
                                         (c0 + c1*t)*x_v^2)) +
                          ((p^2/Temp^2)*(d + e*x_v^2)))

    # [kg / mol] molar mass of the air within laboratory
    M_a <- (28.96546 + 12.011*(x_CO2 - 0.0004))*10^-3
    M_v <- 18.01528e-3 # [kg / mol] $/pm$ 0.00017e-3
    # p [Pa], ambient barometric pressure
    # Temp [K], ambient temperature
    R <- 8.314472 # [J / (mol K)]  $/pm$ 0.000015 universal gas constant
    # h [], relative humidity
    t <- Temp - 273.15 # [deg.C], ambient temperature
    f <- 1.00062 + 3.14e-8*p + 5.6e-7*t^2 # ??

    A <- 1.2378847e-5 # [K^-2]
    B <- -1.9121316e-2 # [K^-1]
    C <- 33.93711047 # []
    D <- -6.3431645e3 # [K]
    p_sv <- exp(A*Temp^2 + B*Temp + C + D/Temp) # [Pa]

    a0 <- 1.58123e-6 # [K Pa^-1]
    a1 <- -2.9331e-8 # [Pa^-1]
    a2 <- 1.1043e-10 # [K^-1 Pa^-1]
    b0 <- 5.707e-6 # [K Pa^-1]
    b1 <- -2.051e-8 # [Pa^-1]
    c0 <- 1.9898e-4 # [K Pa^-1]
    c1 <- -2.376e-6 # [Pa^-1]
    d <- 1.83e-11 # [K^2 Pa^-2]
    e <- -0.765e-8 # [K^2 Pa^-2]

    x_v <- eval(x_v_exp)
    Z <- eval(Z_exp)
    rho_air <- eval(rho_air_exp) * 10^-3
  }
  return(rho_air)
}
