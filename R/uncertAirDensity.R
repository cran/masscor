#' Uncertainties in air density calculations
#'
#' Propagates the uncertainty of environmental conditions measurements
#' to estimated values of air densities calculated using the function
#' [airDensity()] with models \code{'CIMP2007'} y \code{'CIMP.approx'}
#' function. Uncertainty arising from to the chosen model itself is
#' considered. Calculations are made according to the
#' Guide to the Guide to the expression of uncertainty
#' in measurement (GUM, JCGM, 2008) as implemented
#' by the package \link[metRology]{metRology} (Ellison, 2018).
#'
#' @inheritSection calibCert unitsENV
#'
#' @inheritParams airDensity
#' @param u_Temp standard uncertainty of temperature measurement
#' @param u_p standard uncertainty of barometric pressure measurement
#' @param u_h standard uncertainty of relative humidity
#' @param plot logical. If \code{TRUE} (the default), the relative
#'   uncertainty contributions are shown in a \link[graphics]{barplot}.
#' @param printRelSD Logical. If \code{TRUE} (the default), a short
#'   statement indicating relative standard uncertainty
#'   of the air density estimation is printed.
#'
#' @importFrom metRology uncert contribs
#' @return A numeric value of standard uncertainty of calculated air density in \eqn{g~cm^{-3}}.
#'
#' @references
#' Picard, A; Davis, R S; Gläser, M; Fujii, K  (2008).
#' Revised formula for the density of moist air (CIPM-2007).
#' Metrologia, 45(2), 149–155. doi:10.1088/0026-1394/45/2/004
#'
#' Harris, G. (2019). Selected Laboratory and Measurement
#' Practices and Procedures to Support Basic Mass Calibrations.
#' SOP 2 - Recommended Standard Operating Procedure for Applying Air Buoyancy
#' Corrections. National Institute of Standards and Technology
#' (NIST). doi:10.6028/NIST.IR.6969-2019
#'
#' BIMP JCGM (2008) Evaluation of measurement data —
#' Guide to the expression of uncertainty in measurement.
#'
#' Stephen L R Ellison. (2018). metRology: Support for
#' Metrological Applications. R package version 0.9-28-1.
#' https://CRAN.R-project.org/package=metRology
#'
#' @examples
#'  uncertAirDensity(model = 'CIMP2007',
#'                   Temp = 20, p = 1013.25, h = 50,
#'                   u_Temp = 0.29, u_p = 1.01, u_h = 11.3)
#'
#' @export
#' @seealso [airDensity()]

uncertAirDensity <- function(model = 'CIMP2007',
                             Temp = 20, p = 1013.25, h = 50,
                             u_Temp = 2.9, u_p = 10.10, u_h = 11.3,
                             unitsENV = c('deg.C', 'hPa', '%'),
                             plot = TRUE,
                             printRelSD = TRUE) {

  if (any(model == c('Jones1978', 'HASL'))) {
    stop('The "Jones1978" and "HASL" models uncertainties are not avaiable.
         We reccomend calculating air density by using one of CIMP models.')
  }

  Temp <- convertTemperature(from = unitsENV[1], to = 'K', value = Temp)
  Temp <- c(Temp, u_Temp)
  h <- convertRelHum(from = unitsENV[3], to = 'frac', value = c(h, u_h))

  if (model == 'CIMP.approx') {
    u_form <- 2.4e-4 # Diapositiva 117 ANDRES
    p <- convertPressure(from = unitsENV[2], to = 'kPa', value = c(p, u_p))
    rho_air_exp <- expression((3.4848 * p -
                                 (0.9024 * h * exp(0.0612 * (Temp - 273.15))))/
                                (Temp) / 1000 * f_Ec)

    #uncert <- propagate::propagate(expr = rho_air_exp,
    #                               data = cbind(Temp = Temp, p = p, h = h,
    #                                            f_Ec = c(1, u_form)),
    #                               do.sim = FALSE)
    uncert <- metRology::uncert(obj = rho_air_exp,
                                x = list(Temp = Temp[1], p = p[1], h = h[1],
                                         f_Ec = c(1, u_form)[1]),
                                u = list(Temp = Temp[2], p = p[2], h = h[2],
                                         f_Ec = c(1, u_form)[2]),
                                method = 'GUM')
  }

  if (model == 'CIMP2007') {
    u_form <- 22e-6 # A Picard et al Metrologia 45 (2008) 149–155 Table 2
    p <- convertPressure(from = unitsENV[2], to = 'Pa', value = c(p, u_p))

    rho_air_exp <- expression((((p*28.96546e-3)/(
      (1 - ((p/Temp)*(1.58123e-6 + (-2.9331e-8)*(Temp - 273.15) +
                        1.1043e-10*(Temp - 273.15)^2 +
                        (5.707e-6 + (-2.051e-8)*(Temp - 273.15))*
                        (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
                           (1.2378847e-5*Temp^2 +
                              (-1.9121316e-2)*Temp + 33.93711047 +
                              (-6.3431645e3)/Temp) / p) +
                        (1.9898e-4 + -2.376e-6*(Temp - 273.15))*
                        (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
                           (1.2378847e-5*Temp^2 +
                              (-1.9121316e-2)*Temp + 33.93711047 +
                              (-6.3431645e3)/Temp) / p)^2)) +
         ((p^2/Temp^2)*(1.83e-11 + -0.765e-8*
                          (h * (1.00062 + 3.14e-8*p +
                                  5.6e-7*(Temp - 273.15)^2) *
                             (1.2378847e-5*Temp^2 +
                                (-1.9121316e-2)*Temp + 33.93711047 +
                                (-6.3431645e3)/Temp) / p)^2)))*8.314472*Temp)) *
                                (1 - (h * (1.00062 + 3.14e-8*p +
                                             5.6e-7*(Temp - 273.15)^2) *
                                        (1.2378847e-5*Temp^2 +
                                           (-1.9121316e-2)*Temp + 33.93711047 +
                                           (-6.3431645e3)/Temp) / p) *
                                   (1 - 18.01528e-3/28.96546e-3))) *
        10^-3 * f_Ec)

    #uncert <- propagate::propagate(expr = rho_air_exp,
    #                               data = cbind(Temp = Temp, p = p, h = h,
    #                                            f_Ec = c(1, u_form)),
    #                               do.sim = FALSE)

    uncert <- metRology::uncert(obj = rho_air_exp,
                                x = list(Temp = Temp[1], p = p[1], h = h[1],
                                         f_Ec = c(1, u_form)[1]),
                                u = list(Temp = Temp[2], p = p[2], h = h[2],
                                         f_Ec = c(1, u_form)[2]),
                                method = 'GUM')


    #if(F){
    #  # This chunck of code is another (non equivalent?) way to look for CIMP uncertainty
    #rho_air_exp <- expression(
    #  ((p * M_a) /
    #     ((1 - ((p/Temp) *
    #              (a0 + a1*(Temp - 273.15) + a2*(Temp - 273.15)^2 +
    #                 (b0 + b1*(Temp - 273.15)) *
    #                 (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
     #                   (A*Temp^2 + B*Temp + C + D/Temp) / p) +
    #                 (c0 + c1*(Temp - 273.15)) *
    #                 (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
    #                    (A*Temp^2 + B*Temp + C + D/Temp) / p)^2)) +
    #         ((p^2/Temp^2) *
    #            (d + e*(h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
     #                     (A*Temp^2 + B*Temp + C + D/Temp) / p)^2))) *
    #        R * Temp)) *
    #    (1 - (h * (1.00062 + 3.14e-8*p + 5.6e-7*(Temp - 273.15)^2) *
    #            (A*Temp^2 + B*Temp + C + D/Temp) / p) *
    #       (1 - M_v/ M_a)) * 10^-3 * f_Ec)

    #uncert <- propagate::propagate(
    #  expr = rho_air_exp,
    #  data = cbind(Temp = Temp, p = p, h = h,
    #               f_Ec = c(1, u_form),
    #               M_a = c(28.96546e-3, 0), # [kg / mol] molar mass of the air within laboratory
    #               M_v = c(18.01528e-3, 0), # [kg / mol] $/pm$ 0.00017e-3
    #               R = c(8.314472, 0), # [J / (mol K)]  $/pm$ 0.000015 universal gas constant
    #               A = c(1.2378847e-5, 0), # [K^-2]
    #               B = c(-1.9121316e-2, 0), # [K^-1]
    #               C = c(33.93711047, 0), # []
    #               D = c(-6.3431645e3, 0), # [K]
    #               a0 = c(1.58123e-6, 0), # [K Pa^-1]
    #               a1 = c(-2.9331e-8, 0), # [Pa^-1]
    #               a2 = c(1.1043e-10, 0), # [K^-1 Pa^-1]
    #               b0 = c(5.707e-6, 0), # [K Pa^-1]
    #               b1 = c(-2.051e-8, 0), # [Pa^-1]
    #               c0 = c(1.9898e-4, 0), # [K Pa^-1]
    #               c1 = c(-2.376e-6, 0), # [Pa^-1]
    #               d = c(1.83e-11, 0), # [K^2 Pa^-2]
    #               e = c(-0.765e-8, 0) # [K^2 Pa^-2]
    #               ),
    #  do.sim = FALSE)}
  }

  if (plot) barplot(metRology::contribs(uncert, as.sd = TRUE))
  if (printRelSD) cat('Relative uncertainty in air density estimation: ',
                      round(uncert$u.y / uncert$y * 100, 4),
                      '%\n')
  return(as.numeric(uncert$u.y))
}
