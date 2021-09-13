#' Information of balance calibration certificate
#'
#' Creates an object of class \code{calibCert} that contains
#' the information of a balance calibration certificate.
#' The object can later be used to correct mass readings
#' and calculate mass uncertainties.
#' Mandatory arguments for this function are the balance division scale (\code{d}),
#' the results of the indication error test (\code{indError}),
#' the results of repeatability test (\code{rep}),
#' and the results of the eccentricity test (\code{eccen}).
#'
#' The units of \code{d}, \code{indError}, \code{rep} and \code{eccen}
#' shall be provided to the arguments \code{d.units}, \code{indError.units},
#' \code{rep.units} and \code{eccen.units}, respectively. The units can be
#' any multiple or subdivision of the SI unit for mass, the kilogram.
#' The greek letter \eqn{\mu} used to represent a millionth part,
#' is replaced by the vocal \code{u}.
#' Remember that both \code{R} and the SI prefixes are case sensitive.
#'
#' @section unitsENV:
#' Temperature units (\code{Temp}) can be either \code{'deg.C'}
#' (for Celsius degrees) or \code{'K'}.
#' Pressure units (\code{p}) can be any of \code{'mmHg'},
#' \code{'Pa'}, \code{'hPa'} or \code{'kPa'}.
#' Relative humidity (\code{h}) can be expressed as
#' fraction (\code{'frac'}) or as percentage (\code{'%'}).
#' A typical arrangement for the parameter \code{unitsENV}
#' would be \code{c('deg.C', 'hPa', '%')}.
#'
#'
#' @param balanceID character with balance identification. May include
#'   balance model, brand or internal location.
#' @param serial serial number of the balance.
#' @param certificate character with the calibration certificate
#'   number and date of issue.
#' @param d division scale of the balance.
#' @param d.units character with the units of the division scale of the
#'   balance. Default value is \code{'mg'}. See Details for more options.
#' @param indError \code{data.frame} with the indication error test results in
#'   three columns containing balance reading, indication error and associated
#'   uncertainties, respectively, for at least two mass standards.
#' @param indError.units character of length three with the units for each column
#'   in the data frame provided in \code{indError}. Default value is
#'   \code{c('g', 'mg', 'mg')}.
#' @param expanded if \code{TRUE} (the default), uncertainties provided in
#'   \code{indError} are assumed to be expanded uncertainties,
#'   instead of standard uncertainties.
#' @param k coverage factor for the expanded uncertainties when
#'   \code{expanded = TRUE}.
#' @param rep results of the repeatability test. If the test is performed
#'   in only one point, then \code{rep} is a numeric vector of length two
#'   with the balance load and standard deviation for the same object
#'   measured under repeatability conditions. If the test is performed
#'   at more than one point \code{rep} is a data frame with balance loadings
#'   in the first column and standard deviations in the second.
#' @param rep.units character of length two with the units for balance loads
#'   and standard deviations provided in \code{rep}. Default value is
#'   \code{c('g', 'mg')}.
#' @param eccen numeric vector of length two with balance load and maximal
#'   reading difference obtained during eccentricity test.
#' @param eccen.units character of length two with the units for balance loads
#'   and maximal reading difference provided in \code{eccen}. Default value is
#'   \code{c('g', 'mg')}.
#' @param classSTD character with the class of the mass standards used.
#' @param traceability character with information regarding the
#'   traceability of the calibration.
#' @param p barometric pressure at the moment of the calibration.
#' @param Temp ambient temperature at the moment of the calibration.
#' @param h relative humidity at the moment of the calibration.
#' @param unitsENV character vector of length three with the units of
#'   \code{p}, \code{Temp} and \code{h}.
#'   Default is \code{c('deg.C', 'hPa', '\%')}. See **unitsENV** below for more options.
#' @param institution character with the identification of the calibration laboratory.
#' @param accreditation character with the accreditation information of the
#'   calibration laboratory.
#' @param date character with the date of the measurements.
#' @param add.info named list or vector with any additional details included in
#'   the calibration certificate.
#'
#' @return Object of class \code{calibCert} with information of
#'   the calibration certificate for a balance.
#'
#' @seealso S3 methods [print.calibCert()] and [plot.calibCert()] are available.
#'   See [convertMassUnitsSI()] for information about mass units.
#'
#' @examples
#' massSTD  <- c(0.01, 0.5, 1, 10, 20, 50, 100, 120, 150, 200, 220)  ## [g]
#' indError <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.2, -0.2) ## [mg]
#' uncert   <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.5) ## [mg]
#' d <- 0.1 ## [mg]
#'
#' Balance.D1 <- calibCert(balanceID = 'MT XPE 204', serial = 'B403223982',
#'                         d = d, d.units = 'mg',
#'                         indError = data.frame(massSTD, indError, uncert),
#'                         indError.units = c('g', 'mg', 'mg'),
#'                         rep = data.frame(load = c(0.1, 100, 220),
#'                                          sd = c(0.00, 0.04, 0.03)),
#'                         rep.units = c('g', 'mg'),
#'                         eccen = c(100, 0.1), eccen.units = c('g', 'mg'),
#'                         Temp = c(17.4, 17.9), ## [deg.C]
#'                         p = c(750.4, 751.0), ## [hPa]
#'                         h = c(70.5, 71.4), ## [%]
#'                         unitsENV = c('deg.C', 'hPa', '%'),
#'                         institution = 'Instituto Nacional de Metrologia de Colombia',
#'                         date = '2021-03-18')
#' print(Balance.D1)
#'
#' @export
#'

calibCert <- function (balanceID = 'BalanceID', serial = NULL, certificate = NULL,
                       d, d.units = 'mg',
                       indError, indError.units = c('g', 'mg', 'mg'),
                       expanded = TRUE, k = 2,
                       rep, rep.units = c('g', 'mg'),
                       eccen, eccen.units = 'mg',
                       classSTD = NULL, traceability = NULL,
                       Temp = NULL, p = NULL, h = NULL,
                       unitsENV = c('deg.C', 'hPa', '%'),
                       institution = NULL, accreditation = NULL,
                       date = NULL, add.info = NULL) {

  if (nrow(indError) < 2) stop('At least two calibration points must be provided.')

  if (expanded) {
    indErrorUncertExpand <- indError[, 3]
    indError[, 3] <- indError[, 3] / k
  } else {
    indErrorUncertExpand <- indError[, 3] * k
  }

  oldIndError <- indError
  oldIndError[, 3] <- indErrorUncertExpand
  indError <- data.frame(indError[, 1],
                         convertMassUnitsSI(from = indError.units[2],
                                            to = indError.units[1],
                                            value = indError[, 2]),
                         convertMassUnitsSI(from = indError.units[3],
                                            to = indError.units[1],
                                            value = indError[, 3]),
                         convertMassUnitsSI(from = indError.units[3],
                                            to = indError.units[1],
                                            value = indErrorUncertExpand))

  oldRep <- rep
  if (class(rep) %in% c('numeric', 'integer')) {
    rep[1] <- convertMassUnitsSI(from = rep.units[1], to = indError.units[1],
                                 value = rep[1])
    rep[2] <- convertMassUnitsSI(from = rep.units[2], to = indError.units[1],
                                 value = rep[2])
    rep.natur <- 'single'
  } else {
    rep[, 1] <- convertMassUnitsSI(from = rep.units[1],
                                   to = indError.units[1], value = rep[, 1])
    rep[, 2] <- convertMassUnitsSI(from = rep.units[2],
                                   to = indError.units[1], value = rep[, 2])
    rep.natur <- 'multi'
  }

  oldEccen <- eccen
  eccen[1] <- convertMassUnitsSI(from = eccen.units[1], to = indError.units[1],
                                 value = eccen[1])
  eccen[2] <- convertMassUnitsSI(from = eccen.units[2], to = indError.units[1],
                                 value = eccen[2])

  calibCert <- list(balanceID = balanceID,
                    standardUnits = indError.units[1],
                    d = convertMassUnitsSI(from = d.units,
                                           to = indError.units[1],
                                           value = d),
                    indError = indError, k = k,
                    rep = rep, rep.natur = rep.natur,
                    eccen = eccen,
                    orgdUnits = d.units,
                    orgIndErrorUnits = indError.units,
                    orgRepUnits = rep.units,
                    orgEccenUnits = eccen.units,
                    oldIndError = oldIndError,
                    oldRep = oldRep,
                    oldEccen = oldEccen)



  if (!missing(serial)) calibCert$serial <- serial
  if (!missing(classSTD)) calibCert$classSTD <- classSTD
  if (!missing(traceability)) calibCert$traceability <- traceability
  if (!missing(p)) calibCert$p <- p
  if (!missing(Temp)) calibCert$Temp <- Temp
  if (!missing(h)) calibCert$h <- h
  if (!missing(unitsENV)) calibCert$unitsENV <- unitsENV
  #if (!missing(p) && !missing(Temp) && !missing(h) && !missing(unitsENV)) {
  #  calibCert$airDensity <- airDensity(Temp = Temp, p = p, h = h,
  #                                     unitsENV = unitsENV)
  #}
  if (!missing(institution)) calibCert$institution <- institution
  if (!missing(accreditation)) calibCert$accreditation <- accreditation
  if (!missing(date)) calibCert$date <- date
  if (!missing(add.info)) calibCert$add.info <- add.info
  #if (!missing()) calibCert$ <-

  class(calibCert) <- 'calibCert'
  return(calibCert)
}
