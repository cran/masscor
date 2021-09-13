#' Creates an object of class \code{"massStandardKit"}.
#'
#' The object of class \code{"massStandardKit"} is a wrapper for
#' several objects of class \code{"massStandard"}.
#' The object of class \code{"massStandard"} contains the
#' calibration information of a mass standard
#' that is used in routine balance verification (e g. to calculate
#' normalized error. See [normalizedError()]).
#' When several mass standards are part of a kit their information
#' can be in conveniently be stored together in a \code{"massStandardKit"} class object.
#'
#' When two mass standards of equal nominal mass are present in the same kit
#' (typically those of nominal mass in the form \eqn{2\times10^n}),
#' it is necessary to make a distinction
#' between them because their real mass are not likely
#' to be exactly the same. Physically this
#' distinction is achieved by marking one of them, for example, with
#' a dot in the head of the knot
#' weights, or by bending the final part of the lifted
#' extreme in wire weights. To differentiate those
#' duplicated mass standards in the \code{"massStandardKit"}
#' class object, its \code{nominal} mass value
#' must be entered as a character including an
#' asterisk after the value (ie. \code{'200*'}
#' instead of just entering \code{200}).
#' The function returns error if the kit contains
#' duplicated mass standards and no
#' differentiation is indicated.
#'
#' @inheritSection calibCert unitsENV
#'
#' @seealso [massStandard()], [normalizedError()]
#' @inheritParams massStandard
#' @inheritParams calibCert
#' @param nominal vector with nominal mass for each standard all in the same units.
#'   If no weights are duplicated in the kit the vector can be numeric.
#'   If there are duplicated standards some elements must be character type.
#'   See Details.
#' @param convMassCor numeric vector with conventional mass corrections for
#'   each of the mass standard declared in \code{nominal}.
#' @param uncert numeric vector with standard uncertainties of the
#'   conventional mass corrections
#'   for each mass standard declared in \code{nominal}.
#' @param rho numeric vector with densities for each mass standard declared in \code{nominal}.
#'   If not provided the default value of 8.000 g cm\eqn{^{-3}} is used for all weights.
#' @param u_rho numeric vector with uncertainties in the density for each mass standard
#'   declared in \code{nominal}.
#'   If not provided the default value of 0.060 g cm\eqn{^{-3}} is used for all weights.
#' @param unitsrho Units of the density of the mass standards. Default is \code{'g/cm^3'}.
#'
#' @return Object of class \code{"massStandard"} with calibration information of a
#'   mass standards kit.
#'
#' @examples
#' nominal     <- c(1000, 500, 200, '200*', 100)   # [g]
#' convMassCor <- c(0.0, -0.03, 0.03, 0.06, 0.00)  # [mg]
#' uncert      <- c(0.50, 0.25, 0.10, 0.10, 0.05)  # [mg]
#' units       <- c('g', 'mg', 'mg')
#'
#' rho         <- c(8012.217, 8008.640, 8011.126, 8010.722, 8010.935)# [kg/m^3]
#' u_rho       <- c(0.096, 0.090, 0.160, 0.160, 0.321)# [kg/m^3]
#' unitsrho    <- 'kg/m^3'
#'
#' MS.Kit1 <- massStandardKit(nominal = nominal, convMassCor = convMassCor, uncert = uncert,
#'                            units = units, rho = rho, u_rho = u_rho, unitsrho = unitsrho)
#' print(MS.Kit1)
#'
#' @export

massStandardKit <- function(nominal, convMassCor, uncert, units = c('g', 'mg', 'mg'),
                            serial = NULL, manufacturer = NULL, class = NULL,
                            certificate = NULL, traceability = NULL,
                            Temp = NULL, p = NULL, h = NULL,
                            unitsENV = c('deg.C', 'hPa', '%'),
                            expanded = TRUE, k = 2,
                            rho = NULL, u_rho = NULL, unitsrho = 'g/cm^3',
                            institution = NULL, date = NULL, add.info = NULL) {

  # nominal <- as.character(nominal)
  #if (class(nominal) %in% c('numeric', 'integer')) {
    if (length(unique(nominal)) < length(nominal)) {
      stop("There are elements with same nominal mass and need to be differentiated by some way.
           See details in ??massStandardKit")
    }#}

  if (length(nominal) != length(convMassCor) || length(nominal) != length(uncert)) {
     stop('Vectors "nominal", "convMassCor" and "uncert" must all have equal lenght.' )
  }

  # all vectors in same units
  convMassCor <- convertMassUnitsSI(value = convMassCor, from = units[2], to = units[1])
  uncert <- convertMassUnitsSI(value = uncert, from = units[3], to = units[1])

  if (missing(rho)) rho <- rep(8000, length(nominal))
  if (missing(u_rho)) u_rho <- rep(60, length(nominal))

  massStandardKit <- list()
  XX <- data.frame(nominal = rep(NA, length(nominal)),
                   convMassCor = convMassCor,
                   uncert = uncert, expandUncert = uncert,
                   rho = rho, u_rho = u_rho,
                   diff  = rep('', length(nominal)))
  if (expanded) {
    XX$uncert <- XX$uncert / k
  } else {
    XX$expandUncert <- XX$expandUncert * k
  }


  for (i in 1:length(nominal)) {
    if ('*' %in% strsplit(nominal[i], split = '*')[[1]]) {
      nom <- as.numeric(
        paste0(strsplit(nominal[i], split = '')[[1]][-length(strsplit(nominal[i], split = '')[[1]])],
               collapse = ''))
      XX$diff[i] <- '*'
    } else {
      nom <- as.numeric(nominal[i])
    }
    if (is.na(nom)) {
      stop('At least one value in vector "nominal" is not numeric or does not have the format "(number)*"
           See details in ??massStandardKit')
    }

    massStandardKit[[nominal[i]]] <- massStandard(nominal = nom, convMassCor = convMassCor[i],
                                                  uncert = uncert[i],
                                                  units = rep(units[1], 3),
                                                  expanded = expanded, k = k, unitsrho = unitsrho,
                                                  partofakit = TRUE)
    XX$nominal[i] <- nom
    massStandardKit[[nominal[i]]]$unitENV <- NULL
    massStandardKit[[nominal[i]]]$rho <- rho[i]
    massStandardKit[[nominal[i]]]$u_rho <- u_rho[i]
  }

  if (!missing(serial)) massStandardKit$serial <- serial
  if (!missing(manufacturer)) massStandardKit$manufacturer <- manufacturer
  if (!missing(class)) massStandardKit$class <- class
  if (!missing(certificate)) massStandardKit$certificate <- certificate
  if (!missing(traceability)) massStandardKit$traceability <- traceability
  if (!missing(Temp)) massStandardKit$Temp <- Temp
  if (!missing(p)) massStandardKit$p <- p
  if (!missing(h)) massStandardKit$h <- h
  if (!missing(unitsENV)) massStandardKit$unitsENV <- unitsENV
  if (!missing(institution)) massStandardKit$institution <- institution
  if (!missing(date)) massStandardKit$date <- date
  if (!missing(add.info)) massStandardKit$add.info <- add.info

  massStandardKit$merged <- XX

  class(massStandardKit) <- 'massStandardKit'
  return(massStandardKit)
}
