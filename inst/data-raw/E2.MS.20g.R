serial <- 'NIM 190301'
class <- 'E2'
manufacturer <- 'Mettler Toledo'
certificate <- '4687'
traceability <- 'Set of weights class E1. PTB (CIPM MRA). Certificate number PTB 11086 19.'
Temp <- c(17.4, 17.9) ## [deg.C]
p <- c(750.4, 751.0) ## [hPa]
h <- c(70.5, 71.4) ## [%]
unitsENV <- c('deg.C', 'hPa', '%')
institution <- 'Instituto Nacional de Metrologia de Colombia'
date <- '2020-08-12'

nominal     <- 20     # [g]
convMassCor <- 0.004  # [mg]
uncert      <- 0.025  # [mg]
units       <- c('g', 'mg', 'mg')
rho         <- 8013.881  # [kg/m^3]
u_rho       <- 1.606     # [kg/m^3]
unitsrho    <- 'kg/m^3'

E2.MS.20g <- massStandard(nominal = nominal, convMassCor = convMassCor,
                          uncert = uncert, units = units, serial = serial,
                          manufacturer = manufacturer, class = class,
                          traceability = traceability, certificate = certificate,
                          Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                          institution = institution, date = date,
                          rho = rho, u_rho = u_rho, unitsrho = unitsrho)

usethis::use_data(E2.MS.20g, overwrite = TRUE)
