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

nominal     <- c(1000, 500, 200, '200*', 100,
                 50, 20, '20*', 10,
                 5, 2, '2*', 1,
                 0.500, 0.200, '0.200*', 0.100,
                 0.050, 0.020, '0.020*', 0.010,
                 0.005, 0.002, '0.002*', 0.001)   # [g]
convMassCor <- c(0.0, -0.03, 0.03, 0.06, 0.00,
                 0.010, 0.004, 0.010, 0.012,
                 0.007, 0.020, 0.007, -0.004,
                 0.000, 0.009, 0.006, 0.007,
                 0.001, 0.003, 0.002, 0.001,
                 0.002, 0.001, 0.002, 0.002)  # [mg]
uncert      <- c(0.50, 0.25, 0.10, 0.10, 0.05,
                 0.03, 0.025, 0.025, 0.020,
                 0.016, 0.012, 0.012, 0.010,
                 0.008, 0.006, 0.006, 0.005,
                 0.004, 0.003, 0.003, 0.003,
                 0.003, 0.003, 0.003, 0.003)  # [mg]
units       <- c('g', 'mg', 'mg')

rho         <- c(8012.217, 8008.640, 8011.126, 8010.722, 8010.935,
                 8011.937, 8013.881, 8013.721, 8006.924,
                 8006.285, 8009.632, 8008.352, 7997.479,
                 rep(8000, 12)) # [kg/m^3]
u_rho       <- c(0.096, 0.090, 0.160, 0.160, 0.321,
                 0.642, 1.606, 1.606, 5.770,
                 11.538, 28.869, 28.860, 57.563,
                 rep(60, 12))   # [kg/m^3]
unitsrho    <- 'kg/m^3'

Box.E2.MS.Kit <- massStandardKit(nominal = nominal, convMassCor = convMassCor,
                                       uncert = uncert, units = units, serial = serial,
                                       manufacturer = manufacturer, class = class,
                                       traceability = traceability, certificate = certificate,
                                       Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                                       institution = institution, date = date,
                                       rho = rho, u_rho = u_rho, unitsrho = unitsrho)

usethis::use_data(Box.E2.MS.Kit, overwrite = TRUE)
