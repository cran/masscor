massSTD  <- c(0.5, 5, 10, 20, 50, 100, 200, 500, 1000, 1500, 2000)  ## [g]
indError <- c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, -0.01, -0.02, -0.04, -0.04) ## [g]
uncert   <- c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.02, 0.03, 0.04) ## [g]
d <- 10 ## [mg]
traceability <- c('Set of weights class E2. Certificate number 1473 D-K 17296, 2019-05-10.',
                  'Set of weights class F2. Certificate number 5028 INM, 2020-12-09.')

MT.XP.2002 <- calibCert(balanceID = 'MT XP 2002', serial = 'B248568374', certificate = 5142,
                        d = d, d.units = 'mg',
                        indError = data.frame(massSTD, indError, uncert),
                        indError.units = c('g', 'g', 'g'),
                        rep = data.frame(load = c(2, 1000, 2000), sd = c(0, 5, 5)), rep.units = c('g', 'mg'),
                        eccen = c(1000, 20), eccen.units = c('g', 'mg'),
                        classSTD = 'E2 / F2', traceability = traceability,
                        Temp = c(18.8, 29.1), ## [deg.C]
                        p = c(751.4, 751.8), ## [hPa]
                        h = c(60.0, 63.5), ## [%]
                        unitsENV = c('deg.C', 'hPa', '%'),
                        institution = 'Instituto Nacional de Metrologia de Colombia',
                        date = '2021-03-12')

usethis::use_data(MT.XP.2002, overwrite = TRUE)
