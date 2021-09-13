## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# Render html vignetes by using devtools::document(roclets = "vignette")
# Render also pdf vignetes by using rmarkdown::render("vignettes/masscor.Rmd", "all")

## ----  eval = FALSE-----------------------------------------------------------
#  install.packages("masscor")

## ----  eval = FALSE-----------------------------------------------------------
#  devtools::install_github("Crparedes/masscor", build_vignettes = TRUE)

## ----setup--------------------------------------------------------------------
library(masscor)

## ----calibCert1---------------------------------------------------------------
Bal.1.Lab.317 <- calibCert(balanceID = '(Brand) Analytic balance lab 317', 
                           d = 0.1, d.units = 'mg',
                           indError = data.frame(nominal = c(5, 20),         # grams
                                                 error   = c(-0.2, -0.1),    # miligrams
                                                 uncert  = c(0.200, 0.201)), # miligrams
                           indError.units = c('g', 'mg', 'mg'),
                           rep = c(20, 0.02), rep.units = c('g', 'mg'),
                           eccen = c(100, 0.1), eccen.units = c('g','mg'))

## ----calibCert2---------------------------------------------------------------
print(Bal.1.Lab.317)
plot(Bal.1.Lab.317)

## ----calibCert3,  eval = FALSE------------------------------------------------
#  save(Bal.1.Lab.317, file = 'certificateBalance.1_Lab.317.RData')
#  load(file = 'certificateBalance.1_Lab.317.RData')

## ----calibCert4---------------------------------------------------------------
data(MT.XPE.204)
print(MT.XPE.204, complete = TRUE)
plot(MT.XPE.204)

## ----convMass1----------------------------------------------------------------
data(MT.XPE.204)
convMass(calibCert = MT.XPE.204, reading = 211.1342, units = 'g')

## ----convMass2----------------------------------------------------------------
convMass(calibCert = MT.XPE.204, reading = 211.1342, units = 'g', rho = 7.113, 
         rho_air = airDensity(Temp = 21.1, p = 990, h = 46.6, 
                              unitsENV = c("deg.C", "hPa", "%")))

## ----convMass1a---------------------------------------------------------------
(u_err <- uncertErrorCorr(calibCert = MT.XPE.204, reading = 211.1342, units = 'g'))
## Result rounded to three significant figures:
signif(u_err, 3)

## ----convMass1b---------------------------------------------------------------
(u_read <- uncertReading(calibCert = MT.XPE.204, reading = 211.1342, units = 'g'))

## ----convMass1c---------------------------------------------------------------
(u_w <- sqrt(u_err ^ 2 + u_read ^ 2))
(u_w <- uncertConvMass(calibCert = MT.XPE.204, reading = 211.1342, units = 'g'))

## ----MABC1--------------------------------------------------------------------
MABC(rho = 2.115, rho_w = 8, rho_air = 0.001199314) # g/cm^3

## ----airDens------------------------------------------------------------------
Temp <- 21.5 # Celcius degrees
p <- 751     # Hectopascals
h <- 58.1    # Percentaje
unitsENV = c("deg.C", "hPa", "%")
(rho_a.1 <- airDensity(Temp = Temp, p = p, h = h, unitsENV = unitsENV, model = 'CIMP2007'))
(rho_a.2 <- airDensity(Temp = Temp, p = p, h = h, unitsENV = unitsENV, model = 'CIMP.approx'))
(rho_a.3 <- airDensity(Temp = Temp, p = p, h = h, unitsENV = unitsENV, model = 'Jones1978'))
(rho_a.4 <- airDensityHASL(HASL = 2640)) # Height above sea level for Bogotá-Colombia

## ----uncertAirDens------------------------------------------------------------
(u_rho_a.1 <- uncertAirDensity(Temp = Temp, p = p, h = h, unitsENV = unitsENV,
                               u_Temp = 2.9, u_p = 10.10, u_h = 11.3, model = 'CIMP2007', 
                               plot = TRUE))

## ----MABC2--------------------------------------------------------------------
uncertMABC(rho = 2.115, rho_w = 8, rho_air = rho_a.1, 
           u_rho = 0.005/sqrt(3), u_rho_w = 0.060, u_rho_air = u_rho_a.1,
           plot = TRUE)

