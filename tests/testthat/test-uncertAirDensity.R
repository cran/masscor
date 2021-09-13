test_that("uncertAirDensity works", {
  airDensity(model = 'CIMP.approx')
  uncertAirDensity(model = 'CIMP.approx')
  uncertAirDensity(model = 'CIMP.approx', u_Temp = 0.29, u_p = 1.01, u_h = 11.3)
  uncertAirDensity(model = 'CIMP.approx', u_Temp = 0.1, u_p = 0.665, u_h = 10)

  airDensity(model = 'CIMP2007')
  uncertAirDensity(model = 'CIMP2007')
  uncertAirDensity(model = 'CIMP2007', u_Temp = 0.29, u_p = 1.01, u_h = 11.3)
  uncertAirDensity(model = 'CIMP2007', u_Temp = 0.1, u_p = 0.665, u_h = 10)
})
