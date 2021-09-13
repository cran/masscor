test_that("airDensitiesWork", {
  expect_equal(round(airDensity(Temp = 22.3, p = 748.1, h = 37, units = c('deg.C', 'mmHg', '%'), model = 'Jones1978'), 8),
               1.17194e-3) # Example 1 Appendix B NIST Document
  expect_equal(round(airDensity(Temp = 23.4, p = 612.3, h = 23, units = c('deg.C', 'mmHg', '%'), model = 'Jones1978'), 7),
               round(0.95632e-3, 7)) # Example 3 Appendix B NIST Document
  expect_equal(round(airDensity(Temp = 10, p = 730, h = 40, units = c('deg.C', 'mmHg', '%'), model = 'CIMP2007'), 7),
               round(1.19569e-3, 7)) # Appendix MassMetrology p205
  expect_equal(round(airDensity(Temp = 40, p = 760, h = 40, units = c('deg.C', 'mmHg', '%'), model = 'CIMP2007'), 7),
               round(1.11497e-3, 7)) # Appendix MassMetrology p205
  expect_equal(round(airDensity(Temp = 25, p = 760, h = 40, units = c('deg.C', 'mmHg', '%'), model = 'CIMP.approx'), 8),
               round(1.1787e-3, 8))
})
