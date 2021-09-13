test_that("AndresDiapositiva105", {
  data(minimalCert)
  expect_equal(convMass(reading = 12.4835, calibCert = minimalCert), 12.4837)
  expect_equal(convMass(reading = 12483.5, units = 'mg', calibCert = minimalCert), 12483.7)
})
# usethis::use_test("")
