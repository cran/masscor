test_that("convertMassUnitsSI", {
  expect_equal(convertMassUnitsSI(2, 'mg', 'kg'), 2e-6)
  expect_equal(convertMassUnitsSI(2, 'mg', 'ug'), 2e3)
  expect_equal(convertMassUnitsSI(2, 'g', 'ug'), 2e6)
})
