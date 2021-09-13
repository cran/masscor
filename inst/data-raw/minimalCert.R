minimalCert <- calibCert(balanceID = 'minimal',
                         d = 0.1, d.units = 'mg',
                         indError = data.frame(nominal = c(5, 20),         # grams
                                               error   = c(-0.2, -0.1),    # miligrams
                                               uncert  = c(0.200, 0.201)), # miligrams
                         indError.units = c('g', 'mg', 'mg'),
                         rep = c(20, 0.03), rep.units = c('g', 'mg'),
                         eccen = c(20, 0.1), eccen.units = c('g','mg'))

usethis::use_data(minimalCert, overwrite = TRUE)
