# https://stackoverflow.com/questions/43368073/how-to-remove-common-parts-of-strings-in-a-character-vector-in-r
#
mf <- function(x) {
  xsplit = strsplit(x, split = '')
  xdfm <- as.data.frame(do.call(rbind, xsplit))
  res <- list()
  for (i in 1:ncol(xdfm)){
    if (!all(xdfm[, i] == xdfm[1, i])){
      res[[length(res) + 1]] <- as.character(xdfm[, i])
    }
  }
  res <- as.data.frame(do.call(rbind, res))
  res <- apply(res, 2, function(x) paste(x, collapse = "_"))
  return(res)
}


convertTemperature <- function(from, to, value) {
 if (from == to) return(value)
 if (from == 'deg.C' && to == 'K') return(value + 273.15)
 if (from == 'K' && to == 'deg.C') return(value - 273.15)
}

convertPressure <- function(from, to, value) {
  if (from == to) return(value)
  # To Pascals, then to wahtever neccesary
  if (from == 'hPa') value  <- value * 100
  if (from == 'kPa') value  <- value * 1000
  if (from == 'mmHg') value  <- value * 133.322387415
  if (to == 'Pa') return(value)
  if (to == 'hPa') return(value / 100)
  if (to == 'kPa') return(value / 1000)
  if (to == 'mmHg') return(value / 133.322387415)
}


convertRelHum <- function(from, to, value) {
  if (from == to) return(value)
  if (from == 'frac' && to == '%') return(value * 100)
  if (from == '%' && to == 'frac') return(value / 100)
}
