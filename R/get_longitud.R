get_longitud <- function(x){
  x   <- as.character(x)
  lon <- substring(x, 1,3)
  lon <- as.numeric(gsub("\\D", "",lon))

  min <- substring(x, 5,6)
  min <- as.numeric(gsub("\\D", "",min))

  seg <- substring(x, 8, 12)
  seg <- suppressWarnings(as.numeric(sub(",", ".", seg)))
  seg[is.na(seg)] <- 0

  out <- (lon + (min + seg/60)/60)*-1

  return(suppressWarnings(out))
}
