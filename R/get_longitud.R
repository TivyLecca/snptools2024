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

get_longitud_2=
  function (x)
  {
    x <- as.character(x)
    lon <- substring(x, 1, 3)
    lon <- as.numeric(gsub("\\D", "", lon))
    min <- substring(x, 6, 7)
    min <- as.numeric(gsub("\\D", "", min))
    seg <- substring(x, 9, 13)
    seg <- suppressWarnings(as.numeric(sub(",", ".", seg)))
    seg[is.na(seg)] <- 0
    out <- (lon + (min + seg/60)/60) * -1
    return(suppressWarnings(out))
  }

get_longitud_3=
  function (x)
  {
    x <- as.character(x)
    lon <- substring(x, 1, 3)
    lon <- as.numeric(gsub("\\D", "", lon))
    min <- substring(x, 5, 10)
    min <- as.numeric(min)
    lon[is.na(lon)] <- 0
    min[is.na(min)] <- 0
    out <- (lon + (min)/60) * -1
    return(suppressWarnings(out))
  }
