get_latitud <- function(x){
  x <- as.character(x)
  lat <- substring(x, 1,2)
  lat <- as.numeric(lat)
  min <- substring(x, 4,5)
  min <- as.numeric(min)
  seg <- substring(x, 7, 11)
  seg <- as.numeric(seg)

  lat[is.na(lat)] <- 0
  min[is.na(min)] <- 0
  seg[is.na(seg)] <- 0

  out <- (lat + (min + seg/60)/60)*-1

  return(suppressWarnings(out))
}

get_latitud_2=
  function (x)
  {
    x <- as.character(x)
    lat <- substring(x, 1, 2)
    lat <- as.numeric(lat)
    min <- substring(x, 5, 6)
    min <- as.numeric(min)
    seg <- substring(x, 8, 12)
    seg <- as.numeric(seg)
    lat[is.na(lat)] <- 0
    min[is.na(min)] <- 0
    seg[is.na(seg)] <- 0
    out <- (lat + (min + seg/60)/60) * -1
    return(suppressWarnings(out))
  }

get_latitud_3=
  function (x)
  {
    x <- as.character(x)
    lat <- substring(x, 1, 2)
    lat <- as.numeric(lat)
    min <- substring(x, 4, 9)
    min <- as.numeric(min)
    lat[is.na(lat)] <- 0
    min[is.na(min)] <- 0
    out <- (lat + (min)/60) * -1
    return(suppressWarnings(out))
  }
