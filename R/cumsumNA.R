

cumsumNA <- function(x){
  x[!is.na(x)] <- cumsum(x[!is.na(x)])
  return(x)
}
