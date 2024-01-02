id_temp = function(x, inicio_temporada = NULL, fin_temporada = NULL, temporada = temporada){

  if(is.null(inicio_temporada)){
    id.temp = rep(NA, length(x))
    for(i in 1:length(data_temp$fec_ini)){
      id.temp[x >= data_temp$fec_ini[i] & x <= data_temp$fec_fin[i]] = data_temp$temp[i]
    }

  }else{
    id.temp = rep(NA, length(x))
    for(i in 1:length(data_temp$fec_ini)){
      id.temp[x >= inicio_temporada & x <= fin_temporada] = temporada
    }

  }
  return(id.temp)

}
