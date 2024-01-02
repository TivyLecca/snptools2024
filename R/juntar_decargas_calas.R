juntar_decargas_calas = function(descargas = descargas, calas = calas, ...){
  library(dplyr)
  viajes_duplicados = names(table(descargas$emb_fecha)[table(descargas$emb_fecha) > 1])
  descargas2 = descargas[!descargas$emb_fecha %in% viajes_duplicados,]


  #x = descargas[descargas$emb_fecha %in% descargas$emb_fecha[1],]

  merge_descargas_lances <- lapply(split(descargas2, descargas2$emb_fecha, drop = TRUE),
                                   function(x) {

                                     y = calas[calas$emb_fecha %in% x$emb_fecha,]
                                     #y = calas[calas$emb_fecha %in% "4",]
                                     if(dim(y)[1]>0){
                                       x$empresa = y$empresa[1]
                                       x$n_calas = dim(y)[1]
                                     }
                                     as.data.frame(x)
                                   })
  merge_descargas_lances <- merge_descargas_lances %>% lapply(as.data.frame) %>% bind_rows()

  ##
  descargas$empresa[descargas$N_ID %in% merge_descargas_lances$N_ID] = merge_descargas_lances$empresa
  descargas$n_calas[descargas$N_ID %in% merge_descargas_lances$N_ID] = merge_descargas_lances$n_calas

  return(descargas)
}
