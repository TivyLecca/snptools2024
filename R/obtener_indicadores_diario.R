obtener_indicadores_diario <- function(descargas = descargas, calas = calas){
  # library(dplyr)
  # require(lubridate)
  #
  # descargas           = juntar_decargas_calas(descargas = descargas, calas = calas)
  # descargas$temporada = id_temp(descargas$fecha)
  # descargas$fecha2    = strptime(paste0(descargas$fecha, " ",descargas$hora.inicio ), format = "%Y-%m-%d %H:%M")
  # descargas$duracion  = omitir_outliers(descargas$duracion)

  indicadores = lapply(split(descargas, descargas$fecha, drop = TRUE),
                       function(x) {

                         captura  = sum(x$tm.descargadas)

                         capturas_empresas = tapply(x$tm.descargadas, x$empresas, sum)

                         if(length(capturas_empresas[names(capturas_empresas) %in% "AUSTRAL"]) >0){
                           captura_AUSTRAL =   as.numeric(capturas_empresas[names(capturas_empresas) %in% "AUSTRAL"])
                         }else{
                           captura_AUSTRAL = 0
                         }

                         if(length(capturas_empresas[names(capturas_empresas) %in% "CAPRICORNIO"]) >0){
                           captura_CAPRICORNIO =   as.numeric(capturas_empresas[names(capturas_empresas) %in% "CAPRICORNIO"])
                         }else{
                           captura_CAPRICORNIO = 0
                         }


                         if(length(capturas_empresas[names(capturas_empresas) %in% "CENTINELA"]) >0){
                           captura_CENTINELA =   as.numeric(capturas_empresas[names(capturas_empresas) %in% "CENTINELA"])
                         }else{
                           captura_CENTINELA = 0
                         }

                         if(length(capturas_empresas[names(capturas_empresas) %in% "CFG"]) >0){
                           captura_CFG =   as.numeric(capturas_empresas[names(capturas_empresas) %in% "CFG"])
                         }else{
                           captura_CFG = 0
                         }

                         if(length(capturas_empresas[names(capturas_empresas) %in% "DIAMANTE"]) >0){
                           captura_DIAMANTE =   as.numeric(capturas_empresas[names(capturas_empresas) %in% "DIAMANTE"])
                         }else{
                           captura_DIAMANTE = 0
                         }

                         if(length(capturas_empresas[names(capturas_empresas) %in% "EXALMAR"]) >0){
                           captura_EXALMAR =   as.numeric(capturas_empresas[names(capturas_empresas) %in% "EXALMAR"])
                         }else{
                           captura_EXALMAR = 0
                         }

                         if(length(capturas_empresas[names(capturas_empresas) %in% "HAYDUK"]) >0){
                           captura_HAYDUK =   as.numeric(capturas_empresas[names(capturas_empresas) %in% "HAYDUK"])
                         }else{
                           captura_HAYDUK = 0
                         }

                         if(length(capturas_empresas[names(capturas_empresas) %in% "TASA"]) >0){
                           captura_TASA =   as.numeric(capturas_empresas[names(capturas_empresas) %in% "TASA"])
                         }else{
                           captura_TASA = 0
                         }

                         w_calas = calas[calas$emb_fecha %in% x$emb_fecha, ]
                         fecha     = x$fecha[1]
                         temporada = x$temporada[1]
                         barcos_registrados  = length(unique(x$matricula))
                         viajes_registrados  = length(x$matricula)

                         # quitar viajes duplicados
                         id_matricula = table(x$matricula)
                         id_matricula = names(id_matricula[id_matricula == 1])
                         x = x[x$matricula %in% id_matricula,]

                         # indicadores esfuerzo
                         barcos   = length(unique(x$matricula))
                         viajes   = length(x$matricula)
                         duracion = as.numeric(mean(x$duracion, na.rm = T))
                         duracion_median = as.numeric(median(x$duracion, na.rm = T))
                         q0.1_duracion = as.numeric(quantile(x$duracion, probs = 0.1,na.rm = T))
                         q0.9_duracion = as.numeric(quantile(x$duracion, probs = 0.9,na.rm = T))

                         bodega            = sum(x$cbo)
                         recorrio          = mean(x$recorrido, na.rm = T)*viajes
                         recorrido_medio   = mean(x$recorrido, na.rm = T)
                         q0.1_recorrido_medio = as.numeric(quantile(x$recorrido, probs = 0.1,na.rm = T))
                         q0.9_recorrido_medio = as.numeric(quantile(x$recorrido, probs = 0.9,na.rm = T))
                         recorrido_mediana = median(x$recorrido, na.rm = T)

                         # indicadores de cpue
                         z = x[!is.na(x$duracion),]


                         cpue_dur = mean(z$tm.descargadas/as.numeric(z$duracion))

                         y = x[!is.na(x$n_calas),]
                         if(dim(y)[1]>0){
                           cpue_lan = mean(y$tm.descargadas/y$n_calas)
                         }else{
                           cpue_lan = NA
                         }
                         cpue_rec = mean(z$tm.descargadas/as.numeric(z$recorrido))
                         cpue_bod = mean(x$tm.descargadas/x$cbo)
                         lances    = mean(y$n_calas, na.rm = T)

                         # indicadores espaciales
                         if(dim(w_calas)[1]>0){
                           dist_costa  = mean(w_calas$dc, na.rm = T)
                           latitud     = mean(w_calas$latitud, na.rm = T)
                           area        = vmsR:::covered_area2(dc = w_calas$dc, lat = w_calas$latitud, grado = 1/12)
                           area_pesca  = area$area
                           cala_latitud_maxima    = max(w_calas$latitud , na.rm = T)
                           cala_latitud_min       = min(w_calas$latitud , na.rm = T)
                           cala_longitud_maxima   = max(w_calas$longitud, na.rm = T)
                           cala_longitud_minima   = min(w_calas$longitud, na.rm = T)
                         }else{
                           dist_costa = NA
                           latitud= NA
                           area_pesca = NA
                           cala_latitud_maxima    = NA
                           cala_latitud_min       = NA
                           cala_longitud_maxima   = NA
                           cala_longitud_minima   = NA
                         }
                         #area_pesca = NA
                         vms_latitud_maxima    = max(x$lat_max, na.rm = T)
                         vms_latitud_min       = min(x$lat_min, na.rm = T)
                         vms_longitud_maxima   = max(x$lon_max, na.rm = T)
                         vms_longitud_minima   = min(x$lon_max, na.rm = T)


                         dist_maxima_zarpe = mean(x$maxDist_zarpe, na.rm = T)
                         dist_maxima_arribo= mean(x$maxDist_arribo, na.rm = T)
                         velocidad_maxima  = mean(x$vel_max, na.rm = T)
                         inercia_espacial  = mean(x$inercia_espacial, na.rm = T)

                         cbind.data.frame(fecha, temporada, barcos_registrados, viajes_registrados, barcos, viajes, duracion, duracion_median, lances,
                                          bodega, recorrio, recorrido_medio, captura, cpue_dur, cpue_lan, cpue_rec, cpue_bod,

                                          area_pesca,
                                          dist_costa,
                                          latitud,
                                          vms_latitud_maxima,
                                          vms_latitud_min,
                                          vms_longitud_maxima,
                                          vms_longitud_minima,
                                          dist_maxima_zarpe,
                                          dist_maxima_arribo,
                                          velocidad_maxima,
                                          inercia_espacial,
                                          cala_latitud_maxima,
                                          cala_latitud_min,
                                          cala_longitud_maxima,
                                          cala_longitud_minima, captura_AUSTRAL, captura_CAPRICORNIO, captura_CENTINELA, captura_CFG, captura_DIAMANTE, captura_EXALMAR,
                                          captura_HAYDUK, captura_TASA)
                       })
  indicadores <- indicadores %>% lapply(as.data.frame) %>% bind_rows()
  # indicadores$semana = NULL
  #
  # for(i in unique(indicadores$temporada)){
  #   indicadores$semana[indicadores$temporada %in% i]  = generar_semanas(indicadores$fecha[indicadores$temporada %in% i])+1
  # }
  return(indicadores)
}

