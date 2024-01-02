generar_indicadores <- function(descargas = descargas, calas = calas,
                                dir_data = dir_data,inicio_temporada = inicio_temporada,
                                fin_temporada = fin_temporada,temporada = temporada){
  library(dplyr)
  require(lubridate)
  descargas           = juntar_decargas_calas(descargas = descargas, calas = calas)
  descargas$temporada = id_temp(x = descargas$fecha,
                                inicio_temporada = inicio_temporada, fin_temporada = fin_temporada,temporada = temporada)
  descargas$fecha2    = strptime(paste0(descargas$fecha, " ",descargas$hora.inicio ), format = "%Y-%m-%d %H:%M")
  descargas$duracion  = omitir_outliers(descargas$duracion)

  descargas$semana = NULL
  for(i in unique(descargas$temporada)){
    descargas$semana[descargas$temporada %in% i]  = generar_semanas(descargas$fecha[descargas$temporada %in% i])+1
  }

  ind_diarios    = obtener_indicadores_diario(descargas = descargas, calas = calas) # corregir indicadores
  ind_semanales  = obtener_indicadores_semanal(descargas = descargas, calas = calas)
  ## guardar los indicadores
  for(idiario in unique(ind_diarios$temporada)){
    season = ind_diarios[ind_diarios$temporada %in% idiario,]
    save(season, file = paste0(dir_data, "/INDICADORES/diarios/",idiario,".RData"))
  }

  for(isemana in unique(ind_semanales$temporada)){
    season = ind_semanales[ind_semanales$temporada %in% isemana,]
    save(season, file = paste0(dir_data, "/INDICADORES/semanales/",isemana,".RData"))
  }

  output = list()
  output$ind_diarios     = ind_diarios
  output$ind_semanales   = ind_semanales
  return(output)
}
