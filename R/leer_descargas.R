leer_descargas = function(descargas = descargas, ...){
  library(stringr)

  names(descargas) = c("N","ID","Puerto","Planta",
                       "Armador","Embarcacion","CBO","Matricula" ,
                       "Especie","Tolva","Nro.Reporte","TM.Descargadas",
                       "TM.Declarado","Acta.de.EIP","Fecha","Hora.Inicio",
                       "Hora.Termino","Supervisora","Mes","Zona",
                       "temporada","year")
  names(descargas) = tolower(names(descargas))

  library(stringr)


  str_trim(descargas$embarcacion, "right")

  descargas$tm.descargadas = as.numeric(gsub("^\\s+|\\s+$", "", descargas$tm.descargadas))
  descargas$embarcacion = str_trim(descargas$embarcacion, "right")
  descargas$fecha_inicio = strptime(paste0(descargas$fecha," ",descargas$hora.inicio), format = "%d/%m/%Y %H:%M")
  descargas$fecha_fin = strptime(paste0(descargas$fecha," ",descargas$hora.termino), format = "%d/%m/%Y %H:%M")
  descargas$fecha = as.Date(descargas$fecha, format = "%d/%m/%Y")
  descargas$year = format(descargas$fecha, format = "%Y")
  descargas$N_ID = 1:length(descargas$year)
  descargas$temporada = id_temp(x = descargas$fecha)
  descargas$emb_fecha = paste0(descargas$embarcacion, " ",descargas$fecha)

  return(descargas)
}
