
obtener_descargas = function(dir_data = dir_data, descargas = NULL, empresas = empresas, data_cbo = data_cbo,
                             inicio_temporada = inicio_temporada, fin_temporada = fin_temporada,temporada = temporada){
  library(stringi)
  library(stringr)





  #descargas = NULL
  all_empresas_descargas = NULL
  for(i in empresas){
    setwd(paste0(dir_data, i,"/DESCARGAS")  )
    all_descargas = NULL
    archivos = dir()

    if(!is.null(descargas)){
      existe_archivo = archivos[archivos %in% descargas]
      if(length(existe_archivo)==1){
        all_descargas   = read.csv(descargas,fileEncoding = "latin1")
        names(all_descargas) = c( "X","N.","ID" ,"Puerto" ,"Planta", "Armador", "Embarcacion", "CBO" ,
                     "Matricula" ,"Especie","Tolva", "Nro..Reporte", "TM.Descargadas","TM.Declarado","Acta.de.EIP","Fecha" ,
                     "Hora.Inicio","Hora.Termino" ,"Supervisora" ,"Mes","Zona","temporada","anho" )

        #print(names(all_descargas))
      }else{
        print(paste0(i," no contiene el archivo ",descargas))
      }

    }else{
      for(ii in archivos){
        files_descargas = read.csv(ii, fileEncoding = "latin1")
        all_descargas   = rbind(files_descargas, all_descargas)
        names(all_descargas) = c( "X","N.","ID" ,"Puerto" ,"Planta", "Armador", "Embarcacion", "CBO" ,
                                  "Matricula" ,"Especie","Tolva", "Nro..Reporte", "TM.Descargadas","TM.Declarado","Acta.de.EIP","Fecha" ,
                                  "Hora.Inicio","Hora.Termino" ,"Supervisora" ,"Mes","Zona","temporada","anho" )
      }
    }
    names(all_descargas) = tolower(stri_trans_general(names(all_descargas),"Latin-ASCII"))
    all_empresas_descargas = rbind(all_descargas, all_empresas_descargas)
  }

  #trimws(gsub(pattern = "Â", replacement = "", x = all_empresas_descargas$embarcacion))
  all_empresas_descargas$embarcacion = str_trim(gsub(pattern = "Â", replacement = "", x = all_empresas_descargas$embarcacion), "right")
  ## AQUI FALTARIA PEGAR LA BODEGA


  all_empresas_descargas$tm.descargadas = as.numeric(gsub("^\\s+|\\s+$", "", all_empresas_descargas$tm.descargadas))
  all_empresas_descargas$embarcacion = str_trim(all_empresas_descargas$embarcacion, "right")
  all_empresas_descargas$fecha_inicio = strptime(paste0(all_empresas_descargas$fecha," ",all_empresas_descargas$hora.inicio), format = "%d/%m/%Y %H:%M")
  all_empresas_descargas$fecha_fin = strptime(paste0(all_empresas_descargas$fecha," ",all_empresas_descargas$hora.termino), format = "%d/%m/%Y %H:%M")
  all_empresas_descargas$fecha = as.Date(all_empresas_descargas$fecha, format = "%d/%m/%Y")
  all_empresas_descargas$year = format(all_empresas_descargas$fecha, format = "%Y")
  all_empresas_descargas$N_ID = 1:length(all_empresas_descargas$year)
  all_empresas_descargas$temporada = id_temp(x = all_empresas_descargas$fecha,inicio_temporada = inicio_temporada,fin_temporada = fin_temporada, temporada = temporada)
  all_empresas_descargas$emb_fecha = paste0(all_empresas_descargas$embarcacion, " ",all_empresas_descargas$fecha)


  all_empresas_descargas$empresas = NA
  all_empresas_descargas$empresas[all_empresas_descargas$armador %in% "PESQUERA CENTINELA S.A.C."] ="CENTINELA"
  all_empresas_descargas$empresas[all_empresas_descargas$armador %in% "TECNOLOGICA DE ALIMENTOS S.A."] = "TASA"
  all_empresas_descargas$empresas[all_empresas_descargas$armador %in% "PESQUERA HAYDUK S.A."] = "HAYDUK"
  all_empresas_descargas$empresas[all_empresas_descargas$armador %in% "CFG INVESTMENT S.A.C." ] = "CFG"
  all_empresas_descargas$empresas[all_empresas_descargas$armador %in% "PESQUERA DIAMANTE S.A."] = "DIAMANTE"
  all_empresas_descargas$empresas[all_empresas_descargas$armador %in% "PESQUERA EXALMAR S.A.A."] = "EXALMAR"
  all_empresas_descargas$empresas[all_empresas_descargas$armador %in% "PESQUERA CAPRICORNIO S.A."] = "CAPRICORNIO"
  all_empresas_descargas$empresas[all_empresas_descargas$armador %in% "AUSTRAL GROUP S.A.A"] = "AUSTRAL"

  ## agregar capacidad de bodega
  all_empresas_descargas$matricula = str_trim(all_empresas_descargas$matricula, "right")

  all_empresas_descargas$cbo2 = NA

  all_empresas_descargas$matricula2 = NULL
  for(i in seq_along(all_empresas_descargas$matricula)){
    all_empresas_descargas$matricula2[i] = extrae_numero(all_empresas_descargas$matricula[i])
  }

  data_cbo$matricula2 = NULL
  for(i in seq_along(data_cbo$matricula)){
    data_cbo$matricula2[i] = extrae_numero(data_cbo$matricula[i])
  }


  for(i in unique(all_empresas_descargas$matricula2)){
    nn = which(data_cbo$matricula2 %in% i,"CBOD.M3")
    if(length(nn) !=0){
      all_empresas_descargas$cbo2[all_empresas_descargas$matricula2 %in% i] = data_cbo$CBOD.M3[data_cbo$matricula2 %in% i]
      ## update

    }
  }

  return(suppressWarnings(all_empresas_descargas))
}
