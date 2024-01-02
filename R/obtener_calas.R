obtener_calas = function(dir_data = dir_data, calas = NULL, empresas = empresas,inicio_temporada = inicio_temporada, fin_temporada = fin_temporada, temporada = temporada){
  library(stringi)
  library(stringr)

  #descargas = NULL
  all_empresas_calas = NULL
  for(i in empresas){
    setwd(paste0(dir_data, "/", i,"/CALAS")  )
    all_calas = NULL
    archivos = dir()

    if(!is.null(calas)){
      existe_archivo = archivos[archivos %in% calas]
      if(length(existe_archivo)==1){
        all_calas   = read.csv(calas, encoding = "latin1")
        #print(dim(all_calas))
      }else{
        print(paste0(i," no contiene el archivo ",calas))
      }

    }else{
      for(ii in archivos){
        files_calas = read.csv(ii, encoding = "latin1")
        #print(dim(files_calas))

        all_calas   = rbind(files_calas, all_calas)
      }
    }
    names(all_calas) = tolower(stri_trans_general(names(all_calas),"Latin-ASCII"))
    all_empresas_calas = rbind(all_calas, all_empresas_calas)
  }


  names(all_empresas_calas) = tolower(names(all_empresas_calas))
  all_empresas_calas$embarcacion = all_empresas_calas$nave
  all_empresas_calas$fecha = as.Date(all_empresas_calas$fecha, format = "%d/%m/%Y")
  all_empresas_calas$emb_fecha = paste0(all_empresas_calas$embarcacion, " ",all_empresas_calas$fecha)

  all_empresas_calas$temporada = NA
  all_empresas_calas$temporada = id_temp(x = all_empresas_calas$fecha, inicio_temporada = inicio_temporada, fin_temporada = fin_temporada, temporada = temporada)


  all_empresas_calas$empresas = gsub("^\\s+|\\s+$", "", all_empresas_calas$empresa)
  all_empresas_calas$empresas[all_empresas_calas$empresa %in% "PESQUERA CENTINELA S.A.C"]= "CENTINELA"
  all_empresas_calas$empresas[all_empresas_calas$empresa %in% "TASA"]                    = "TASA"
  all_empresas_calas$empresas[all_empresas_calas$empresa %in% "HAYDUK"]                  = "HAYDUK"
  all_empresas_calas$empresas[all_empresas_calas$empresa %in% "CFG-COPEINCA"]            = "CFG"
  all_empresas_calas$empresas[all_empresas_calas$empresa %in% "PESQUERA DIAMANTE S.A."]  = "DIAMANTE"
  all_empresas_calas$empresas[all_empresas_calas$empresa %in% "PESQUERA EXALMAR S.A.A."] = "EXALMAR"
  all_empresas_calas$empresas[all_empresas_calas$empresa %in% "CAPRICORNIO"]             = "CAPRICORNIO"
  all_empresas_calas$empresas[all_empresas_calas$empresa %in% "AUSTRAL GROUP SAA"]       = "AUSTRAL"


  all_empresas_calas = all_empresas_calas[!is.na(all_empresas_calas$temporada),]

  all_empresas_calas$semana = NULL
  for(i in unique(all_empresas_calas$temporada)){
    all_empresas_calas$semana[all_empresas_calas$temporada %in% i]= generar_semanas(all_empresas_calas$fecha[all_empresas_calas$temporada %in% i])+1
  }
  #all_empresas_calas

  for(icala in unique(all_empresas_calas$temporada)){
    season = all_empresas_calas[all_empresas_calas$temporada %in% icala,]
    save(season, file = paste0(dir_data, "/CALAS/",icala,".RData"))
  }


  return(all_empresas_calas)
}
