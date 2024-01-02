obtener_vms = function (dir_data = dir_data, empresas = empresas, temporada = temporada, inicio_temporada = inicio_temporada, fin_temporada = fin_temporada){

  allData_vms_all = NULL
  allData_vms_tray_all = NULL

  for(i in seq_along(empresas)){


    setwd(paste0(dir_data,"/",empresas[i],"/OUTPUT/"))
    archivos = dir()
    arc = paste0("datos_vms_",temporada,".RData")
    archivos = archivos[archivos %in% arc]

    if(length(archivos) !=0){

      load(paste0(dir_data,"/",empresas[i],"/OUTPUT/datos_vms_",temporada,".RData"))

      allData_vms           = output_vms$datos_vms_viaje
      allData_vms_tray      = output_vms$datos_vms
      allData_vms$fecha     = substring(allData_vms$fecha_arribo,1,10)
      allData_vms$emb_fecha = paste0(allData_vms$Cod_Barco, " ",allData_vms$fecha)

      oo                         = table(allData_vms$emb_fecha)
      allData_vms                = allData_vms[!allData_vms$emb_fecha %in% unique(names(oo[oo >1])),]
      allData_vms                =   allData_vms[, c("emb_fecha", "temporada", "inercia_espacial", "sinuosidad","lat_max", "lat_min",  "lon_max", "lon_min",
                                                     "maxDist_zarpe", "maxDist_arribo", "dist_costa_max",   "duracion",  "recorrido", "inercia_espacial", "vel_max")]
      allData_vms_tray$fecha     = as.Date(substring(allData_vms_tray$Date,1,10), format = "%Y-%m-%d")
      allData_vms_tray$temporada = id_temp(x = allData_vms_tray$fecha,
                                           inicio_temporada = inicio_temporada, fin_temporada = fin_temporada, temporada = temporada) ##

      allData_vms_tray$emb_fecha = paste0(allData_vms_tray$Name_vessel," ",allData_vms_tray$fecha )
      xx                         = tapply(allData_vms_tray$temporada, allData_vms_tray$emb_fecha, unique, na.rm = T)
      allData_vms_tray           = allData_vms_tray[!allData_vms_tray$emb_fecha %in% names(xx[is.na(xx)]),]
      allData_vms_tray$emb_fecha = paste0(allData_vms_tray$Name_vessel, " ",allData_vms_tray$fecha)

      allData_vms_tray$semana = NULL
      for(i in unique(allData_vms_tray$temporada)){
        allData_vms_tray$semana[allData_vms_tray$temporada %in% i]= generar_semanas(allData_vms_tray$fecha_arribo[allData_vms_tray$temporada %in% i])+1
      }
      allData_vms_all = rbind(allData_vms, allData_vms_all)
      allData_vms_tray_all = rbind(allData_vms_tray, allData_vms_tray_all)

      save(allData_vms_tray_all, file = paste0(dir_data, "/TRAYECTORIAS/",temporada,".RData"))
    }
  }
  return(allData_vms_all)
}

