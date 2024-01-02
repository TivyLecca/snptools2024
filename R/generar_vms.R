generar_vms = function(data = data, vessel = "Cod_Barco", harbor = harbor, dir_data = dir_data,
                       name_empresa = name_empresa, barcos_duplicados = barcos_duplicados){
  require(vmsR)
  require(maps)
  ## Procesamiento de datos VMS
  data_proc2      <- processing_vms(data = data, vessel = vessel, harbor = harbor)

  ## Identificacion de las trayectorias de los viajes
  data_proc_list2 <- split(data_proc2, data_proc2$Cod_Barco)

  dharbor <- 2   # maxima distancia en puerto
  rmin <- 6*6    # numero de registro minimo en el viaje
  vmax <- 16     # velocidad maxima
  vmin <- 3      # velocidad minima
  hmax <- 0.5    # 30 min paso de tiempo maximo
  dur_viaje <- 6 # en horas
  nodes     <- 6 # numero de nucleos a usar

  cl = makeCluster(nodes, type = "SOCK")
  start_time <- Sys.time()
  registerDoSNOW(cl)
  cluster = foreach(i = seq_along(data_proc_list2), .inorder = FALSE) %dopar%{
    try(vmsR::identify_trip(data = data_proc_list2[[i]], dharbor = dharbor,
                            rmin = rmin, vmax = vmax, vmin = vmin, hmax = hmax,
                            dur_viaje = dur_viaje, polygon = vmsR::PERU_SP))
  }
  stopCluster(cl)
  end_time   <- Sys.time()
  end_time - start_time

  data_tray <- cluster %>% lapply(as.data.frame) %>% bind_rows()
  head(data_tray)

  ## Identificar los lances de pesca
  data_tray <- data_tray[data_tray$mistake %in% c(0,1,2),] # seleccionamos los viajes correctamente identificados
  datos_vms <- identify_set(data = data_tray , thres = 0.51, loops = 50, freq_vms = "high", namefile = NULL) # la red entrenada del paquete
  datos_vms$empresa <- name_empresa

  datos_vms        = datos_vms[!datos_vms$Cod_Barco %in% barcos_duplicados,]
  datos_vms$N      = 1:length(datos_vms$Cod_Barco)

  datos_vms_viaje = vms_viaje(data = datos_vms)
  datos_vms_viaje$temporada = id_temp(x = format(datos_vms_viaje$fecha_arribo, format = "%Y-%m-%d"),
                                      inicio_temporada = inicio_temporada, fin_temporada = fin_temporada, temporada = temporada)

  datos_vms$fecha_arribo = as.Date("1990-01-01", format = "%Y-%m-%d")
  datos_vms$fecha_zarpe  = as.Date("1990-01-01", format = "%Y-%m-%d")
  datos_vms$temporada    = NA

  for(i in seq_along(datos_vms_viaje$N1)){
    r1 = datos_vms_viaje$N1[i]
    r2 = datos_vms_viaje$N2[i]
    datos_vms$fecha_arribo[r1:r2] =  datos_vms_viaje$fecha_arribo[i]
    datos_vms$fecha_zarpe[r1:r2]  =  datos_vms_viaje$fecha_zarpe[i]
    datos_vms$temporada[r1:r2]    =  datos_vms_viaje$temporada[i]
  }

  datos_vms_viaje = datos_vms_viaje[!is.na(datos_vms_viaje$temporada), ]
  write_vms2(data = datos_vms, directorio = file.path(dir_data,name_empresa,"OUTPUT"),
            file = NULL, file_harbor = harbor2) # guardar informacion # revisar write_vms y ver si tiene ayudas

  return(list(datos_vms=datos_vms, datos_vms_viaje = datos_vms_viaje))
}
