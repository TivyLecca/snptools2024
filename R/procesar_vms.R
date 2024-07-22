prcocesar_vms=function (temporada = temporada, dir_data = dir_data, empresas = empresas)
{
  require(vmsR)
  barcos_duplicados = c("CECI", "DON ROBERTH", "MARIA MERCEDES")
  for (i in seq_along(empresas)) {
    if (empresas[i] %in% "AUSTRAL") {
      setwd(file.path(dir_data, empresas[i], "SISESAT"))
      archivos = dir()
      archivo_temp = paste0("SISESAT_", temporada, ".csv")
      archivos = archivos[archivos %in% archivo_temp]
      if (length(archivos) == 0) {
        print(paste0("#### No hay datos VMS de ", empresas[i],
                     ".. ####"))
      }
      if (length(archivos) != 0) {
        juntar_vms = read.csv(archivos, fileEncoding = "latin1")
        names(juntar_vms) = tolower(names(juntar_vms))#tolower sirve para pasar de mayuscula a minuscula
        names(juntar_vms) = c("nombre", "latitud", "longitud",
                              "rumbo", "velocidad", "referencia", "hora",
                              "fechahora", "fecha", "guardar.marca.de.tiempo",
                              "dispositivo.de.retardo.guardado", "rumbo.promedio")
        juntar_vms = juntar_vms[, c("nombre", "latitud", "longitud", "fechahora")]
        juntar_vms$Lon = get_longitud_3(juntar_vms$longitud) #revisar si queda como version 1 o 2
        juntar_vms$Lat = get_latitud(juntar_vms$latitud)
        if(nchar(juntar_vms$fechahora[1])<= 16)
        {juntar_vms$Date = strptime(paste0(juntar_vms$fechahora,":00"),
                                    format = "%d/%m/%Y %H:%M:%S")
        }else{juntar_vms$Date = strptime(juntar_vms$fechahora,
                                         format = "%d/%m/%Y %H:%M:%S")
        }


        #        juntar_vms$Date = strptime(juntar_vms$fechahora,  format = "%d/%m/%Y %H:%M:%S")
        juntar_vms$Cod_Barco = juntar_vms$nombre
        juntar_vms$Name_vessel = juntar_vms$nombre
        juntar_vms$Vel_VMS = NA
        juntar_vms$Course = NA
        juntar_vms = juntar_vms[, c("Cod_Barco", "Name_vessel",
                                    "Date", "Lon", "Lat", "Vel_VMS", "Course")]
        juntar_vms = juntar_vms[juntar_vms$Cod_Barco !=  " ", ]

        output_vms = generar_vms(data = juntar_vms, vessel = "Cod_Barco",
                                 harbor = harbor, dir_data = dir_data, name_empresa = empresas[i],
                                 barcos_duplicados = barcos_duplicados)
        save(output_vms, file = paste0(dir_data, "/",
                                       empresas[i], "/OUTPUT/datos_vms_", temporada,
                                       ".RData"))
        print(paste0("#### Los datos VMS de ", empresas[i],
                     " fueron procesados correctamente.. ####"))
      }
    }
    if (empresas[i] %in% "CAPRICORNIO") {
      setwd(file.path(dir_data, empresas[i], "SISESAT"))
      archivos = dir()
      archivo_temp = paste0("SISESAT_", temporada, ".csv")
      archivos = archivos[archivos %in% archivo_temp]
      if (length(archivos) == 0) {
        print(paste0("#### No hay datos VMS de ", empresas[i],
                     ".. ####"))
      }
      if (length(archivos) != 0) {
        juntar_vms = read.csv(archivos, fileEncoding = "latini")
        names(juntar_vms) = tolower(names(juntar_vms))
        names(juntar_vms) = c("nombre", "latitud", "longitud",
                              "rumbo", "velocidad", "referencia", "hora",
                              "fechahora", "fecha", "guardar.marca.de.tiempo",
                              "dispositivo.de.retardo.guardado", "rumbo.promedio")
        juntar_vms = juntar_vms[, c("nombre", "latitud",
                                    "longitud", "fechahora")]
        juntar_vms$Lon = get_longitud(juntar_vms$longitud)
        juntar_vms$Lat = get_latitud(juntar_vms$latitud)

        if(nchar(juntar_vms$fechahora[1])<=16){juntar_vms$Date = strptime(paste0(juntar_vms$fechahora,":00"),
                                                                          format = "%d/%m/%Y %H:%M:%S")
        }else{juntar_vms$Date = strptime(juntar_vms$fechahora,
                                         format = "%d/%m/%Y %H:%M:%S")
        }
        #        juntar_vms$Date = strptime(juntar_vms$fechahora,
        #                                   format = "%d/%m/%Y %H:%M:%S")
        juntar_vms$Cod_Barco = juntar_vms$nombre
        juntar_vms$Name_vessel = juntar_vms$nombre
        juntar_vms$Vel_VMS = NA
        juntar_vms$Course = NA
        juntar_vms = juntar_vms[, c("Cod_Barco", "Name_vessel",
                                    "Date", "Lon", "Lat", "Vel_VMS", "Course")]
        juntar_vms = juntar_vms[juntar_vms$Cod_Barco !=
                                  " ", ]
        output_vms = generar_vms(data = juntar_vms, vessel = "Cod_Barco",
                                 harbor = harbor, dir_data = dir_data, name_empresa = empresas[i],
                                 barcos_duplicados = barcos_duplicados)
        save(output_vms, file = paste0(dir_data, "/",
                                       empresas[i], "/OUTPUT/datos_vms_", temporada,
                                       ".RData"))
        print(paste0("#### Los datos VMS de ", empresas[i],
                     " fueron procesados correctamente.. ####"))
      }
    }
    if (empresas[i] %in% "CENTINELA") {
      setwd(file.path(dir_data, empresas[i], "SISESAT"))
      archivos = dir()
      archivo_temp = paste0("SISESAT_", temporada, ".csv")
      archivos = archivos[archivos %in% archivo_temp]
      if (length(archivos) == 0) {
        print(paste0("#### No hay datos VMS de ", empresas[i],
                     ".. ####"))
      }
      if (length(archivos) != 0) {
        juntar_vms = read.csv(archivos, fileEncoding = "latin1")
        names(juntar_vms) = tolower(names(juntar_vms))
        names(juntar_vms) = c("empresa", "nave", "fecha.y.hora.de.la.posicion",
                              "velocidad", "nombre.de.bandera", "referencia.de.la.baliza",
                              "longitud", "latitud", "rumbo", "puerto", "rumbo.promedio")
        juntar_vms = juntar_vms[, c("nave", "latitud", "longitud", "fecha.y.hora.de.la.posicion")]
        juntar_vms$Lon = get_longitud_2(juntar_vms$longitud)
        juntar_vms$Lat = get_latitud_2(juntar_vms$latitud)

        if(nchar(juntar_vms$fecha.y.hora.de.la.posicion[1])<=16){
          juntar_vms$fecha.y.hora.de.la.posicion=gsub(pattern="/",replacement="-",x= juntar_vms$fecha.y.hora.de.la.posicion)
          juntar_vms$Date = strptime(paste0(juntar_vms$fecha.y.hora.de.la.posicion,":00"),  format = "%d-%m-%Y %H:%M:%S")
        }else{
          juntar_vms$fecha.y.hora.de.la.posicion=gsub(pattern="/",replacement="-",x= juntar_vms$fecha.y.hora.de.la.posicion)
          juntar_vms$Date = strptime(paste0(juntar_vms$fecha.y.hora.de.la.posicion,":00"),  format = "%d-%m-%Y %H:%M:%S")
        }
        #       juntar_vms$Date = strptime(juntar_vms$fecha.y.hora.de.la.posicion,  format = "%d-%m-%Y %H:%M:%S")
        juntar_vms$Cod_Barco = juntar_vms$nave
        juntar_vms$Name_vessel = juntar_vms$nave
        juntar_vms$Vel_VMS = NA
        juntar_vms$Course = NA
        juntar_vms = juntar_vms[, c("Cod_Barco", "Name_vessel", "Date", "Lon", "Lat", "Vel_VMS", "Course")]
        output_vms = generar_vms(data = juntar_vms, vessel = "Cod_Barco",
                                 harbor = harbor, dir_data = dir_data, name_empresa = empresas[i],
                                 barcos_duplicados = barcos_duplicados)
        save(output_vms, file = paste0(dir_data, "/",
                                       empresas[i], "/OUTPUT/datos_vms_", temporada,
                                       ".RData"))
        print(paste0("#### Los datos VMS de ", empresas[i],
                     " fueron procesados correctamente.. ####"))
      }
    }
    if (empresas[i] %in% "CFG") {
      setwd(file.path(dir_data, empresas[i], "SISESAT"))
      archivos = dir()
      archivo_temp = paste0("SISESAT_", temporada, ".csv")
      archivos = archivos[archivos %in% archivo_temp]
      if (length(archivos) == 0) {
        print(paste0("#### No hay datos VMS de ", empresas[i],
                     ".. ####"))
      }
      if (length(archivos) != 0) {
        juntar_vms = read.csv(archivos, fileEncoding = "latin1")
        names(juntar_vms) = tolower(names(juntar_vms))
        names(juntar_vms) = c("flota", "codigo", "embarcacion",
                              "matricula", "fecha", "hora", "longitud", "latitud",
                              "velocidad", "rumbo")
        juntar_vms$fecha.y.hora.de.la.posicion = paste0(juntar_vms$fecha,
                                                        " ", juntar_vms$hora)
        juntar_vms = juntar_vms[, c("embarcacion", "latitud",
                                    "longitud", "fecha.y.hora.de.la.posicion")]
        juntar_vms$Lon = juntar_vms$longitud
        juntar_vms$Lat = juntar_vms$latitud

        if(nchar(juntar_vms$fecha.y.hora.de.la.posicion[1])<= 16){
          juntar_vms$Date = strptime(paste0(juntar_vms$fecha.y.hora.de.la.posicion,":00"), format = "%d/%m/%Y %H:%M:%S")
        }else{
          juntar_vms$Date = strptime(juntar_vms$fecha.y.hora.de.la.posicion, format = "%d/%m/%Y %H:%M:%S")
        }

        #        juntar_vms$Date = strptime(juntar_vms$fecha.y.hora.de.la.posicion,
        #format = "%d/%m/%Y %H:%M:%S")
        juntar_vms$Cod_Barco = juntar_vms$embarcacion
        juntar_vms$Name_vessel = juntar_vms$embarcacion
        juntar_vms$Vel_VMS = NA
        juntar_vms$Course = NA
        juntar_vms = juntar_vms[, c("Cod_Barco", "Name_vessel", "Date", "Lon", "Lat", "Vel_VMS", "Course")]
        output_vms = generar_vms(data = juntar_vms, vessel = "Cod_Barco",
                                 harbor = harbor, dir_data = dir_data, name_empresa = empresas[i],
                                 barcos_duplicados = barcos_duplicados)
        save(output_vms, file = paste0(dir_data, "/",
                                       empresas[i], "/OUTPUT/datos_vms_", temporada,
                                       ".RData"))
        print(paste0("#### Los datos VMS de ", empresas[i],
                     " fueron procesados correctamente.. ####"))
      }
    }
    if (empresas[i] %in% "DIAMANTE") {
      setwd(file.path(dir_data, empresas[i], "SISESAT"))
      archivos = dir()
      archivo_temp = paste0("SISESAT_", temporada, ".csv")
      archivos = archivos[archivos %in% archivo_temp]
      if (length(archivos) == 0) {
        print(paste0("#### No hay datos VMS de ", empresas[i],
                     ".. ####"))
      }
      if (length(archivos) != 0) {
        juntar_vms = read.csv(archivos, fileEncoding = "latin1")
        names(juntar_vms) = tolower(names(juntar_vms))
        names(juntar_vms) = c("nave", "n..imo", "referencia.baliza.activa",
                              "latitud", "longitud", "rumbo", "velocidad",
                              "fecha.y.hora.de.la.posicion", "referencia",
                              "tipo.de.nave", "referencia.de.la.baliza",
                              "rumbo.promedio", "hora.de.la.posición", "marca.de.tiempo.del.proveedor",
                              "fecha.de.la.posicion", "marca.de.tiempo.recibida",
                              "guardar.marca.de.tiempo", "dispositivo.de.retardo.guardado",
                              "retrasar.la.transmisión.de.datos", "dispositivo.de.retraso.proveedor",
                              "retraso.proveedor.recepcion", "retraso.recibido.guardado",
                              "id.zona.de.la.baliza")
        juntar_vms = juntar_vms[, c("nave", "latitud", "longitud", "fecha.y.hora.de.la.posicion")]
        juntar_vms$Lon = get_longitud_2(juntar_vms$longitud)
        juntar_vms$Lat = get_latitud_2(juntar_vms$latitud)


        if(nchar(juntar_vms$fecha.y.hora.de.la.posicion[1])<=16){
          juntar_vms$Date = strptime(paste0(juntar_vms$fecha.y.hora.de.la.posicion,":00"), format = "%d/%m/%Y %H:%M:%S")
        }else{
          juntar_vms$Date = strptime(juntar_vms$fecha.y.hora.de.la.posicion, format = "%d/%m/%Y %H:%M:%S")
        }



        #juntar_vms$Date = strptime(juntar_vms$fecha.y.hora.de.la.posicion, format = "%d/%m/%Y %H:%M")
        juntar_vms$Cod_Barco = juntar_vms$nave
        juntar_vms$Name_vessel = juntar_vms$nave
        juntar_vms$Vel_VMS = NA
        juntar_vms$Course = NA
        juntar_vms = juntar_vms[, c("Cod_Barco", "Name_vessel", "Date", "Lon", "Lat", "Vel_VMS", "Course")]
        output_vms = generar_vms(data = juntar_vms, vessel = "Cod_Barco",
                                 harbor = harbor, dir_data = dir_data, name_empresa = empresas[i],
                                 barcos_duplicados = barcos_duplicados)
        save(output_vms, file = paste0(dir_data, "/",
                                       empresas[i], "/OUTPUT/datos_vms_", temporada,
                                       ".RData"))
        print(paste0("#### Los datos VMS de ", empresas[i],
                     " fueron procesados correctamente.. ####"))
      }
    }
    if (empresas[i] %in% "EXALMAR") {
      setwd(file.path(dir_data, empresas[i], "SISESAT"))
      archivos = dir()
      archivo_temp = paste0("SISESAT_", temporada, ".csv")
      archivos = archivos[archivos %in% archivo_temp]
      if (length(archivos) == 0) {
        print(paste0("#### No hay datos VMS de ", empresas[i],
                     ".. ####"))
      }
      if (length(archivos) != 0) {
        juntar_vms = read.csv(archivos, fileEncoding = "latin1")
        names(juntar_vms) = tolower(names(juntar_vms))
        names(juntar_vms) = c("FLOTA", "EMBARCACION", "MATRICULA", "FECHA", "HORA", "LONGITUD", "LATITUD", "VELOCIDAD", "RUMBO")
        juntar_vms$FECHA_HORA = paste0(juntar_vms$FECHA, " ", juntar_vms$HORA)
        juntar_vms = juntar_vms[, c("EMBARCACION", "LATITUD", "LONGITUD", "FECHA_HORA")]
        juntar_vms$Lon = juntar_vms$LONGITUD
        juntar_vms$Lat = juntar_vms$LATITUD


        if(nchar(juntar_vms$FECHA_HORA[1])<=16){
          juntar_vms$Date = strptime(paste0(juntar_vms$FECHA_HORA,":00"), format = "%d/%m/%Y %H:%M:%S")
        }else{
          juntar_vms$Date = strptime(juntar_vms$FECHA_HORA, format = "%d/%m/%Y %H:%M:%S")
        }


        # juntar_vms$Date = strptime(juntar_vms$FECHA,
        #                             format = "%d/%m/%Y %H:%M")
        juntar_vms$Cod_Barco = juntar_vms$EMBARCACION
        juntar_vms$Name_vessel = juntar_vms$EMBARCACION
        juntar_vms$Vel_VMS = NA
        juntar_vms$Course = NA
        juntar_vms = juntar_vms[, c("Cod_Barco", "Name_vessel", "Date", "Lon", "Lat", "Vel_VMS", "Course")]
        output_vms = generar_vms(data = juntar_vms, vessel = "Cod_Barco",
                                 harbor = harbor, dir_data = dir_data, name_empresa = empresas[i],
                                 barcos_duplicados = barcos_duplicados)
        save(output_vms, file = paste0(dir_data, "/",
                                       empresas[i], "/OUTPUT/datos_vms_", temporada,
                                       ".RData"))
        print(paste0("#### Los datos VMS de ", empresas[i],
                     " fueron procesados correctamente.. ####"))
      }
    }
    if (empresas[i] %in% "HAYDUK") {
      setwd(file.path(dir_data, empresas[i], "SISESAT"))
      archivos = dir()
      archivo_temp = paste0("SISESAT_", temporada, ".csv")
      archivos = archivos[archivos %in% archivo_temp]
      if (length(archivos) == 0) {
        print(paste0("#### No hay datos VMS de ", empresas[i],
                     ".. ####"))
      }
      if (length(archivos) != 0) {
        juntar_vms = read.csv(archivos, fileEncoding = "latin1")
        #names(juntar_vms) = iconv(names(juntar_vms), to = "ASCII//TRANSLIT")

        names(juntar_vms)= c("Nombre.de.la.nave","matricula","Nombre.de.bandera","Fecha.de.la.posicion","Latitud","Longitud","Rumbo","Velocidad","Rumbo.promedio",
                             "Fecha.y.hora.de.la.posicion","Hora.de.la.posicion","Marca.de.tiempo.del.proveedor","Marca.de.tiempo.recibida",
                             "Guardar.marca.de.tiempo","Dispositivo.de.retardo.guardado","Retrasar.la.transmision.de.datos","Dispositivo.de.Retraso.Proveedor",
                             "Retraso.Proveedor.Recepcion"," Retraso.recibido.guardado","ID.zona.de.la.baliza")

        juntar_vms$Fecha.de.la.posicion = paste0(juntar_vms$Fecha.de.la.posicion,  " ", juntar_vms$Hora.de.la.posicion)
        juntar_vms$Longitud = get_longitud_2(juntar_vms$Longitud)
        juntar_vms$Latitud = get_latitud_2(juntar_vms$Latitud)


        if(nchar(juntar_vms$Fecha.de.la.posicion[1])<=16){
          juntar_vms$Date = strptime(paste0(juntar_vms$Fecha.de.la.posicion,":00"), format = "%d/%m/%Y %H:%M:%S")
        }else{
          juntar_vms$Date = strptime(juntar_vms$Fecha.de.la.posicion, format = "%d/%m/%Y %H:%M:%S")
        }


        # if (nchar(juntar_vms$Fecha.de.la.posicion[1]) == 19) {
        #   juntar_vms$Fecha.de.la.posicion = strptime(juntar_vms$Fecha.de.la.posicion,
        #                                              format = "%d/%m/%Y %H:%M:%S")
        # }else {
        #   juntar_vms$Fecha.de.la.posicion = strptime(juntar_vms$Fecha.de.la.posicion,
        #                                              format = "%d/%m/%Y %H:%M")
        #}
        juntar_vms = juntar_vms[c("Nombre.de.la.nave", "Nombre.de.la.nave", "Fecha.de.la.posicion", "Longitud", "Latitud")]
        names(juntar_vms) = tolower(names(juntar_vms))
        juntar_vms = juntar_vms[, c("nombre.de.la.nave",
                                    "latitud", "longitud", "fecha.de.la.posicion")]
        juntar_vms$Lon = juntar_vms$longitud
        juntar_vms$Lat = juntar_vms$latitud
        juntar_vms$Date = juntar_vms$fecha.de.la.posicion
        juntar_vms$Cod_Barco = juntar_vms$nombre.de.la.nave
        juntar_vms$Name_vessel = juntar_vms$nombre.de.la.nave
        juntar_vms$Vel_VMS = NA
        juntar_vms$Course = NA
        juntar_vms = juntar_vms[, c("Cod_Barco", "Name_vessel", "Date", "Lon", "Lat", "Vel_VMS", "Course")]
        output_vms = generar_vms(data = juntar_vms, vessel = "Cod_Barco",
                                 harbor = harbor, dir_data = dir_data, name_empresa = empresas[i],
                                 barcos_duplicados = barcos_duplicados)
        save(output_vms, file = paste0(dir_data, "/",
                                       empresas[i], "/OUTPUT/datos_vms_", temporada,
                                       ".RData"))
        print(paste0("#### Los datos VMS de ", empresas[i],
                     " fueron procesados correctamente.. ####"))
      }
    }
    if (empresas[i] %in% "TASA") {
      setwd(file.path(dir_data, empresas[i], "SISESAT"))
      archivos = dir()
      archivo_temp = paste0("SISESAT_", temporada, ".csv")
      archivos = archivos[archivos %in% archivo_temp]
      if (length(archivos) == 0) {
        print(paste0("#### No hay datos VMS de ", empresas[i],
                     ".. ####"))
      }
      if (length(archivos) != 0) {
        juntar_vms = read.csv(archivos, fileEncoding = "latin1")
        names(juntar_vms) = c("nombre", "id", "fecha_hora", "latitud", "longitud", "velocidad", "rumbo")
        juntar_vms = juntar_vms[!is.na(juntar_vms$longitud),
        ]
        juntar_vms = juntar_vms[!is.na(juntar_vms$latitud),
        ]
        juntar_vms = juntar_vms[!is.na(juntar_vms$fecha_hora),
        ]
        names(juntar_vms) = tolower(names(juntar_vms))

        juntar_vms = juntar_vms[, c("nombre", "latitud",
                                    "longitud", "fecha_hora")]
        juntar_vms$Lon = juntar_vms$longitud
        juntar_vms$Lat = juntar_vms$latitud

        if(nchar(juntar_vms$fecha_hora[1])<=16){
          juntar_vms$Date = strptime(paste0(juntar_vms$fecha_hora,":00"), format = "%d/%m/%Y %H:%M:%S")
        }else{
          juntar_vms$Date = strptime(juntar_vms$fecha_hora, format = "%d/%m/%Y %H:%M:%S")
        }

        #   juntar_vms$Date = strptime(juntar_vms$fecha_hora, format = "%d/%m/%Y %H:%M")
        juntar_vms$Cod_Barco = juntar_vms$nombre
        juntar_vms$Name_vessel = juntar_vms$nombre
        juntar_vms$Vel_VMS = NA
        juntar_vms$Course = NA
        juntar_vms = juntar_vms[, c("Cod_Barco", "Name_vessel", "Date", "Lon", "Lat", "Vel_VMS", "Course")]
        output_vms = generar_vms(data = juntar_vms, vessel = "Cod_Barco",
                                 harbor = harbor, dir_data = dir_data, name_empresa = empresas[i],
                                 barcos_duplicados = barcos_duplicados)
        save(output_vms, file = paste0(dir_data, "/",
                                       empresas[i], "/OUTPUT/datos_vms_", temporada,
                                       ".RData"))
        print(paste0("#### Los datos VMS de ", empresas[i],
                     " fueron procesados correctamente.. ####"))
      }
    }
  }
  return(invisible())
}
