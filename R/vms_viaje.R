vms_viaje <- function (data,...){
  require(vmsR)
  #
  data$Cod_Barco_trip <- paste0(data$Cod_Barco, "-", data$trip)
  id_barco_trip = table(data$Cod_Barco_trip)
  id_barco_trip = names(id_barco_trip[id_barco_trip == 1])
  data = data[!data$Cod_Barco_trip %in% id_barco_trip,]

  data_viaje <- lapply(split(data, data$Cod_Barco_trip, drop = TRUE),
                       function(x) {
                         N1        <- x$N[1]
                         N2        <- x$N[length(x$Harbor)]
                         empresa = x$empresa[1]
                         if (sum(x$calas) > 0) {

                           Cod_Barco <- x$Cod_Barco[1]
                           Name_Vessel <- x$Name_vessel[1]
                           puerto_zarpe <- x$Harbor[1]
                           puerto_arribo <- x$Harbor[length(x$Harbor)]
                           fecha_zarpe <- x$Date[1]
                           fecha_arribo <- x$Date[length(x$Date)]
                           duracion <- difftime(time1 = x$Date[length(x$Date)],
                                                time2 = x$Date[1], units = "hours")
                           recorrido <- sum(x$Dist_Emisiones, na.rm = T)
                           dist_costa_max <- max(x$dist_costa, na.rm = T)
                           dist_costa_pesca <- mean(x$dist_costa[x$calas ==
                                                                   1], na.rm = T)
                           maxDist_zarpe <- max(dist_ortodromica(x1 = x$Lon[1],
                                                                 y1 = x$Lat[1], x2 = x$Lon, y2 = x$Lat), na.rm = T)
                           maxDist_arribo <- max(dist_ortodromica(x1 = x$Lon[length(x$Lon)],
                                                                  y1 = x$Lat[length(x$Lon)], x2 = x$Lon, y2 = x$Lat),
                                                 na.rm = T)
                           sinuosidad <- recorrido/(maxDist_zarpe + maxDist_arribo)
                           course_zarpe <- x$Course[1]
                           course_arribo <- x$Course[length(x$Course)]
                           num_lances <- sum(x$calas, na.rm = T)
                           if (sum(x$calas, na.rm = T) == 1) {
                             tiempo_entre_calas = 0
                             lon_cala_mean <- x$Lon[x$calas == 1]
                             lat_cala_mean <- x$Lat[x$calas == 1]
                             lon_primera_cala <- x$Lon[x$calas == 1]
                             lat_primera_cala <- x$Lat[x$calas == 1]
                             lon_ultima_cala <- x$Lon[x$calas == 1]
                             lat_ultima_cala <- x$Lat[x$calas == 1]
                             recorrido_cala_a_cala <- 0
                             inercia_espacial <- 0
                           }
                           else {
                             tiempo_entre_calas = as.numeric(difftime(time1 = x$Date[x$calas ==
                                                                                       1][length(x$Date[x$calas == 1])], time2 = x$Date[x$calas ==
                                                                                                                                          1][1], units = "hours"))
                             lon_cala_mean <- mean(x$Lon[x$calas == 1],
                                                   na.rm = T)
                             lat_cala_mean <- mean(x$Lat[x$calas == 1],
                                                   na.rm = T)
                             lon_primera_cala <- x$Lon[x$calas == 1][1]
                             lat_primera_cala <- x$Lat[x$calas == 1][1]
                             lon_ultima_cala <- x$Lon[x$calas == 1][length(x$Lon[x$calas ==
                                                                                   1])]
                             lat_ultima_cala <- x$Lat[x$calas == 1][length(x$Lon[x$calas ==
                                                                                   1])]
                             ubicacion_calas <- range(which(x$calas ==
                                                              1))
                             recorrido_cala_a_cala <- sum(x$Dist_Emisiones[ubicacion_calas[1]:ubicacion_calas[2]])
                             CG_lon <- mean(x$Lon)
                             CG_lat <- mean(x$Lat)
                             dist_CG <- dist_ortodromica(x1 = CG_lon, y1 = CG_lat, x2 = x$Lon, y2 = x$Lat)
                             inercia_espacial <- mean(dist_CG, na.rm = T)
                           }
                           vel_max <- max(x$Vel.Cal, na.rm = T)
                           lat_max <- max(x$Lat, na.rm = T)
                           lat_min <- min(x$Lat, na.rm = T)
                           lon_max <- max(x$Lon, na.rm = T)
                           lon_min <- min(x$Lon, na.rm = T)
                         }
                         else {
                           Cod_Barco <- x$Cod_Barco[1]
                           Name_Vessel <- x$Name_vessel[1]
                           puerto_zarpe <- x$Harbor[1]
                           puerto_arribo <- x$Harbor[length(x$Harbor)]
                           fecha_zarpe <- x$Date[1]
                           fecha_arribo <- x$Date[length(x$Date)]
                           duracion <- difftime(time1 = x$Date[length(x$Date)],
                                                time2 = x$Date[1], units = "hours")
                           recorrido <- sum(x$Dist_Emisiones[-1], na.rm = T)
                           dist_costa_max <- max(x$dist_costa, na.rm = T)
                           dist_costa_pesca <- 0
                           maxDist_zarpe <- max(dist_ortodromica(x1 = x$Lon[1],
                                                                 y1 = x$Lat[1], x2 = x$Lon, y2 = x$Lat), na.rm = T)
                           maxDist_arribo <- max(dist_ortodromica(x1 = x$Lon[length(x$Lon)],
                                                                  y1 = x$Lat[length(x$Lon)], x2 = x$Lon, y2 = x$Lat),
                                                 na.rm = T)
                           sinuosidad <- recorrido/(maxDist_zarpe + maxDist_arribo)
                           course_zarpe <- x$Course[1]
                           course_arribo <- x$Course[length(x$Course)]
                           num_lances <- 0
                           tiempo_entre_calas <- 0
                           vel_max <- max(x$Vel.Cal, na.rm = T)
                           lat_max <- max(x$Lat, na.rm = T)
                           lat_min <- min(x$Lat, na.rm = T)
                           lon_max <- max(x$Lon, na.rm = T)
                           lon_min <- min(x$Lon, na.rm = T)
                           lon_cala_mean <- 0
                           lat_cala_mean <- 0
                           lon_primera_cala <- 0
                           lat_primera_cala <- 0
                           lon_ultima_cala <- 0
                           lat_ultima_cala <- 0
                           recorrido_cala_a_cala <- 0
                           inercia_espacial <- 0
                         }
                         cbind.data.frame(N1, N2, empresa, Cod_Barco, Name_Vessel, fecha_zarpe,
                                          fecha_arribo, puerto_zarpe, puerto_arribo, duracion,
                                          recorrido, maxDist_zarpe, maxDist_arribo, dist_costa_max,
                                          dist_costa_pesca, num_lances, tiempo_entre_calas,
                                          vel_max, lat_max, lat_min, lon_max, lon_min,
                                          course_zarpe, course_arribo, sinuosidad, recorrido_cala_a_cala, inercia_espacial)
                       })
  suppressWarnings({
    data_viaje <- data_viaje %>% lapply(as.data.frame) %>%
      bind_rows()
  })
  return(data_viaje)
}
