semanas <- function(inicio.temp, fin.temp, ...){
  require(lubridate)

  daySeason = seq.Date(as.Date(inicio.temp), as.Date(fin.temp),by =  "day")
  day      =  weekdays(daySeason)
  ##
  dayinicio = weekdays(min(inicio.temp))

  season = data.frame(daySeason = daySeason, day = day)
  #nweek = c(rep(0, min(which(day == "lunes"))-1), sort(rep(1:length(day[day=="lunes"]),7)))
  nweek = c(rep(0, min(which(day == dayinicio))-1), sort(rep(1:length(day[day==dayinicio]),7)))
  season$week <- nweek[1:length(day)]
  return(season)
}
