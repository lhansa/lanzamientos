disparo <- function(v, alpha, g = 9.8, instantes = 100){
  ux <- v * cos(alpha)
  uy <- v * sin(alpha)
  
  if(uy <= 0) return(data_frame(tiempo = 0, x = 0, y = 0))
  
  tmax <- 2 * uy / g
  t <- seq(0, tmax, length.out = instantes)
  
  x <- ux*t
  y <- uy*t-(g*t^2)/2
  
  return(data_frame(tiempo = t, x = x, y = y))
  
}

disparo_rozamiento <- function(v, alpha, g = 9.8, instantes = 100){
  
  
  ux <- v * cos(alpha)
  uy <- v * sin(alpha)
  
  if(uy <= 0) return(data_frame(tiempo = 0, x = 0, y = 0))
  
  tmax <- 2 * uy / g
  t <- seq(0, tmax, length.out = instantes)
  
  x <- ux*(1-exp(-t))
  y <- (1+uy)*(1-exp(-t))-t
  
  return(data_frame(tiempo = t, x = x, y = y))
  
}


