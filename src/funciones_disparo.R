disparo <- function(v, alpha, g = 9.8, instantes = 100){
  ux <- v * cos(alpha)
  uy <- v * sin(alpha)
  
  if(uy <= 0) return(list(x=0, y=0))
  
  tmax <- 2 * uy / g
  t <- seq(0, tmax, length.out = instantes)
  
  x <- ux*t
  y <- uy*t-(g*t^2)/2
  
  return(data_frame(tiempo = t, x = x, y = y))
  
}


# function [s,r]=disparoRozam(v,alpha,Tmax)
# 
# u0=v*cos(alpha);
# w0=v*sin(alpha);
# 
# if w0 <= 0 %si disparamos hacia abajo
# r=0; s=0;
# return;
# end
# 
# tau=linspace(0,Tmax,100);
# 
# s=u0*(1-exp(-tau));
# r=(1+w0)*(1-exp(-tau))-tau;
# 
# end