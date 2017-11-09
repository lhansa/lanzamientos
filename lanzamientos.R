# % Script 1 - Práctica 1
# 
# %==========================REMEMBER=============================%
#   %  f=exp(-x.^2-y.^2/2).*cos(4*x)+exp(-3*((x+0.05).^2+y.^2/2));  %
#   %===============================================================%
#   
#   clear all
# close all
# 
# %% Caída libre
# 
# %parámetros
# g=9.8; %gravedad
# u0=10; %velocidad
# v0=15; %velocidad

rm(list = ls()); gc()

library(tidyverse)
library(gganimate)

source("src/funciones_disparo.R")
## Caída libre -------------------------------------------------

g <- 9.8
u0 <- 10
v0 <- 15

tmax <- 10
t <- seq(0, tmax, length.out = 100)

x <- v0*t
y <- u0*t-g*t^2/2

datos <- data_frame(tiempo = t, x = x, y = y)

P <- ggplot(datos, aes(x, y, frame = t, cumulative = TRUE)) + 
  geom_point(size = 1, alpha = 0.2)

gganimate(P, interval = 0.1)


# 
# Tmax=10;
# t=linspace(0,Tmax,1000);
# t=t(:);
# 
# x=v0*t;
# y=u0*t-g*t.^2/2;
# 
# figure('color','w')
# subplot(1,2,1)
# plot(t,x,t,y,'LineWidth',2)
# xlabel('tiempo (s)','FontSize',14)

# subplot(1,2,2)
# plot(x,y,'LineWidth',2)
# xlabel('x (m)','FontSize',12)
# ylabel('y (m)','FontSize',12)

## Disparos --------------------------------------
# 
# %% Disparos desde diferentes ángulos
# 
# 
# v=10; %velocidad del proyectil
# N=10; %número de disparos que vamos a simular

v <- 10
n <- 5

datos <- map(1:n, function(k) {
  alpha <- pi / 2 * k / n
  return(disparo(v, alpha, instantes = 50))
}) %>%
  bind_rows(.id = "disparo") %>%
  mutate(disparo = as.factor(as.numeric(disparo))) %>%
  arrange(tiempo)

P <- ggplot(datos, aes(x, y, frame = tiempo, cumulative = TRUE)) + 
  geom_point(aes(color = disparo), size = 1, alpha = 0.7) + 
  scale_color_brewer(palette = "Set1")
P

gganimate(P, interval = 0.1)

# 
# figure('color','w')
# for k=1:N,
# alpha=pi/2*k/N;
# [x,y]=disparo(v,alpha);
# plot(x,y,'LineWidth',2)
# hold on
# end
# xlabel('x (m)','FontSize',12)
# ylabel('y (m)','FontSize',12)
# grid on
# 
# %% Disparo con rozamiento
# 
# v=1; %velocidad del proyectil
# Tmax=2;
# N=100;
# 
# figure('color','w')
# for k=1:N
# alpha=pi/2*k/(N+1);
# [s,r]=disparoRozam(v,alpha,Tmax);
# plot(s,r,'LineWidth',2)
# hold on
# end
# xlabel('x (m)','FontSize',12)
# ylabel('y (m)','FontSize',12)
# axis([0 0.6 0 0.4])
# grid on