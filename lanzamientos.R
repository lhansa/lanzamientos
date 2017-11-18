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


## Disparos --------------------------------------

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

## Disparos con rozamiento ---------------------------------

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