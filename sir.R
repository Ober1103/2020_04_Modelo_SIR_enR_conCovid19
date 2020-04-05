library(deSolve)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

sirmod <- function(t, y, parms) {
  # Condiciones iniciales
  S <- y[1]
  I <- y[2]
  R <- y[3]
  # Parametros del modelo
  beta <- parms["beta"]
  mu <- parms["mu"]
  gamma <- parms["gamma"]
  N <- parms["N"]
  # Ecuaciones
  dS <- mu * (N - S) - beta * S * I/N
  dI <- beta * S * I/N - (mu + gamma) * I
  dR <- gamma * I - mu * R
  res <- c(dS, dI, dR)
  # Resultados del gradiente
  list(res)
}

tiempo <- seq(0, 26, by = 1/10)
parms <- c(mu = 0, beta = 2, gamma = 1/2, N = 1)
inic <- c(S = 0.999, I = 0.001, R = 0)

df <- ode(y=inic, times=tiempo, func=sirmod, parms=parms)
df <- as_tibble(df)

df %>% 
  gather(comp, val, -time) %>% 
  ggplot(aes(time, val, color=comp)) +
  geom_line()

df %>% 
  plot_ly(x=~time) %>% 
  add_trace(y=~S, name="Susceptibles", mode="lines") %>% 
  add_trace(y=~I, name="Infectados", mode="lines") %>% 
  add_trace(y=~R, name="Removidos", mode="lines")

