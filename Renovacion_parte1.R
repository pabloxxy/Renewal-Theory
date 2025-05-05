### Proceso de renovación ###

### Inputs
t <- 100 # tiempo final
# parámetros de la distribución de los tiempos interraribo
# en este caso una Gamma(alpha, beta)
alpha <- 1.8
beta <- 1.5


#Output: un vector con los tiempos de arribo de una realización 

#el proceso siempre inicia en 0
W <- c(0) 

#sumar tiempos interraribo hasta que se pase del tiempo final
while (W[length(W)] < t) {
  T_i <- rgamma(1, alpha, beta)
  W <- c(W, W[length(W)] + T_i)
}

#Gráfuca del proceso
plot(W, 0:(length(W)-1), type = "s", xlim = c(0, t), ylim = c(0, max(W)))

#Gráfica de la esperanza del proceso
abline(a = 0, b = 1/(alpha/beta), col = "red")

