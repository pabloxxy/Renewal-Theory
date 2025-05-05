### Proceso de renovación con premio ###

## Inputs
t <- 100 #tiempo final
lambda <- 3 #intensidad del Poisson
#parámetros de la distribución de los saltos, aquí Yn ~ Gamma(alpha, beta)
alpha <- 0.5
beta <- 1.5 

## Outputs: vector con los tiempos de salto y vector con la suma de los saltos
W_n <- c(0)
X_t <- c(0)

while (W_n[length(W_n)] < t) {
  T_i <- rexp(1, lambda)
  Y_i <- rgamma(1, alpha, beta)
  W_n <- c(W_n, W_n[length(W_n)] + T_i)
  X_t <- c(X_t, X_t[length(X_t)] + Y_i)
}

W_n
X_t

#Gráfica del proceso
plot(W_n, X_t, type = "s", xlim = c(0, t), ylim = c(0, max(X_t)))

#Gráfica de la esperanza del proceso
abline(a = 0, b = (alpha/beta)/(1/lambda), col = "red")
