### Cramer-Lundberg ###

## Inputs
t <- 100000 
u <- 1000 
c <- 1 
lambda <- 0.1 #intensidad del Poisson
#parámetros de la distribución de los saltos, aquí Yn ~ exp(alpha)
alpha <- 0.1005

## Outputs: vector con los tiempos de salto y vector con el valor del proceso
W_n <- c(0)
R_t <- c(u)

# Como el proceso es creciente entre saltos, para calcular la probabilidad
# de ruina, solo nos interesará cuánto vale en los momentos en que salta.
# Es decir, nuestro vector solo tendrá los R_{T_i}.

while (W_n[length(W_n)] < t) {
  T_i <- rexp(1, lambda)
  W_n <- c(W_n, W_n[length(W_n)] + T_i)
  R_t <- c(R_t, c*T_i + R_t[length(R_t)] - rexp(1, alpha))
}

T_i
R_t

#Gráfica del proceso
plot(W_n, R_t, type = "l", xlim = c(0, t), ylim = c(min(R_t), max(R_t)))

#Probabilidad de ruina
psi <- (lambda/(alpha*c))*(exp(-(alpha-(lambda/c))*u))  
psi

