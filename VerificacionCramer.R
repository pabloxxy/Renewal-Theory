iteraciones <- 100

procesos_arruinados <- 0

vector_arruinados <- c(0)
vector_probabilidades <- c(0)

t <- 50000 
u <- 1000 
c <- 1 
lambda <- 0.1 
alpha <- 0.1005



#Monte Carlo. Repetiremos el proceso varias veces y contaremos en cuántas se arruina.
for(i in 1:iteraciones) {
  
  W_n <- c(0)
  R_t <- c(u)
  
  #Si el proceso se arruina lo detenemos.
  while(R_t[length(R_t)] > 0 & length(R_t) < t){
    T_i <- rexp(1, lambda)
    W_n <- c(W_n, W_n[length(W_n)] + T_i)
    R_t <- c(R_t, c*T_i + R_t[length(R_t)] - rexp(1, alpha))
  }
  
  print(length(R_t))
  
  #Contamos todos los procesos que se arruinaron.
  if(length(R_t) < t){
    procesos_arruinados <- procesos_arruinados + 1
  }
  
  #Este vector es para poder graficar.
  vector_arruinados <- c(vector_arruinados, (1/i)*procesos_arruinados)
  print(vector_arruinados[length(vector_arruinados)])
}

#Calculamos la probabilidad de ruina.
probabilidad_ruina <- procesos_arruinados/iteraciones
probabilidad_ruina


#Calculamos la probabilidad de ruina teórica.
psi <- (lambda/(alpha*c))*(exp(-(alpha-(lambda/c))*u))
psi

#Calculamos el error (debe ser cercano a 0).
probabilidad_ruina - psi

print(length(vector_arruinados))
print(length(0:iteraciones))

plot(0:iteraciones, vector_arruinados, type = "l")
abline(h = psi, col = "red")

