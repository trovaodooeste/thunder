# Definição dos parâmetros
alpha <- 0.3 # participação do capital na função de produção
beta_seq <- seq(0.5, 1.5, by=0.1) # sequência de elasticidades intertemporais
delta <- 0.1 # taxa de depreciação do capital físico
rho <- 0.05 # taxa de desconto intertemporal

# Definição da função de utilidade
u <- function(c) {
  if (beta == 1) {
    log(c)
  } else {
    (c^(1-beta) - 1)/(1-beta)
  }
}

# Definição da função de produção
f <- function(k, A) {
  A * k^alpha
}

# Definição das equações dinâmicas
dkdt <- function(t, y, p) {
  with(as.list(c(y, p)), {
    f(k, A) - delta * k
  })
}

dAdt <- function(t, y, p) {
  with(as.list(c(y, p)), {
    0
  })
}

dcdt <- function(t, y, p) {
  with(as.list(c(y, p)), {
    beta_seq[i] * ud(c0) - lambda
  })
}

dldt <- function(t, y, p) {
  with(as.list(c(y, p)), {
    -udd(c0) * (1-l)
  })
}

# Definição das derivadas da função de utilidade
ud <- function(c) {
  c^(-beta)
}

udd <- function(c) {
  -beta * c^(-beta-1)
}

library(deSolve)

# Definição das derivadas da função de utilidade
u_prime <- function(S) {
  1 / (1 + exp(-S))
}

# Definição da derivada do estoque
dS_dt <- function(S, t, param1, param2) {
  r <- param1
  a <- param2
  
  u_prime_S <- u_prime(S)
  dS_dt <- r - a * u_prime_S
  
  return(dS_dt)
}

# Definição dos tempos e condições iniciais
times <- seq(0, 10, by = 0.01)
S0 <- 2
X0 <- c(S0)

# Definição dos parâmetros
r <- 0.1 # taxa de crescimento
a <- 0.5 # elasticidade-preço da demanda
b <- 0.5 # elasticidade-preço da oferta
c <- 0.1 # custo fixo

# Função das derivadas
dX_dt <- function(t, X, parms) {
  with(as.list(c(X, parms)), {
    S <- X[1]
    dS_dt <- r - a * u_prime(S) + b * u_prime(S)
    return(list(dS_dt))
  })
}
# Simulação da dinâmica do estoque
out <- ode(y = X0, times = times, func = dX_dt, parms = list(param1 = r, param2 = a, param3 = b))
S <- out[, 2] # estoque ao longo do tempo

# Cálculo da trajetória ótima
S_optimal <- numeric(length = length(times))
for (i in seq_along(times)) {
  t <- times[i]
  S_t <- S[i]
  lambda_t <- r + u_prime(S_t)*a - u_prime(S_t)*b
  S_optimal[i] <- (S0^(u_prime(S_t)*a/lambda_t))*exp(-c/lambda_t*(t - 0))^(u_prime(S_t)*b/lambda_t)
}

# Plot da dinâmica do estoque e da trajetória ótima
plot(times, S, type = "l", xlab = "Tempo", ylab = "Estoque")
lines(times, S_optimal, col = "red") 
legend("topright", c("Estoque", "Trajetória ótima"), col = c("black", "red"), lty = c(1, 1))


