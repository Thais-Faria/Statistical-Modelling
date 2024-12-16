library(bbmle)
library(sads)

#### POISSON ####
# Argumentos de exemplo
x = rep(1,10) # "10 experimentos com 1 contagem cada"

# Função para um modelo de Poisson #
NegLogLikPois <- function(x, lambda) { 
  -sum( dpois(x=x, lambda=lambda, log=TRUE )  ) 
  }
# Ajustando o modelo aos dados (contagens)
pois.mle <- mle2(NegLogLikPois, start=list(lambda=10), data = list(x=x))

# Resultado do ajuste
summary(pois.mle)
# Log-verossimilhança negativa mínima e AIC
logLik(pois.mle) 
AIC(logLik(pois.mle) )
# O valor do lambda encontrado para o modelo ajustado
coef(pois.mle)
# No caso do Modelo de Poisson, o lambda esperado deve ser igual à média. Vamos checar:
mean(x)
# Gráfico dos valores plausíveis de lambda
plotprofmle( profile(pois.mle) )
# A função que poderia ser usada para gerar dados com o modelo ajustado é
rpois(x, coef(pois.mle))

#### BINOMIAL ####
x = rep(1,10) # "10 experimentos com 1 sucesso cada"
size = rep(5,10) # "10 experimentos com 5 tentativas cada"

# Função para um modelo Binomial #
NegLogLikBinom <- function(x, size, prob) { 
  -sum(dbinom(x = x, size = size, prob = prob, log = TRUE))
}

# Ajustando o modelo aos dados (sucessos)
binom.mle <- mle2(NegLogLikBinom, start=list(prob = 0.5), data = list(x=x, size=size))

# Resultado do ajuste
summary(binom.mle)
# Log-verossimilhança negativa mínima e AIC
logLik(binom.mle) 
AIC(logLik(binom.mle))
# O valor da probabilidade encontrado para o modelo ajustado
coef(binom.mle)
plotprofmle( profile(binom.mle) )
# A função que poderia ser usada para gerar dados com o modelo ajustado é
rbinom(x, size, coef(binom.mle))