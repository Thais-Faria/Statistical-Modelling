set.seed(1618)
library(ggplot2)
library(bbmle)
library(sads)
library(DHARMa)

### Criando dados que não precisam ter nada a ver com os nossos ###

# População total

base.pop <- seq(3000, 10000, 200)
anos <- 1:length(base.pop)

#Adicionando ruído (normal) na população. Na vida real o ruído teria auto-correlação temporal
pop.count <- base.pop + floor(rnorm(max(anos)) * 1000) 

model.base.pop <- lm(base.pop ~ anos) #modelo da população base
plot(x = anos, y = base.pop)
abline(model.base.pop)
summary(model.base.pop)
coefficients(model.base.pop)

model.pop.count <- lm(pop.count ~ anos) #modelo da população com ruído normal
plot(x = anos, y = pop.count)
abline(model.pop.count)
summary(model.pop.count)
coefficients(model.pop.count)

# GINI
base.gini <- rep(50, length(base.pop))
gini.float <- base.gini + (rnorm(max(anos)) * 20)

model.base.gini <- lm(base.gini ~ anos) #modelo do GINI base
plot(x = anos, y = base.gini)
abline(model.base.gini)
summary(model.base.gini)
coefficients(model.base.gini)

model.gini.float <- lm(gini.float ~ anos) #modelo do GINI com ruído normal
plot(x = anos, y = gini.float)
abline(model.gini.float)
summary(model.gini.float)
coefficients(model.gini.float)

# Transtorno mental
pop.illness <- floor(pop.count/(gini.float/2))
dataset <- data.frame(anos,pop.count, gini.float, pop.illness)

ggplot(dataset) +
  aes(x = anos, y = pop.count) +
  geom_col(fill = "#112446") +
  geom_col(
    aes(x = anos,
        y = pop.illness),
    fill = "red"
  ) +
  theme_minimal()

## TENTANDO MODELAR ALGUMA COISA DE ACORDO COM OS JEITO MOSTRADO NOS TUTORIAIS##
# A ideia aqui é usar de base pras nossas tabelas, os dados foram criados só pra ter certeza que os códigos não tavam bugados

glm.pois <- glm(pop.illness ~ gini.float,
                family = poisson(link = "log"),
                data = data)

glm.bin <- glm(cbind(pop.illness, pop.count - pop.illness) ~ gini.float,
               family=binomial(link="logit"),
               data = data)

logLik(glm.pois)
logLik(glm.bin)

AIC(glm.pois)
AIC(glm.bin)

coef(glm.pois)
coef(glm.bin)

plot(glm.pois)
plot(glm.bin)

sim_res_pois <- simulateResiduals(fittedModel = glm.pois)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin)
plot(sim_res_bin)





## ANTIGO, NÃO IMPORTA ##

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