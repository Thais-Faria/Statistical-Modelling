######################
# GLMS WITH OUR DATA #
######################

library(here)
library(ggplot2)
library(bbmle)
library(sads)
library(DHARMa)

# Getting processed decade data

abs_decades <- read.table(here("data","processed","decade_means_absolute.txt"), header=T)

rel_decades <- read.table(here("data","processed","decade_means_relative.txt"), header=T)

# Let's try to model

glm.linear <- lm(schizophrenia ~ gini,
                  data=abs_decades)

glm.pois <- glm(schizophrenia ~ gini,
                data=abs_decades,
                family=poisson(link="log"))

glm.bin <- glm(schizophrenia ~ gini,
               data=rel_decades,
               family=binomial(link="logit"))

logLik(glm.linear)
logLik(glm.pois)
logLik(glm.bin)

AIC(glm.linear)
AIC(glm.pois)
AIC(glm.bin)

coef(glm.linear)
coef(glm.pois)
coef(glm.bin)

par(mfrow=c(2,2))
plot(glm.linear)
plot(glm.pois)
plot(glm.bin)
dev.off()

sim_res_lm <- simulateResiduals(fittedModel = glm.linear)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin)
plot(sim_res_bin)

# Apparently the binomial model is the most accurate.