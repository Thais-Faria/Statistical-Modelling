###################################
# LINEAR MODELS BETWEEN ILLNESSES #
###################################

library(here)

decades <- read.table(here("data","processed","decade_means_absolute.txt"), header=T)

# mean by country

decades$country <- as.factor(decades$country)

schizophrenia <- aggregate(decades$schizophrenia~factor(decades$country), FUN=mean)
depression <- aggregate(decades$depression~factor(decades$country), FUN=mean)
anxiety <- aggregate(decades$anxiety~factor(decades$country), FUN=mean)
bipolar <- aggregate(decades$bipolar~factor(decades$country), FUN=mean)
ed <- aggregate(decades$ed~factor(decades$country), FUN=mean)
gini <- aggregate(decades$gini~factor(decades$country), FUN=mean)

# linear models

lm_sd <- lm(schizophrenia$`decades$schizophrenia`~depression$`decades$depression`)
summary(lm_sd)
par(mfrow=c(2,2))
plot(lm_sd)

lm_sa <- lm(schizophrenia$`decades$schizophrenia`~anxiety$`decades$anxiety`)
summary(lm_sa)
par(mfrow=c(2,2))
plot(lm_sa)

lm_sb <- lm(schizophrenia$`decades$schizophrenia`~bipolar$`decades$bipolar`)
summary(lm_sb)
par(mfrow=c(2,2))
plot(lm_sb)

lm_se <- lm(schizophrenia$`decades$schizophrenia`~ed$`decades$ed`)
summary(lm_se)
par(mfrow=c(2,2))
plot(lm_se)

lm_ab <- lm(anxiety$`decades$anxiety`~bipolar$`decades$bipolar`)
summary(lm_ab)
par(mfrow=c(2,2))
plot(lm_ab)

lm_ae <- lm(anxiety$`decades$anxiety`~ed$`decades$ed`)
summary(lm_ae)
par(mfrow=c(2,2))
plot(lm_ae)

lm_be <- lm(bipolar$`decades$bipolar`~ed$`decades$ed`)
summary(lm_be)
par(mfrow=c(2,2))
plot(lm_be)
