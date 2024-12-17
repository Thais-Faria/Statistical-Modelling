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

abs_decades$decade[which(abs_decades$decade=="1990-1999")] <- 1
abs_decades$decade[which(abs_decades$decade=="2000-2009")] <- 2
abs_decades$decade[which(abs_decades$decade=="2010-2019")] <- 3
abs_decades$decade <- as.factor(abs_decades$decade)
abs_decades$schizophrenia <- log(abs_decades$schizophrenia)
abs_decades$depression <- log(abs_decades$depression)
abs_decades$anxiety <- log(abs_decades$anxiety)
abs_decades$bipolar <- log(abs_decades$bipolar)
abs_decades$ed <- log(abs_decades$ed)

# Getting population data as well

worldbank_1 <- read.csv(here("data","raw","worldbank","WDICSV.csv"), sep=",", header=T)
pop <- worldbank_1[which(worldbank_1$Indicator.Name=="Population, total"),] # only population data
pop <- pop[which(pop$Country.Name %in% abs_decades$country),]
pop <- t(pop)

colnames(pop) <- pop[1,]
pop <- as.data.frame(pop)
pop <- pop[5:68,]
pop[65,] <- colnames(pop)
pop$year <- rownames(pop)
pop$year <- sub("X", "", pop$year)

abs_decades$pop <- rep(NA, length(abs_decades$country))

countries <- as.character(pop[65,])

pop <- pop[1:64,]

pop <- apply(pop, FUN=as.numeric, MARGIN=c(1,2))
pop <- data.frame(pop)

for(i in 1:length(abs_decades$country)){
  
  abs_decades$pop[which(abs_decades$country==countries[i] & abs_decades$decade==1)] <- mean(pop[(which(pop$year>1989 & pop$year<2000)),i])
  
  abs_decades$pop[which(abs_decades$country==countries[i] & abs_decades$decade==2)] <- mean(pop[(which(pop$year>1999 & pop$year<2010)),i])
  
  abs_decades$pop[which(abs_decades$country==countries[i] & abs_decades$decade==3)] <- mean(pop[(which(pop$year>2009 & pop$year<2020)),i])
  
}

abs_decades$pop <- log(abs_decades$pop)

abs_decades_raw <- abs_decades

# Rounding everything bcs the counting models cant take decimals
abs_decades$schizophrenia <- round(abs_decades$schizophrenia)
abs_decades$depression <- round(abs_decades$depression)
abs_decades$anxiety <- round(abs_decades$anxiety)
abs_decades$bipolar <- round(abs_decades$bipolar)
abs_decades$ed <- round(abs_decades$ed)
abs_decades$pop <- round(abs_decades$pop)


# Let's try to model

##############################
# SCHIZOPHRENIA
##############################

#### ONLY YEAR ####

glm.linear.year <- lm(schizophrenia ~ decade,
                 data=abs_decades)

glm.pois.year <- glm(schizophrenia ~ decade,
                data=abs_decades,
                family=poisson(link="log"))

glm.bin.year <- glm(cbind(schizophrenia,pop) ~ decade,
               data=abs_decades,
               family=binomial(link="logit"))

logLik(glm.linear.year)
logLik(glm.pois.year)
logLik(glm.bin.year)

AIC(glm.linear.year)
AIC(glm.pois.year)
AIC(glm.bin.year)

coef(glm.linear.year)
coef(glm.pois.year)
coef(glm.bin.year)

par(mfrow=c(2,2))
plot(glm.linear.year)
plot(glm.pois.year)
plot(glm.bin.year)
dev.off()

plots <- matrix(c(1,1,2,3,4,5), ncol=2, byrow=T)

png(filename=here("images","models","schizo_linear_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Linear esquizofrenia (tempo)"), cex=2, bty="n")
plot(glm.linear.year)
dev.off()

png(filename=here("images","models","schizo_poisson_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson esquizofrenia (tempo)"), cex=2, bty="n")
plot(glm.pois.year)
dev.off()

png(filename=here("images","models","schizo_binomial_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial esquizofrenia (tempo)"), cex=2, bty="n")
plot(glm.bin.year)
dev.off()

png(filename=here("images","models","schizo_linear_tempo_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.year)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","schizo_poisson_tempo_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.year)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","schizo_binomial_tempo_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.year)
plot(sim_res_bin)
dev.off()



#### WITH GINI ####

glm.linear.gini <- lm(schizophrenia ~ decade + gini,
                      data=abs_decades)

glm.pois.gini <- glm(schizophrenia ~ decade + gini,
                     data=abs_decades,
                     family=poisson(link="log"))

glm.bin.gini <- glm(cbind(schizophrenia,pop) ~ decade + gini,
                    data=abs_decades,
                    family=binomial(link="logit"))

logLik(glm.linear.gini)
logLik(glm.pois.gini)
logLik(glm.bin.gini)

AIC(glm.linear.gini)
AIC(glm.pois.gini)
AIC(glm.bin.gini)

coef(glm.linear.gini)
coef(glm.pois.gini)
coef(glm.bin.gini)

par(mfrow=c(2,2))
plot(glm.linear.gini)
plot(glm.pois.gini)
plot(glm.bin.gini)
dev.off() # Waaaay better!!!!!

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.gini)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.gini)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.gini)
plot(sim_res_bin)


png(filename=here("images","models","schizo_linear_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Linear esquizofrenia (gini)"), cex=2, bty="n")
plot(glm.linear.gini)
dev.off()

png(filename=here("images","models","schizo_poisson_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson esquizofrenia (gini)"), cex=2, bty="n")
plot(glm.pois.gini)
dev.off()

png(filename=here("images","models","schizo_binomial_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial esquizofrenia (gini)"), cex=2, bty="n")
plot(glm.bin.gini)
dev.off()

png(filename=here("images","models","schizo_linear_gini_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.gini)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","schizo_poisson_gini_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.gini)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","schizo_binomial_gini_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.gini)
plot(sim_res_bin)
dev.off()

#### WITH COUNTRY AS PROXY FOR GENETIC HOMOGENEITY ####

glm.linear.country <- lm(schizophrenia ~ decade + country,
                         data=abs_decades)

glm.pois.country <- glm(schizophrenia ~ decade + country,
                        data=abs_decades,
                        family=poisson(link="log"))

glm.bin.country <- glm(cbind(schizophrenia,pop) ~ decade + country,
                       data=abs_decades,
                       family=binomial(link="logit"))

logLik(glm.linear.country)
logLik(glm.pois.country)
logLik(glm.bin.country)

AIC(glm.linear.country)
AIC(glm.pois.country)
AIC(glm.bin.country)

coef(glm.linear.country)
coef(glm.pois.country)
coef(glm.bin.country)

par(mfrow=c(2,2))
plot(glm.linear.country)
plot(glm.pois.country)
plot(glm.bin.country)
dev.off() 

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country)
plot(sim_res_bin)

png(filename=here("images","models","schizo_linear_country.png"))
layout(plots)
plot.new()
legend("center",paste("Linear esquizofrenia (country)"), cex=2, bty="n")
plot(glm.linear.country)
dev.off()

png(filename=here("images","models","schizo_poisson_country.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson esquizofrenia (country)"), cex=2, bty="n")
plot(glm.pois.country)
dev.off()

png(filename=here("images","models","schizo_binomial_country.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial esquizofrenia (country)"), cex=2, bty="n")
plot(glm.bin.country)
dev.off()

png(filename=here("images","models","schizo_linear_country_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","schizo_poisson_country_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","schizo_binomial_country_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country)
plot(sim_res_bin)
dev.off()


#### WITH COUNTRY AS PROXY FOR GENETIC HOMOGENEITY AND GINI ####

glm.linear.country.gini <- lm(schizophrenia ~ decade + country + gini,
                         data=abs_decades)

glm.pois.country.gini <- glm(schizophrenia ~ decade + country + gini,
                        data=abs_decades,
                        family=poisson(link="log"))

glm.bin.country.gini <- glm(cbind(schizophrenia,pop) ~ decade + country + gini,
                       data=abs_decades,
                       family=binomial(link="logit"))

logLik(glm.linear.country.gini)
logLik(glm.pois.country.gini)
logLik(glm.bin.country.gini)

AIC(glm.linear.country.gini)
AIC(glm.pois.country.gini)
AIC(glm.bin.country.gini)

coef(glm.linear.country.gini)
coef(glm.pois.country.gini)
coef(glm.bin.country.gini)

par(mfrow=c(2,2))
plot(glm.linear.country.gini)
plot(glm.pois.country.gini)
plot(glm.bin.country.gini)
dev.off() 

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country.gini)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country.gini)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country.gini)
plot(sim_res_bin)

png(filename=here("images","models","schizo_linear_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Linear esquizofrenia (country + gini)"), cex=2, bty="n")
plot(glm.linear.country.gini)
dev.off()

png(filename=here("images","models","schizo_poisson_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson esquizofrenia (country + gini)"), cex=2, bty="n")
plot(glm.pois.country.gini)
dev.off()

png(filename=here("images","models","schizo_binomial_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial esquizofrenia (country + gini)"), cex=2, bty="n")
plot(glm.bin.country.gini)
dev.off()

png(filename=here("images","models","schizo_linear_country_gini_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country.gini)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","schizo_poisson_country_gini_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country.gini)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","schizo_binomial_country_gini_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country.gini)
plot(sim_res_bin)
dev.off()



##############################
# DEPRESSION
##############################

#### ONLY YEAR ####

glm.linear.year <- lm(depression ~ decade,
                      data=abs_decades)

glm.pois.year <- glm(depression ~ decade,
                     data=abs_decades,
                     family=poisson(link="log"))

glm.bin.year <- glm(cbind(depression,pop) ~ decade,
                    data=abs_decades,
                    family=binomial(link="logit"))

logLik(glm.linear.year)
logLik(glm.pois.year)
logLik(glm.bin.year)

AIC(glm.linear.year)
AIC(glm.pois.year)
AIC(glm.bin.year)

coef(glm.linear.year)
coef(glm.pois.year)
coef(glm.bin.year)

par(mfrow=c(2,2))
plot(glm.linear.year)
plot(glm.pois.year)
plot(glm.bin.year)
dev.off()

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.year)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.year)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.year)
plot(sim_res_bin)

plots <- matrix(c(1,1,2,3,4,5), ncol=2, byrow=T)

png(filename=here("images","models","depression_linear_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Linear depressão (tempo)"), cex=2, bty="n")
plot(glm.linear.year)
dev.off()

png(filename=here("images","models","depression_poisson_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson depressão (tempo)"), cex=2, bty="n")
plot(glm.pois.year)
dev.off()

png(filename=here("images","models","depression_binomial_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial depressão (tempo)"), cex=2, bty="n")
plot(glm.bin.year)
dev.off()

png(filename=here("images","models","depression_linear_tempo_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.year)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","depression_poisson_tempo_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.year)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","depression_binomial_tempo_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.year)
plot(sim_res_bin)
dev.off()

#### WITH GINI ####

glm.linear.gini <- lm(depression ~ decade + gini,
                      data=abs_decades)

glm.pois.gini <- glm(depression ~ decade + gini,
                     data=abs_decades,
                     family=poisson(link="log"))

glm.bin.gini <- glm(cbind(depression,pop) ~ decade + gini,
                    data=abs_decades,
                    family=binomial(link="logit"))

logLik(glm.linear.gini)
logLik(glm.pois.gini)
logLik(glm.bin.gini)

AIC(glm.linear.gini)
AIC(glm.pois.gini)
AIC(glm.bin.gini)

coef(glm.linear.gini)
coef(glm.pois.gini)
coef(glm.bin.gini)

par(mfrow=c(2,2))
plot(glm.linear.gini)
plot(glm.pois.gini)
plot(glm.bin.gini)
dev.off()

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.gini)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.gini)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.gini)
plot(sim_res_bin)

png(filename=here("images","models","depression_linear_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Linear depressão (gini)"), cex=2, bty="n")
plot(glm.linear.gini)
dev.off()

png(filename=here("images","models","depression_poisson_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson depressão (gini)"), cex=2, bty="n")
plot(glm.pois.gini)
dev.off()

png(filename=here("images","models","depression_binomial_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial depressão (gini)"), cex=2, bty="n")
plot(glm.bin.gini)
dev.off()

png(filename=here("images","models","depression_linear_gini_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.gini)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","depression_poisson_gini_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.gini)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","depression_binomial_gini_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.gini)
plot(sim_res_bin)
dev.off()


#### WITH COUNTRY AS PROXY FOR GENETIC HOMOGENEITY ####

glm.linear.country <- lm(depression ~ decade + country,
                         data=abs_decades)

glm.pois.country <- glm(depression ~ decade + country,
                        data=abs_decades,
                        family=poisson(link="log"))

glm.bin.country <- glm(cbind(depression,pop) ~ decade + country,
                       data=abs_decades,
                       family=binomial(link="logit"))

logLik(glm.linear.country)
logLik(glm.pois.country)
logLik(glm.bin.country)

AIC(glm.linear.country)
AIC(glm.pois.country)
AIC(glm.bin.country)

coef(glm.linear.country)
coef(glm.pois.country)
coef(glm.bin.country)

par(mfrow=c(2,2))
plot(glm.linear.country)
plot(glm.pois.country)
plot(glm.bin.country)
dev.off()

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country)
plot(sim_res_bin)

png(filename=here("images","models","depression_linear_country.png"))
layout(plots)
plot.new()
legend("center",paste("Linear depressão (country)"), cex=2, bty="n")
plot(glm.linear.country)
dev.off()

png(filename=here("images","models","depression_poisson_country.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson depressão (country)"), cex=2, bty="n")
plot(glm.pois.country)
dev.off()

png(filename=here("images","models","depression_binomial_country.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial depressão (country)"), cex=2, bty="n")
plot(glm.bin.country)
dev.off()

png(filename=here("images","models","depression_linear_country_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","depression_poisson_country_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","depression_binomial_country_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country)
plot(sim_res_bin)
dev.off()

#### WITH COUNTRY AS PROXY FOR GENETIC HOMOGENEITY AND GINI ####

glm.linear.country.gini <- lm(depression ~ decade + country + gini,
                              data=abs_decades)

glm.pois.country.gini <- glm(depression ~ decade + country + gini,
                             data=abs_decades,
                             family=poisson(link="log"))

glm.bin.country.gini <- glm(cbind(depression,pop) ~ decade + country + gini,
                            data=abs_decades,
                            family=binomial(link="logit"))

logLik(glm.linear.country.gini)
logLik(glm.pois.country.gini)
logLik(glm.bin.country.gini)

AIC(glm.linear.country.gini)
AIC(glm.pois.country.gini)
AIC(glm.bin.country.gini)

coef(glm.linear.country.gini)
coef(glm.pois.country.gini)
coef(glm.bin.country.gini)

par(mfrow=c(2,2))
plot(glm.linear.country.gini)
plot(glm.pois.country.gini)
plot(glm.bin.country.gini)
dev.off() 

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country.gini)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country.gini)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country.gini)
plot(sim_res_bin)

png(filename=here("images","models","depression_linear_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Linear depressão (country + gini)"), cex=2, bty="n")
plot(glm.linear.country.gini)
dev.off()

png(filename=here("images","models","depression_poisson_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson depressão (country + gini)"), cex=2, bty="n")
plot(glm.pois.country.gini)
dev.off()

png(filename=here("images","models","depression_binomial_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial depressão (country + gini)"), cex=2, bty="n")
plot(glm.bin.country.gini)
dev.off()

png(filename=here("images","models","depression_linear_country_gini_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country.gini)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","depression_poisson_country_gini_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country.gini)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","depression_binomial_country_gini_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country.gini)
plot(sim_res_bin)
dev.off()

##############################
# ANXIETY
##############################

#### ONLY YEAR ####

glm.linear.year <- lm(anxiety ~ decade,
                      data=abs_decades)

glm.pois.year <- glm(anxiety ~ decade,
                     data=abs_decades,
                     family=poisson(link="log"))

glm.bin.year <- glm(cbind(anxiety,pop) ~ decade,
                    data=abs_decades,
                    family=binomial(link="logit"))

logLik(glm.linear.year)
logLik(glm.pois.year)
logLik(glm.bin.year)

AIC(glm.linear.year)
AIC(glm.pois.year)
AIC(glm.bin.year)

coef(glm.linear.year)
coef(glm.pois.year)
coef(glm.bin.year)

par(mfrow=c(2,2))
plot(glm.linear.year)
plot(glm.pois.year)
plot(glm.bin.year)
dev.off() # Terrible, as expected

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.year)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.year)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.year)
plot(sim_res_bin)

png(filename=here("images","models","anxiety_linear_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Linear ansiedade (tempo)"), cex=2, bty="n")
plot(glm.linear.year)
dev.off()

png(filename=here("images","models","anxiety_poisson_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson ansiedade (tempo)"), cex=2, bty="n")
plot(glm.pois.year)
dev.off()

png(filename=here("images","models","anxiety_binomial_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial ansiedade (tempo)"), cex=2, bty="n")
plot(glm.bin.year)
dev.off()

png(filename=here("images","models","anxiety_linear_tempo_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.year)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","anxiety_poisson_tempo_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.year)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","anxiety_binomial_tempo_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.year)
plot(sim_res_bin)
dev.off()


#### WITH GINI ####

glm.linear.gini <- lm(anxiety ~ decade + gini,
                      data=abs_decades)

glm.pois.gini <- glm(anxiety ~ decade + gini,
                     data=abs_decades,
                     family=poisson(link="log"))

glm.bin.gini <- glm(cbind(anxiety,pop) ~ decade + gini,
                    data=abs_decades,
                    family=binomial(link="logit"))

logLik(glm.linear.gini)
logLik(glm.pois.gini)
logLik(glm.bin.gini)

AIC(glm.linear.gini)
AIC(glm.pois.gini)
AIC(glm.bin.gini)

coef(glm.linear.gini)
coef(glm.pois.gini)
coef(glm.bin.gini)

par(mfrow=c(2,2))
plot(glm.linear.gini)
plot(glm.pois.gini)
plot(glm.bin.gini)
dev.off() # Waaaay better!!!!!

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.gini)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.gini)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.gini)
plot(sim_res_bin)

png(filename=here("images","models","anxiety_linear_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Linear ansiedade (gini)"), cex=2, bty="n")
plot(glm.linear.gini)
dev.off()

png(filename=here("images","models","anxiety_poisson_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson ansiedade (gini)"), cex=2, bty="n")
plot(glm.pois.gini)
dev.off()

png(filename=here("images","models","anxiety_binomial_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial ansiedade (gini)"), cex=2, bty="n")
plot(glm.bin.gini)
dev.off()

png(filename=here("images","models","anxiety_linear_gini_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.gini)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","anxiety_poisson_gini_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.gini)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","anxiety_binomial_gini_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.gini)
plot(sim_res_bin)
dev.off()

#### WITH COUNTRY AS PROXY FOR GENETIC HOMOGENEITY ####

glm.linear.country <- lm(anxiety ~ decade + country,
                         data=abs_decades)

glm.pois.country <- glm(anxiety ~ decade + country,
                        data=abs_decades,
                        family=poisson(link="log"))

glm.bin.country <- glm(cbind(anxiety,pop) ~ decade + country,
                       data=abs_decades,
                       family=binomial(link="logit"))

logLik(glm.linear.country)
logLik(glm.pois.country)
logLik(glm.bin.country)

AIC(glm.linear.country)
AIC(glm.pois.country)
AIC(glm.bin.country)

coef(glm.linear.country)
coef(glm.pois.country)
coef(glm.bin.country)

par(mfrow=c(2,2))
plot(glm.linear.country)
plot(glm.pois.country)
plot(glm.bin.country)
dev.off() 

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country)
plot(sim_res_bin)

png(filename=here("images","models","anxiety_linear_country.png"))
layout(plots)
plot.new()
legend("center",paste("Linear ansiedade (country)"), cex=2, bty="n")
plot(glm.linear.country)
dev.off()

png(filename=here("images","models","anxiety_poisson_country.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson ansiedade (country)"), cex=2, bty="n")
plot(glm.pois.country)
dev.off()

png(filename=here("images","models","anxiety_binomial_country.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial ansiedade (country)"), cex=2, bty="n")
plot(glm.bin.country)
dev.off()

png(filename=here("images","models","anxiety_linear_country_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","anxiety_poisson_country_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","anxiety_binomial_country_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country)
plot(sim_res_bin)
dev.off()

#### WITH COUNTRY AS PROXY FOR GENETIC HOMOGENEITY AND GINI ####

glm.linear.country.gini <- lm(anxiety ~ decade + country + gini,
                              data=abs_decades)

glm.pois.country.gini <- glm(anxiety ~ decade + country + gini,
                             data=abs_decades,
                             family=poisson(link="log"))

glm.bin.country.gini <- glm(cbind(anxiety,pop) ~ decade + country + gini,
                            data=abs_decades,
                            family=binomial(link="logit"))

logLik(glm.linear.country.gini)
logLik(glm.pois.country.gini)
logLik(glm.bin.country.gini)

AIC(glm.linear.country.gini)
AIC(glm.pois.country.gini)
AIC(glm.bin.country.gini)

coef(glm.linear.country.gini)
coef(glm.pois.country.gini)
coef(glm.bin.country.gini)

par(mfrow=c(2,2))
plot(glm.linear.country.gini)
plot(glm.pois.country.gini)
plot(glm.bin.country.gini)
dev.off() 

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country.gini)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country.gini)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country.gini)
plot(sim_res_bin)

png(filename=here("images","models","anxiety_linear_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Linear ansiedade (country + gini)"), cex=2, bty="n")
plot(glm.linear.country.gini)
dev.off()

png(filename=here("images","models","anxiety_poisson_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson ansiedade (country + gini)"), cex=2, bty="n")
plot(glm.pois.country.gini)
dev.off()

png(filename=here("images","models","anxiety_binomial_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial ansiedade (country + gini)"), cex=2, bty="n")
plot(glm.bin.country.gini)
dev.off()

png(filename=here("images","models","anxiety_linear_country_gini_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country.gini)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","anxiety_poisson_country_gini_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country.gini)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","anxiety_binomial_country_gini_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country.gini)
plot(sim_res_bin)
dev.off()

##############################
# BIPOLAR
##############################

#### ONLY YEAR ####

glm.linear.year <- lm(bipolar ~ decade,
                      data=abs_decades)

glm.pois.year <- glm(bipolar ~ decade,
                     data=abs_decades,
                     family=poisson(link="log"))

glm.bin.year <- glm(cbind(bipolar,pop) ~ decade,
                    data=abs_decades,
                    family=binomial(link="logit"))

logLik(glm.linear.year)
logLik(glm.pois.year)
logLik(glm.bin.year)

AIC(glm.linear.year)
AIC(glm.pois.year)
AIC(glm.bin.year)

coef(glm.linear.year)
coef(glm.pois.year)
coef(glm.bin.year)

par(mfrow=c(2,2))
plot(glm.linear.year)
plot(glm.pois.year)
plot(glm.bin.year)
dev.off()

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.year)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.year)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.year)
plot(sim_res_bin)

png(filename=here("images","models","bipolar_linear_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Linear bipolar (tempo)"), cex=2, bty="n")
plot(glm.linear.year)
dev.off()

png(filename=here("images","models","bipolar_poisson_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson bipolar (tempo)"), cex=2, bty="n")
plot(glm.pois.year)
dev.off()

png(filename=here("images","models","bipolar_binomial_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial bipolar (tempo)"), cex=2, bty="n")
plot(glm.bin.year)
dev.off()

png(filename=here("images","models","bipolar_linear_tempo_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.year)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","bipolar_poisson_tempo_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.year)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","bipolar_binomial_tempo_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.year)
plot(sim_res_bin)
dev.off()

#### WITH GINI ####

glm.linear.gini <- lm(bipolar ~ decade + gini,
                      data=abs_decades)

glm.pois.gini <- glm(bipolar ~ decade + gini,
                     data=abs_decades,
                     family=poisson(link="log"))

glm.bin.gini <- glm(cbind(bipolar,pop) ~ decade + gini,
                    data=abs_decades,
                    family=binomial(link="logit"))

logLik(glm.linear.gini)
logLik(glm.pois.gini)
logLik(glm.bin.gini)

AIC(glm.linear.gini)
AIC(glm.pois.gini)
AIC(glm.bin.gini)

coef(glm.linear.gini)
coef(glm.pois.gini)
coef(glm.bin.gini)

par(mfrow=c(2,2))
plot(glm.linear.gini)
plot(glm.pois.gini)
plot(glm.bin.gini)
dev.off()

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.gini)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.gini)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.gini)
plot(sim_res_bin)

png(filename=here("images","models","bipolar_linear_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Linear bipolar (gini)"), cex=2, bty="n")
plot(glm.linear.gini)
dev.off()

png(filename=here("images","models","bipolar_poisson_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson bipolar (gini)"), cex=2, bty="n")
plot(glm.pois.gini)
dev.off()

png(filename=here("images","models","bipolar_binomial_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial bipolar (gini)"), cex=2, bty="n")
plot(glm.bin.gini)
dev.off()

png(filename=here("images","models","bipolar_linear_gini_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.gini)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","bipolar_poisson_gini_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.gini)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","bipolar_binomial_gini_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.gini)
plot(sim_res_bin)
dev.off()

#### WITH COUNTRY AS PROXY FOR GENETIC HOMOGENEITY ####

glm.linear.country <- lm(bipolar ~ decade + country,
                         data=abs_decades)

glm.pois.country <- glm(bipolar ~ decade + country,
                        data=abs_decades,
                        family=poisson(link="log"))

glm.bin.country <- glm(cbind(bipolar,pop) ~ decade + country,
                       data=abs_decades,
                       family=binomial(link="logit"))

logLik(glm.linear.country)
logLik(glm.pois.country)
logLik(glm.bin.country)

AIC(glm.linear.country)
AIC(glm.pois.country)
AIC(glm.bin.country)

coef(glm.linear.country)
coef(glm.pois.country)
coef(glm.bin.country)

par(mfrow=c(2,2))
plot(glm.linear.country)
plot(glm.pois.country)
plot(glm.bin.country)
dev.off()

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country)
plot(sim_res_bin)

png(filename=here("images","models","bipolar_linear_country.png"))
layout(plots)
plot.new()
legend("center",paste("Linear bipolar (country)"), cex=2, bty="n")
plot(glm.linear.country)
dev.off()

png(filename=here("images","models","bipolar_poisson_country.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson bipolar (country)"), cex=2, bty="n")
plot(glm.pois.country)
dev.off()

png(filename=here("images","models","bipolar_binomial_country.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial bipolar (country)"), cex=2, bty="n")
plot(glm.bin.country)
dev.off()

png(filename=here("images","models","bipolar_linear_country_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","bipolar_poisson_country_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","bipolar_binomial_country_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country)
plot(sim_res_bin)
dev.off()

#### WITH COUNTRY AS PROXY FOR GENETIC HOMOGENEITY AND GINI ####

glm.linear.country.gini <- lm(bipolar ~ decade + country + gini,
                              data=abs_decades)

glm.pois.country.gini <- glm(bipolar ~ decade + country + gini,
                             data=abs_decades,
                             family=poisson(link="log"))

glm.bin.country.gini <- glm(cbind(bipolar,pop) ~ decade + country + gini,
                            data=abs_decades,
                            family=binomial(link="logit"))

logLik(glm.linear.country.gini)
logLik(glm.pois.country.gini)
logLik(glm.bin.country.gini)

AIC(glm.linear.country.gini)
AIC(glm.pois.country.gini)
AIC(glm.bin.country.gini)

coef(glm.linear.country.gini)
coef(glm.pois.country.gini)
coef(glm.bin.country.gini)

par(mfrow=c(2,2))
plot(glm.linear.country.gini)
plot(glm.pois.country.gini)
plot(glm.bin.country.gini)
dev.off() 

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country.gini)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country.gini)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country.gini)
plot(sim_res_bin)

png(filename=here("images","models","bipolar_linear_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Linear bipolar (country + gini)"), cex=2, bty="n")
plot(glm.linear.country.gini)
dev.off()

png(filename=here("images","models","bipolar_poisson_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson bipolar (country + gini)"), cex=2, bty="n")
plot(glm.pois.country.gini)
dev.off()

png(filename=here("images","models","bipolar_binomial_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial bipolar (country + gini)"), cex=2, bty="n")
plot(glm.bin.country.gini)
dev.off()

png(filename=here("images","models","bipolar_linear_country_gini_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country.gini)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","bipolar_poisson_country_gini_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country.gini)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","bipolar_binomial_country_gini_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country.gini)
plot(sim_res_bin)
dev.off()

##############################
# ED
##############################

#### ONLY YEAR ####

glm.linear.year <- lm(ed ~ decade,
                      data=abs_decades)

glm.pois.year <- glm(ed ~ decade,
                     data=abs_decades,
                     family=poisson(link="log"))

glm.bin.year <- glm(cbind(ed,pop) ~ decade,
                    data=abs_decades,
                    family=binomial(link="logit"))

logLik(glm.linear.year)
logLik(glm.pois.year)
logLik(glm.bin.year)

AIC(glm.linear.year)
AIC(glm.pois.year)
AIC(glm.bin.year)

coef(glm.linear.year)
coef(glm.pois.year)
coef(glm.bin.year)

par(mfrow=c(2,2))
plot(glm.linear.year)
plot(glm.pois.year)
plot(glm.bin.year)
dev.off()

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.year)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.year)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.year)
plot(sim_res_bin)

png(filename=here("images","models","ed_linear_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Linear ed (tempo)"), cex=2, bty="n")
plot(glm.linear.year)
dev.off()

png(filename=here("images","models","ed_poisson_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson ed (tempo)"), cex=2, bty="n")
plot(glm.pois.year)
dev.off()

png(filename=here("images","models","ed_binomial_tempo.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial ed (tempo)"), cex=2, bty="n")
plot(glm.bin.year)
dev.off()

png(filename=here("images","models","ed_linear_tempo_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.year)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","ed_poisson_tempo_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.year)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","ed_binomial_tempo_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.year)
plot(sim_res_bin)
dev.off()

#### WITH GINI ####

glm.linear.gini <- lm(ed ~ decade + gini,
                      data=abs_decades)

glm.pois.gini <- glm(ed ~ decade + gini,
                     data=abs_decades,
                     family=poisson(link="log"))

glm.bin.gini <- glm(cbind(ed,pop) ~ decade + gini,
                    data=abs_decades,
                    family=binomial(link="logit"))

logLik(glm.linear.gini)
logLik(glm.pois.gini)
logLik(glm.bin.gini)

AIC(glm.linear.gini)
AIC(glm.pois.gini)
AIC(glm.bin.gini)

coef(glm.linear.gini)
coef(glm.pois.gini)
coef(glm.bin.gini)

par(mfrow=c(2,2))
plot(glm.linear.gini)
plot(glm.pois.gini)
plot(glm.bin.gini)
dev.off()

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.gini)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.gini)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.gini)
plot(sim_res_bin)

png(filename=here("images","models","ed_linear_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Linear ed (gini)"), cex=2, bty="n")
plot(glm.linear.gini)
dev.off()

png(filename=here("images","models","ed_poisson_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson ed (gini)"), cex=2, bty="n")
plot(glm.pois.gini)
dev.off()

png(filename=here("images","models","ed_binomial_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial ed (gini)"), cex=2, bty="n")
plot(glm.bin.gini)
dev.off()

png(filename=here("images","models","ed_linear_gini_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.gini)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","ed_poisson_gini_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.gini)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","ed_binomial_gini_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.gini)
plot(sim_res_bin)
dev.off()

#### WITH COUNTRY AS PROXY FOR GENETIC HOMOGENEITY ####

glm.linear.country <- lm(ed ~ decade + country,
                         data=abs_decades)

glm.pois.country <- glm(ed ~ decade + country,
                        data=abs_decades,
                        family=poisson(link="log"))

glm.bin.country <- glm(cbind(ed,pop) ~ decade + country,
                       data=abs_decades,
                       family=binomial(link="logit"))

logLik(glm.linear.country)
logLik(glm.pois.country)
logLik(glm.bin.country)

AIC(glm.linear.country)
AIC(glm.pois.country)
AIC(glm.bin.country)

coef(glm.linear.country)
coef(glm.pois.country)
coef(glm.bin.country)

par(mfrow=c(2,2))
plot(glm.linear.country)
plot(glm.pois.country)
plot(glm.bin.country)
dev.off()

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country)
plot(sim_res_bin)

png(filename=here("images","models","ed_linear_country.png"))
layout(plots)
plot.new()
legend("center",paste("Linear ed (country)"), cex=2, bty="n")
plot(glm.linear.country)
dev.off()

png(filename=here("images","models","ed_poisson_country.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson ed (country)"), cex=2, bty="n")
plot(glm.pois.country)
dev.off()

png(filename=here("images","models","ed_binomial_country.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial ed (country)"), cex=2, bty="n")
plot(glm.bin.country)
dev.off()

png(filename=here("images","models","ed_linear_country_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","ed_poisson_country_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","ed_binomial_country_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country)
plot(sim_res_bin)
dev.off()

#### WITH COUNTRY AS PROXY FOR GENETIC HOMOGENEITY AND GINI ####

glm.linear.country.gini <- lm(ed ~ decade + country + gini,
                              data=abs_decades)

glm.pois.country.gini <- glm(ed ~ decade + country + gini,
                             data=abs_decades,
                             family=poisson(link="log"))

glm.bin.country.gini <- glm(cbind(ed,pop) ~ decade + country + gini,
                            data=abs_decades,
                            family=binomial(link="logit"))

logLik(glm.linear.country.gini)
logLik(glm.pois.country.gini)
logLik(glm.bin.country.gini)

AIC(glm.linear.country.gini)
AIC(glm.pois.country.gini)
AIC(glm.bin.country.gini)

coef(glm.linear.country.gini)
coef(glm.pois.country.gini)
coef(glm.bin.country.gini)

par(mfrow=c(2,2))
plot(glm.linear.country.gini)
plot(glm.pois.country.gini)
plot(glm.bin.country.gini)
dev.off() 

sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country.gini)
plot(sim_res_lm)
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country.gini)
plot(sim_res_pois)
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country.gini)
plot(sim_res_bin)

png(filename=here("images","models","ed_linear_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Linear ed (country + gini)"), cex=2, bty="n")
plot(glm.linear.country.gini)
dev.off()

png(filename=here("images","models","ed_poisson_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Poisson ed (country + gini)"), cex=2, bty="n")
plot(glm.pois.country.gini)
dev.off()

png(filename=here("images","models","ed_binomial_country_gini.png"))
layout(plots)
plot.new()
legend("center",paste("Binomial ed (country + gini)"), cex=2, bty="n")
plot(glm.bin.country.gini)
dev.off()

png(filename=here("images","models","ed_linear_country_gini_dharma.png"))
sim_res_lm <- simulateResiduals(fittedModel = glm.linear.country.gini)
plot(sim_res_lm)
dev.off()

png(filename=here("images","models","ed_poisson_country_gini_dharma.png"))
sim_res_pois <- simulateResiduals(fittedModel = glm.pois.country.gini)
plot(sim_res_pois)
dev.off()

png(filename=here("images","models","ed_binomial_country_gini_dharma.png"))
sim_res_bin <- simulateResiduals(fittedModel = glm.bin.country.gini)
plot(sim_res_bin)
dev.off()

#











#### OLD - I DID IT WRONG Let's try to model ####

glm.linear <- lm(schizophrenia ~ gini,
                  data=abs_decades)

glm.pois <- glm(schizophrenia ~ gini,
                data=abs_decades,
                family=poisson(link="log"))

glm.bin <- glm(schizophrenia ~ gini,
               data=abs_decades,
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