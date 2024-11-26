##################################
# TABLE


prevalence <- read.csv(here::here("data","raw","1-mental-illnesses-prevalence.csv"), header=T, sep=",")

brasil <- split(prevalence, as.factor(prevalence$Entity))$Brazil

gini <- read.csv(here::here("data","raw","gini.csv"), header=T, sep=",")
gini$SIPOVGINIBRA[which(gini$SIPOVGINIBRA==".")] <- NA


brasil_with_gini <- data.frame(
  year=brasil[,3],
  gini=gini$SIPOVGINIBRA[c(10:39)],
  schizophrenia=brasil[,4],
  depression=brasil[,5],
  anxiety=brasil[,6],
  bipolar=brasil[,7],
  ed=brasil[,8]
)

# Saving only this first

write.table(brasil_with_gini, file=(here::here("data","processed","relative_BR_prevalence_with_gini.txt")), sep="\t", row.names=F)


# Now adding the population data

population_1940_2010 <- read.csv(here::here("data","raw","population_brasil.csv"), header=T, sep=";")
population_1940_2010$total <- population_1940_2010[,2]+population_1940_2010[,3]
population_1940_2010 <- population_1940_2010[,c(1,8)]

population_2001_2019 <- readxl::read_excel(here::here("data","raw","serie_2001_2019_TCU.xls"), range="A5:T39")
population_2001_2019 <- population_2001_2019[-1,]
population_2001_2019 <- as.data.frame(t(population_2001_2019[1,]))
population_2001_2019$year <- rownames(population_2001_2019)
population_2001_2019 <- population_2001_2019[-1,]
population_2001_2019 <- population_2001_2019[,c(2,1)]
colnames(population_2001_2019) <- c("Anos","total")

population <- rbind(population_1940_2010,population_2001_2019)
population <- population[-8,]

# Saving only population data

write.table(population, file=(here::here("data","processed","BR_population_historical_series.txt")), sep="\t", row.names=F)


# Now generating absolute prevalence data

population <- population[-c(1:4),]
population[1,1] <- "1990"
population[1,2] <- population[2,2]
population[c(23:30),1] <- seq(1992, 1999, by=1)
population[c(23:30),2] <- rep(population[2,2], times=8)
population$total <- as.numeric(population$total)

population <- dplyr::arrange(population, Anos)

multiply <- function(x, y){
  result <- x*y
  result
}

brasil_with_gini_absolute <- brasil_with_gini

brasil_with_gini_absolute[,c(3:7)] <- multiply(brasil_with_gini[,c(3:7)],population$total)

# Saving this table

write.table(brasil_with_gini_absolute, file=(here::here("data","processed","absolute_BR_prevalence_with_gini.txt")), sep="\t", row.names=F)
