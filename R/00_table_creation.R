################################################
# CREATE DISORDERS X POPULATION  AND GINI DATA #
################################################

library(here)

##### PER COUNTRY #####

# Get raw data

# disorder prevalence dataset
prevalence <- read.csv(here("data","raw","1-mental-illnesses-prevalence.csv"), header=T, sep=",")

# remove continents from table

countries <- unique(prevalence$Entity)
countries <- countries[-c(2,5,12,66,67,85,109,112,205,211)]

prevalence_by_country <- prevalence[which(prevalence$Entity %in% countries==T),]

# get Gini index data by country

gini <- read.csv(here("data","raw","economic-inequality-gini-index.csv"), header=T, sep=",")

# create dataset of disorder prevalence with Gini
prevalence_with_gini <- data.frame(
  country=prevalence_by_country$Entity,
  year=prevalence_by_country[,3],
  schizophrenia=prevalence_by_country[,4],
  depression=prevalence_by_country[,5],
  anxiety=prevalence_by_country[,6],
  bipolar=prevalence_by_country[,7],
  ed=prevalence_by_country[,8]
)

prevalence_with_gini$gini <- rep(NA, length(prevalence_with_gini$country))

countries_gini <- unique(gini$Entity)

prevalence_with_gini <- prevalence_with_gini[which(prevalence_with_gini$country %in% countries_gini),]

for(i in 1:length(prevalence_with_gini)){
  
  if(prevalence_with_gini[i]==gini$Entity[i]){prevalence_with_gini$gini[i] <- gini$Gini.coefficient}
  
}











# Get raw data

# disorder prevalence dataset
prevalence <- read.csv(here::here("data","raw","1-mental-illnesses-prevalence.csv"), header=T, sep=",")

# only Brasil data
brasil <- split(prevalence, as.factor(prevalence$Entity))$Brazil

# get Gini index data for Brasil
gini <- read.csv(here::here("data","raw","gini.csv"), header=T, sep=",")
gini$SIPOVGINIBRA[which(gini$SIPOVGINIBRA==".")] <- NA

# create dataset of disorder prevalence with Gini
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

# Create population dataset from 1940 to 2019
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
