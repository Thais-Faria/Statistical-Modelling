################################################
# CREATE DISORDERS X POPULATION  AND GINI DATA #
################################################

library(here)

##### PER COUNTRY #####

## RELATIVE

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
  ed=prevalence_by_country[,8],
  gini=rep(NA,length(prevalence_by_country$Entity))
)

countries_gini <- unique(gini$Entity)

prevalence_with_gini <- prevalence_with_gini[which(prevalence_with_gini$country %in% countries_gini),]

countries_prevalence <- unique(prevalence_with_gini$country)


# Create data frame to store the information

prevalence_gini_intersection <- data.frame(
  country=NA,
  year=NA,
  schizophrenia=NA,
  depression=NA,
  anxiety=NA,
  bipolar=NA,
  ed=NA,
  gini=NA
)

# Let's retrieve the gini data

for(i in 1:length(countries_prevalence)){
  
  b <- gini[which(gini$Entity==countries_prevalence[i]),]
  
  a <- prevalence_with_gini[which(prevalence_with_gini$country==countries_prevalence[i]),]
  
  a <- a[which((a$year) %in% gini$Year[which(gini$Entity==countries_prevalence[i])]),]
  
  b <- b[b$Year %in% a$year,]
  
  a$gini <- b[which(b$Entity==countries_prevalence[i]),4]
  
  prevalence_gini_intersection <- rbind(prevalence_gini_intersection, a)

}

prevalence_gini_intersection <- prevalence_gini_intersection[-1,] # done!

# Saving this table

write.table(prevalence_gini_intersection, file=here("data", "processed", "relative_world_prevalence_with_gini.txt"), sep="\t", row.names=F)



## ABSOLUTE

library(here)

# Get processed relative data

relative_gini <- read.table(here("data","processed","relative_world_prevalence_with_gini.txt"), sep="\t", header=T)

# Get raw population data

worldbank_1 <- read.csv(here("data","raw","worldbank","WDICSV.csv"), sep=",", header=T)

pop <- worldbank_1[which(worldbank_1$Indicator.Name=="Population, total"),] # only population data

pop <- pop[which(pop$Country.Name %in% relative_gini$country),] # only population data for the countries we have in gini

# Create data frame to store the information

absolute_gini <- relative_gini
absolute_gini$pop <- rep(NA, length(absolute_gini$country))

# Retrieve population data

for(j in 1:length(pop$Country.Name)){
  a <- relative_gini[(grep(pop$Country.Name[j], relative_gini$country)),]
  b <- pop[j,]
  year <- a$year
  c <- colnames(b)
  c <- sub("X", "", c)
  colnames(b) <- c
  b <- b[,c(1,which(c %in% year))]

  for(i in 2:length(colnames(b))){
    a$schizophrenia[which(a$year == colnames(b)[i])] <- as.numeric(a$schizophrenia[which(a$year == colnames(b)[i])]*b[i])
    a$depression[which(a$year == colnames(b)[i])] <- as.numeric(a$depression[which(a$year == colnames(b)[i])]*b[i])
    a$anxiety[which(a$year == colnames(b)[i])] <- as.numeric(a$anxiety[which(a$year == colnames(b)[i])]*b[i])
    a$bipolar[which(a$year == colnames(b)[i])] <- as.numeric(a$bipolar[which(a$year == colnames(b)[i])]*b[i])
    a$ed[which(a$year == colnames(b)[i])] <- as.numeric(a$ed[which(a$year == colnames(b)[i])]*b[i])
    a$pop[which(a$year == colnames(b)[i])] <- b[i]
  }
  
  absolute_gini[(grep(pop$Country.Name[j], relative_gini$country)),] <- a

}

# Add population data to table

# Remove points without population data

absolute_gini <- absolute_gini[-which(absolute_gini$schizophrenia < 1),]

absolute_gini$pop <- as.numeric(absolute_gini$pop)

# Save this data

write.table(absolute_gini, file=here("data", "processed", "absolute_world_prevalence_with_gini.txt"), sep="\t", row.names=F)












##### OLD ONLY BRAZIL ####


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
