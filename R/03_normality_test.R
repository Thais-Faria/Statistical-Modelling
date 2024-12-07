############################
# NORMALITY TEST (QQ PLOT) #
############################

# Get absolute population data before splitting into decades

absolute_gini <- read.table(here::here("data","processed","absolute_world_prevalence_with_gini.txt"), sep="\t", header=T)

# Organize data in time series by country

time_series <- split(absolute_gini, absolute_gini$country)

# create data frame to store the data

normality <- data.frame(
  country=unique(absolute_gini$country),
  is.normal=rep(NA, length(unique(absolute_gini$country))),
  obs=rep(NA, length(unique(absolute_gini$country)))
)

# QQ Plots

library(car)

# Albania

par(mfrow=c(3,2))
qqPlot(time_series$Albania$schizophrenia, main="Schizophrenia")
qqPlot(time_series$Albania$depression, main='Depression')
qqPlot(time_series$Albania$anxiety, main="Anxiety")
qqPlot(time_series$Albania$bipolar, main="Bipolar")
qqPlot(time_series$Albania$ed, main="Eating disorders")       

normality[which(normality$country=="Albania"),2] <- "yes"

#Algeria

par(mfrow=c(3,2))
qqPlot(time_series$Algeria$schizophrenia, main="Schizophrenia")
qqPlot(time_series$Algeria$depression, main='Depression')
qqPlot(time_series$Algeria$anxiety, main="Anxiety")
qqPlot(time_series$Algeria$bipolar, main="Bipolar")
qqPlot(time_series$Algeria$ed, main="Eating disorders")

normality[which(normality$country=="Algeria"),2] <- "yes"
normality[which(normality$country=="Algeria"),3] <- "only two observations for each MD"

# Angola

par(mfrow=c(3,2))
qqPlot(time_series$Angola$schizophrenia, main="Schizophrenia")
qqPlot(time_series$Angola$depression, main='Depression')
qqPlot(time_series$Angola$anxiety, main="Anxiety")
qqPlot(time_series$Angola$bipolar, main="Bipolar")
qqPlot(time_series$Angola$ed, main="Eating disorders")

normality[which(normality$country=="Angola"),2] <- "yes"
normality[which(normality$country=="Angola"),3] <- "only three observations for each MD"

# Armenia

par(mfrow=c(3,2))
qqPlot(time_series$Armenia$schizophrenia, main="Schizophrenia")
qqPlot(time_series$Armenia$depression, main='Depression')
qqPlot(time_series$Armenia$anxiety, main="Anxiety")
qqPlot(time_series$Armenia$bipolar, main="Bipolar")
qqPlot(time_series$Armenia$ed, main="Eating disorders")

normality[which(normality$country=="Armenia"),2] <- "yes"

# Australia

par(mfrow=c(3,2))
qqPlot(time_series$Australia$schizophrenia, main="Schizophrenia")
qqPlot(time_series$Australia$depression, main='Depression')
qqPlot(time_series$Australia$anxiety, main="Anxiety")
qqPlot(time_series$Australia$bipolar, main="Bipolar")
qqPlot(time_series$Australia$ed, main="Eating disorders")

normality[which(normality$country=="Australia"),2] <- "yes" 

# Austria

par(mfrow=c(3,2))
qqPlot(time_series$Austria$schizophrenia, main="Schizophrenia")
qqPlot(time_series$Austria$depression, main='Depression')
qqPlot(time_series$Austria$anxiety, main="Anxiety")
qqPlot(time_series$Austria$bipolar, main="Bipolar")
qqPlot(time_series$Austria$ed, main="Eating disorders")

normality[which(normality$country=="Austria"),2] <- "yes" 

# Azerbaijan

par(mfrow=c(3,2))
qqPlot(time_series$Azerbaijan$schizophrenia, main="Schizophrenia")
qqPlot(time_series$Azerbaijan$depression, main='Depression')
qqPlot(time_series$Azerbaijan$anxiety, main="Anxiety")
qqPlot(time_series$Azerbaijan$bipolar, main="Bipolar")
qqPlot(time_series$Azerbaijan$ed, main="Eating disorders")

normality[which(normality$country=="Azerbaijan"),2] <- "yes"

# Bangladesh

par(mfrow=c(3,2))
qqPlot(time_series$Bangladesh$schizophrenia, main="Schizophrenia")
qqPlot(time_series$Bangladesh$depression, main='Depression')
qqPlot(time_series$Bangladesh$anxiety, main="Anxiety")
qqPlot(time_series$Bangladesh$bipolar, main="Bipolar")
qqPlot(time_series$Bangladesh$ed, main="Eating disorders")

normality[which(normality$country=="Bangladesh"),2] <- "yes"

# Belarus

par(mfrow=c(3,2))
qqPlot(time_series$Belarus$schizophrenia, main="Schizophrenia")
qqPlot(time_series$Belarus$depression, main='Depression')
qqPlot(time_series$Belarus$anxiety, main="Anxiety")
qqPlot(time_series$Belarus$bipolar, main="Bipolar")
qqPlot(time_series$Belarus$ed, main="Eating disorders")

normality[which(normality$country=="Belarus"),2] <- "yes"

# Belgium

par(mfrow=c(3,2))
qqPlot(time_series$Belgium$schizophrenia, main="Schizophrenia")
qqPlot(time_series$Belgium$depression, main='Depression')
qqPlot(time_series$Belgium$anxiety, main="Anxiety")
qqPlot(time_series$Belgium$bipolar, main="Bipolar")
qqPlot(time_series$Belgium$ed, main="Eating disorders")

normality[which(normality$country=="Belgium"),2] <- "yes"

# Belize

par(mfrow=c(3,2))
qqPlot(time_series$Belize$schizophrenia, main="Schizophrenia")
qqPlot(time_series$Belize$depression, main='Depression')
qqPlot(time_series$Belize$anxiety, main="Anxiety")
qqPlot(time_series$Belize$bipolar, main="Bipolar")
qqPlot(time_series$Belize$ed, main="Eating disorders")

normality[which(normality$country=="Belize"),2] <- "yes"

