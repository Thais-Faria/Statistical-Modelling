#################################################
# VISUALIZING THE DATA SORTED INTO TIME WINDOWS #
#################################################

library(here)
library(ggplot2)
library(cowplot)

# Get mean decade data

decades <- read.table(here("data","processed","decade_means.txt"), sep="\t", header=T)

#### BY DECADE ####

decades$country <- as.factor(decades$country)

decada <- split(decades, decades$decade)

colors <- c("#80c4ba","#e0ac89","#e8b3d4","#acc47c","#cccccc")

### Plotting

# Anxiety
a1 <- ggplot(as.data.frame(decada$`1990-1999`)) +
  geom_bar(aes(x=country, y=anxiety), fill=colors[1], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("1990-1999 - Anxiety")

a2 <- ggplot(as.data.frame(decada$`2000-2009`)) +
  geom_bar(aes(x=country, y=anxiety), fill=colors[1], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("2000-2009 - Anxiety")

a3 <- ggplot(as.data.frame(decada$`2010-2019`)) +
  geom_bar(aes(x=country, y=anxiety), fill=colors[1], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("2010-2019 - Anxiety")

# Depression
a4 <- ggplot(as.data.frame(decada$`1990-1999`)) +
  geom_bar(aes(x=country, y=depression), fill=colors[2], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("1990-1999 - Depression")

a5 <- ggplot(as.data.frame(decada$`2000-2009`)) +
  geom_bar(aes(x=country, y=depression), fill=colors[2], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("2000-2009 - Depression")

a6 <- ggplot(as.data.frame(decada$`2010-2019`)) +
  geom_bar(aes(x=country, y=depression), fill=colors[2], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("2010-2019 - Depression")

# Bipolar
a7 <- ggplot(as.data.frame(decada$`1990-1999`)) +
  geom_bar(aes(x=country, y=bipolar), fill=colors[3], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("1990-1999 - Bipolar")

a8 <- ggplot(as.data.frame(decada$`2000-2009`)) +
  geom_bar(aes(x=country, y=bipolar), fill=colors[3], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("2000-2009 - Bipolar")

a9 <- ggplot(as.data.frame(decada$`2010-2019`)) +
  geom_bar(aes(x=country, y=bipolar), fill=colors[3], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("2010-2019 - Bipolar")

# Schizo
a10 <- ggplot(as.data.frame(decada$`1990-1999`)) +
  geom_bar(aes(x=country, y=schizophrenia), fill=colors[4], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("1990-1999 - Schizophrenia")

a11 <- ggplot(as.data.frame(decada$`2000-2009`)) +
  geom_bar(aes(x=country, y=schizophrenia), fill=colors[4], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("2000-2009 - Schizophrenia")

a12 <- ggplot(as.data.frame(decada$`2010-2019`)) +
  geom_bar(aes(x=country, y=schizophrenia), fill=colors[4], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("2010-2019 - Schizophrenia")

# ED
a13 <- ggplot(as.data.frame(decada$`1990-1999`)) +
  geom_bar(aes(x=country, y=ed), fill=colors[5], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("1990-1999 - Eating disorders")

a14 <- ggplot(as.data.frame(decada$`2000-2009`)) +
  geom_bar(aes(x=country, y=ed), fill=colors[5], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("2000-2009 - Eating disorders")

a15 <- ggplot(as.data.frame(decada$`2010-2019`)) +
  geom_bar(aes(x=country, y=ed), fill=colors[5], stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("2010-2019 - Eating disorders")

pdf(file=here("images","prevalence_by_decade.pdf"), height=15, width=30)
plot_grid(a1,a4,a7,a10,a13,a2,a5,a8,a11,a14,a3,a6,a9,a12,a15, nrow=3, ncol=5)
dev.off()

#### BY COUNTRY - NOT FINISHED ####

# Separate by country

decades$decade <- as.factor(decades$decade)

countries <- split(decades, decades$country)

### Plotting ###

# Algeria

algeria <- ggplot(as.data.frame(countries$Algeria)) +
              geom_bar(aes(x=decade, y=anxiety, fill="Anxiety"), stat="identity") +
              geom_bar(aes(x=decade, y=depression, fill="Depression"), stat="identity") +
              geom_bar(aes(x=decade, y=bipolar, fill="Bipolar"), stat="identity") +
              geom_bar(aes(x=decade, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
              geom_bar(aes(x=decade, y=ed, fill="Eating disorders"), stat="identity") +
              labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
              #theme(legend.position="bottom") +
              ggtitle("Algeria")


# Botswana

botswana <- ggplot(as.data.frame(countries$Botswana)) +
  geom_bar(aes(x=decade, y=depression, fill="Depression"), stat="identity") +
  geom_bar(aes(x=decade, y=anxiety, fill="Anxiety"), stat="identity") +
  geom_bar(aes(x=decade, y=bipolar, fill="Bipolar"), stat="identity") +
  geom_bar(aes(x=decade, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
  geom_bar(aes(x=decade, y=ed, fill="Eating disorders"), stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  #theme(legend.position="bottom") +
  ggtitle("Botswana")


# Burkina Faso

burkina <- ggplot(as.data.frame(countries$'Burkina Faso')) +
  geom_bar(aes(x=decade, y=depression, fill="Depression"), stat="identity") +
  geom_bar(aes(x=decade, y=anxiety, fill="Anxiety"), stat="identity") +
  geom_bar(aes(x=decade, y=bipolar, fill="Bipolar"), stat="identity") +
  geom_bar(aes(x=decade, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
  geom_bar(aes(x=decade, y=ed, fill="Eating disorders"), stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  #theme(legend.position="bottom") +
  ggtitle("Burkina Faso")

# Burundi

burundi <- ggplot(as.data.frame(countries$Burundi)) +
                    geom_bar(aes(x=decade, y=depression, fill="Depression"), stat="identity") +
                    geom_bar(aes(x=decade, y=anxiety, fill="Anxiety"), stat="identity") +
                    geom_bar(aes(x=decade, y=bipolar, fill="Bipolar"), stat="identity") +
                    geom_bar(aes(x=decade, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
                    geom_bar(aes(x=decade, y=ed, fill="Eating disorders"), stat="identity") +
                    labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
                    #theme(legend.position="bottom") +
                    ggtitle("Burundi")

# Cameroon

cameroon <- ggplot(as.data.frame(countries$Cameroon)) +
  geom_bar(aes(x=decade, y=depression, fill="Depression"), stat="identity") +
  geom_bar(aes(x=decade, y=anxiety, fill="Anxiety"), stat="identity") +
  geom_bar(aes(x=decade, y=bipolar, fill="Bipolar"), stat="identity") +
  geom_bar(aes(x=decade, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
  geom_bar(aes(x=decade, y=ed, fill="Eating disorders"), stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  #theme(legend.position="bottom") +
  ggtitle("Cameroon")


# Central African Republic

car <- ggplot(as.data.frame(countries$'Central African Republic')) +
  geom_bar(aes(x=decade, y=depression, fill="Depression"), stat="identity") +
  geom_bar(aes(x=decade, y=anxiety, fill="Anxiety"), stat="identity") +
  geom_bar(aes(x=decade, y=bipolar, fill="Bipolar"), stat="identity") +
  geom_bar(aes(x=decade, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
  geom_bar(aes(x=decade, y=ed, fill="Eating disorders"), stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  #theme(legend.position="bottom") +
  #guides(fill=guide_legend(nrow=2,byrow=TRUE))
  ggtitle("Central African Republic")

# Cote d'Ivoire

cote <- ggplot(as.data.frame(countries$"Cote d'Ivoire")) +
  geom_bar(aes(x=decade, y=depression, fill="Depression"), stat="identity") +
  geom_bar(aes(x=decade, y=anxiety, fill="Anxiety"), stat="identity") +
  geom_bar(aes(x=decade, y=bipolar, fill="Bipolar"), stat="identity") +
  geom_bar(aes(x=decade, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
  geom_bar(aes(x=decade, y=ed, fill="Eating disorders"), stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  #theme(legend.position="bottom") +
  ggtitle("Cote d'Ivoire")

# Egypt

egypt <- ggplot(as.data.frame(countries$'Egypt')) +
  geom_bar(aes(x=decade, y=anxiety, fill="Anxiety"), stat="identity") +
  geom_bar(aes(x=decade, y=depression, fill="Depression"), stat="identity") +
  geom_bar(aes(x=decade, y=bipolar, fill="Bipolar"), stat="identity") +
  geom_bar(aes(x=decade, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
  geom_bar(aes(x=decade, y=ed, fill="Eating disorders"), stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  #theme(legend.position="bottom") +
  ggtitle("Egypt")

# Eswatini

eswatini <- ggplot(as.data.frame(countries$'Eswatini')) +
  geom_bar(aes(x=decade, y=depression, fill="Depression"), stat="identity") +
  geom_bar(aes(x=decade, y=anxiety, fill="Anxiety"), stat="identity") +
  geom_bar(aes(x=decade, y=bipolar, fill="Bipolar"), stat="identity") +
  geom_bar(aes(x=decade, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
  geom_bar(aes(x=decade, y=ed, fill="Eating disorders"), stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  #theme(legend.position="bottom") +
  ggtitle("Eswatini")

pdf(file=here("images","prevalence_decades_by_country_africa.pdf"), height=12, width=20)
plot_grid(algeria, botswana, burkina, burundi, cameroon, car, cote, egypt, eswatini, nrow=3, ncol=3)
dev.off()

# Zambia

ggplot(as.data.frame(countries$Zambia)) +
  geom_bar(aes(x=decade, y=depression, fill="Depression"), stat="identity") +
  geom_bar(aes(x=decade, y=anxiety, fill="Anxiety"), stat="identity") +
  geom_bar(aes(x=decade, y=bipolar, fill="Bipolar"), stat="identity") +
  geom_bar(aes(x=decade, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
  geom_bar(aes(x=decade, y=ed, fill="Eating disorders"), stat="identity") +
  labs(x="Country", y="Prevalence (relative)", fill="Disorders") +
  theme(legend.position="bottom") +
  ggtitle("Zambia")
