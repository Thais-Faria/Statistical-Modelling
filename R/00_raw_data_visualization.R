library(here)
library(ggplot2)
library(scales)
library(cowplot)

#### WORLD DATA ####

# Get relative population data

relative_gini <- read.table(here("data","processed","relative_world_prevalence_with_gini.txt"), head=T)
relative_gini$year <- as.factor(relative_gini$year) # ggplot needs year as factor to plot

# Make plot with relative population data (%)

ggplot(relative_gini) +
        geom_bar(aes(x=year, y=anxiety, fill="Anxiety"), stat="identity") +
        geom_bar(aes(x=year, y=depression, fill="Depression"), stat="identity") +
        geom_bar(aes(x=year, y=bipolar, fill="Bipolar"), stat="identity") +
        geom_bar(aes(x=year, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
        geom_bar(aes(x=year, y=ed, fill="Eating disorders"), stat="identity") +
        labs(x="Year", y="Prevalence (relative)", fill="Disorders") +
        theme(legend.position="bottom")

# Saving plot

ggsave(path=here("images"), filename="prevalence_by_year_world.pdf", plot=last_plot(), device="pdf", width=12, height=6)

pdf(file=here("images","prevalence_by_year_world.pdf"), height=8.27, width=11.69)
plot(rel)
dev.off()


#### OLD - ONLY BRAZIL ####


# Get absolute population data

abs_gini <- read.table(here("data","processed","absolute_BR_prevalence_with_gini.txt"), head=T)
abs_gini$year <- as.factor(abs_gini$year) # ggplot needs year as factor to plot


# Get relative population data

relative_gini <- read.table(here("data","processed","relative_BR_prevalence_with_gini.txt"), head=T)
relative_gini$year <- as.factor(relative_gini$year) # ggplot needs year as factor to plot

# Make plot with absolute population data

abs <- ggplot(abs_gini) +
        geom_bar(aes(x=year, y=anxiety, fill="Anxiety"), stat="identity") +
        geom_bar(aes(x=year, y=depression, fill="Depression"), stat="identity") +
        geom_bar(aes(x=year, y=bipolar, fill="Bipolar"), stat="identity") +
        geom_bar(aes(x=year, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
        geom_bar(aes(x=year, y=ed, fill="Eating disorders"), stat="identity") +
        labs(x="Year", y="Prevalence (absolute)", fill="Disorders") +
        theme(legend.position="none")

# Make plot with relative population data (%)

rel <- ggplot(relative_gini) +
        geom_bar(aes(x=year, y=anxiety, fill="Anxiety"), stat="identity") +
        geom_bar(aes(x=year, y=depression, fill="Depression"), stat="identity") +
        geom_bar(aes(x=year, y=bipolar, fill="Bipolar"), stat="identity") +
        geom_bar(aes(x=year, y=schizophrenia, fill="Schizophrenia"), stat="identity") +
        geom_bar(aes(x=year, y=ed, fill="Eating disorders"), stat="identity") +
        labs(x="Year", y="Prevalence (relative)", fill="Disorders") +
        theme(legend.position="bottom")

# Saving plot

pdf(file=here("images","prevalence_by_year.pdf"), height=8.27, width=11.69)
plot_grid(abs, rel, nrow=2, ncol=1)
dev.off()


