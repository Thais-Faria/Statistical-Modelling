#########################################
# SEPARATING THE DATA INTO TIME WINDOWS #
#########################################

library(here)

##############################
####### RELATIVE DATA ########
##############################

# Getting the processed data

relative_gini <- read.table(here("data", "processed", "relative_world_prevalence_with_gini.txt"), header=T, sep="\t")

# Creating dataframes to store the decade information

decade1 <- relative_gini[which(relative_gini$year < 2000),]
decade2 <- relative_gini[which(relative_gini$year > 1999 & relative_gini$year < 2010),]
decade3 <- relative_gini[which(relative_gini$year > 2009),]

countries <- unique(decade1$country)

# Creating dataframes to store the mean decade information

decade1_mean <- data.frame(country=rep(NA, length(countries)),
                           schizophrenia=rep(NA, length(countries)),
                           depression=rep(NA, length(countries)),
                           anxiety=rep(NA, length(countries)),
                           bipolar=rep(NA, length(countries)),
                           ed=rep(NA, length(countries)),
                           gini=rep(NA, length(countries))
)


decade2_mean <- data.frame(country=rep(NA, length(countries)),
                           schizophrenia=rep(NA, length(countries)),
                           depression=rep(NA, length(countries)),
                           anxiety=rep(NA, length(countries)),
                           bipolar=rep(NA, length(countries)),
                           ed=rep(NA, length(countries)),
                           gini=rep(NA, length(countries))
)

decade3_mean <- data.frame(country=rep(NA, length(countries)),
                           schizophrenia=rep(NA, length(countries)),
                           depression=rep(NA, length(countries)),
                           anxiety=rep(NA, length(countries)),
                           bipolar=rep(NA, length(countries)),
                           ed=rep(NA, length(countries)),
                           gini=rep(NA, length(countries))
)

# Getting means to store in the dataframes

for(i in 1:length(countries)){
  
  decade1_mean$country[i] <- countries[i]
  decade1_mean$schizophrenia[i] <- mean(decade1$schizophrenia[which(decade1$country==countries[i])])
  decade1_mean$depression[i] <- mean(decade1$depression[which(decade1$country==countries[i])])
  decade1_mean$anxiety[i] <- mean(decade1$anxiety[which(decade1$country==countries[i])])
  decade1_mean$bipolar[i] <- mean(decade1$bipolar[which(decade1$country==countries[i])])
  decade1_mean$ed[i] <- mean(decade1$ed[which(decade1$country==countries[i])])
  decade1_mean$gini[i] <- mean(decade1$gini[which(decade1$country==countries[i])])
  
}

decade1_mean <- na.exclude(decade1_mean)




for(i in 1:length(countries)){
  
  decade2_mean$country[i] <- countries[i]
  decade2_mean$schizophrenia[i] <- mean(decade2$schizophrenia[which(decade2$country==countries[i])])
  decade2_mean$depression[i] <- mean(decade2$depression[which(decade2$country==countries[i])])
  decade2_mean$anxiety[i] <- mean(decade2$anxiety[which(decade2$country==countries[i])])
  decade2_mean$bipolar[i] <- mean(decade2$bipolar[which(decade2$country==countries[i])])
  decade2_mean$ed[i] <- mean(decade2$ed[which(decade2$country==countries[i])])
  decade2_mean$gini[i] <- mean(decade2$gini[which(decade2$country==countries[i])])
  
}

decade2_mean <- na.exclude(decade2_mean)




for(i in 1:length(countries)){
  
  decade3_mean$country[i] <- countries[i]
  decade3_mean$schizophrenia[i] <- mean(decade3$schizophrenia[which(decade3$country==countries[i])])
  decade3_mean$depression[i] <- mean(decade3$depression[which(decade3$country==countries[i])])
  decade3_mean$anxiety[i] <- mean(decade3$anxiety[which(decade3$country==countries[i])])
  decade3_mean$bipolar[i] <- mean(decade3$bipolar[which(decade3$country==countries[i])])
  decade3_mean$ed[i] <- mean(decade3$ed[which(decade3$country==countries[i])])
  decade3_mean$gini[i] <- mean(decade3$gini[which(decade3$country==countries[i])])
  
}

decade3_mean <- na.exclude(decade3_mean)




# Save this information as an R file

save(decade1, decade2, decade3, decade1_mean, decade2_mean, decade3_mean, file=here("data","processed","decade_data.RData"))

# Condense decade mean information in only one table

decade1_mean$decade <- "1990-1999"
decade2_mean$decade <- "2000-2009"
decade3_mean$decade <- "2010-2019"

decades <- rbind(decade1_mean, decade2_mean, decade3_mean)

decades <- decades[order(decades$country),]


# Now sorting by continent

Africa <- c("Algeria","Botswana","Burkina Faso","Burundi","Cameroon","Central African Republic","Cote d'Ivoire","Egypt","Eswatini","Ethiopia","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Laos","Lesotho","Madagascar","Malawi","Mali","Mauritania","Morocco","Mozambique","Namibia","Niger","Nigeria","Senegal","Seychelles","South Africa","Tanzania","Tunisia","Uganda","Zambia")
America <- c("Belize","Bolivia","Brazil","Canada","Chile","Colombia","Costa Rica","Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay","Peru","Saint Lucia","Trinidad and Tobago","United States","Venezuela")
Asia <- c("Azerbaijan","Bangladesh","China","India","Indonesia","Iran","Israel","Jordan","Kazakhstan","Malaysia","Mongolia","Nepal","Pakistan","Papua New Guinea","Philippines","Sri Lanka","Syria","Taiwan","Tajikistan","Thailand","Turkmenistan","Uzbekistan","Vietnam","Yemen")
Europe <- c("Albania","Armenia","Austria","Belarus","Belgium","Bulgaria","Czechia","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Moldova","Netherlands","Norway","Poland","Romania","Russia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Turkey","Ukraine","United Kingdom")
Oceania <- c("Australia")

decades$continent <- rep(NA, length(decades$country))

for(i in 1:length(decades$continent)){
  if(decades$country[i] %in% Africa){decades$continent[i] <- "Africa"}
  if(decades$country[i] %in% America){decades$continent[i] <- "America"}
  if(decades$country[i] %in% Asia){decades$continent[i] <- "Asia"}
  if(decades$country[i] %in% Europe){decades$continent[i] <- "Europe"}
  if(decades$country[i] %in% Oceania){decades$continent[i] <- "Oceania"}
}

decades <- decades[order(decades$continent),]

# Saving this information as a table

write.table(decades, file=here("data", "processed", "decade_means_relative.txt"), sep="\t", row.names=F)









##############################
####### ABSOLUTE DATA ########
##############################

# Getting the processed data

absolute_gini <- read.table(here("data", "processed", "absolute_world_prevalence_with_gini.txt"), header=T, sep="\t")

# Creating dataframes to store the decade information

decade1 <- absolute_gini[which(absolute_gini$year < 2000),]
decade2 <- absolute_gini[which(absolute_gini$year > 1999 & absolute_gini$year < 2010),]
decade3 <- absolute_gini[which(absolute_gini$year > 2009),]

countries <- unique(decade1$country)

# Creating dataframes to store the mean decade information

decade1_mean <- data.frame(country=rep(NA, length(countries)),
                           schizophrenia=rep(NA, length(countries)),
                           depression=rep(NA, length(countries)),
                           anxiety=rep(NA, length(countries)),
                           bipolar=rep(NA, length(countries)),
                           ed=rep(NA, length(countries)),
                           gini=rep(NA, length(countries))
)


decade2_mean <- data.frame(country=rep(NA, length(countries)),
                           schizophrenia=rep(NA, length(countries)),
                           depression=rep(NA, length(countries)),
                           anxiety=rep(NA, length(countries)),
                           bipolar=rep(NA, length(countries)),
                           ed=rep(NA, length(countries)),
                           gini=rep(NA, length(countries))
)


decade3_mean <- data.frame(country=rep(NA, length(countries)),
                           schizophrenia=rep(NA, length(countries)),
                           depression=rep(NA, length(countries)),
                           anxiety=rep(NA, length(countries)),
                           bipolar=rep(NA, length(countries)),
                           ed=rep(NA, length(countries)),
                           gini=rep(NA, length(countries))
)

# Getting means to store in the dataframes

for(i in 1:length(countries)){
  
  decade1_mean$country[i] <- countries[i]
  decade1_mean$schizophrenia[i] <- mean(decade1$schizophrenia[which(decade1$country==countries[i])])
  decade1_mean$depression[i] <- mean(decade1$depression[which(decade1$country==countries[i])])
  decade1_mean$anxiety[i] <- mean(decade1$anxiety[which(decade1$country==countries[i])])
  decade1_mean$bipolar[i] <- mean(decade1$bipolar[which(decade1$country==countries[i])])
  decade1_mean$ed[i] <- mean(decade1$ed[which(decade1$country==countries[i])])
  decade1_mean$gini[i] <- mean(decade1$gini[which(decade1$country==countries[i])])
  
}

decade1_mean <- na.exclude(decade1_mean)




for(i in 1:length(countries)){
  
  decade2_mean$country[i] <- countries[i]
  decade2_mean$schizophrenia[i] <- mean(decade2$schizophrenia[which(decade2$country==countries[i])])
  decade2_mean$depression[i] <- mean(decade2$depression[which(decade2$country==countries[i])])
  decade2_mean$anxiety[i] <- mean(decade2$anxiety[which(decade2$country==countries[i])])
  decade2_mean$bipolar[i] <- mean(decade2$bipolar[which(decade2$country==countries[i])])
  decade2_mean$ed[i] <- mean(decade2$ed[which(decade2$country==countries[i])])
  decade2_mean$gini[i] <- mean(decade2$gini[which(decade2$country==countries[i])])
  
}

decade2_mean <- na.exclude(decade2_mean)




for(i in 1:length(countries)){
  
  decade3_mean$country[i] <- countries[i]
  decade3_mean$schizophrenia[i] <- mean(decade3$schizophrenia[which(decade3$country==countries[i])])
  decade3_mean$depression[i] <- mean(decade3$depression[which(decade3$country==countries[i])])
  decade3_mean$anxiety[i] <- mean(decade3$anxiety[which(decade3$country==countries[i])])
  decade3_mean$bipolar[i] <- mean(decade3$bipolar[which(decade3$country==countries[i])])
  decade3_mean$ed[i] <- mean(decade3$ed[which(decade3$country==countries[i])])
  decade3_mean$gini[i] <- mean(decade3$gini[which(decade3$country==countries[i])])
  
}

decade3_mean <- na.exclude(decade3_mean)

# Condense decade mean information in only one table

decade1_mean$decade <- "1990-1999"
decade2_mean$decade <- "2000-2009"
decade3_mean$decade <- "2010-2019"

decades <- rbind(decade1_mean, decade2_mean, decade3_mean)

decades <- decades[order(decades$country),]


# Now sorting by continent

Africa <- c("Algeria","Botswana","Burkina Faso","Burundi","Cameroon","Central African Republic","Cote d'Ivoire","Egypt","Eswatini","Ethiopia","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Laos","Lesotho","Madagascar","Malawi","Mali","Mauritania","Morocco","Mozambique","Namibia","Niger","Nigeria","Senegal","Seychelles","South Africa","Tanzania","Tunisia","Uganda","Zambia")
America <- c("Belize","Bolivia","Brazil","Canada","Chile","Colombia","Costa Rica","Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana","Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay","Peru","Saint Lucia","Trinidad and Tobago","United States","Venezuela")
Asia <- c("Azerbaijan","Bangladesh","China","India","Indonesia","Iran","Israel","Jordan","Kazakhstan","Malaysia","Mongolia","Nepal","Pakistan","Papua New Guinea","Philippines","Sri Lanka","Syria","Taiwan","Tajikistan","Thailand","Turkmenistan","Uzbekistan","Vietnam","Yemen")
Europe <- c("Albania","Armenia","Austria","Belarus","Belgium","Bulgaria","Czechia","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Moldova","Netherlands","Norway","Poland","Romania","Russia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Turkey","Ukraine","United Kingdom")
Oceania <- c("Australia")

decades$continent <- rep(NA, length(decades$country))

for(i in 1:length(decades$continent)){
  if(decades$country[i] %in% Africa){decades$continent[i] <- "Africa"}
  if(decades$country[i] %in% America){decades$continent[i] <- "America"}
  if(decades$country[i] %in% Asia){decades$continent[i] <- "Asia"}
  if(decades$country[i] %in% Europe){decades$continent[i] <- "Europe"}
  if(decades$country[i] %in% Oceania){decades$continent[i] <- "Oceania"}
}

decades <- decades[order(decades$continent),]

# Saving this information as a table

write.table(decades, file=here("data", "processed", "decade_means_absolute.txt"), sep="\t", row.names=F)
