#rasterOptions(tmpdir = "pfad")

#### Packages #### 

library(xlsx)
library(readxl)
library(reshape)
library(RStoolbox)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
#library(made4)


#### Global variable ####

setwd("B:/temp/temp_clemens/BA_EnMap/speclib/")

#### Data ####

metadata_full <- read_excel(path = "B:/temp/temp_clemens/BA_EnMap/speclib/00_BA_metadata.xlsx", sheet = 2, range="B1:X2556")
specLib <- readSLI("AVIRIS/BayArea/02_BAspeclib_pure.sli")

#### Processing ####

#transform .sli into readable dataframe
specLib_transformed <- as.data.frame(t(specLib))
specLib_transformed <- cbind(rownames(specLib_transformed), data.frame(specLib_transformed, row.names=NULL))
colnames(specLib_transformed) = c("ID", paste("wl",specLib_transformed[1,2:201], sep = ""))
specLib_transformed <- specLib_transformed[-1,]

#extract columns of metadata that are needed
metaSub <- metadata_full[,c(1,3,9,13,14)]
metaSub$ViewAngleRounded <- round(metaSub$ViewAngle/5)*5

#combine meta and spectral library
combinedDF <- left_join(metaSub, specLib_transformed, by = c("Full_ID" = "ID"))
combinedDF <- combinedDF[,-94] #remove column 94(wavelength 944), because it's twice present

Level4_list <- unique(combinedDF$Level4)
Level4DF <- list()

for (i in 1:length(Level4_list)){
  Level4DF[[i]] <- subset(combinedDF, Level4 == Level4_list[i])
}

for (i in 1:length(Level4DF)){
  setDT(Level4DF[[i]])
  Level4DF[[i]] <- melt(Level4DF[[i]], id=c("Full_ID","Line", "Level4", "ViewAngle", "ViewBin", "ViewAngleRounded"))
  Level4DF[[i]]$variable_int <- as.numeric(substring(Level4DF[[i]]$variable,3))
}

#### Plotting ####
#length(Level4DF)

for (i in 1:length(Level4DF)){
  ggplot(Level4DF[[i]], aes(x = variable_int, y = value, colour = as.factor(Line)))+
    geom_line(aes(group=Full_ID))+
    facet_wrap(~ ViewBin)
  name <- paste(Level4_list[i],"png", sep=".")
  ggsave(paste("ViewBin",name, sep="_"), scale=2)
}

for (i in 1:length(Level4DF)){
  ggplot(Level4DF[[i]], aes(x = variable_int, y = value, colour = ViewAngleRounded))+
    geom_line(aes(group=Full_ID))+
    scale_colour_gradient2(low = "blue", mid = "red", high = "green")+
    facet_wrap(~ Line)
  name <- paste(Level4_list[i],"png", sep=".")
  ggsave(paste("Plots/FlightLine",name, sep="_"), scale=2)
}

ggplot(Level4DF[[3]], aes(x = variable_int, y = value, colour = ViewAngleRounded))+
  geom_line(aes(group=Full_ID))+
  scale_colour_gradient2(low = "blue", mid = "red", high = "darkgreen")+
  facet_wrap(~ Line)


