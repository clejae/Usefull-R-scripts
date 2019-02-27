library(readxl)
library(dplyr)
library(rgdal)

meta <- read_excel("B:/Project_California/00_Data/02_Spectral_Library/05_BA/000_BA_metadata.xlsx", sheet = 1)

lib <- read.csv("B:/temp/temp_clemens/speclibs/EnMAP/lib05_EnMAP_preFinal/library_all_final_cleaned_v1_mean.csv")
meta_sub <- meta[,c(3,15)]

t1 <- table(lib$Level5)
barplot(t1)

lib_comb <- left_join(lib, meta_sub, by = "Full_ID")
lib_comb <- subset(lib_comb, ViewBin == "Nadir")

t2 <- table(lib_comb$Level5)
barplot(t2)

lib_comb = lib_comb[,-1]

levels(lib_comb$Level2)

lib_comb$Level2[lib_comb$Level2 == "Imperv"] = "Back"

write.csv(lib_comb, "B:/temp/temp_clemens/speclibs/EnMAP/lib07_EnMAP_nadir/library_nadir_classwisecroco.csv", row.names = F)

shape <- readOGR("B:/Project_California/00_Data/02_Spectral_Library/05_BA/EnMAP_library/EnMAP_Shp/shape_all_final_cleaned_v1.shp")

shape_sub <- shape[shape@data$ViewBin == "Nadir",]
setwd("B:/temp/temp_clemens/speclibs/EnMAP/lib07_EnMAP_nadir")
writeOGR(shape_sub, ".", "shape_nadir", driver = "ESRI Shapefile")

ids <- as.data.frame(shape_sub@data$Full_ID)
write.csv(ids, "B:/temp/temp_clemens/speclibs/EnMAP/lib07_EnMAP_nadir/nadir_spectraIDs.csv", row.names = F)
