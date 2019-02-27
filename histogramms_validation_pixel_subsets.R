setwd('B:/temp/temp_clemens')

library(rgdal)
library(dplyr)

sub_north01 <- readOGR(dsn = "BA_EnMap/GIS/Subsets/val_poly_sub_north01.shp")
sub_north02 <- readOGR(dsn = "BA_EnMap/GIS/Subsets/val_poly_sub_north02.shp")
sub_north03 <- readOGR(dsn = "BA_EnMap/GIS/Subsets/val_poly_sub_north03.shp")
sub_south01 <- readOGR(dsn = "BA_EnMap/GIS/Subsets/val_poly_sub_south01.shp")
sub_south02 <- readOGR(dsn = "BA_EnMap/GIS/Subsets/val_poly_sub_south02.shp")

df_north01 <- sub_north01@data
df_north02 <- sub_north02@data
df_north03 <- sub_north03@data
df_south01 <- sub_south01@data
df_south02 <- sub_south02@data

####
df_north01$l2_wveg <- df_north01$btr + df_north01$con + df_north01$shr
df_north01$l2_nwveg <- df_north01$gra
df_north01$l2_back <- df_north01$soi + df_north01$imp + df_north01$other
  
hist(df_north01$l2_nwveg, labels = T)
hist(df_north01$l2_wveg, labels = T)

####
df_north02$l2_wveg <- df_north02$btr + df_north02$con + df_north02$shr
df_north02$l2_nwveg <- df_north02$gra
df_north02$l2_back <- df_north02$soi + df_north02$imp + df_north02$other

hist(df_north02$l2_nwveg, labels = T)
hist(df_north02$l2_wveg, labels = T)

####
df_north03$l2_wveg <- df_north03$btr + df_north03$con + df_north03$shr
df_north03$l2_nwveg <- df_north03$gra
df_north03$l2_back <- df_north03$soi + df_north03$imp + df_north03$other

hist(df_north03$l2_nwveg, labels = T)
hist(df_north03$l2_wveg, labels = T)

####
df_south01$l2_wveg <- df_south01$btr + df_south01$con + df_south01$shr
df_south01$l2_nwveg <- df_south01$gra
df_south01$l2_back <- df_south01$soi + df_south01$imp + df_south01$other

hist(df_south01$l2_nwveg, labels = T)
hist(df_south01$l2_wveg, labels = T)

####
df_south02$l2_wveg <- df_south02$btr + df_south02$con + df_south02$shr
df_south02$l2_nwveg <- df_south02$gra
df_south02$l2_back <- df_south02$soi + df_south02$imp + df_south02$other

hist(df_south02$l2_nwveg, labels = T)
hist(df_south02$l2_wveg, labels = T)


