#install.packages("ggmap")
#install.packages("maptools")
library(readxl)
library(dplyr)
library(rgdal)
library(ggplot2)
library(ggmap)
library(maptools)
library(raster)

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#import df with coordinates of all viewit polygons 2015
df.viewit <- read.csv(file = "B:/Project_California/00_Data/03_GIS/hyspiri_viewit_2015/BayAarea_viewit_2015_coordinates.txt", sep = ",", header = T)
df.viewit$POINT_ID <- as.character(df.viewit$POINT_ID)

#import df with metadata
df.meta <- read_excel(path = "B:/Project_California/00_Data/02_Spectral_Library/05_BA/BA_metadata.xlsx")
df.meta$ID_join <- as.character(substr(df.meta$obj_ID,1, nchar(df.meta$obj_ID)-1))
df.merge <- left_join(df.meta, df.viewit, by = c("ID_join"="POINT_ID"))

df.merge$XCoord <- as.character(df.merge$XCoord)
df.merge$YCoord <- as.character(df.merge$YCoord)
#df.merge$XCoord <- gsub("\\,", ".", df.merge$XCoord)
#df.merge$YCoord <- gsub("\\,", ".", df.merge$YCoord)

df.merge$YCoord <- as.numeric(df.merge$YCoord)

#count the polygons which do not have a correct ID
numberNA <- sum(is.na(df.merge$XCoord))
df.NA <- df.merge[is.na(df.merge$XCoord),]

#create metadata for subset areas
#df.meta.a <- subset(df.merge, XCoord >=524310 & XCoord <= 556050 & YCoord >= 4220100 & YCoord <= 4272780)
#df.meta.b <- subset(df.merge, XCoord >=609719 & XCoord <= 642453 & YCoord >= 4106054 & YCoord <= 4160010)
#hist.a <- as.data.frame(table(df.meta.a$level3))
#hist(hist.a)
#hist.b <- as.data.frame(table(df.meta.b$level3))
#hist(hist.b)

#load bay area shapefile
files_flightlines <- list.files(pattern="*.shp", path = "B:/Project_California/00_Data/03_GIS/flight_lines/")
flightlines <-  readOGR(dsn = "B:/Project_California/00_Data/03_GIS/flight_lines", layer = "BA_boxes", encoding = "ESRI_Shapefile")

#assign new CRS to an variable
newCRS <-  CRS("+proj=utm +zone=10 +datum=WGS84")

#use this variable to transform the CRS of the flightlines 
flightlines <- spTransform(flightlines, newCRS)

flightlines@data$id = rownames(flightlines@data)
flightlines_polygons = fortify(flightlines, region="id")
flightlines_df <- left_join(flightlines_polygons, flightlines@data, by="id")
flightlines_summer <- subset(flightlines_df, Season == 'Summer' & Year == '2013')

map <- ggplot(flightlines_summer)+
  aes(long,lat,group=group, fill=Box)+
  geom_polygon()+
  coord_equal()+
  geom_path(color="white")+
  geom_point(data = points.df, aes(x=XCoord, y=YCoord))

plot(map)


na.x.coord <- complete.cases(df.merge$XCOORD)
coords <- na.omit(df.merge[,18:19])
df.merge.na <- df.merge[na.x.coord,]
training_points <- SpatialPointsDataFrame(coords, df.merge.na)

crs(training_points) <- "+proj=utm +zone=10 +datum=WGS84"

points.df <- data.frame(training_points)
ggplot()+
  geom_point(data = points.df, aes(x=XCoord, y=YCoord))

map2 <- plot(flightlines)
plot(map2)
