library(rgdal)
library(dplyr)

centr <- readOGR("B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD/ABCD_validation_polygons_centroids.shp")
centrdf <- centr@data
centrdf$PolyID2 <- paste(centrdf$SubsetArea, centrdf$PolyID_1, sep="_")

val <- read.csv("B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD_polygon_estimates_ordered.csv")

final <- left_join(val, centrdf[,c(8,10:11)], by=c("uniquePolyID"="PolyID2"))

final$angleclass <- "nadir"
final$angleclass[final$viewangle > 5 & final$viewangle < 20] <- "offPOS"
final$angleclass[final$viewangle < -5 & final$viewangle > -20] <- "offNEG"
final$angleclass[final$viewangle > 20 ] <- "exPOS"
final$angleclass[final$viewangle < -20 ] <- "exNEG"

final$angle_5 <- round(final$viewangle/5)*5

write.csv(x = final, file = "B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD_polygon_estimates_ordered_viewangle.csv", row.names = F)

poly <- readOGR("B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD/ABCD_validation_polygons02.shp")
polydf <- centr@data
polydf$PolyID2 <- paste(polydf$SubsetArea, polydf$PolyID_1, sep="_")

val <- read.csv(file = "B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD_polygon_estimates_ordered_viewangle.csv")
final <- left_join(val, polydf[,c(8,10)], by=c("uniquePolyID"="PolyID2"))
write.csv(x = final, file = "B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD_polygon_estimates_ordered_viewangle.csv", row.names = F)

