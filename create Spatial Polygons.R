library(sp)
library(rgdal)

bLP=c(566903, 4103299)

createRectPolyShape <- function(bottomLeftPoint, xLength, yLength, ID, cs){
  x_coord <- c(bottomLeftPoint[1], bottomLeftPoint[1]+xLength, bottomLeftPoint[1]+xLength, bottomLeftPoint[1], bottomLeftPoint[1])
  y_coord <- c(bottomLeftPoint[2], bottomLeftPoint[2], bottomLeftPoint[2]+yLength, bottomLeftPoint[2]+yLength,bottomLeftPoint[2])
  coord_m <- cbind(x_coord, y_coord)
  
  p <- Polygon(coord_m)
  ps <- Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  proj4string(sps) = cs
  data = data.frame(ID=ID)
  spdf = SpatialPolygonsDataFrame(sps, data)
  return(spdf)
}

t <- createRectPolyShape(c(566903, 4103299), 40000, 8000, 1, CRS("+init=epsg:32610"))
t2 <- createRectPolyShape(c(606099, 4149039), 40000, 8000, 2, CRS("+init=epsg:32610"))
t3 <- createRectPolyShape(c(536432, 4237625), 40000, 8000, 3, CRS("+init=epsg:32610"))

writeOGR(obj = t, dsn = "B:/temp/temp_clemens/01_letter/site1_southwest.shp", driver="ESRI Shapefile", layer="site1")
writeOGR(obj = t2, dsn = "B:/temp/temp_clemens/01_letter/site2_southmiddle.shp", driver="ESRI Shapefile", layer="site2")
writeOGR(obj = t3, dsn = "B:/temp/temp_clemens/01_letter/site3_northmiddle.shp", driver="ESRI Shapefile", layer="site3")
