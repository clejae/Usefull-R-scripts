library(raster)

wd <-"B:/temp/temp_clemens/BA_EnMap/Outputs/uplgr_revised_CorrSub_noStScal/"

setwd(wd)

files_raster <-  list.files(pattern="*.img$", path = wd, full.names = TRUE)

raster_list <- lapply(files_raster, raster)
raster_stack <- stack(raster_list)
raster_sum <- sum(raster_stack)
raster_combined <- raster_sum/length(raster_list)
m <- c(min(raster_combined@data@values)-0.1,0,0, 1,max(raster_combined@data@values),1)
rclmat <- matrix(m, ncol=3, byrow = T)
raster_combined_recl <- reclassify(raster_combined, rclmat)

writeRaster(raster_combined, filename = paste(wd, "uplgr_revised_CorrSub_noStScal_combined.tif", sep = ""), format="GTiff")
writeRaster(raster_combined_recl, filename = paste(wd, "uplgr_revised_CorrSub_noStScal_combined_recl.tif", sep = ""), format="GTiff", overwrite=T)
