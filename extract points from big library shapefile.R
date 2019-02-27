# 1. Packages ####
library(maptools)
library(RStoolbox)
library(stringr)
library(readxl) #to read excel in the right way
library(xlsx) #to write excel

# 2. Data ####
setwd("B:/temp/temp_clemens/BA_EnMap/GIS/training_data/")

#shapefile with position of all AVIRIS training ROI
spec_all<-rgdal::readOGR(dsn="B:/temp/temp_clemens/BA_EnMap/GIS/training_data" , layer="00_BA_all_points" , encoding="ESRIShapefile")
#library03, spectra from AVIRIS image
spec_areab <- readSLI(path = 'B:/temp/temp_clemens/BA_EnMap/speclib/EnMAP/lib03_totalsub/EnMAP_speclib_area_b_totalsub')
#library04, spectra from EnMAP image
spec_enmap <- readSLI(path = 'B:/temp/temp_clemens/BA_EnMap/speclib/EnMAP/lib04_EnMAP_totalsub/EnMAP_totalsub')
#metadate of full library, cj-version 16.05.2018
meta <- read_excel(path = "B:/temp/temp_clemens/BA_EnMap/speclib/00_BA_metadata_cj.xlsx", sheet = 1)

# 3. Processing ####

#extracts IDs from library 04
spec_enmap <- as.data.frame(t(spec_enmap))
spec_enmap <- cbind(rownames(spec_enmap), data.frame(spec_enmap, row.names=NULL))
colnames(spec_enmap) <- c("ID", paste("band", 1:208, sep=""))
spec_enmap <- spec_enmap[-1,]
spec_enmap$ID <- str_match(spec_enmap[,1], "Mean:(.*?)\\[")
spec_enmap$ID <- as.character(spec_enmap$ID[,2])


#extracts IDs from library 03
spec_areb <- as.data.frame(t(spec_areab)) 
spec_areab <- cbind(rownames(spec_areb), data.frame(spec_areb, row.names=NULL))  
colnames(spec_areab) <- c("ID", paste("band",1:201,sep="")) #renames columns with meaningful names
spec_areab <- spec_areab[-1,] # deletes first colums
#ID cleaning
spec_areab$ID2 <- str_match(spec_areab[,1], "_(.?)*") 
spec_areab$ID2 <- as.character(spec_areab$ID2[,1])
spec_areab$ID2 <- substr(spec_areab$ID2, 2,length(spec_areab$ID2))
#some more ID cleaning of ID with different structure
for (i in 216:224){
  spec_areab[i,203] <- substr(spec_areab[i,203], 10, 100)
}
spec_areab[196,203] <- "20272c_57"
list <- as.data.frame(spec_areab$ID2)

#extract IDs from Shapefile combine with coordinates and
#then extract all coordinates of the spectra that are in library03
df.data <- spec_all@data
df.coords <- as.data.frame(spec_all@coords)
df.data_coords <- cbind(df.data,df.coords)
df <- left_join(list, df.data_coords, by = c("spec_areab$ID2"="FULL_ID"))
#output
write.csv(df, file="B:/temp/temp_clemens/BA_EnMap/GIS/training_data/01_BA_ClusterSub.csv")

#join meta data with spectra of library04 to combine IDs with class names
df2 <- left_join(spec_enmap, meta, by = c("ID"="Full_ID"))
df3 <- df2[,c(1,217:221)]
#output
write.xlsx(df3,file="B:/temp/temp_clemens/BA_EnMap/speclib/EnMAP/lib04_EnMAP_totalsub/EnMAP-totalsub-classnames.xlsx", sheetName = "lib", row.names = FALSE)

