
library(reshape)
library(RStoolbox)
library(dplyr)
library(stringr)
library(readxl)


readspectra <- function(x){
  x <- as.data.frame(t(x))
  x <- cbind(rownames(x), data.frame(x, row.names=NULL))
  colnames(x) <- c("ID", paste("band",1:224, sep = ""))
  x <- x[-1,]
  x$ID <- str_match(x[,1], "Mean:(.*?)\\[")
  x$ID <- as.character(x$ID[,2])
  return(x)
}


speclib <- readSLI("B:/temp/temp_clemens/BA_EnMap/speclib/speclib_area_uplgr")
speclib.df <- readspectra(speclib)


meta.df <- read_excel("B:/Project_California/00_Data/02_Spectral_Library/05_BA/BA_metadata.xlsx", sheet = 1)
meta.df$ID <- c(meta.df$obj_ID,meta.df$line)

meta.df$ID_line <- paste(meta.df$obj_ID, meta.df$line, sep="_")

speclib.merge <- left_join(speclib.df, meta.df, by = c("ID"="ID_line"))

#count the polygons which do not have a correct ID
numberNA <- sum(is.na(speclib.merge$level3))
df.NA <- speclib.merge[is.na(speclib.merge$level3),]

write.csv(speclib.merge, file = "B:/temp/temp_clemens/BA_EnMap/speclib/list.csv")
