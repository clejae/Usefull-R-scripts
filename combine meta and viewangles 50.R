library(readxl)
library(reshape)
library(RStoolbox)
library(dplyr)
library(stringr)
library(ggplot2)
library(xlsx) #to write excel


#this function turns the dataframe around,
#extract meangful IDs
#and provides better column names
readviewangle <- function(x){
  x <- as.data.frame(t(x))
  x <- cbind(rownames(x), data.frame(x, row.names=NULL))
  colnames(x) <- c("ID", "viewangle")
  x <- x[-1,]
  x$ID <- str_match(x[,1], "Mean:(.*?)\\[")
  x$ID <- as.character(x$ID[,2])
  return(x)
}

#metadate of full library, cj-version 16.05.2018
meta <- read_excel(path = "B:/temp/temp_clemens/BA_EnMap/speclib/00_BA_metadata_cj.xlsx", sheet = 1)
viewangles50 <- readSLI(path = 'B:/Project_California/00_Data/02_Spectral_Library/05_BA/speclib/old_speclib1/viewangles/viewangle_50')
viewangles50 <- readviewangle(viewangles50)

viewangles50$ID_line <- paste(viewangles50$ID, '50', sep="_")


#check for dubble entries in the dfs
ID.viewangles <- viewangles50$ID_line
duplicates.viewangles <- ID.viewangles[duplicated(ID.viewangles)]

header50_correct <- c("ID50","viewangle_correct","ID_line")
names(viewangles50) <- header50_correct

final.df <- left_join(meta, viewangles50, by = c("Full_ID" = "ID_line"))

write.xlsx(viewangles50, file="B:/temp/temp_clemens/BA_EnMap/speclib/BA_viewangles_50.xlsx", sheetName = "viewangles_50")
write.xlsx(final.df, file="B:/temp/temp_clemens/BA_EnMap/speclib/BA_meta_180523.xlsx", sheetName = "full_library")
