# 1. Preparations ####
#install.packages("RStoolbox")
#install.packages("readxl")
install.packages("made4")
library(xlsx)
library(readxl)
library(reshape)
library(RStoolbox)
library(dplyr)
library(stringr)
library(ggplot2)
library(made4)

setwd("B:/Project_California/00_Data/02_Spectral_Library/05_BA/")

# 2. Define functions####
#this function turns the dataframe around,
#extract meangful IDs
#and provides better column names
readspectra <- function(x){
  x <- as.data.frame(t(x))
  x <- cbind(rownames(x), data.frame(x, row.names=NULL))
  colnames(x) <- c("ID", paste("band",1:224, sep = ""))
  x <- x[-1,]
  x$ID <- str_match(x[,1], "Mean:(.*?)\\[")
  x$ID <- as.character(x$ID[,2])
  return(x)
}

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


# 3. Load data ####

#loads Metadata Excel File
meta <- read_excel(path = "BA_metadata.xlsx", sheet = 1)

#lists all files that are a speclib
files_speclib <-  list.files(pattern="*speclib*", path = "B:/Project_California/00_Data/02_Spectral_Library/05_BA/speclib", full.names = TRUE)
#removes every 2nd part of the list, which are the headerfiles
files_speclib <- files_speclib[c(T,F)]
#lists all files that are a speclib
files_viewangle <- list.files(pattern="*viewangle*", path = "B:/Project_California/00_Data/02_Spectral_Library/05_BA/speclib", full.names = TRUE)
#removes every 2nd part of the list, which are the headerfiles
files_viewangle <- files_viewangle[c(T,F)]

#loads all files in the three lists
speclibs <- lapply(files_speclib, readSLI)
viewangles <- lapply(files_viewangle, readSLI)

#converts the first column into a usable ID column
speclibs <- lapply(speclibs,readspectra)
viewangles <- lapply(viewangles, readviewangle)

#adds a column which indicates from which line the viewangle comes
viewangles[[1]]$line <- 48

for (k in 2:4){
  viewangles[[k]]$line <- 48+k
}

for (k in 5:10){
  viewangles[[k]]$line <- 49+k
}

speclibs[[1]]$line <- 48

for (k in 2:4){
  speclibs[[k]]$line <- 48+k
}

for (k in 5:10){
  speclibs[[k]]$line <- 49+k
}

#creates two big dataframes of the spectral libraries and the viewangle library
speclibs.df <- bind_rows(speclibs)
viewangles.df <- bind_rows(viewangles)

#creates ID that is linked to the fligh line number
speclibs.df$ID_line <- paste(speclibs.df$ID, speclibs.df$line, sep="_")
viewangles.df$ID_line <- paste(viewangles.df$ID, viewangles.df$line, sep="_")
meta$ID_line <- paste(meta$obj_ID, meta$line, sep="_")

#check for dubble entries in the dfs
ID.viewangles <- viewangles.df$ID_line
ID.speclibs <- speclibs.df$ID_line
duplicates.viewangles <- ID.viewangles[duplicated(ID.viewangles)]
duplicates.speclibs <- ID.speclibs[duplicated(ID.speclibs)]

#combines the spectral library, the viewangle library and the metadata into one df
final.df <- left_join(viewangles.df, speclibs.df, by = "ID_line")
final.df <- left_join(final.df, meta, by = "ID_line")

#check for dubble entries in the final df
ID.final <- final.df$ID_line
duplicates.final <- ID.final[duplicated(ID.final)]

#export the dfs
write.xlsx(viewangles.df, file="BA_viewangles.xlsx", sheetName = "viewangles")
write.xlsx(speclibs.df, file="BA_spectra.xlsx", sheetName = "spectra")
write.xlsx(final.df, file="BA_training_library.xlsx", sheetName = "training")

#rounds the viewangle
final.df$viewangle_5 <- round(final.df$viewangle/5)*5

# 5. Plotting ####
ggplot(final.df,aes(x = viewangle_5, y = band44, color=shadow))+
  geom_point()+
  #geom_text(aes(label=ID),hjust=0, vjust=0, size=3)+
  facet_wrap(~ level3)

ggplot(final.df,aes(x = viewangle_5, y = band44, color=Summer))+
  geom_point()+
  #geom_text(aes(label=ID),hjust=0, vjust=0, size=3)+
  facet_wrap(~ level3)
