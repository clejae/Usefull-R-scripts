library(readxl)
library(reshape)
library(RStoolbox)
library(dplyr)
library(stringr)
library(ggplot2)

setwd("B:/temp/temp_clemens/")

meta <- read_excel(path = "BA_EnMap/speclib/Meta_area_b.xlsx", sheet = 1)
viewangle <- read_excel(path = "B:/Project_California/00_Data/02_Spectral_Library/05_BA/BA_viewangles.xlsx", sheet = 1)
speclib_B <- readSLI(path = "BA_EnMap/speclib/EnMAP/lib02_revised/EnMAP_speclib_area_b_revisited_CorrSub_specsub")

x <- as.data.frame(t(speclib_B))
x <- cbind(rownames(x), data.frame(x, row.names=NULL))
x$ID <- str_match(x[,1], "Mean:(.*?)\\[")
x$ID <- as.character(x$ID[,2])
colnames(x) = c(paste("wl",x[1, ], sep = ""))
x <- x[-1,-1]


final.df <- left_join(x, viewangle, by = c("wlNA" = "ID_line"))
final.df2 <- left_join(final.df, meta, by = c("wlNA" = "ID"))

ggplot(final.df2,aes(x = viewangle, y = wl479))+
  geom_point()+
  #geom_text(aes(label=ID),hjust=0, vjust=0, size=3)+
  facet_wrap(~ level3)
