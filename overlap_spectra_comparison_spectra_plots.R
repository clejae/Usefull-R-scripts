#---------------------------------------------------------------------- Library & wd  ----------------------------------------------------------------------####
library(RStoolbox)
library(ggplot2)
library(stringr)
library(reshape2)
library(dplyr)
library(rlist)

setwd("B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/")

#---------------------------------------------------------------------- Functions  ----------------------------------------------------------------------####
readspectra <- function(x){
  x <- as.data.frame(t(x)) #this function turns the dataframe around,
  x <- cbind(rownames(x), data.frame(x, row.names=NULL))
  colnames(x) <- c("ID", paste("band",1:242, sep = "")) #extract meangful IDs #and provides better column names
  x <- x[-1,]
  x$ID <- str_match(x[,1], "Mean:(.*?)\\[")
  x$ID <- as.character(x$ID[,2])
  return(x)
}

# second version, because the renaming of the spectra is not neccesarry
readspectra2 <- function(x){
  x <- as.data.frame(t(x)) #this function turns the dataframe around,
  x <- cbind(rownames(x), data.frame(x, row.names=NULL))
  colnames(x) <- c("ID", paste("band",1:242, sep = "")) #extract meangful IDs #and provides better column names
  x <- x[-1,]
  
  x$ID <- as.character(x$ID)
  return(x)
}

readspectra3 <- function(x){
  x <- as.data.frame(t(x)) #this function turns the dataframe around,
  x <- cbind(rownames(x), data.frame(x, row.names=NULL))
  colnames(x) <- c("ID", paste("band",1:195, sep = "")) #extract meangful IDs #and provides better column names
  x <- x[-1,]
  
  x$ID <- as.character(x$ID)
  return(x)
}

#---------------------------------------------------------------------- Data  ----------------------------------------------------------------------####

# this file indicates which bands to keep and which to remove
bandrem <- read.csv("band_rem_forR.txt")

# all lists with ending _R are the spectra from the western flight line
# R indicates that they come from the left side of this flight line
# _L indicates that they come from the eastern flight line

# first create list with files containing "L_globalcroco
# then list apply of readSLI from package RStoolbox 
# then apply own function to transform spectra and provide IDs and column names
globalcroco_L <- lapply(list.files(pattern="*L_globalcroco*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
globalcroco_Lt <- lapply(globalcroco_L, readspectra)

# create a big dataframe with all spectra from the eastern flight lines
# then remove unneeded bands
# then scale
globalcroco_Lt <-bind_rows(globalcroco_Lt)
globalcroco_Lt <- globalcroco_Lt[,bandrem==1]
globalcroco_Lt[,2:ncol(globalcroco_Lt)] <- globalcroco_Lt[,2:ncol(globalcroco_Lt)]/10000

globalcroco_R <- lapply(list.files(pattern="*R_globalcroco*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
globalcroco_Rt <- lapply(globalcroco_R, readspectra)

globalcroco_Rt <- bind_rows(globalcroco_Rt)
globalcroco_Rt <- globalcroco_Rt[,bandrem==1]
globalcroco_Rt[,2:ncol(globalcroco_Rt)] <- globalcroco_Rt[,2:ncol(globalcroco_Rt)]/10000

####

# uncorrected_L <- lapply(list.files(pattern="*L_uncorrected*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
# uncorrected_Lt <- lapply(uncorrected_L, readspectra2)
# 
# uncorrected_Lt <-bind_rows(uncorrected_Lt)
# uncorrected_Lt <- uncorrected_Lt[,bandrem==1]
# uncorrected_Lt[,2:ncol(uncorrected_Lt)] <- uncorrected_Lt[,2:ncol(uncorrected_Lt)]/10000
# 
# uncorrected_R <- lapply(list.files(pattern="*R_uncorrected*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
# uncorrected_Rt <- lapply(uncorrected_R, readspectra2)
# 
# uncorrected_Rt <- bind_rows(uncorrected_Rt)
# uncorrected_Rt <- uncorrected_Rt[,bandrem==1]
# uncorrected_Rt[,2:ncol(uncorrected_Rt)] <- uncorrected_Rt[,2:ncol(uncorrected_Rt)]/10000

uncorrected <- readspectra3(readSLI(path = "speclibs/00_overlap_speclib_nogaps.sli"))


uncorrected_Lt <- uncorrected[bool_L == T,]
uncorrected_Lt[,2:ncol(uncorrected_Lt)] <- uncorrected_Lt[,2:ncol(uncorrected_Lt)]/10000
uncorrected_Rt <- uncorrected[bool_R == T,]
uncorrected_Rt[,2:ncol(uncorrected_Rt)] <- uncorrected_Rt[,2:ncol(uncorrected_Rt)]/10000
####

classwise_L <- lapply(list.files(pattern="*L_classwise*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
classwise_Lt <- lapply(classwise_L, readspectra2)

classwise_Lt <- bind_rows(classwise_Lt)
classwise_Lt <- classwise_Lt[,bandrem==1]
classwise_Lt[,2:ncol(classwise_Lt)] <- classwise_Lt[,2:ncol(classwise_Lt)]/10000

classwise_R <- lapply(list.files(pattern="*R_classwise*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
classwise_Rt <- lapply(classwise_R, readspectra2)

classwise_Rt <- bind_rows(classwise_Rt)
classwise_Rt <- classwise_Rt[,bandrem==1]
classwise_Rt[,2:ncol(classwise_Rt)] <- classwise_Rt[,2:ncol(classwise_Rt)]/10000

####

# the next two libraries contain already all spectra from the two methods
# I needed to separate eastern and western flight lines with boolean lists

bool_R <- c(rep(c(rep(T,20),rep(F,20)),10))
bool_L <- c(rep(c(rep(F,20),rep(T,20)),10))

brightnorm <- readspectra3(readSLI(path = "speclibs/00_overlap_speclib_nogaps_brightnorm.sli"))
#brightnorm <- brightnorm[,bandrem==1]

brightnorm_Lt <- brightnorm[bool_L == T,]
brightnorm_Rt <- brightnorm[bool_R == T,]

####
savgol <- readspectra3(readSLI(path = "speclibs/00_overlap_speclib_nogaps_savgol.sli"))
#savgol <- savgol[,bandrem==1]

savgol_Lt <- savgol[bool_L == T,]
savgol_Rt <- savgol[bool_R == T,]

#savgol_Lt[savgol_Lt==0] <- NA
#savgol_Rt[savgol_Rt==0] <- NA

####
contrem <- readspectra3(readSLI(path = "speclibs/00_overlap_speclib_nogaps_contrem.sli"))

contrem_Lt <- contrem[bool_L == T,]
contrem_Rt <- contrem[bool_R == T,]

########## Output of all spectra 

#l <- list(brightnorm_Lt,brightnorm_Rt,classwise_Lt,classwise_Rt,globalcroco_Rt,globalcroco_Lt,savgol_Lt,savgol_Rt,uncorrected_Rt,uncorrected_Lt)
#l2 <- c("brightnorm_L.csv","brightnorm_R.csv","classwise_L.csv","classwise_R.csv","globalcroco_R.csv","globalcroco_L.csv","savgol_L.csv","savgol_R.csv","uncorrected_R.csv","uncorrected_L.csv")
#
#for (i in 1:length(l)){
#  write.csv(l[[i]], l2[i], row.names = F)
#}

#---------------------------------------------------------------------- Plotting ----------------------------------------------------------------------####
k <- 1
for (k in 81:90){
  df <- rbind(savgol_Lt[k,], savgol_Rt[k,])
  df_plot <- melt(df)

  p1 <- ggplot(df_plot, aes(x=variable, y=value, group=ID, color=ID))+
    geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  df <- rbind(uncorrected_Lt[k,], uncorrected_Rt[k,])
  df_plot <- melt(df)
  
  p2 <- ggplot(df_plot, aes(x=variable, y=value, group=ID, color=ID))+
    geom_line()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #p2
  #p1
  
  ggsave(filename=paste("plots/spectra/53-54_",k,"_Savgol.jpeg", sep = "" ), plot=p1, width=18, height=10, units = "cm", device = "jpg")
  ggsave(filename=paste("plots/spectra/53-54_",k,"_Uncorrected_check.jpeg", sep = "" ), plot=p2, width=18, height=10, units = "cm", device = "jpg")
  
}

for (k in 1:10){
  df <- rbind(savgol_Lt[k,], savgol_Rt[k,])
  df_plot <- melt(df)

  p1 <- ggplot(df_plot, aes(x=variable, y=value, group=ID, color=ID))+
    geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  df <- rbind(uncorrected_Lt[k,], uncorrected_Rt[k,])
  df_plot <- melt(df)
  
  p2 <- ggplot(df_plot, aes(x=variable, y=value, group=ID, color=ID))+
    geom_line()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #p2
  #p1
  
  ggsave(filename=paste("plots/spectra/49-50_",k,"_Savgol.jpeg", sep = "" ), plot=p1, width=18, height=10, units = "cm", device = "jpg")
  ggsave(filename=paste("plots/spectra/49-50_",k,"_Uncorrected_check.jpeg", sep = "" ), plot=p2, width=18, height=10, units = "cm", device = "jpg")
  
}
