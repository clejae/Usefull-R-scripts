#---------------------------------------------------------------------- Library & wd  ----------------------------------------------------------------------####
library(RStoolbox)
library(ggplot2)
library(stringr)
library(reshape2)
library(dplyr)
library(rlist)
library(IDPmisc)

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

uncorrected_L <- lapply(list.files(pattern="*L_uncorrected*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
uncorrected_Lt <- lapply(uncorrected_L, readspectra2)

uncorrected_Lt <-bind_rows(uncorrected_Lt)
uncorrected_Lt <- uncorrected_Lt[,bandrem==1]
uncorrected_Lt[,2:ncol(uncorrected_Lt)] <- uncorrected_Lt[,2:ncol(uncorrected_Lt)]/10000

uncorrected_R <- lapply(list.files(pattern="*R_uncorrected*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
uncorrected_Rt <- lapply(uncorrected_R, readspectra2)

uncorrected_Rt <- bind_rows(uncorrected_Rt)
uncorrected_Rt <- uncorrected_Rt[,bandrem==1]
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

#---------------------------------------------------------------------- Analysis ----------------------------------------------------------------------####

########## extract maximum of all spectra per library, which is used for the calculation of the proportional deviance

# maxList <- c()
# 
# max_uncorrL <- max(uncorrected_Lt[,2:ncol(uncorrected_Lt)]) # omitting the first column, which is the ID
# max_uncorrR <- max(uncorrected_Rt[,2:ncol(uncorrected_Rt)])
# maxList[1] <- max(c(max_uncorrL, max_uncorrR))
# 
# max_globalL <- max(globalcroco_Lt[,2:ncol(globalcroco_Lt)])
# max_globalR <- max(globalcroco_Rt[,2:ncol(globalcroco_Rt)])
# maxList[2] <- max(c(max_globalL, max_globalR))
# 
# max_classwiseL <- max(classwise_Lt[,2:ncol(classwise_Lt)])
# max_classwiseR <- max(classwise_Rt[,2:ncol(classwise_Rt)])
# maxList[3] <- max(c(max_classwiseL, max_classwiseR))
# 
# max_brightnormL <- max(brightnorm_Lt[,2:ncol(brightnorm_Lt)])
# max_brightnormR <- max(brightnorm_Rt[,2:ncol(brightnorm_Rt)])
# maxList[4] <- max(c(max_brightnormL, max_brightnormR))
# 
# max_savgolL <- max(abs(savgol_Lt[,2:ncol(savgol_Lt)])) # absolute values because derivatives can be negatives
# max_savgolR <- max(abs(savgol_Rt[,2:ncol(savgol_Rt)]))
# maxList[5] <- max(c(max_savgolL, max_savgolR))
# 
# max_contremL <- max(abs(contrem_Lt[,2:ncol(contrem_Lt)])) # absolute values because derivatives can be negatives
# max_contremR <- max(abs(contrem_Rt[,2:ncol(contrem_Rt)]))
# maxList[5] <- max(c(max_contremL, max_contremR))
# 
# maxBands_uncorrList <- c()
# maxBands_globalList <- c()
# maxBands_classwiseList <- c()
# maxBands_brightnormList <- c()
# maxBands_savgolList <- c()
# maxBands_maxcontremList <- c()
# 
# for (k in 1:(ncol(uncorrected_Lt)-1)){
#   
#   ##### take maximum from maxima of both sides
#   
#   #maxBands_uncorrList[k] <- c(max(uncorrected_Lt[,k+1]),max(uncorrected_Rt[,k+1]))
#   #maxBands_globalList[k] <- c(max(globalcroco_Lt[,k+1]),max(globalcroco_Rt[,k+1]))
#   #maxBands_classwiseList[k] <- c(max(classwise_Lt[,k+1]),max(classwise_Rt[,k+1]))
#   #maxBands_brightnormList[k] <- c(max(brightnorm_Lt[,k+1]),max(brightnorm_Rt[,k+1]))
#   #maxBands_savgolList[k] <- c(max(savgol_Lt[,k+1]),max(savgol_Rt[,k+1]))
#   
#   ##### take the sum of the maxima of both sides
#   
#   maxBands_uncorrList[k] <- max(uncorrected_Lt[,k+1])+max(uncorrected_Rt[,k+1])
#   maxBands_globalList[k] <- max(globalcroco_Lt[,k+1])+max(globalcroco_Rt[,k+1])
#   maxBands_classwiseList[k] <- max(classwise_Lt[,k+1])+max(classwise_Rt[,k+1])
#   maxBands_brightnormList[k] <- max(brightnorm_Lt[,k+1])+max(brightnorm_Rt[,k+1])
#   maxBands_savgolList[k] <- max(savgol_Lt[,k+1])+max(savgol_Rt[,k+1])
#   maxBands_contremList[k] <- max(contrem_Lt[,k+1])+max(contrem_Rt[,k+1])
# 
# }

########## calculate the deviance between eastern and western spectra

dev_uncorrList <- c()
dev_globalList <- c()
dev_classwiseList <- c()
dev_brightnormList <- c()
dev_savgolList <- c()
dev_contremList <- c()

for (k in 1:200){
  
  #### absolute mean deviances
  
  #dev_uncorr <- round(mean(t(abs(uncorrected_Lt[k,2:ncol(uncorrected_Lt)] - uncorrected_Rt[k,2:ncol(uncorrected_Rt)]))),4)
  #dev_global <- round(mean(t(abs(globalcroco_Lt[k,2:ncol(globalcroco_Lt)] - globalcroco_Rt[k,2:ncol(globalcroco_Rt)]))),4)
  #dev_classwise <- round(mean(t(abs(classwise_Lt[k,2:ncol(classwise_Lt)] - classwise_Rt[k,2:ncol(classwise_Rt)]))),4)
  #dev_brightnorm <- round(mean(t(abs(brightnorm_Lt[k,2:ncol(brightnorm_Lt)] - brightnorm_Rt[k,2:ncol(brightnorm_Rt)]))),4)
  #dev_savgol <- round(mean(t(abs(savgol_Lt[k,2:ncol(savgol_Lt)] - savgol_Rt[k,2:ncol(savgol_Rt)]))),4)
  
  ##### proportional mean deviances v1 (divide mean deviance with total maximum)
  
  #dev_uncorr <- round(mean(t(abs(uncorrected_Lt[k,2:ncol(uncorrected_Lt)] - uncorrected_Rt[k,2:ncol(uncorrected_Rt)])))/maxList[1],4)
  #dev_global <- round(mean(t(abs(globalcroco_Lt[k,2:ncol(globalcroco_Lt)] - globalcroco_Rt[k,2:ncol(globalcroco_Rt)])))/maxList[2],4)
  #dev_classwise <- round(mean(t(abs(classwise_Lt[k,2:ncol(classwise_Lt)] - classwise_Rt[k,2:ncol(classwise_Rt)])))/maxList[3],4)
  #dev_brightnorm <- round(mean(t(abs(brightnorm_Lt[k,2:ncol(brightnorm_Lt)] - brightnorm_Rt[k,2:ncol(brightnorm_Rt)])))/maxList[4],4)
  #dev_savgol <- round(mean(t(abs(savgol_Lt[k,2:ncol(savgol_Lt)] - savgol_Rt[k,2:ncol(savgol_Rt)])))/maxList[5],4)
  
  ##### proportion of mean deviance from band maximum 
  
  # 1. calculation of difference in each band
  # 2. divide difference with maximum value --> proportion of difference from the band maximum
  
  #dev_uncorr <- round(mean(t(abs(uncorrected_Lt[k,2:ncol(uncorrected_Lt)] - uncorrected_Rt[k,2:ncol(uncorrected_Rt)])))/maxBands_uncorrList,4)
  #dev_global <- round(mean(t(abs(globalcroco_Lt[k,2:ncol(globalcroco_Lt)] - globalcroco_Rt[k,2:ncol(globalcroco_Rt)])))/maxBands_globalList,4)
  #dev_classwise <- round(mean(t(abs(classwise_Lt[k,2:ncol(classwise_Lt)] - classwise_Rt[k,2:ncol(classwise_Rt)])))/maxBands_classwiseList,4)
  #dev_brightnorm <- round(mean(t(abs(brightnorm_Lt[k,2:ncol(brightnorm_Lt)] - brightnorm_Rt[k,2:ncol(brightnorm_Rt)])))/maxBands_brightnormList,4)
  
  # special case: savgol
  # the first two and the last two maximum values are 0, thus I omit them (dividing by zero returns infinitive values)
  # I take the absolute of the maximum, because derivatives can be negative
  
  #nur bänder 77-86 benutzen!!!!!!!!!!!!!!!!!!!!!
  
  #dev_savgol <- round(mean(t(abs(savgol_Lt[k,4:(ncol(savgol_Lt)-2)] - savgol_Rt[k,4:(ncol(savgol_Rt)-2)])))/abs(maxBands_savgolList[3:193]),4)
  
  #####
  
  # dev_uncorr <- round(mean(t(abs(uncorrected_Lt[k,2:ncol(uncorrected_Lt)] / uncorrected_Rt[k,2:ncol(uncorrected_Rt)]))),4)
  # dev_global <- round(mean(t(abs(globalcroco_Lt[k,2:ncol(globalcroco_Lt)] / globalcroco_Rt[k,2:ncol(globalcroco_Rt)]))),4)
  # dev_classwise <- round(mean(t(abs(classwise_Lt[k,2:ncol(classwise_Lt)] / classwise_Rt[k,2:ncol(classwise_Rt)]))),4)
  # dev_brightnorm <- round(mean(t(abs(brightnorm_Lt[k,2:ncol(brightnorm_Lt)] / brightnorm_Rt[k,2:ncol(brightnorm_Rt)]))),4)
  # dev_savgol <- round(mean(t(abs(savgol_Lt[k,2:(ncol(savgol_Lt))] / savgol_Rt[k,2:(ncol(savgol_Rt))]))),4)
  # dev_contrem <- round(mean(t(abs(contrem_Lt[k,2:ncol(contrem_Lt)] / contrem_Rt[k,2:ncol(contrem_Rt)]))),4)
  # 
  # dev_uncorr <- round(mean(t(abs(uncorrected_Rt[k,2:ncol(uncorrected_Rt)] / uncorrected_Lt[k,2:ncol(uncorrected_Lt)]))),4)
  # dev_global <- round(mean(t(abs(globalcroco_Rt[k,2:ncol(globalcroco_Rt)] / globalcroco_Lt[k,2:ncol(globalcroco_Lt)]))),4)
  # dev_classwise <- round(mean(t(abs(classwise_Rt[k,2:ncol(classwise_Rt)] / classwise_Lt[k,2:ncol(classwise_Lt)]))),4)
  # dev_brightnorm <- round(mean(t(abs(brightnorm_Rt[k,2:ncol(brightnorm_Rt)] / brightnorm_Lt[k,2:ncol(brightnorm_Lt)]))),4)
  # dev_savgol <- round(mean(t(abs(savgol_Rt[k,2:(ncol(savgol_Rt))] / savgol_Lt[k,2:(ncol(savgol_Lt))]))),4)
  # dev_contrem <- round(mean(t(abs(contrem_Rt[k,2:ncol(contrem_Rt)] / contrem_Lt[k,2:ncol(contrem_Lt)]))),4)
  
  # if(sum(uncorrected_Rt[k,2:ncol(uncorrected_Rt)]) < sum(uncorrected_Lt[k,2:ncol(uncorrected_Lt)])){
  #   dev_uncorr <- round(mean(t(abs(uncorrected_Rt[k,2:ncol(uncorrected_Rt)] / uncorrected_Lt[k,2:ncol(uncorrected_Lt)]))),4)
  # }else{
  #   dev_uncorr <- round(mean(t(abs(uncorrected_Lt[k,2:ncol(uncorrected_Lt)] / uncorrected_Rt[k,2:ncol(uncorrected_Rt)]))),4)
  # }
  # if(sum(globalcroco_Rt[k,2:ncol(globalcroco_Rt)]) < sum(globalcroco_Lt[k,2:ncol(globalcroco_Lt)])){
  #   dev_global <- round(mean(t(abs(globalcroco_Rt[k,2:ncol(globalcroco_Rt)] / globalcroco_Lt[k,2:ncol(globalcroco_Lt)]))),4)
  # }else{
  #   dev_global <- round(mean(t(abs(globalcroco_Lt[k,2:ncol(globalcroco_Lt)] / globalcroco_Rt[k,2:ncol(globalcroco_Rt)]))),4)
  # }
  # if(sum(classwise_Rt[k,2:ncol(classwise_Rt)]) < sum(classwise_Lt[k,2:ncol(classwise_Lt)])){
  #   dev_classwise <- round(mean(t(abs(classwise_Rt[k,2:ncol(classwise_Rt)] / classwise_Lt[k,2:ncol(classwise_Lt)]))),4)
  # }else{
  #   dev_classwise <- round(mean(t(abs(classwise_Lt[k,2:ncol(classwise_Lt)] / classwise_Rt[k,2:ncol(classwise_Rt)]))),4)
  # }
  # if(sum(brightnorm_Rt[k,2:ncol(brightnorm_Rt)]) < sum(brightnorm_Lt[k,2:ncol(brightnorm_Lt)])){
  #   dev_brightnorm <- round(mean(t(abs(brightnorm_Rt[k,2:ncol(brightnorm_Rt)] / brightnorm_Lt[k,2:ncol(brightnorm_Lt)]))),4)
  # }else{
  #   dev_brightnorm <- round(mean(t(abs(brightnorm_Lt[k,2:ncol(brightnorm_Lt)] / brightnorm_Rt[k,2:ncol(brightnorm_Rt)]))),4)
  # }
  # if(sum(savgol_Rt[k,2:ncol(savgol_Rt)]) < sum(savgol_Lt[k,2:ncol(savgol_Lt)])){
  #   dev_savgol <- round(mean(t(abs(savgol_Rt[k,2:ncol(savgol_Rt)] / savgol_Lt[k,2:ncol(savgol_Lt)]))),4)
  # }else{
  #   dev_savgol <- round(mean(t(abs(savgol_Lt[k,2:ncol(savgol_Lt)] / savgol_Rt[k,2:ncol(savgol_Rt)]))),4)
  # }
  # if(sum(contrem_Rt[k,2:ncol(contrem_Rt)]) < sum(contrem_Lt[k,2:ncol(contrem_Lt)])){
  #   dev_contrem <- round(mean(t(abs(contrem_Rt[k,2:ncol(contrem_Rt)] / contrem_Lt[k,2:ncol(contrem_Lt)]))),4)
  # }else{
  #   dev_contrem <- round(mean(t(abs(contrem_Lt[k,2:ncol(contrem_Lt)] / contrem_Rt[k,2:ncol(contrem_Rt)]))),4)
  # }
  
  
  if(sum(uncorrected_Rt[k,2:ncol(uncorrected_Rt)]) > sum(uncorrected_Lt[k,2:ncol(uncorrected_Lt)])){
    dev_uncorr <- round(mean(t(uncorrected_Rt[k,2:ncol(uncorrected_Rt)] / uncorrected_Lt[k,2:ncol(uncorrected_Lt)])),4)
  }else{
    dev_uncorr <- round(mean(t(uncorrected_Lt[k,2:ncol(uncorrected_Lt)] / uncorrected_Rt[k,2:ncol(uncorrected_Rt)])),4)
  }
  if(sum(globalcroco_Rt[k,2:ncol(globalcroco_Rt)]) > sum(globalcroco_Lt[k,2:ncol(globalcroco_Lt)])){
    dev_global <- round(mean(t(globalcroco_Rt[k,2:ncol(globalcroco_Rt)] / globalcroco_Lt[k,2:ncol(globalcroco_Lt)])),4)
  }else{
    dev_global <- round(mean(t(globalcroco_Lt[k,2:ncol(globalcroco_Lt)] / globalcroco_Rt[k,2:ncol(globalcroco_Rt)])),4)
  }
  if(sum(classwise_Rt[k,2:ncol(classwise_Rt)]) > sum(classwise_Lt[k,2:ncol(classwise_Lt)])){
    dev_classwise <- round(mean(t(classwise_Rt[k,2:ncol(classwise_Rt)] / classwise_Lt[k,2:ncol(classwise_Lt)])),4)
  }else{
    dev_classwise <- round(mean(t(classwise_Lt[k,2:ncol(classwise_Lt)] / classwise_Rt[k,2:ncol(classwise_Rt)])),4)
  }
  if(sum(brightnorm_Rt[k,2:ncol(brightnorm_Rt)]) > sum(brightnorm_Lt[k,2:ncol(brightnorm_Lt)])){
    dev_brightnorm <- round(mean(t(brightnorm_Rt[k,2:ncol(brightnorm_Rt)] / brightnorm_Lt[k,2:ncol(brightnorm_Lt)])),4)
  }else{
    dev_brightnorm <- round(mean(t(brightnorm_Lt[k,2:ncol(brightnorm_Lt)] / brightnorm_Rt[k,2:ncol(brightnorm_Rt)])),4)
  }
  if(sum(savgol_Rt[k,2:ncol(savgol_Rt)]) > sum(savgol_Lt[k,2:ncol(savgol_Lt)])){
    dev_savgol <- round(mean(t(savgol_Rt[k,2:ncol(savgol_Rt)] / savgol_Lt[k,2:ncol(savgol_Lt)])),4)
  }else{
    dev_savgol <- round(mean(t(savgol_Lt[k,2:ncol(savgol_Lt)] / savgol_Rt[k,2:ncol(savgol_Rt)])),4)
  }
  if(sum(contrem_Rt[k,2:ncol(contrem_Rt)]) > sum(contrem_Lt[k,2:ncol(contrem_Lt)])){
    dev_contrem <- round(mean(t(contrem_Rt[k,2:ncol(contrem_Rt)] / contrem_Lt[k,2:ncol(contrem_Lt)])),4)
  }else{
    dev_contrem <- round(mean(t(contrem_Lt[k,2:ncol(contrem_Lt)] / contrem_Rt[k,2:ncol(contrem_Rt)])),4)
  }
  
  
  # if(sum(uncorrected_Rt[k,2:ncol(uncorrected_Rt)]) < sum(uncorrected_Lt[k,2:ncol(uncorrected_Lt)])){
  #   dev_uncorr <- round(mean(t(uncorrected_Rt[k,2:ncol(uncorrected_Rt)] / uncorrected_Lt[k,2:ncol(uncorrected_Lt)])),4)
  # }else{
  #   dev_uncorr <- round(mean(t(uncorrected_Lt[k,2:ncol(uncorrected_Lt)] / uncorrected_Rt[k,2:ncol(uncorrected_Rt)])),4)
  # }
  # if(sum(globalcroco_Rt[k,2:ncol(globalcroco_Rt)]) < sum(globalcroco_Lt[k,2:ncol(globalcroco_Lt)])){
  #   dev_global <- round(mean(t(globalcroco_Rt[k,2:ncol(globalcroco_Rt)] / globalcroco_Lt[k,2:ncol(globalcroco_Lt)])),4)
  # }else{
  #   dev_global <- round(mean(t(globalcroco_Lt[k,2:ncol(globalcroco_Lt)] / globalcroco_Rt[k,2:ncol(globalcroco_Rt)])),4)
  # }
  # if(sum(classwise_Rt[k,2:ncol(classwise_Rt)]) < sum(classwise_Lt[k,2:ncol(classwise_Lt)])){
  #   dev_classwise <- round(mean(t(classwise_Rt[k,2:ncol(classwise_Rt)] / classwise_Lt[k,2:ncol(classwise_Lt)])),4)
  # }else{
  #   dev_classwise <- round(mean(t(classwise_Lt[k,2:ncol(classwise_Lt)] / classwise_Rt[k,2:ncol(classwise_Rt)])),4)
  # }
  # if(sum(brightnorm_Rt[k,2:ncol(brightnorm_Rt)]) < sum(brightnorm_Lt[k,2:ncol(brightnorm_Lt)])){
  #   dev_brightnorm <- round(mean(t(brightnorm_Rt[k,2:ncol(brightnorm_Rt)] / brightnorm_Lt[k,2:ncol(brightnorm_Lt)])),4)
  # }else{
  #   dev_brightnorm <- round(mean(t(brightnorm_Lt[k,2:ncol(brightnorm_Lt)] / brightnorm_Rt[k,2:ncol(brightnorm_Rt)])),4)
  # }
  # if(sum(savgol_Rt[k,2:ncol(savgol_Rt)]) < sum(savgol_Lt[k,2:ncol(savgol_Lt)])){
  #   dev_savgol <- round(mean(t(savgol_Rt[k,2:ncol(savgol_Rt)] / savgol_Lt[k,2:ncol(savgol_Lt)])),4)
  # }else{
  #   dev_savgol <- round(mean(t(savgol_Lt[k,2:ncol(savgol_Lt)] / savgol_Rt[k,2:ncol(savgol_Rt)])),4)
  # }
  # if(sum(contrem_Rt[k,2:ncol(contrem_Rt)]) < sum(contrem_Lt[k,2:ncol(contrem_Lt)])){
  #   dev_contrem <- round(mean(t(contrem_Rt[k,2:ncol(contrem_Rt)] / contrem_Lt[k,2:ncol(contrem_Lt)])),4)
  # }else{
  #   dev_contrem <- round(mean(t(contrem_Lt[k,2:ncol(contrem_Lt)] / contrem_Rt[k,2:ncol(contrem_Rt)])),4)
  # }
  
  dev_uncorrList[k] <- dev_uncorr
  dev_globalList[k] <- dev_global
  dev_classwiseList[k] <- dev_classwise
  dev_brightnormList[k] <- dev_brightnorm
  dev_savgolList[k] <- dev_savgol
  dev_contremList[k] <- dev_contrem
}

########## Output of all deciances

#l <- list(dev_uncorrList,dev_globalList,dev_classwiseList,dev_brightnormList,dev_savgolList)
#l2 <- c("res_uncorr.csv","res_global.csv","res_classwise.csv","res_brightnorm.csv","res_savgol.csv")
#for (i in 1:length(l)){
#  write.csv(l[[i]], l2[i], row.names = F)
#}

########## calculate the average deviance per flight line and method

devFL_uncorr <- c()
devFL_globalcroco <- c()
devFL_casswise <- c()
devFL_brightnorm <- c()
devFL_savgol <- c()
devFL_contrem <- c()

i=1
for (k in 1:10){
  devFL_uncorr[k] <- mean(NaRV.omit(dev_uncorrList[i:(i+19)]))
  devFL_globalcroco[k] <- mean(NaRV.omit(dev_globalList[i:(i+19)]))
  devFL_casswise[k] <- mean(NaRV.omit(dev_classwiseList[i:(i+19)]))
  devFL_brightnorm[k] <- mean(NaRV.omit(dev_brightnormList[i:(i+19)]))
  devFL_savgol[k] <- mean(NaRV.omit(dev_savgolList[i:(i+19)]))
  devFL_contrem[k] <- mean(NaRV.omit(dev_contremList[i:(i+19)]))

  # devFL_uncorr[k] <- median(dev_uncorrList[i:(i+19)])
  # devFL_globalcroco[k] <- median(dev_globalList[i:(i+19)])
  # devFL_casswise[k] <- median(dev_classwiseList[i:(i+19)])
  # devFL_brightnorm[k] <- median(dev_brightnormList[i:(i+19)])
  # devFL_savgol[k] <- median(dev_savgolList[i:(i+19)])
  # devFL_contrem[k] <- median(dev_contremList[i:(i+19)])
  i=i+20
}

########## Output of all residuals

#l <- list(devFL_uncorr,devFL_globalcroco,devFL_casswise,devFL_brightnorm,devFL_savgol)
#l2 <- c("res_uncorr_aggr.csv","res_global_aggr.csv","res_classwise_aggr.csv","res_brightnorm_aggr.csv","res_savgol_aggr.csv")
#for (i in 1:length(l)){
#  write.csv(l[[i]], l2[i], row.names = F)
#}

overlaps <- c("49-50","50-51","51-52","52-53","53-54","54-55","55-56","56-57","57-58","58-59")

devFL_df_orig <- data.frame(Overlaps = overlaps, Uncorr = devFL_uncorr, VegCroco = devFL_globalcroco, Classwise = devFL_casswise, Brightnorm = devFL_brightnorm, SavGol = devFL_savgol, ContRem = devFL_contrem)
devFL_df <- melt(devFL_df_orig)

########## Output of averaged residuals

#write.csv(devFL_df_orig, file = "residuals per method and flight line.csv", row.names = F)

#---------------------------------------------------------------------- Plotting ----------------------------------------------------------------------####

##### Plot Deviances

p <- ggplot(devFL_df, aes( x = Overlaps, y=value, width = .75))+
  geom_point(aes(shape=variable, color=variable), size = 3)+
  ylab("Mean ratio")+
  theme_bw()+
  theme(legend.title =  element_blank())+
  scale_fill_manual(values = c("#0b3954","#087e8b","#0985ea","#30a5ff","#83bcc6","#4286f4"))+
  geom_hline(yintercept = 1)+
  #geom_jitter(aes(shape=variable), width = 0.2, size = 3)+
  scale_shape_manual(values=c(0,1,16,2,6,17))+
  #scale_y_continuous(limits = c(0.5, 1.6))+
  geom_line(data = devFL_df, mapping = aes(group=variable))
p

p <- ggplot(devFL_df, aes(fill=variable, x = Overlaps, y=value, width = .75))+
  geom_bar(position="dodge", stat="identity")+
  ylab("Difference porportion from band maximum")+
  theme_bw()+
  theme(legend.title =  element_blank())+
  scale_fill_manual(values = c("#0b3954","#087e8b","#0985ea","#30a5ff","#83bcc6","#4286f4"))
p
ggsave(filename=paste("plots_residuals/Comparison_diff_prop_maxsum.jpeg", sep = "" ), plot=p, width=18, height=10, units = "cm", device = "jpg")



p <- ggplot(filter(devFL_df, variable == "Uncorr" | variable == "VegCroco" | variable == "Classwise"), aes(fill=variable, x = Overlaps, y=value))+
  geom_bar(position="dodge", stat="identity")+
  ylab("Mean Residuals")+
  theme(legend.title =  element_blank())

p <- ggplot(filter(devFL_df, variable == "Uncorr"), aes(x = Overlaps, y=value))+
  geom_bar(stat="identity")+
  ggtitle(label = "Uncorrected images")+
  ylab("Mean Residuals")
ggsave(filename=paste("plots_residuals/Uncorr.jpeg", sep = "" ), plot=p, width=18, height=10, units = "cm", device = "jpg")

p <- ggplot(filter(devFL_df, variable == "VegCroco"), aes(x = Overlaps, y=value))+
  geom_bar(stat="identity")+
  ggtitle(label = "Vegetation cross correction")+
  ylab("Mean Residuals")
ggsave(filename=paste("plots_residuals/VegCroco.jpeg", sep = "" ), plot=p, width=18, height=10, units = "cm", device = "jpg")

p <- ggplot(filter(devFL_df, variable == "Classwise"), aes(x = Overlaps, y=value))+
  geom_bar(stat="identity")+
  ggtitle(label = "Classwise cross correction")+
  ylab("Mean Residuals")
ggsave(filename=paste("plots_residuals/Classwise.jpeg", sep = "" ), plot=p, width=18, height=10, units = "cm", device = "jpg")

p <- ggplot(filter(devFL_df, variable == "Brightnorm"), aes(x = Overlaps, y=value))+
  geom_bar(stat="identity")+
  ggtitle(label = "Brightness normalization over Sum")+
  ylab("Mean Residuals")
ggsave(filename=paste("plots_residuals/Brightnorm.jpeg", sep = "" ), plot=p, width=18, height=10, units = "cm", device = "jpg")

p <- ggplot(filter(devFL_df, variable == "SavGol"), aes(x = Overlaps, y=value))+
  geom_bar(stat="identity")+
  ggtitle(label = "Savitzky-Golay-Filter")+
  ylab("Mean Residuals")
ggsave(filename=paste("plots_residuals/SavGol.jpeg", sep = "" ), plot=p, width=18, height=10, units = "cm", device = "jpg")

plot(savgol_Lt[1,])
