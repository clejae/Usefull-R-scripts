#---------------------------------------------------------------------- Library & wd  ----------------------------------------------------------------------####
library(RStoolbox)
library(ggplot2)
library(stringr)
library(reshape2)
library(dplyr)
library(rlist)
library(IDPmisc)

setwd("B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/speclibs/final/")

#---------------------------------------------------------------------- Functions  ----------------------------------------------------------------------####
readspectra <- function(x){
  x <- as.data.frame(t(x)) #this function turns the dataframe around,
  x <- cbind(rownames(x), data.frame(x, row.names=NULL))
  colnames(x) <- c("ID", paste("band",1:(ncol(x)-1), sep = "")) #extract meangful IDs #and provides better column names
  x <- x[-1,]
  
  x$ID <- as.character(x$ID)
  return(x)
}

divMaxfromMinfrom2Vectors <- function(x,y){
  diviList <- c()
  for (i in 2:ncol(x)){
    t <- min(c(uncorrected_Lt[k,i], uncorrected_Rt[k,i]))/max(c(uncorrected_Lt[k,i], uncorrected_Rt[k,i]))
    divi_uncorrList[(i-1)] <-t  
  }
}

divi_uncorrList <- c()
for (i in 2:ncol(uncorrected_Lt)){
  t <- min(c(uncorrected_Lt[k,i], uncorrected_Rt[k,i]))/max(c(uncorrected_Lt[k,i], uncorrected_Rt[k,i]))
  divi_uncorrList[(i-1)] <-t 
}
dev_uncorrList[k] <- mean(na.omit(divi_uncorrList))


#---------------------------------------------------------------------- Data  ----------------------------------------------------------------------####
bool_R <- c(rep(c(rep(T,20),rep(F,20)),10))
bool_L <- c(rep(c(rep(F,20),rep(T,20)),10))

globalcroco <- readspectra(readSLI(path = "00_overlap_speclib_globalcroco_bands195.sli"))

globalcroco_Lt <- globalcroco[bool_L == T,]
globalcroco_Lt[,2:ncol(globalcroco_Lt)] <- globalcroco_Lt[,2:ncol(globalcroco_Lt)]/10000
globalcroco_Rt <- globalcroco[bool_R == T,]
globalcroco_Rt[,2:ncol(globalcroco_Rt)] <- globalcroco_Rt[,2:ncol(globalcroco_Rt)]/10000

####
uncorrected <- readspectra(readSLI(path = "00_overlap_speclib_nocorr_195bands.sli"))

uncorrected_Lt <- uncorrected[bool_L == T,]
uncorrected_Lt[,2:ncol(uncorrected_Lt)] <- uncorrected_Lt[,2:ncol(uncorrected_Lt)]/10000
uncorrected_Rt <- uncorrected[bool_R == T,]
uncorrected_Rt[,2:ncol(uncorrected_Rt)] <- uncorrected_Rt[,2:ncol(uncorrected_Rt)]/10000
####

classwise <- readspectra(readSLI(path = "00_overlap_speclib_classwise_bands195.sli"))

classwise_Lt <- classwise[bool_L == T,]
classwise_Lt[,2:ncol(classwise_Lt)] <- classwise_Lt[,2:ncol(classwise_Lt)]/10000
classwise_Rt <- classwise[bool_R == T,]
classwise_Rt[,2:ncol(classwise_Rt)] <- classwise_Rt[,2:ncol(classwise_Rt)]/10000

####

brightnorm <- readspectra(readSLI(path = "00_overlap_speclib_brightnorm_195bands.sli"))

brightnorm_Lt <- brightnorm[bool_L == T,]
brightnorm_Rt <- brightnorm[bool_R == T,]

####
savgol <- readspectra(readSLI(path = "00_overlap_speclib_savgol_165bands.sli"))

savgol_Lt <- savgol[bool_L == T,]
savgol_Rt <- savgol[bool_R == T,]

####
contrem <- readspectra(readSLI(path = "00_overlap_speclib_contrem_177bands.sli"))

contrem_Lt <- contrem[bool_L == T,]
contrem_Rt <- contrem[bool_R == T,]

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

# dev_uncorrList <- c()
# dev_globalList <- c()
# dev_classwiseList <- c()
# dev_brightnormList <- c()
# dev_savgolList <- c()
# dev_contremList <- c()
# 
# for (k in 1:200){
  
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
  
  
  # if(sum(uncorrected_Rt[k,2:ncol(uncorrected_Rt)]) > sum(uncorrected_Lt[k,2:ncol(uncorrected_Lt)])){
  #   dev_uncorr <- round(mean(t(uncorrected_Rt[k,2:ncol(uncorrected_Rt)] / uncorrected_Lt[k,2:ncol(uncorrected_Lt)])),4)
  # }else{
  #   dev_uncorr <- round(mean(t(uncorrected_Lt[k,2:ncol(uncorrected_Lt)] / uncorrected_Rt[k,2:ncol(uncorrected_Rt)])),4)
  # }
  # if(sum(globalcroco_Rt[k,2:ncol(globalcroco_Rt)]) > sum(globalcroco_Lt[k,2:ncol(globalcroco_Lt)])){
  #   dev_global <- round(mean(t(globalcroco_Rt[k,2:ncol(globalcroco_Rt)] / globalcroco_Lt[k,2:ncol(globalcroco_Lt)])),4)
  # }else{
  #   dev_global <- round(mean(t(globalcroco_Lt[k,2:ncol(globalcroco_Lt)] / globalcroco_Rt[k,2:ncol(globalcroco_Rt)])),4)
  # }
  # if(sum(classwise_Rt[k,2:ncol(classwise_Rt)]) > sum(classwise_Lt[k,2:ncol(classwise_Lt)])){
  #   dev_classwise <- round(mean(t(classwise_Rt[k,2:ncol(classwise_Rt)] / classwise_Lt[k,2:ncol(classwise_Lt)])),4)
  # }else{
  #   dev_classwise <- round(mean(t(classwise_Lt[k,2:ncol(classwise_Lt)] / classwise_Rt[k,2:ncol(classwise_Rt)])),4)
  # }
  # if(sum(brightnorm_Rt[k,2:ncol(brightnorm_Rt)]) > sum(brightnorm_Lt[k,2:ncol(brightnorm_Lt)])){
  #   dev_brightnorm <- round(mean(t(brightnorm_Rt[k,2:ncol(brightnorm_Rt)] / brightnorm_Lt[k,2:ncol(brightnorm_Lt)])),4)
  # }else{
  #   dev_brightnorm <- round(mean(t(brightnorm_Lt[k,2:ncol(brightnorm_Lt)] / brightnorm_Rt[k,2:ncol(brightnorm_Rt)])),4)
  # }
  # if(sum(savgol_Rt[k,2:ncol(savgol_Rt)]) > sum(savgol_Lt[k,2:ncol(savgol_Lt)])){
  #   dev_savgol <- round(mean(t(savgol_Rt[k,2:ncol(savgol_Rt)] / savgol_Lt[k,2:ncol(savgol_Lt)])),4)
  # }else{
  #   dev_savgol <- round(mean(t(savgol_Lt[k,2:ncol(savgol_Lt)] / savgol_Rt[k,2:ncol(savgol_Rt)])),4)
  # }
  # if(sum(contrem_Rt[k,2:ncol(contrem_Rt)]) > sum(contrem_Lt[k,2:ncol(contrem_Lt)])){
  #   dev_contrem <- round(mean(t(contrem_Rt[k,2:ncol(contrem_Rt)] / contrem_Lt[k,2:ncol(contrem_Lt)])),4)
  # }else{
  #   dev_contrem <- round(mean(t(contrem_Lt[k,2:ncol(contrem_Lt)] / contrem_Rt[k,2:ncol(contrem_Rt)])),4)
  # }
  
  
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
  
  # dev_uncorrList[k] <- dev_uncorr
  # dev_globalList[k] <- dev_global
  # dev_classwiseList[k] <- dev_classwise
  # dev_brightnormList[k] <- dev_brightnorm
  # dev_savgolList[k] <- dev_savgol
  # dev_contremList[k] <- dev_contrem
# }

dev_uncorrList <- c()
dev_globalList <- c()
dev_classwiseList <- c()
dev_brightnormList <- c()
dev_savgolList <- c()
dev_contremList <- c()

for (k in 1:200){
  ### Min / Max
  divi_uncorrList <- c()
  for (i in 2:ncol(uncorrected_Lt)){
    t <- min(c(uncorrected_Lt[k,i], uncorrected_Rt[k,i]))/max(c(uncorrected_Lt[k,i], uncorrected_Rt[k,i]))
    divi_uncorrList[(i-1)] <-t
  }
  dev_uncorrList[k] <- mean(na.omit(divi_uncorrList))

  divi_globalcrocoList <- c()
  for (i in 2:ncol(globalcroco_Lt)){
    t <- min(c(globalcroco_Lt[k,i], globalcroco_Rt[k,i]))/max(c(globalcroco_Lt[k,i], globalcroco_Rt[k,i]))
    divi_globalcrocoList[(i-1)] <-t
  }
  dev_globalList[k] <- mean(na.omit(divi_globalcrocoList))

  divi_classwiseList <- c()
  for (i in 2:ncol(classwise_Lt)){
    t <- min(c(classwise_Lt[k,i], classwise_Rt[k,i]))/max(c(classwise_Lt[k,i], classwise_Rt[k,i]))
    divi_classwiseList[(i-1)] <-t
  }
  dev_classwiseList[k] <- mean(na.omit(divi_classwiseList))

  divi_contremList <- c()
  for (i in 2:ncol(contrem_Lt)){
    t <- min(c(contrem_Lt[k,i], contrem_Rt[k,i]))/max(c(contrem_Lt[k,i], contrem_Rt[k,i]))
    divi_contremList[(i-1)] <-t
  }
  dev_contremList[k] <- mean(na.omit(divi_contremList))

  divi_brightnormList <- c()
  for (i in 2:ncol(brightnorm_Lt)){
    t <- min(c(brightnorm_Lt[k,i], brightnorm_Rt[k,i]))/max(c(brightnorm_Lt[k,i], brightnorm_Rt[k,i]))
    divi_brightnormList[(i-1)] <-t
  }
  dev_brightnormList[k] <- mean(na.omit(divi_brightnormList))

  divi_savgolList <- c()
  for (i in 2:ncol(savgol_Lt)){
    t <- min(c(abs(savgol_Lt[k,i]), abs(savgol_Rt[k,i])))/max(c(abs(savgol_Lt[k,i]), abs(savgol_Rt[k,i])))
    divi_savgolList[(i-1)] <-t
  }
  dev_savgolList[k] <- mean(na.omit(divi_savgolList))
  
  #### Max / Min
  # divi_uncorrList <- c()
  # for (i in 2:ncol(uncorrected_Lt)){
  #   t <- max(c(uncorrected_Lt[k,i], uncorrected_Rt[k,i]))/min(c(uncorrected_Lt[k,i], uncorrected_Rt[k,i]))
  #   divi_uncorrList[(i-1)] <-t 
  # }
  # dev_uncorrList[k] <- mean(na.omit(divi_uncorrList))
  # 
  # divi_globalcrocoList <- c()
  # for (i in 2:ncol(globalcroco_Lt)){
  #   t <- max(c(globalcroco_Lt[k,i], globalcroco_Rt[k,i]))/min(c(globalcroco_Lt[k,i], globalcroco_Rt[k,i]))
  #   divi_globalcrocoList[(i-1)] <-t 
  # }
  # dev_globalList[k] <- mean(na.omit(divi_globalcrocoList))
  # 
  # divi_classwiseList <- c()
  # for (i in 2:ncol(classwise_Lt)){
  #   t <- max(c(classwise_Lt[k,i], classwise_Rt[k,i]))/min(c(classwise_Lt[k,i], classwise_Rt[k,i]))
  #   divi_classwiseList[(i-1)] <-t 
  # }
  # dev_classwiseList[k] <- mean(na.omit(divi_classwiseList))
  # 
  # divi_contremList <- c()
  # for (i in 2:ncol(contrem_Lt)){
  #   t <- max(c(abs(contrem_Lt[k,i]), abs(contrem_Rt[k,i])))/min(c(abs(contrem_Lt[k,i]), abs(contrem_Rt[k,i])))
  #   divi_contremList[(i-1)] <-t 
  # }
  # dev_contremList[k] <- mean(na.omit(divi_contremList))
  # 
  # divi_brightnormList <- c()
  # for (i in 2:ncol(brightnorm_Lt)){
  #   t <- max(c(brightnorm_Lt[k,i], brightnorm_Rt[k,i]))/min(c(brightnorm_Lt[k,i], brightnorm_Rt[k,i]))
  #   divi_brightnormList[(i-1)] <-t 
  # }
  # dev_brightnormList[k] <- mean(na.omit(divi_brightnormList))
  # 
  # divi_savgolList <- c()
  # for (i in 2:ncol(savgol_Lt)){
  #   t <- max(c(abs(savgol_Lt[k,i]), abs(savgol_Rt[k,i])))/min(c(abs(savgol_Lt[k,i]), abs(savgol_Rt[k,i])))
  #   divi_savgolList[(i-1)] <-t 
  # }
  # dev_savgolList[k] <- mean(na.omit(divi_savgolList))
}

ttt <- rbind(savgol_Lt[1,2:166],savgol_Rt[1,2:166]) #band 82
ttt <- rbind(contrem_Lt[1,2:178],contrem_Rt[1,2:178]) #band 82

cor_uncorrList <- c()
cor_globalList <- c()
cor_classwiseList <- c()
cor_brightnormList <- c()
cor_savgolList <- c()
cor_contremList <- c()

slope_uncorrList <- c()
slope_globalList <- c()
slope_classwiseList <- c()
slope_brightnormList <- c()
slope_savgolList <- c()
slope_contremList <- c()

int_uncorrList <- c()
int_globalList <- c()
int_classwiseList <- c()
int_brightnormList <- c()
int_savgolList <- c()
int_contremList <- c()

k = 1
for (k in 1:200){
  df_temp <- as.data.frame(t(rbind(uncorrected_Lt[k,2:ncol(uncorrected_Lt)],uncorrected_Rt[k,2:ncol(uncorrected_Rt)])))
  names(df_temp) <- c("pred","ref")
  model <- lm(formula = pred ~ ref, df_temp)
  cor_uncorrList[k] = round(summary(model)$r.squared, digits = 5) #rsquared
  slope_uncorrList[k] = round(summary(model)$coefficients[2,1], digits = 2) #slope
  int_uncorrList[k] = round(summary(model)$coefficients[1,1], digits = 2) #intercept
  
  df_temp <- as.data.frame(t(rbind(globalcroco_Lt[k,2:ncol(globalcroco_Lt)],globalcroco_Rt[k,2:ncol(globalcroco_Rt)])))
  names(df_temp) <- c("pred","ref")
  model <- lm(formula = pred ~ ref, df_temp)
  cor_globalList[k] = round(summary(model)$r.squared, digits = 5) #rsquared
  slope_globalList[k] = round(summary(model)$coefficients[2,1], digits = 2) #slope
  int_globalList[k] = round(summary(model)$coefficients[1,1], digits = 2) #intercept
  
  df_temp <- as.data.frame(t(rbind(classwise_Lt[k,2:ncol(classwise_Lt)],classwise_Rt[k,2:ncol(classwise_Rt)])))
  names(df_temp) <- c("pred","ref")
  model <- lm(formula = pred ~ ref, df_temp)
  cor_classwiseList[k] = round(summary(model)$r.squared, digits = 5) #rsquared
  slope_classwiseList[k] = round(summary(model)$coefficients[2,1], digits = 2) #slope
  int_classwiseList[k] = round(summary(model)$coefficients[1,1], digits = 2) #intercept
  
  df_temp <- as.data.frame(t(rbind(contrem_Lt[k,2:ncol(contrem_Lt)],contrem_Rt[k,2:ncol(contrem_Rt)])))
  names(df_temp) <- c("pred","ref")
  model <- lm(formula = pred ~ ref, df_temp)
  cor_contremList[k] = round(summary(model)$r.squared, digits = 5) #rsquared
  slope_contremList[k] = round(summary(model)$coefficients[2,1], digits = 2) #slope
  int_contremList[k] = round(summary(model)$coefficients[1,1], digits = 2) #intercept

  df_temp <- as.data.frame(t(rbind(brightnorm_Lt[k,2:ncol(brightnorm_Lt)],brightnorm_Rt[k,2:ncol(brightnorm_Rt)])))
  names(df_temp) <- c("pred","ref")
  model <- lm(formula = pred ~ ref, df_temp)
  cor_brightnormList[k] = round(summary(model)$r.squared, digits = 5) #rsquared
  slope_brightnormList[k] = round(summary(model)$coefficients[2,1], digits = 2) #slope
  int_brightnormList[k] = round(summary(model)$coefficients[1,1], digits = 2) #intercept

  df_temp <- as.data.frame(t(rbind(savgol_Lt[k,2:ncol(savgol_Lt)],savgol_Rt[k,2:ncol(savgol_Rt)])))
  names(df_temp) <- c("pred","ref")
  model <- lm(formula = pred ~ ref, df_temp)
  cor_savgolList[k] = round(summary(model)$r.squared, digits = 5) #rsquared
  slope_savgolList[k] = round(summary(model)$coefficients[2,1], digits = 2) #slope
  int_savgolList[k] = round(summary(model)$coefficients[1,1], digits = 2) #intercept
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

j=1
for (k in 1:10){
  devFL_uncorr[k] <- mean(NaRV.omit(dev_uncorrList[j:(j+19)]))
  devFL_globalcroco[k] <- mean(NaRV.omit(dev_globalList[j:(j+19)]))
  devFL_casswise[k] <- mean(NaRV.omit(dev_classwiseList[j:(j+19)]))
  devFL_brightnorm[k] <- mean(NaRV.omit(dev_brightnormList[j:(j+19)]))
  devFL_savgol[k] <- mean(NaRV.omit(dev_savgolList[j:(j+19)]))
  devFL_contrem[k] <- mean(NaRV.omit(dev_contremList[j:(j+19)]))
  
  # devFL_uncorr[k] <- median(dev_uncorrList[j:(j+19)])
  # devFL_globalcroco[k] <- median(dev_globalList[j:(j+19)])
  # devFL_casswise[k] <- median(dev_classwiseList[j:(j+19)])
  # devFL_brightnorm[k] <- median(dev_brightnormList[j:(j+19)])
  # devFL_savgol[k] <- median(dev_savgolList[j:(j+19)])
  # devFL_contrem[k] <- median(dev_contremList[j:(j+19)])
   j=j+20
}

corFL_uncorr <- c()
corFL_globalcroco <- c()
corFL_casswise <- c()
corFL_brightnorm <- c()
corFL_savgol <- c()
corFL_contrem <- c()

slopeFL_uncorr <- c()
slopeFL_globalcroco <- c()
slopeFL_casswise <- c()
slopeFL_brightnorm <- c()
slopeFL_savgol <- c()
slopeFL_contrem <- c()

intFL_uncorr <- c()
intFL_globalcroco <- c()
intFL_casswise <- c()
intFL_brightnorm <- c()
intFL_savgol <- c()
intFL_contrem <- c()

j=1
for (k in 1:10){
  corFL_uncorr[k] <- mean(NaRV.omit(cor_uncorrList[j:(j+19)]))
  corFL_globalcroco[k] <- mean(NaRV.omit(cor_globalList[j:(j+19)]))
  corFL_casswise[k] <- mean(NaRV.omit(cor_classwiseList[j:(j+19)]))
  corFL_brightnorm[k] <- mean(NaRV.omit(cor_brightnormList[j:(j+19)]))
  corFL_savgol[k] <- mean(NaRV.omit(cor_savgolList[j:(j+19)]))
  corFL_contrem[k] <- mean(NaRV.omit(cor_contremList[j:(j+19)]))
  
  slopeFL_uncorr[k] <- mean(NaRV.omit(slope_uncorrList[j:(j+19)]))
  slopeFL_globalcroco[k] <- mean(NaRV.omit(slope_globalList[j:(j+19)]))
  slopeFL_casswise[k] <- mean(NaRV.omit(slope_classwiseList[j:(j+19)]))
  slopeFL_brightnorm[k] <- mean(NaRV.omit(slope_brightnormList[j:(j+19)]))
  slopeFL_savgol[k] <- mean(NaRV.omit(slope_savgolList[j:(j+19)]))
  slopeFL_contrem[k] <- mean(NaRV.omit(slope_contremList[j:(j+19)]))
  
  intFL_uncorr[k] <- mean(NaRV.omit(int_uncorrList[j:(j+19)]))
  intFL_globalcroco[k] <- mean(NaRV.omit(int_globalList[j:(j+19)]))
  intFL_casswise[k] <- mean(NaRV.omit(int_classwiseList[j:(j+19)]))
  intFL_brightnorm[k] <- mean(NaRV.omit(int_brightnormList[j:(j+19)]))
  intFL_savgol[k] <- mean(NaRV.omit(int_savgolList[j:(j+19)]))
  intFL_contrem[k] <- mean(NaRV.omit(int_contremList[j:(j+19)]))

  j=j+20
}


########## Output of all residuals

#l <- list(devFL_uncorr,devFL_globalcroco,devFL_casswise,devFL_brightnorm,devFL_savgol)
#l2 <- c("res_uncorr_aggr.csv","res_global_aggr.csv","res_classwise_aggr.csv","res_brightnorm_aggr.csv","res_savgol_aggr.csv")
#for (i in 1:length(l)){
#  write.csv(l[[i]], l2[i], row.names = F)
#}

overlaps <- c("49-50","50-51","51-52","52-53","53-54","54-55","55-56","56-57","57-58","58-59")

devFL_df_orig <- data.frame(Overlaps = overlaps, NOCORR = devFL_uncorr, CTGLOB = devFL_globalcroco, CTSTRAT = devFL_casswise, BNORM = devFL_brightnorm, SAVGOL = devFL_savgol, CONREM = devFL_contrem)
devFL_df <- melt(devFL_df_orig)

corFL_df_orig <- data.frame(Overlaps = overlaps, NOCORR = corFL_uncorr, CTGLOB = corFL_globalcroco, CTSTRAT = corFL_casswise, BNORM = corFL_brightnorm, SAVGOL = corFL_savgol, CONREM = corFL_contrem)
corFL_df <- melt(corFL_df_orig)


########## Output of averaged residuals

#write.csv(devFL_df_orig, file = "residuals per method and flight line.csv", row.names = F)

#---------------------------------------------------------------------- Plotting ----------------------------------------------------------------------####

##### Plot Deviances

p <- ggplot(devFL_df, aes( x = Overlaps, y=value, width = .75))+
  geom_point(aes(shape=variable, color=variable), size = 3)+
  ylab("Log: Median ratio")+
  xlab("Flight line overlap areas")+
  theme_bw()+
  theme(legend.title =  element_blank())+
  scale_fill_manual(values = c("#0b3954","#087e8b","#0985ea","#30a5ff","#83bcc6","#4286f4"))+
  geom_hline(yintercept = 1)+
  #geom_jitter(aes(shape=variable), width = 0.2, size = 3)+
  scale_shape_manual(values=c(0,1,16,2,6,17))+
  #scale_y_continuous(limits = c(0.5, 1.6))+
  geom_line(data = devFL_df, mapping = aes(group=variable))+
  scale_y_continuous(trans='log2', limits = c(0.4, 1.2))
p

p <- ggplot(corFL_df, aes( x = Overlaps, y=value, width = .75))+
  geom_point(aes(shape=variable, color=variable), size = 3)+
  ylab("Log: Median ratio")+
  xlab("Flight line overlap areas")+
  theme_bw()+
  theme(legend.title =  element_blank())+
  scale_fill_manual(values = c("#0b3954","#087e8b","#0985ea","#30a5ff","#83bcc6","#4286f4"))+
  geom_hline(yintercept = 1)+
  #geom_jitter(aes(shape=variable), width = 0.2, size = 3)+
  scale_shape_manual(values=c(0,1,16,2,6,17))+
  #scale_y_continuous(limits = c(0.5, 1.6))+
  geom_line(data = corFL_df, mapping = aes(group=variable))+
  scale_y_continuous(limits = c(0.9, 1.1))
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
