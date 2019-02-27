#---------------------------------------------------------------------- Library & wd  ----------------------------------------------------------------------####
library(RStoolbox)
library(ggplot2)
library(stringr)
library(reshape2)
library(dplyr)
library(rlist)

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

#---------------------------------------------------------------------- Plotting ----------------------------------------------------------------------####
k <- 1
for (k in 21:60){
  df <- rbind(globalcroco_Lt[k,], globalcroco_Rt[k,])
  df_plot <- melt(df)

  p1 <- ggplot(df_plot, aes(x=variable, y=value, group=ID, color=ID))+
    geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "bottom")
  
  df <- rbind(uncorrected_Lt[k,], uncorrected_Rt[k,])
  df_plot <- melt(df)
  
  p2 <- ggplot(df_plot, aes(x=variable, y=value, group=ID, color=ID))+
    geom_line()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "bottom")
  
   df <- rbind(savgol_Lt[k,], savgol_Rt[k,])
   df_plot <- melt(df)
   
   p3 <- ggplot(df_plot, aes(x=variable, y=value, group=ID, color=ID))+
     geom_line()+
     theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "bottom") 
  
  df <- rbind(classwise_Lt[k,], classwise_Rt[k,])
  df_plot <- melt(df)
  
  p4 <- ggplot(df_plot, aes(x=variable, y=value, group=ID, color=ID))+
    geom_line()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "bottom") 
  #p2
  #p1
  if (k<=20){
    overlap = "49_50_"
  } else if (k > 20 & k <= 40){
    overlap = "50_51_"
  } else if (k > 40 & k <= 60){
    overlap = "51_52_"
  } else if (k > 60 & k <= 80){
      overlap = "52_53_"
  } else if (k > 80 & k <= 100){
    overlap = "53_54_"
  } else if (k > 100 & k <= 120){
    overlap = "54_55_"
  } else if (k > 120 & k <= 140){
    overlap = "55_56_"
  } else if (k > 140 & k <= 160){
    overlap = "56_57_"
  } else if (k > 160 & k <= 180){
    overlap = "57_58_"
  } else if (k > 180 & k <= 200){
    overlap = "58_59_"
  }
  
  ggsave(filename=paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/plots/spectra/",overlap,k,"_03_Vegcroco.jpeg", sep = "" ), plot=p1, width=18, height=10, units = "cm", device = "jpg")
  #ggsave(filename=paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/plots/spectra/",overlap,k,"_01_Uncorrected.jpeg", sep = "" ), plot=p2, width=18, height=10, units = "cm", device = "jpg")
  #ggsave(filename=paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/plots/spectra/",overlap,k,"_05_Savgol.jpeg", sep = "" ), plot=p3, width=18, height=10, units = "cm", device = "jpg")
  ggsave(filename=paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/plots/spectra/",overlap,k,"_02_Classwise.jpeg", sep = "" ), plot=p4, width=18, height=10, units = "cm", device = "jpg")
  
}



df1 <- cbind(globalcroco_Lt[21:40,1], globalcroco_Lt[21:40,2:ncol(globalcroco_Lt)] / globalcroco_Rt[21:40,2:ncol(globalcroco_Rt)])

l = c()
for (i in 21:40){
  dev <- mean(t(abs(globalcroco_Lt[i,2:ncol(globalcroco_Lt)] / globalcroco_Rt[i,2:ncol(globalcroco_Rt)])))
  l[i-20] <- dev
  }

mean(t(abs(classwise_Lt[k,2:ncol(classwise_Lt)] / classwise_Rt[k,2:ncol(classwise_Rt)])))
df1_m <- melt(df1)

ggplot(df1_m, aes(x=variable, y=value))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "bottom")+
  #scale_y_continuous(limits = c(-0.09, 0.06))

ggsave(filename=paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/plots/residuals/49_50_ratio_boxplot_globalcroco.jpeg", sep = "" ),  width=18, height=10, units = "cm", device = "jpg")


df2 <- cbind(classwise_Lt[21:40,1], classwise_Lt[21:40,2:ncol(classwise_Lt)] / classwise_Rt[21:40,2:ncol(classwise_Rt)])
df2_m <- melt(df2)

ggplot(df2_m, aes(x=variable, y=value))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "bottom")+
  #scale_y_continuous(limits = c(-0.09, 0.06))

ggsave(filename=paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/plots/residuals/49_50_ratio_boxplot_classwise.jpeg", sep = "" ),  width=18, height=10, units = "cm", device = "jpg")
