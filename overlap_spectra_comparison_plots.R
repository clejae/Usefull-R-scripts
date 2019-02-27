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

#---------------------------------------------------------------------- Data  ----------------------------------------------------------------------####

# this file indicates which bands to keep and which to remove
wl <- read.csv("wavelength.txt")
wl[88,] <- "945"

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
globalcroco_Lt[,2:ncol(globalcroco_Lt)] <- globalcroco_Lt[,2:ncol(globalcroco_Lt)]/10000

globalcroco_R <- lapply(list.files(pattern="*R_globalcroco*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
globalcroco_Rt <- lapply(globalcroco_R, readspectra)

globalcroco_Rt <- bind_rows(globalcroco_Rt)
globalcroco_Rt[,2:ncol(globalcroco_Rt)] <- globalcroco_Rt[,2:ncol(globalcroco_Rt)]/10000

####

uncorrected_L <- lapply(list.files(pattern="*L_uncorrected*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
uncorrected_Lt <- lapply(uncorrected_L, readspectra2)

uncorrected_Lt <-bind_rows(uncorrected_Lt)
uncorrected_Lt[,2:ncol(uncorrected_Lt)] <- uncorrected_Lt[,2:ncol(uncorrected_Lt)]/10000

uncorrected_R <- lapply(list.files(pattern="*R_uncorrected*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
uncorrected_Rt <- lapply(uncorrected_R, readspectra2)

uncorrected_Rt <- bind_rows(uncorrected_Rt)
uncorrected_Rt[,2:ncol(uncorrected_Rt)] <- uncorrected_Rt[,2:ncol(uncorrected_Rt)]/10000

####

classwise_L <- lapply(list.files(pattern="*L_classwise*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
classwise_Lt <- lapply(classwise_L, readspectra2)

classwise_Lt <- bind_rows(classwise_Lt)
classwise_Lt[,2:ncol(classwise_Lt)] <- classwise_Lt[,2:ncol(classwise_Lt)]/10000

classwise_R <- lapply(list.files(pattern="*R_classwise*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
classwise_Rt <- lapply(classwise_R, readspectra2)

classwise_Rt <- bind_rows(classwise_Rt)
classwise_Rt[,2:ncol(classwise_Rt)] <- classwise_Rt[,2:ncol(classwise_Rt)]/10000

####

# the next two libraries contain already all spectra from the two methods
# I needed to separate eastern and western flight lines with boolean lists

bool_R <- c(rep(c(rep(T,20),rep(F,20)),10))
bool_L <- c(rep(c(rep(F,20),rep(T,20)),10))

brightnorm <- readspectra2(readSLI(path = "speclibs/00_overlap_speclib_brightnorm.sli"))

brightnorm_Lt <- brightnorm[bool_L == T,]
brightnorm_Rt <- brightnorm[bool_R == T,]

####
savgol <- readspectra2(readSLI(path = "speclibs/00_overlap_speclib_savgol.sli"))

savgol_Lt <- savgol[bool_L == T,]
savgol_Rt <- savgol[bool_R == T,]

#---------------------------------------------------------------------- Plotting ----------------------------------------------------------------------####

h1 = 3
h2 = 4

#### Plot Spectra

df <- rbind(uncorrected_Lt[h1,2:ncol(uncorrected_Lt)], uncorrected_Rt[h1,2:ncol(uncorrected_Rt)],uncorrected_Lt[h2,2:ncol(uncorrected_Lt)], uncorrected_Rt[h2,2:ncol(uncorrected_Rt)])
names(df) <- wl$wavelength
df$var <- c("S1 - eastern","S2 - eastern","S1 - western","S2 - western")

df_melt <- melt(df)
df_melt$variable <- as.numeric(as.character(df_melt$variable))
p_uncorr <- ggplot(df_melt, aes(x = variable, y = value, group = var, color = var))+
  ggtitle(label="Uncorrected 52-53")+
  geom_line(size = 1)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.title = element_blank())+
  scale_color_manual(values = c("#0b3954","#a63446","#1880bc","#ed314d"), guide=F)+
  scale_x_continuous(breaks=seq(500,2000,500))+
  xlab(label = "Wavelength")+
  ylab(label = "Reflectance")+
  geom_hline(yintercept = 0)


####

df <- rbind(globalcroco_Lt[h1,2:ncol(globalcroco_Lt)], globalcroco_Rt[h1,2:ncol(globalcroco_Rt)],globalcroco_Lt[h2,2:ncol(globalcroco_Lt)], globalcroco_Rt[h2,2:ncol(globalcroco_Rt)])
names(df) <- wl$wavelength
df$var <- c("S1 - eastern","S2 - eastern","S1 - western","S2 - western")

df_melt <- melt(df)
df_melt$variable <- as.numeric(as.character(df_melt$variable))
p_global <- ggplot(df_melt, aes(x = variable, y = value, group = var, color = var))+
  ggtitle(label="VegCroco 52-53")+
  geom_line(size = 1)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.title = element_blank())+
  scale_color_manual(values = c("#0b3954","#a63446","#1880bc","#ed314d"), guide=F)+
  scale_x_continuous(breaks=seq(500,2000,500))+
  xlab(label = "Wavelength")+
  ylab(label = "Reflectance")+
  geom_hline(yintercept = 0)

####

df <- rbind(classwise_Lt[h1,2:ncol(classwise_Lt)], classwise_Rt[h1,2:ncol(classwise_Rt)],classwise_Lt[h2,2:ncol(classwise_Lt)], classwise_Rt[h2,2:ncol(classwise_Rt)])
names(df) <- wl$wavelength
df$var <- c("S1 - eastern","S2 - eastern","S1 - western","S2 - western")

df_melt <- melt(df)
df_melt$variable <- as.numeric(as.character(df_melt$variable))
p_classwise <- ggplot(df_melt, aes(x = variable, y = value, group = var, color = var))+
  ggtitle(label="Classwise 52-53")+
  geom_line(size = 1)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.title = element_blank())+
  scale_color_manual(values = c("#0b3954","#a63446","#1880bc","#ed314d"), guide=F)+
  scale_x_continuous(breaks=seq(500,2000,500))+
  xlab(label = "Wavelength")+
  ylab(label = "Reflectance")+
  geom_hline(yintercept = 0)


####

df <- rbind(brightnorm_Lt[h1,2:ncol(brightnorm_Lt)], brightnorm_Rt[h1,2:ncol(brightnorm_Rt)],brightnorm_Lt[h2,2:ncol(brightnorm_Lt)], brightnorm_Rt[h2,2:ncol(brightnorm_Rt)])
names(df) <- wl$wavelength
df$var <- c("S1 - eastern","S2 - eastern","S1 - western","S2 - western")

df_melt <- melt(df)
df_melt$variable <- as.numeric(as.character(df_melt$variable))
p_brightnorm <- ggplot(df_melt, aes(x = variable, y = value, group = var, color = var))+
  ggtitle(label="Brightnorm 52-53")+
  geom_line(size = 1)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.title = element_blank())+
  scale_color_manual(values = c("#0b3954","#a63446","#1880bc","#ed314d"), guide=F)+
  scale_x_continuous(breaks=seq(500,2000,500))+
  xlab(label = "Wavelength")+
  ylab(label = "Reflectance")+
  geom_hline(yintercept = 0)


####

df <- rbind(savgol_Lt[h1,2:ncol(savgol_Lt)], savgol_Rt[h1,2:ncol(savgol_Rt)],savgol_Lt[h2,2:ncol(savgol_Lt)], savgol_Rt[h2,2:ncol(savgol_Rt)])
names(df) <- wl$wavelength
df$var <- c("S1 - eastern","S2 - eastern","S1 - western","S2 - western")

df_melt <- melt(df)
df_melt$variable <- as.numeric(as.character(df_melt$variable))
p_savgol <- ggplot(df_melt, aes(x = variable, y = value, group = var, color = var))+
  ggtitle(label="Savgol 52-53")+
  geom_line(size = 1)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.title = element_blank())+
  scale_color_manual(values = c("#0b3954","#a63446","#1880bc","#ed314d"), guide=F)+
  scale_x_continuous(breaks=seq(500,2000,500))+
  xlab(label = "Wavelength")+
  ylab(label = "Reflectance")+
  geom_hline(yintercept = 0)

p_uncorr
p_global
p_classwise
p_brightnorm
p_savgol


ggsave( plot=p_uncorr, filename = "plots/VegCroco 52-53.jpg",device = "jpeg", width = 5, height = 3.5, dpi = 600)  
ggsave(filename = "plots/Savgol 52-53.jpg",device = "jpeg", width = 5, height = 3.5, dpi = 600)  
ggsave(filename = "plots/Brightnorm 52-53.jpg",device = "jpeg", width = 5, height = 3.5, dpi = 600)  
ggsave(filename = "plots/Class wise CroCo 52-53.jpg",device = "jpeg", width = 5, height = 3.5, dpi = 600)  
ggsave(filename = "plots/Uncorrected 52-53.jpg",device = "jpeg", width = 5, height = 3.5, dpi = 600)  
