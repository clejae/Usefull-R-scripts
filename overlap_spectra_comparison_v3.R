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

# min/max
dev_uncorrList <- c()
dev_globalList <- c()
dev_classwiseList <- c()
dev_brightnormList <- c()
# dev_savgolList <- c()
dev_contremList <- c()

# max/min
dev_uncorrList2 <- c()
dev_globalList2 <- c()
dev_classwiseList2 <- c()
dev_brightnormList2 <- c()
# dev_savgolList2 <- c()
dev_contremList2 <- c()

for (k in 1:200){
  # ### Min / Max
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

  # divi_savgolList <- c()
  # for (i in 2:ncol(savgol_Lt)){
  #   t <- min(c(abs(savgol_Lt[k,i]), abs(savgol_Rt[k,i])))/max(c(abs(savgol_Lt[k,i]), abs(savgol_Rt[k,i])))
  #   divi_savgolList[(i-1)] <-t
  # }
  # dev_savgolList[k] <- mean(na.omit(divi_savgolList))
  
  # #### Max / Min
  divi_uncorrList2 <- c()
  for (i in 2:ncol(uncorrected_Lt)){
    t <- max(c(uncorrected_Lt[k,i], uncorrected_Rt[k,i]))/min(c(uncorrected_Lt[k,i], uncorrected_Rt[k,i]))
    divi_uncorrList2[(i-1)] <-t
  }
  dev_uncorrList2[k] <- mean(na.omit(divi_uncorrList2))

  divi_globalcrocoList2 <- c()
  for (i in 2:ncol(globalcroco_Lt)){
    t <- max(c(globalcroco_Lt[k,i], globalcroco_Rt[k,i]))/min(c(globalcroco_Lt[k,i], globalcroco_Rt[k,i]))
    divi_globalcrocoList2[(i-1)] <-t
  }
  dev_globalList2[k] <- mean(na.omit(divi_globalcrocoList2))

  divi_classwiseList2 <- c()
  for (i in 2:ncol(classwise_Lt)){
    t <- max(c(classwise_Lt[k,i], classwise_Rt[k,i]))/min(c(classwise_Lt[k,i], classwise_Rt[k,i]))
    divi_classwiseList2[(i-1)] <-t
  }
  dev_classwiseList2[k] <- mean(na.omit(divi_classwiseList2))

  divi_contremList2 <- c()
  for (i in 2:ncol(contrem_Lt)){
    t <- max(c(abs(contrem_Lt[k,i]), abs(contrem_Rt[k,i])))/min(c(abs(contrem_Lt[k,i]), abs(contrem_Rt[k,i])))
    divi_contremList2[(i-1)] <-t
  }
  dev_contremList2[k] <- mean(na.omit(divi_contremList2))
  
  divi_brightnormList2 <- c()
  for (i in 2:ncol(brightnorm_Lt)){
    t <- max(c(brightnorm_Lt[k,i], brightnorm_Rt[k,i]))/min(c(brightnorm_Lt[k,i], brightnorm_Rt[k,i]))
    divi_brightnormList2[(i-1)] <-t
  }
  dev_brightnormList2[k] <- mean(na.omit(divi_brightnormList2))

  # divi_savgolList2 <- c()
  # for (i in 2:ncol(savgol_Lt)){
  #   t <- max(c(abs(savgol_Lt[k,i]), abs(savgol_Rt[k,i])))/min(c(abs(savgol_Lt[k,i]), abs(savgol_Rt[k,i])))
  #   divi_savgolList2[(i-1)] <-t
  # }
  # dev_savgolList2[k] <- mean(na.omit(divi_savgolList2))
}

# ttt <- rbind(savgol_Lt[1,2:166],savgol_Rt[1,2:166]) #band 82

# correlations, slope and intercept

cor_uncorrList <- c()
cor_globalList <- c()
cor_classwiseList <- c()
cor_brightnormList <- c()
#cor_savgolList <- c()
cor_contremList <- c()

slope_uncorrList <- c()
slope_globalList <- c()
slope_classwiseList <- c()
slope_brightnormList <- c()
#slope_savgolList <- c()
slope_contremList <- c()

int_uncorrList <- c()
int_globalList <- c()
int_classwiseList <- c()
int_brightnormList <- c()
#int_savgolList <- c()
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
  
  # df_temp <- as.data.frame(t(rbind(savgol_Lt[k,2:ncol(savgol_Lt)],savgol_Rt[k,2:ncol(savgol_Rt)])))
  # names(df_temp) <- c("pred","ref")
  # model <- lm(formula = pred ~ ref, df_temp)
  # cor_savgolList[k] = round(summary(model)$r.squared, digits = 5) #rsquared
  # slope_savgolList[k] = round(summary(model)$coefficients[2,1], digits = 2) #slope
  # int_savgolList[k] = round(summary(model)$coefficients[1,1], digits = 2) #intercept
}

########## calculate the average of all variables per flight line and method

devFL_uncorr <- c()
devFL_globalcroco <- c()
devFL_casswise <- c()
devFL_brightnorm <- c()
#devFL_savgol <- c()
devFL_contrem <- c()

devFL_uncorr2 <- c()
devFL_globalcroco2 <- c()
devFL_casswise2 <- c()
devFL_brightnorm2 <- c()
#devFL_savgol2 <- c()
devFL_contrem2 <- c()

j=1
for (k in 1:10){
  devFL_uncorr[k] <- mean(NaRV.omit(dev_uncorrList[j:(j+19)]))
  devFL_globalcroco[k] <- mean(NaRV.omit(dev_globalList[j:(j+19)]))
  devFL_casswise[k] <- mean(NaRV.omit(dev_classwiseList[j:(j+19)]))
  devFL_brightnorm[k] <- mean(NaRV.omit(dev_brightnormList[j:(j+19)]))
  #devFL_savgol[k] <- mean(NaRV.omit(dev_savgolList[j:(j+19)]))
  devFL_contrem[k] <- mean(NaRV.omit(dev_contremList[j:(j+19)]))
  
  # devFL_uncorr2[k] <- mean(NaRV.omit(dev_uncorrList2[j:(j+19)]))
  # devFL_globalcroco2[k] <- mean(NaRV.omit(dev_globalList2[j:(j+19)]))
  # devFL_casswise2[k] <- mean(NaRV.omit(dev_classwiseList2[j:(j+19)]))
  # devFL_brightnorm2[k] <- mean(NaRV.omit(dev_brightnormList2[j:(j+19)]))
  # #devFL_savgol2[k] <- mean(NaRV.omit(dev_savgolList2[j:(j+19)]))
  # devFL_contrem2[k] <- mean(NaRV.omit(dev_contremList2[j:(j+19)]))
  
  j=j+20
}



# corFL_uncorr <- c()
# corFL_globalcroco <- c()
# corFL_casswise <- c()
# corFL_brightnorm <- c()
# #corFL_savgol <- c()
# corFL_contrem <- c()
# 
# slopeFL_uncorr <- c()
# slopeFL_globalcroco <- c()
# slopeFL_casswise <- c()
# slopeFL_brightnorm <- c()
# #slopeFL_savgol <- c()
# slopeFL_contrem <- c()
# 
# intFL_uncorr <- c()
# intFL_globalcroco <- c()
# intFL_casswise <- c()
# intFL_brightnorm <- c()
# #intFL_savgol <- c()
# intFL_contrem <- c()
# 
# j=1
# for (k in 1:10){
#   corFL_uncorr[k] <- mean(NaRV.omit(cor_uncorrList[j:(j+19)]))
#   corFL_globalcroco[k] <- mean(NaRV.omit(cor_globalList[j:(j+19)]))
#   corFL_casswise[k] <- mean(NaRV.omit(cor_classwiseList[j:(j+19)]))
#   corFL_brightnorm[k] <- mean(NaRV.omit(cor_brightnormList[j:(j+19)]))
#   #corFL_savgol[k] <- mean(NaRV.omit(cor_savgolList[j:(j+19)]))
#   corFL_contrem[k] <- mean(NaRV.omit(cor_contremList[j:(j+19)]))
#   
#   slopeFL_uncorr[k] <- mean(NaRV.omit(slope_uncorrList[j:(j+19)]))
#   slopeFL_globalcroco[k] <- mean(NaRV.omit(slope_globalList[j:(j+19)]))
#   slopeFL_casswise[k] <- mean(NaRV.omit(slope_classwiseList[j:(j+19)]))
#   slopeFL_brightnorm[k] <- mean(NaRV.omit(slope_brightnormList[j:(j+19)]))
#   #slopeFL_savgol[k] <- mean(NaRV.omit(slope_savgolList[j:(j+19)]))
#   slopeFL_contrem[k] <- mean(NaRV.omit(slope_contremList[j:(j+19)]))
#   
#   intFL_uncorr[k] <- mean(NaRV.omit(int_uncorrList[j:(j+19)]))
#   intFL_globalcroco[k] <- mean(NaRV.omit(int_globalList[j:(j+19)]))
#   intFL_casswise[k] <- mean(NaRV.omit(int_classwiseList[j:(j+19)]))
#   intFL_brightnorm[k] <- mean(NaRV.omit(int_brightnormList[j:(j+19)]))
#   #intFL_savgol[k] <- mean(NaRV.omit(int_savgolList[j:(j+19)]))
#   intFL_contrem[k] <- mean(NaRV.omit(int_contremList[j:(j+19)]))
#   
#   j=j+20
# }

########## Output of all residuals

overlaps <- c("49-50","50-51","51-52","52-53","53-54","54-55","55-56","56-57","57-58","58-59")

devFL_df_orig <- data.frame(Overlaps = overlaps, NOCORR = devFL_uncorr, CTGLOB = devFL_globalcroco, CTSTRAT = devFL_casswise, BNORM = devFL_brightnorm,  CONREM = devFL_contrem)
devFL_df <- melt(devFL_df_orig)

# devFL_df_orig2 <- data.frame(Overlaps = overlaps, NOCORR = devFL_uncorr2, CTGLOB = devFL_globalcroco2, CTSTRAT = devFL_casswise2, BNORM = devFL_brightnorm2,  CONREM = devFL_contrem2)
# devFL_df2 <- melt(devFL_df_orig2)
# 
# corFL_df_orig <- data.frame(Overlaps = overlaps, NOCORR = corFL_uncorr, CTGLOB = corFL_globalcroco, CTSTRAT = corFL_casswise, BNORM = corFL_brightnorm,  CONREM = corFL_contrem)
# corFL_df <- melt(corFL_df_orig)
# 
# slopeFL_df_orig <- data.frame(Overlaps = overlaps, NOCORR = slopeFL_uncorr, CTGLOB = slopeFL_globalcroco, CTSTRAT = slopeFL_casswise, BNORM = slopeFL_brightnorm,  CONREM = slopeFL_contrem)
# slopeFL_df <- melt(slopeFL_df_orig)
# 
# intFL_df_orig <- data.frame(Overlaps = overlaps, NOCORR = intFL_uncorr, CTGLOB = intFL_globalcroco, CTSTRAT = intFL_casswise, BNORM = intFL_brightnorm, CONREM = intFL_contrem)
# intFL_df <- melt(intFL_df_orig)

#---------------------------------------------------------------------- Plotting ----------------------------------------------------------------------####
outputwd = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/plots/residuals/"
# min/max ratios
p1 <- ggplot(devFL_df, aes( x = Overlaps, y=value, width = .75))+
  geom_line(data = devFL_df, mapping = aes(group=variable))+
  geom_point(aes(shape=variable, color=variable), size = 3, fill = "white")+
  scale_shape_manual(values=c(22 ,16,21,17,24))+
  scale_color_manual(values = c("#000000","#000000","#000000","#000000","#000000"))+
  ylab("Spectral ratio (Max/Min)")+
  xlab("Flight line overlap areas")+
  theme_bw()+
  theme(legend.title =  element_blank(), panel.grid = element_line(colour = "white"), text = element_text(family="serif"))+
  geom_hline(yintercept = 1, linetype="dashed")+
  scale_y_continuous(limits = c(0.65, 1.01))
p1

ggsave(filename=paste(outputwd,"XXX_Min-Max_band-wise-absolute.jpeg", sep = ""), plot=p1, width=16, height=6, units = "cm", device = "jpg", dpi= 1000)


# max/min ratios
p2 <- ggplot(devFL_df2, aes( x = Overlaps, y=value, width = .75))+
  geom_point(aes(shape=variable, color=variable), size = 3)+
  ylab("Spectral ratio (Min/Max")+
  xlab("Flight line overlap areas")+
  theme_bw()+
  theme(legend.title =  element_blank())+
  scale_fill_manual(values = c("#0b3954","#087e8b","#0985ea","#30a5ff","#83bcc6","#4286f4"))+
  geom_hline(yintercept = 1)+
  #geom_jitter(aes(shape=variable), width = 0.2, size = 3)+
  scale_shape_manual(values=c(0,1,16,2,6,17))+
  #scale_y_continuous(limits = c(0.5, 1.6))+
  geom_line(data = devFL_df2, mapping = aes(group=variable))+
  scale_y_continuous(limits = c(0.9, 4.8))
p2

# correlations
p3 <- ggplot(corFL_df, aes( x = Overlaps, y=value, width = .75))+
  geom_point(aes(shape=variable, color=variable), size = 3)+
  ylab("Mean correlations")+
  xlab("Flight line overlap areas")+
  theme_bw()+
  theme(legend.title =  element_blank())+
  scale_fill_manual(values = c("#0b3954","#087e8b","#0985ea","#30a5ff","#83bcc6","#4286f4"))+
  geom_hline(yintercept = 1)+
  #geom_jitter(aes(shape=variable), width = 0.2, size = 3)+
  scale_shape_manual(values=c(0,1,16,2,6,17))+
  #scale_y_continuous(limits = c(0.5, 1.6))+
  geom_line(data = corFL_df, mapping = aes(group=variable))+
  scale_y_continuous( limits = c(0.9, 1.1))
p3

# slope
p4 <- ggplot(slopeFL_df, aes( x = Overlaps, y=value, width = .75))+
  geom_point(aes(shape=variable, color=variable), size = 3)+
  ylab("Mean slope")+
  xlab("Flight line overlap areas")+
  theme_bw()+
  theme(legend.title =  element_blank())+
  scale_fill_manual(values = c("#0b3954","#087e8b","#0985ea","#30a5ff","#83bcc6","#4286f4"))+
  geom_hline(yintercept = 1)+
  #geom_jitter(aes(shape=variable), width = 0.2, size = 3)+
  scale_shape_manual(values=c(0,1,16,2,6,17))+
  #scale_y_continuous(limits = c(0.5, 1.6))+
  geom_line(data = slopeFL_df, mapping = aes(group=variable))+
  scale_y_continuous(limits = c(0.7, 1.2))
p4

# intercept
p5 <- ggplot(intFL_df, aes( x = Overlaps, y=value, width = .75))+
  geom_point(aes(shape=variable, color=variable), size = 3)+
  ylab("Mean intercept")+
  xlab("Flight line overlap areas")+
  theme_bw()+
  theme(legend.title =  element_blank())+
  scale_fill_manual(values = c("#0b3954","#087e8b","#0985ea","#30a5ff","#83bcc6","#4286f4"))+
  geom_hline(yintercept = 0)+
  #geom_jitter(aes(shape=variable), width = 0.2, size = 3)+
  scale_shape_manual(values=c(0,1,16,2,6,17))+
  #scale_y_continuous(limits = c(0.5, 1.6))+
  geom_line(data = intFL_df, mapping = aes(group=variable))+
  scale_y_continuous(limits = c(-1, 2))
p5

outputwd = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/plots/residuals/"

ggsave(filename=paste(outputwd,"XXX_Max-Min_band-wise-absolute.jpeg", sep = "" ), plot=p2, width=18, height=10, units = "cm", device = "jpg")
ggsave(filename=paste(outputwd,"XXX_Correlations-OverlapSpectraModels.jpeg", sep = "" ), plot=p3, width=18, height=10, units = "cm", device = "jpg")
ggsave(filename=paste(outputwd,"XXX_Slope-of-OverlapSpectraModels.jpeg", sep = "" ), plot=p4, width=18, height=10, units = "cm", device = "jpg")
ggsave(filename=paste(outputwd,"XXX_Intercept-of-OverlapSpectraModels.jpeg.jpeg", sep = "" ), plot=p5, width=18, height=10, units = "cm", device = "jpg")
