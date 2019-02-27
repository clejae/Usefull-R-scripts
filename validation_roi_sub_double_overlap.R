library(readxl)
library(dplyr)
library(rgdal)
library(raster)
library(ggplot2)

# prepare validation table
values = read.csv("B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD_polygon_estimates_ordered_viewangle_sc.csv")

a = read_excel("B:/temp/temp_clemens/aaa_crosstrack/clemens/validation.xlsx")
a$Roi_name2 = ifelse(test = substr(a$ROI_name, nchar(a$ROI_name), nchar(a$ROI_name ))==")", substr(a$ROI_name, 1,3), substr(a$ROI_name, 1,4))
a$Roi_name2 = ifelse(nchar(a$Roi_name2)==3, paste(substr(a$Roi_name2,1,2),0,substr(a$Roi_name2,3,3), sep=""), a$Roi_name2)

val = left_join(a, values[,2:30], by=c("Roi_name2"="ID"))
val$VAClass[val$Roi_name2 == "A_25"] <- "offNeg"

level = "3b"
modelnames= c("savgol_smoothed","contrem_smoothed")
#"uncorrected",,"savgol","sumnorm","contrem","classwisecroco","globalcroco"

classes_2b = c("wve","nwve","back")
classes_3b = c("tree", "shrub", "grass", "back")

#using validation file _cj
#class_indezes_2b = c(10,11,12)

#using validation file _sc
class_indezes_2b = c(20,21,22)
class_indezes_3b = c(23,24,25,26)

#val_X = read.csv("B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD_polygon_estimates_ordered_viewangle_cj.csv")
#val = read.csv("B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD_polygon_estimates_ordered_viewangle_sc.csv")

viewclasses = c("exNEG","exPOS", "nadir", "offNEG", "offPOS")

polyrem = c(-6,-10,-11,-14,-29,-34,-141)
val = val[polyrem,]

for(j in 1:length(modelnames)){
  modelname = modelnames[j]
  
  wd_str = paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/02_nadir/roisub/level", level, "_", modelname, "_roisub_nadir/", sep = "")
  setwd(wd_str)
  
  stack = stack("aggregations/mean.bsq")
  n_layers = nlayers(stack)
  
  for(i in 1:n_layers){
    # load raster band
    ras = stack[[i]]
    
    #### normal plot 
    # prepare df
    df = as.data.frame(rowMeans(as.matrix(ras))*100)
    df = as.data.frame(df[polyrem,])
    
    if (n_layers == 3){
      df$ref = val[,class_indezes_2b[i]]
    } else if (n_layers == 4){
      df$ref = val[,class_indezes_3b[i]]
    }
    
    df[,2] <- as.vector(df[,2])
    colnames(df) = c('pred','ref')
    
    # estimate model fit
    model = lm(formula = pred ~ ref, df)
    rsquared = round(summary(model)$r.squared, digits = 2)
    slope = round(summary(model)$coefficients[2,1], digits = 2)
    intercept = round(summary(model)$coefficients[1,1], digits = 2)
    errors = df$ref - df$pred
    mae = round(mean(abs(errors)), digits = 2)
    rmse = round(sqrt(mean(errors^2)), digits = 2)
    #df = cbind(df, val[polyrem,c(15,16)]) #using validation file _cj
    df = cbind(df, val[,6]) #using validation file _sc
    
    # create plot
    if (n_layers == 3){
      title_str <- paste(modelname,classes_2b[i],"per view angle class", sep= " ")
    } else if (n_layers == 4){
      title_str <- paste(modelname,classes_3b[i],"per view angle class", sep= " ")
    }
    
    plt = ggplot(df, aes(ref, pred))+
      ggtitle(label = title_str)+
      theme_bw()+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
      geom_text(aes(1,95,label = paste("R² = ", as.character(rsquared)), hjust = 0 ))+
      geom_text(aes(1,90,label = paste("y = ", as.character(slope), "x + ", as.character(intercept)), hjust = 0))+
      geom_text(aes(1,85,label = paste("MAE = ", as.character(mae)), hjust = 0))+
      geom_text(aes(1,80,label = paste("RMSE = ", as.character(rmse)), hjust = 0))+
      geom_smooth(method = "lm",se =F, color="black", size = .1)+
      geom_abline(slope= 1, intercept = 0, alpha=.5, linetype = "dashed")+
      scale_color_manual(values=c("#ca0020", "#0571b0", "#000000", "#f4a582", "#92c5de"), name="")+
      xlim(0,100)+
      ylim(0,100)+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0)+
      geom_hline(yintercept = 100)+
      geom_vline(xintercept = 100)+
      geom_point(data= df, aes(color=factor(VAClass)), size=2)
    
    # save plot
    if (n_layers == 3){
      out_str <- paste("validation/l",level,"_", classes_2b[i], "_", modelname, "_perAngle.jpg", sep = "")
    } else if (n_layers == 4){
      out_str <- paste("validation/l",level,"_", classes_3b[i], "_", modelname, "_perAngle.jpg", sep = "")
    }
    
    #out_str <- paste("validation/l2_", classes[i], "_", modelname, "_perAngle.jpg", sep = "")
    ggsave(filename=out_str, plot=plt, width=17, height=15, units = "cm", device = "jpg")
  }
}
