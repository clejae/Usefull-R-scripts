library(rgdal)
library(raster)
library(ggplot2)

# Run ####
level = "3b"
modelnames= c("uncorrected","classwisecroco","globalcroco","savgol","sumnorm","contrem","classwisecroco_norm")

classes_2b = c("wve","nwve","back")
classes_3b = c("tree", "shrub", "grass", "back")

#using validation file _cj
#class_indezes_2b = c(10,11,12)

#using validation file _sc
class_indezes_2b = c(15,16,17)
class_indezes_3b = c(18,19,20,21)

#val_X = read.csv("B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD_polygon_estimates_ordered_viewangle_cj.csv")
val = read.csv("B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD_polygon_estimates_ordered_viewangle_sc.csv")

viewclasses = c("exNEG","exPOS", "nadir", "offNEG", "offPOS")

polyrem = c(-178, - 184, -185, -193, -202, -205, -207)

for(j in 1:length(modelnames)){
  modelname = modelnames[j]

  wd_str = paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/level", level, "_", modelname, "_roisub_reducedlib/", sep = "")
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
    
    names(df) = c("pred","ref")
    
    # estimate model fit
    model = lm(formula = pred ~ ref, df)
    rsquared = round(summary(model)$r.squared, digits = 2)
    slope = round(summary(model)$coefficients[2,1], digits = 2)
    intercept = round(summary(model)$coefficients[1,1], digits = 2)
    errors = df$ref - df$pred
    mae = round(mean(abs(errors)), digits = 2)
    rmse = round(sqrt(mean(errors^2)), digits = 2)
    #df = cbind(df, val[polyrem,c(15,16)]) #using validation file _cj
    df = cbind(df, val[,c(27,28)]) #using validation file _sc
    
    # create plot
    if (n_layers == 3){
      title_str <- paste(modelname,classes_2b[i],"per angle", sep= " ")
    } else if (n_layers == 4){
      title_str <- paste(modelname,classes_3b[i],"per angle", sep= " ")
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
      geom_point(data= df, aes(color=factor(angleclass)), size=2)
    
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
# ###




# Graveyard ####
# for(j in 1:length(modelnames)){
#   modelname = modelnames[j]
#   
#   wd_str = paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/", modelname, "/", sep = "")
#   setwd(wd_str)
#   
#   stack = stack("aggregations/mean.bsq")
#   n_layers = nlayers(stack)
#   
#   for(i in 1:n_layers){
#     # load raster band
#     ras = stack[[i]]
#     
#     #### plot per view angle class
#     for( x in 1:length(viewclasses)){
#       
#       # prepare df
#       df = as.data.frame(rowMeans(as.matrix(ras))*100)
#       df$ref = val[,class_indezes[i]]
#       
#       df$angleclass = val$angleclass
#       
#       view_str = viewclasses[x] 
#       df <- subset(df, angleclass == view_str)
#       df <- df[,c(1,2)]
#       
#       names(df) = c("pred","ref")
#       
#       # estimate model fit
#       model = lm(formula = pred ~ ref, df)
#       rsquared = round(summary(model)$r.squared, digits = 2)
#       slope = round(summary(model)$coefficients[2,1], digits = 2)
#       intercept = round(summary(model)$coefficients[1,1], digits = 2)
#       errors = df$ref - df$pred
#       mae = round(mean(abs(errors)), digits = 2)
#       rmse = round(sqrt(mean(errors^2)), digits = 2)
#       val2 = subset(val, angleclass == view_str)
#       df = cbind(df, val2[,c(15,16)])
#       
#       # create plot
#       plt = ggplot(df, aes(ref, pred))+
#         ggtitle(label = paste(modelname, classes[i], view_str,"per angle", sep= " "))+
#         theme_bw()+
#         theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#               panel.grid.minor = element_blank(), legend.position="bottom")+
#         geom_text(aes(1,95,label = paste("R² = ", as.character(rsquared)), hjust = 0 ))+
#         geom_text(aes(1,90,label = paste("y = ", as.character(slope), "x + ", as.character(intercept)), hjust = 0))+
#         geom_text(aes(1,85,label = paste("MAE = ", as.character(mae)), hjust = 0))+
#         geom_text(aes(1,80,label = paste("RMSE = ", as.character(rmse)), hjust = 0))+
#         geom_smooth(method = "lm",se =F, color="black", size = .1)+
#         geom_abline(slope= 1, intercept = 0, alpha=.5, linetype = "dashed")+
#         scale_color_manual(values=c("#ca0020", "#0571b0", "#000000", "#f4a582", "#92c5de"), name="")+
#         xlim(0,100)+
#         ylim(0,100)+
#         geom_hline(yintercept = 0)+
#         geom_vline(xintercept = 0)+
#         geom_hline(yintercept = 100)+
#         geom_vline(xintercept = 100)+
#         geom_point(data= df, aes(color=factor(angleclass)), size=2)
#       
#       # save plot
#       out_str <- paste("validation/l2_", classes[i], "_", modelname, "_", view_str, ".jpg", sep = "")
#       ggsave(filename=out_str, plot=plt, width=16, height=15, units = "cm", device = "jpg")
#     }
#   }
# }



# per flight line
#    plt2 = ggplot(df, aes(ref, pred))+
#      ggtitle(label = paste(modelname,classes[i],"per flight line", sep= " "))+
#      theme_bw()+
#      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank())+
#      geom_text(aes(1,95,label = paste("R² = ", as.character(rsquared)), hjust = 0 ))+
#      geom_text(aes(1,90,label = paste("y = ", as.character(slope), "x + ", as.character(intercept)), hjust = 0))+
#      geom_text(aes(1,85,label = paste("MAE = ", as.character(mae)), hjust = 0))+
#      geom_text(aes(1,80,label = paste("RMSE = ", as.character(rmse)), hjust = 0))+
#      geom_smooth(method = "lm",se =F, color="black", size = .1)+
#      geom_abline(slope= 1, intercept = 0, alpha=.5, linetype = "dashed")+
#      #scale_color_manual(values=c("#ca0020", "#0571b0", "#000000", "#f4a582", "#92c5de"), name="")+
#      xlim(0,100)+
#      ylim(0,100)+
#      geom_hline(yintercept = 0)+
#      geom_vline(xintercept = 0)+
#      geom_hline(yintercept = 100)+
#      geom_vline(xintercept = 100)+
#      geom_point(data= df, aes(color=factor(Box)), size=2)
#    
#    # save plot
#    out_str2 <- paste("validation/l2_", classes[i], "_", modelname, "_perFlightLine.jpg", sep = "")
#    ggsave(filename=out_str2, plot=plt2, width=17, height=15, units = "cm", device = "jpg")
# ####