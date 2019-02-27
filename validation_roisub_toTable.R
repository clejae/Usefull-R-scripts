library(rgdal)
library(raster)
library(ggplot2)
library(reshape2)
library(dplyr)

# Accuracies to table

level = "3b"
#modelnames= c("uncorrected","classwisecroco","globalcroco","savgol","sumnorm","contrem","classwisecroco_norm")
modelnames= c("classwisecroco","globalcroco","sumnorm","uncorrected","savgol")
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

final_df = setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("Model","Class","Rsquared","Slope","Intercept","MAE","RMSE"))

for(j in 1:length(modelnames)){
  modelname = modelnames[j]
  
  wd_str = paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/02_nadir/roisub/old/level", level, "_", modelname, "_roisub_nadir/", sep = "")
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
    
    if (n_layers == 3){
      cur_class <- classes_2b[i]
    } else if (n_layers == 4){
      cur_class <- classes_3b[i]
    }
    
    # estimate model fit
    model = lm(formula = pred ~ ref, df)
    rsquared = round(summary(model)$r.squared, digits = 2)
    slope = round(summary(model)$coefficients[2,1], digits = 2)
    intercept = round(summary(model)$coefficients[1,1], digits = 2)
    errors = df$ref - df$pred
    mae = round(mean(abs(errors)), digits = 2)
    rmse = round(sqrt(mean(errors^2)), digits = 2)
    
    
    subdf <- data.frame(modelname, cur_class, rsquared, slope, intercept, mae, rmse)
    names(subdf) <- c("Model","Class","Rsquared","Slope","Intercept","MAE","RMSE")
    
    final_df <- rbind(final_df , subdf)
    
  }
}

final_df_m <- melt(final_df)
print(levels(final_df_m$Model))  ## This will show the levels of x are "Levels: a b c d e"
final_df_m$Model = factor(final_df_m$Model,levels(final_df_m$Model)[c(1,3,2,7,5,4,6)])

ggplot(filter(final_df_m, variable == "Rsquared"), aes(x = Model, y = value))+
  geom_bar(stat = "identity")+
  facet_wrap(facets = ~Class)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
ggplot(filter(final_df_m, variable == "MAE"), aes(x = Model, y = value))+
  geom_bar(stat = "identity")+
  facet_wrap(facets = ~Class)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(filter(final_df_m, variable == "RMSE"), aes(x = Model, y = value))+
  geom_bar(stat = "identity")+
  facet_wrap(facets = ~Class)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(filter(final_df_m, variable == "Intercept"), aes(x = Model, y = value))+
  geom_bar(stat = "identity")+
  facet_wrap(facets = ~Class)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(filter(final_df_m, variable == "Slope"), aes(x = Model, y = value))+
  geom_bar(stat = "identity")+
  facet_wrap(facets = ~Class)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#######################################################################################################
final_df_viewangles = setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("Model","Angle","Class","Rsquared","Slope","Intercept","MAE","RMSE"))

for(j in 1:length(modelnames)){
  modelname = modelnames[j]
  
  wd_str = paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/02_nadir/roisub/old/level", level, "_", modelname, "_roisub_nadir/", sep = "")
  setwd(wd_str)
  
  stack = stack("aggregations/mean.bsq")
  n_layers = nlayers(stack)
  
  for(i in 1:n_layers){
    # load raster band
    ras = stack[[i]]
    
    for(x in 1:length(viewclasses)){
    
    #### normal plot 
    # prepare df
    df = as.data.frame(rowMeans(as.matrix(ras))*100)
    df = as.data.frame(df[polyrem,])
    
    if (n_layers == 3){
      df$ref = val[,class_indezes_2b[i]]
    } else if (n_layers == 4){
      df$ref = val[,class_indezes_3b[i]]
    }
    
    df$angleclass = val$angleclass
    
    view_str = viewclasses[x]
    df <- subset(df, angleclass == view_str)
    df <- df[,c(1,2)]
    
    names(df) = c("pred","ref")
    
    if (n_layers == 3){
      cur_class <- classes_2b[i]
    } else if (n_layers == 4){
      cur_class <- classes_3b[i]
    }
    
    # estimate model fit
    model = lm(formula = pred ~ ref, df)
    rsquared = round(summary(model)$r.squared, digits = 2)
    slope = round(summary(model)$coefficients[2,1], digits = 2)
    intercept = round(summary(model)$coefficients[1,1], digits = 2)
    errors = df$ref - df$pred
    mae = round(mean(abs(errors)), digits = 2)
    rmse = round(sqrt(mean(errors^2)), digits = 2)
    
    subdf <- data.frame(modelname, view_str, cur_class, rsquared, slope, intercept, mae, rmse)
    names(subdf) <- c("Model","Angle","Class","Rsquared","Slope","Intercept","MAE","RMSE")
    
    final_df_viewangles <- rbind(final_df_viewangles , subdf)
    
  }
}
}

final_df_viewangles_m <- melt(final_df_viewangles)
print(levels(final_df_viewangles_m$Angle))  ## This will show the levels of x are "Levels: a b c d e"
final_df_viewangles_m$Angle = factor(final_df_viewangles_m$Angle,levels(final_df_viewangles_m$Angle)[c(1,4,3,5,2)])

ggplot(filter(final_df_viewangles_m, variable == "Rsquared"), aes(x = Model, y = value))+
  geom_jitter(width=0.2, aes(color=Angle), size=3)+
  facet_wrap(facets = ~Class)+
  scale_color_manual(values=c("#ca0020", "#f4a582", "#000000", "#92c5de", "#0571b0","#19c140"), name="")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
ggplot(filter(final_df_viewangles_m, variable == "MAE"), aes(x = Model, y = value))+
  geom_jitter(width=0.2, aes(color=Angle), size=3)+
  facet_wrap(facets = ~Class)+
  scale_color_manual(values=c("#ca0020", "#f4a582", "#000000", "#92c5de", "#0571b0","#19c140"), name="")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(filter(final_df_viewangles_m, variable == "Rsquared"), aes(x = Angle, y = value))+
  #geom_jitter(width=0.2, aes(color=Model), size=3)+
  geom_point(aes(color=Model), size=3)+
  facet_wrap(facets = ~Class)+
  scale_color_manual(values=c("#ca0020", "#f4a582", "#000000", "#92c5de", "#0571b0","#19c140"), name="")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(filter(final_df_viewangles_m, variable == "MAE"), aes(x = Angle, y = value))+
  #geom_jitter(width=0.2, aes(color=Model), size=3)+
  geom_point(aes(color=Model), size=3)+
  facet_wrap(facets = ~Class)+
  scale_color_manual(values=c("#ca0020", "#f4a582", "#000000", "#92c5de", "#0571b0","#19c140"), name="")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

t <- table(val$angleclass)
