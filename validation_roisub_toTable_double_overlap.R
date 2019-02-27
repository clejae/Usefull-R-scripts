library(rgdal)
library(raster)
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
# Accuracies to table

# prepare validation table
values = read.csv("B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD_polygon_estimates_ordered_viewangle_sc.csv")

a = read_excel("B:/temp/temp_clemens/aaa_crosstrack/clemens/validation.xlsx")
a$Roi_name2 = ifelse(test = substr(a$ROI_name, nchar(a$ROI_name), nchar(a$ROI_name ))==")", substr(a$ROI_name, 1,3), substr(a$ROI_name, 1,4))
a$Roi_name2 = ifelse(nchar(a$Roi_name2)==3, paste(substr(a$Roi_name2,1,2),0,substr(a$Roi_name2,3,3), sep=""), a$Roi_name2)

val = left_join(a, values[,2:30], by=c("Roi_name2"="ID"))
val$VAClass[val$Roi_name2 == "A_25"] <- "offNeg"

level = "3b"
#modelnames= c("uncorrected","classwisecroco","globalcroco","savgol","sumnorm","contrem")
modelnames= c("uncorrected","classwisecroco","globalcroco","sumnorm","contrem_smoothed","savgol_smoothed")
classes_2b = c("wve","nwve","back")
classes_3b = c("tree", "shrub", "grass", "back")

#using validation file _cj
#class_indezes_2b = c(10,11,12)

#using validation file _sc
class_indezes_2b = c(20,21,22)
class_indezes_3b = c(23,24,25,26)

viewclasses = c("exNeg","exPos", "nadir", "offNeg", "offPos")
flines = c(49:59)

polyrem = c(-6,-10,-11,-14,-29,-34,-141)
val = val[polyrem,]

final_df = setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("Model","Class","Rsquared","Slope","Intercept","MAE","RMSE"))

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

write.csv(x = final_df, file = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/02_nadir/roisub/model_perfomances_endmember_included.csv", row.names = F)

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
final_df_flines = setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("Model","Box","Class","Rsquared","Slope","Intercept","MAE","RMSE"))

for(j in 1:length(modelnames)){
  modelname = modelnames[j]
  
  wd_str = paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/02_nadir/roisub/level", level, "_", modelname, "_roisub_nadir/", sep = "")
  setwd(wd_str)
  
  stack = stack("aggregations/mean.bsq")
  n_layers = nlayers(stack)
  
  for(i in 1:n_layers){
    # load raster band
    ras = stack[[i]]
    
    for(x in 49:59){
      print(x)
      #### normal plot 
      # prepare df
      df = as.data.frame(rowMeans(as.matrix(ras))*100)
      df = as.data.frame(df[polyrem,])
      
      if (n_layers == 3){
        df$ref = val[,class_indezes_2b[i]]
      } else if (n_layers == 4){
        df$ref = val[,class_indezes_3b[i]]
      }
      
      #df$angleclass = val$VAClass
      df$Box = val$Box
      
      #view_str = viewclasses[x]
      df <- subset(df, Box == x)
      #df <- subset(df, Box != 49)
      df <- df[,c(1,2)]
      
      df[,2] <- as.vector(df[,2])
      colnames(df) = c('pred','ref')
      
      if (n_layers == 3){
        cur_class <- classes_2b[i]
      } else if (n_layers == 4){
        cur_class <- classes_3b[i]
      }
      
      if (nrow(df) > 1){
      # estimate model fit
      model = lm(formula = pred ~ ref, df)
      rsquared = round(summary(model)$r.squared, digits = 2)
      slope = round(summary(model)$coefficients[2,1], digits = 2)
      intercept = round(summary(model)$coefficients[1,1], digits = 2)
      errors = df$ref - df$pred
      mae = round(mean(abs(errors)), digits = 2)
      rmse = round(sqrt(mean(errors^2)), digits = 2)
      } else{
        rsquared = NA
        slope = NA
        intercept = NA
        errors = NA
        mae = NA
        rmse = NA
      }
        
      subdf <- data.frame(modelname, x, cur_class, rsquared, slope, intercept, mae, rmse)
      names(subdf) <- c("Model","Box","Class","Rsquared","Slope","Intercept","MAE","RMSE")
      
      final_df_flines <- rbind(final_df_flines , subdf)
      
    }
  }
}

final_df_flines_m <- melt(final_df_flines, id.vars = c("Model","Box","Class"))

ggplot(filter(final_df_flines_m, variable == "Rsquared"), aes(x = Box, y = value))+
  geom_jitter(width=0.2, aes(color=Model), size=3)+
 # geom_point(aes(color=Model), size=3)+
  facet_wrap(facets = ~Class)+
  scale_color_manual(values=c("#ca0020", "#f4a582", "#000000", "#92c5de", "#0571b0","#19c140"), name="")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Rsquared")

ggplot(filter(final_df_flines_m, variable == "MAE"), aes(x = Box, y = value))+
  geom_jitter(width=0.2, aes(color=Model), size=3)+
  #geom_point(aes(color=Model), size=3)+
  facet_wrap(facets = ~Class)+
  scale_color_manual(values=c("#ca0020", "#f4a582", "#000000", "#92c5de", "#0571b0","#19c140"), name="")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("MAE")

#######################################################################################################
final_df_viewangles = setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("Model","Angle","Class","Rsquared","Slope","Intercept","MAE","RMSE"))

for(j in 1:length(modelnames)){
  modelname = modelnames[j]
  
  wd_str = paste("B:/temp/temp_clemens/ComparisonBrightCorrMethods/02_nadir/roisub/level", level, "_", modelname, "_roisub_nadir/", sep = "")
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
      
      df$angleclass = val$VAClass
      df$Box = val$Box
      
      view_str = viewclasses[x]
      df <- subset(df, angleclass == view_str)
      df <- subset(df, Box != 49)
      df <- df[,c(1,2)]
      
      df[,2] <- as.vector(df[,2])
      colnames(df) = c('pred','ref')
      
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

final_df_viewangles_m <- melt(final_df_viewangles, id.vars = c("Model","Angle","Class"))
print(levels(final_df_viewangles_m$Angle))  ## This will show the levels of x are "Levels: a b c d e"
final_df_viewangles_m$Angle = factor(final_df_viewangles_m$Angle,levels(final_df_viewangles_m$Angle)[c(1,4,3,5,2)])

ggplot(filter(final_df_flines_m, variable == "Rsquared"), aes(x = Angle, y = value))+
  geom_jitter(width=0.2, aes(color=Model), size=3)+
  # geom_point(aes(color=Model), size=3)+
  facet_wrap(facets = ~Class)+
  scale_color_manual(values=c("#ca0020", "#f4a582", "#000000", "#92c5de", "#0571b0","#19c140"), name="")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Rsquared")

ggplot(filter(final_df_flines_m, variable == "MAE"), aes(x = Angle, y = value))+
  geom_jitter(width=0.2, aes(color=Model), size=3)+
  #geom_point(aes(color=Model), size=3)+
  facet_wrap(facets = ~Class)+
  scale_color_manual(values=c("#ca0020", "#f4a582", "#000000", "#92c5de", "#0571b0","#19c140"), name="")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("MAE")

#
# ggplot(filter(final_df_viewangles_m, variable == "Rsquared"),
#        aes(x = Model, y = value)) +
#   geom_jitter(width = 0.2, aes(color = Angle), size = 3) +
#   facet_wrap(facets = ~ Class) +
#   scale_color_manual(
#     values = c(
#       "#ca0020",
#       "#f4a582",
#       "#000000",
#       "#92c5de",
#       "#0571b0",
#       "#19c140"
#     ),
#     name = ""
#   ) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ylab("Rsquared")
# 
# ggplot(filter(final_df_flines_m, variable == "MAE"),
#        aes(x = Model, y = value)) +
#   geom_jitter(width = 0.2, aes(color = Angle), size = 3) +
#   facet_wrap(facets = ~ Class) +
#   scale_color_manual(
#     values = c(
#       "#ca0020",
#       "#f4a582",
#       "#000000",
#       "#92c5de",
#       "#0571b0",
#       "#19c140"
#     ),
#     name = ""
#   ) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ylab("MAE")