##### Packages #####
library(rgdal)
library(sf)
library(raster)
library(ggplot2)
library(reshape2)
library(ggedit)

setwd("B:/temp/temp_clemens/aaa_crosstrack/results/")

##### Functions #####
deg2rad <- function(deg) {(deg * pi) / (180)}
rad2deg <- function(rad) {(rad * 180) / (pi)}

# Create a line feature that has:
# a starting point, a given slope in degrees and a given length in m

# https://www.calculator.net/triangle-calculator.html?vc=61&vx=112000&vy=&va=29&vz=&vb=90&angleunits=d&x=83&y=31

create_rotated_line <- function(lineLength, startPoint, angleA, cs){
  # lineLength is the hypotenuse
  # the other two side are calculated and used to determine the endpoint of the hypotenuse
  
  # calculation is done using a rectangular triangle
  # therefore one additional angle is known and the third can be calculated
  angleB <- 90 
  angleC <- 180 - angleA - angleB
  
  #calculating the long side opposite of the hypotenuse = increase in y
  #(angle * pi)/180 converts degree to rad
  yIncrease <- lineLength*sin((angleA * pi) / (180))/sin((angleB * pi) / (180))
  
  #calculatinf the smalles side = increase in x
  xIncrease <- lineLength*sin((angleC * pi) / (180))/sin((angleB * pi) / (180))
  
  # creating the line
  l1 <- cbind(c(startPoint[1], startPoint[1] + xIncrease), c(startPoint[2], startPoint[2] + yIncrease))
  Sl1 <- Line(l1)
  
  S1 <- Lines(list(Sl1), ID="1")
  
  Sl <- SpatialLines(list(S1))
  Sl@proj4string <- cs
  return((Sl))
}

##### Load data #####
vegcroco <- stack("BA_croco_mosaic_specsub.bsq")
uncorrected <- stack("BA_bilin_mosaic_specsub.bsq")
classwise <- stack("B:/Project_California/99_temp/bcor/03_temp_mosaic/E_L2A_suBA_mosaic_crco.bsq")
flightlines <- stack("B:/temp/temp_clemens/raster/flightline_raster/mosaic_flightlines.bsq")

cs <- vegcroco@crs

Sl2 <- shapefile("line2.shp")

##### Analysis #####
#Sl1 <- create_rotated_line(lineLength = 120000, startPoint = c(560295, 4114049), angleA = 29, cs = cs )
Sl2 <- create_rotated_line(lineLength = 120000, startPoint = c(506794, 4211366), angleA = 29, cs = cs )

# extractions of values
e_vegcroco74 <- as.data.frame(extract(vegcroco$Band.74, Sl2))
e_uncorrected74 <- as.data.frame(extract(uncorrected$Band.74, Sl2))
e_classwise74 <- as.data.frame(extract(classwise$Band.74..871.000000.Nanometers., Sl2))
e_flightline <- as.data.frame(extract(flightlines, Sl2))

e_vegcroco74[e_vegcroco74==65000] <- NA

df_extract2 <- cbind(e_uncorrected74/10000, e_vegcroco74/10000, e_classwise74, e_flightline)
names(df_extract2) <- c("B74_uncorrected", "B74_vegcroco", "B74_classwise","flightline")
df_extract2 <- na.omit(df_extract2)
df_extract2 <- as.data.frame(df_extract2[nrow(df_extract2):1,])

df_extract2$B74_uncorrected <- stats::filter(df_extract2$B74_uncorrected, rep(1/31, 31), method = "convolution", sides = 2)
df_extract2$B74_classwise <- stats::filter(df_extract2$B74_classwise, rep(1/31, 31), method = "convolution", sides = 2)
df_extract2$B74_vegcroco <- stats::filter(df_extract2$B74_vegcroco, rep(1/31, 31), method = "convolution", sides = 2)
df_extract2 <- na.omit(df_extract2)

df_extract2$Pixel <- c(1:length(df_extract2[,1]))

df_plot1 <- dplyr::filter(df_extract2, flightline > 49 & flightline < 55 )
df_plot <- melt(df_plot1,  id.vars = 'Pixel', variable.name = 'Method')

df_plot2 <- dplyr::filter(df_extract2, flightline >= 55 & flightline <= 59 )
df_plot_m2 <- melt(df_plot2,  id.vars = 'Pixel', variable.name = 'Method')

##### Plotting #####

p <- ggplot(dplyr::filter(df_plot, Method != "flightline" & Method != "B74_classwise"), aes(x=Pixel, y=value, color=Method))+
  geom_line(size=.5)+
  ylab("Reflectance")+
  theme_bw()+
  theme(legend.position="bottom", text = element_text(size=15),panel.grid = element_blank())+
  #annotate(geom="text", x = 710, y= 0.42, label="50", color="black")+
  geom_vline(xintercept = 955, colour = "grey50", linetype = "dashed")+
  #annotate(geom="text", x = 1220, y= 0.42, label="51", color="black")+
  geom_vline(xintercept = 1445, colour = "grey50", linetype = "dashed")+
  #annotate(geom="text", x = 1670, y= 0.42, label="52", color="black")+
  geom_vline(xintercept = 1869, colour = "grey50", linetype = "dashed")+
  #annotate(geom="text", x = 2100, y= 0.42, label="53", color="black")+
  geom_vline(xintercept = 2329, colour = "grey50", linetype = "dashed")+
  #annotate(geom="text", x = 2600, y= 0.42, label="54", color="black")+
  #geom_vline(xintercept = 2834, colour = "grey50", linetype = "dashed")+
  #annotate(geom="text", x = 3100, y= 0.42, label="55", color="black")+
  scale_color_manual(name="Profile of band 74 (871 nm) of:", values=c("black", "#5fad00"), labels = c("A) uncorrected mosaic", "B) class-wise corrected mosaic"))

ggsave(filename = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/cross_track_profiles/subset_50-55_uncor_vegcor.jpg", plot = p,device = "jpg",width = 16,height = 4, dpi=600)

p2 <- ggplot(dplyr::filter(df_plot_m2, Method != "flightline" & Method != "B74_classwise"), aes(x=Pixel, y=value, color=Method))+
  geom_line(size=.5)+
  ylab("Reflectance")+
  theme_bw()+
  theme(legend.position="bottom", text = element_text(size=15),panel.grid = element_blank())+
  #annotate(geom="text", x = 710, y= 0.42, label="50", color="black")+
  geom_vline(xintercept = 3280, colour = "grey50", linetype = "dashed")+
  #annotate(geom="text", x = 1220, y= 0.42, label="51", color="black")+
  geom_vline(xintercept = 3695, colour = "grey50", linetype = "dashed")+
  #annotate(geom="text", x = 1670, y= 0.42, label="52", color="black")+
  geom_vline(xintercept = 4149, colour = "grey50", linetype = "dashed")+
  #annotate(geom="text", x = 2100, y= 0.42, label="53", color="black")+
  geom_vline(xintercept = 4646, colour = "grey50", linetype = "dashed")+
  #annotate(geom="text", x = 2600, y= 0.42, label="54", color="black")+
  #geom_vline(xintercept = 2834, colour = "grey50", linetype = "dashed")+
  #annotate(geom="text", x = 3100, y= 0.42, label="55", color="black")+
  scale_color_manual(name="Profile of band 74 (871 nm) of:", values=c("black", "#5fad00"), labels = c("A) uncorrected mosaic", "B) class-wise corrected mosaic"))

ggsave(filename = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/cross_track_profiles/subset_56-59_uncor_vegcor.jpg", plot = p2 ,device = "jpg",width = 16,height = 4, dpi=600)

##### Output #####
