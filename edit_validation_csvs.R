library(readxl)
library(plyr)

df <- read_excel("B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD/ABCD_pixel_estimates.xlsx")

df$uniquePolyID <- paste(df$PolyID, df$SubsetArea, sep = "_")


data_aggr = ddply(df, c("uniquePolyID"), summarise,
                  l4_gra    = mean(gra),
                  l4_shr    = mean(shr),
                  l4_btr    = mean(btr),
                  l4_con    = mean(con),
                  l4_soi    = mean(soi),
                  l4_imp    = mean(imp),
                  l4_other  = mean(other)
)

data_aggr$l2_woodyveg <- data_aggr$l4_shr + data_aggr$l4_btr + data_aggr$l4_con
data_aggr$l2_nonwoodyveg <- data_aggr$l4_gra
data_aggr$l2_other <- data_aggr$l4_soi + data_aggr$l4_imp + data_aggr$l4_other

write.csv(data_aggr, file = "B:/temp/temp_clemens/Validation/EnMAP-Pixel-Validation/ABCD_polygon_estimates.csv", row.names = F)
