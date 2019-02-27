# Install and Load needed packages - no change needed
install.packages("xlsx")
install.packages("dplyr")
library("xlsx")
library("dplyr")
library("stringr")


# Change working directory 
setwd("O:/Student_Data/CJaenicke/00_SHK/BA_training data/48")

#read list with all possible polygon IDs. 
#Copy it first to your wd! Or change the path in the line below.
df.all = read.xlsx("O:/Student_Data/CJaenicke/00_SHK/BA_training data/54/name_list.xlsx", sheetIndex = 1, header = F)


#Read list from spectral library
df.fl = read.csv("speclib_48.txt")

#Extract only the IDs
df.fl = df.fl[-c(1,69:292),,drop=F] 
df.fl_ID = str_match(df.fl[,1], "Mean: (.*?) ")
df.fl_ID = as.data.frame(df.fl_ID)
df.fl_ID$V2 = as.character(df.fl_ID$V2)
df.fl_ID$ID = substr(df.fl_ID$V2,1, nchar(df.fl_ID$V2)-1)

df.all$X1 = as.factor(df.all$X1)
df.merge <- left_join(df.fl_ID,df.all,by = c("ID" = "X1"))

write.csv(df.merge, file="BA_meta_48.csv")


