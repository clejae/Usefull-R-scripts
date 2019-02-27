library(RStoolbox)
library(plyr)

bandrem <- read.csv("B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/band_rem_forR.txt")

globalcroco <- lapply(list.files(pattern="*globalcroco*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
globalcroco_m <- Reduce(function(...) merge(..., by="wavelength", all.x=TRUE), globalcroco)
globalcroco_m <- globalcroco_m[bandrem==1,]

writeSLI(globalcroco_m, path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/speclibs/final/00_overlap_speclib_globalcroco_bands195.sli",wavl.units = "Nanomenter",mode = "bin")

classwise <- lapply(list.files(pattern="*classwise*", path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison", full.names = TRUE)[c(F,T)], readSLI)
classwise_m <- Reduce(function(...) merge(..., by="wavelength", all.x=TRUE), classwise)
classwise_m <- classwise_m[bandrem==1,]

writeSLI(classwise_m, path = "B:/temp/temp_clemens/ComparisonBrightCorrMethods/overlap_spectra_comparison/speclibs/final/00_overlap_speclib_classwise_bands195.sli",wavl.units = "Nanomenter",mode = "bin")

