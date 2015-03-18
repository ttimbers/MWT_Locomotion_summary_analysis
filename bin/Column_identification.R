##load MWT data
setwd("/Users/catrinaloucks/Documents/PhD/MWT_Locomotion_summary_analysis/data/chore_data")
N2 <- read.table("merged.file")

##split up column V1 into identifier and time
library(stringr)
df_V1 <- N2$V1
plate_tag  <- "/[0-9_]{1,}"
plate_names  <- str_extract(V1, plate_tag)
plate_names  <- sub("/", "", plate_names)
time_tag  <- ":[0-9.]{1,}"
time  <- str_extract(df_V1, time_tag)
time  <- sub(":", "", time)
df.N2 <- cbind(plate_names, time, N2[,c(2:7)])
N2 <- df.N2
rm(df.N2)

##clean up the workspace
rm(time, time_tag, plate_names, plate_tag, df_V1)

##rename columns  
colnames(N2) <- c("plate", "time", "ID", "bias", "speed", "morphwidth", "midline", "area")