##load MWT data
setwd("data/chore_data")
data <- read.table("merged.file")

## split up column V1 into identifier and time
library(stringr)
df_V1 <- data$V1
plate_tag  <- "2[0-9]{8}"
plate_names  <- str_extract(V1, plate_tag)
## plate_names  <- sub("/", "", plate_names)
time_tag  <- ":[0-9.]{1,}"
time  <- str_extract(df_V1, time_tag)
time  <- sub(":", "", time)
df.data <- cbind(plate_names, time, data[,c(2:7)])
data <- df.data
rm(df.data)

##clean up the workspace
rm(time, time_tag, plate_names, plate_tag, df_V1)

##rename columns  
colnames(data) <- c("plate", "time", "ID", "bias", "speed", "morphwidth", "midline", "area")

## save data as a file