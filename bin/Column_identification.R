##load MWT data
data <- read.table("data/chore_data/merged.file")

## split up column V1 into date, plate, time and strain 
library(stringr)
date <- str_extract(data$V1, "[0-9]{8}")
plate <- str_extract(data$V1, "[0-9]{8}_[0-9]{6}")
time <- str_extract(data$V1, "[0-9]+[.][0-9]+")
strain <- str_extract(data$V1,"[A-Za-z]+[-]?[0-9]+")

## combine new columns with merged file
new.data <- cbind(date, plate, time, strain, data[,2:dim(data)[2]])
data <- new.data
rm(new.data)

##clean up the workspace
rm(date, plate, time, strain)

##rename columns  
colnames(data) <- c("date", "plate", "time", "strain", "ID", "bias", "speed", "morphwidth", "midline", "area", "loc_x", "loc_y")

## save data as a file
write.table(data, file="data/chore_data/merged.file.parsed", col.names=TRUE, row.names=FALSE, quote=FALSE, append=FALSE)