##load MWT data
##fromfile <- read.table("data/chore_data/merged.file")
##for using merge.file as an input into the following function - extract.col(read.table("data/chore_data/merged.file"))
extract.col <- function(data){
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
  colnames(data) <- c("date", "plate", "time", "strain", "frame", "ID", "number", "goodnumber", "persistance", "area", "speed", "angularspeed", "length", "rellength", "width", "relwidth", "aspect", "relaspect", "midline", "morphwidth", "kink", "bias", "pathlen", "curve", "dir", "loc_x", "loc_y", "vel_x", "vel_y", "orient", "crab")
  
  return(data)
  
}

##using function to extract column names
parsed.data  <- extract.col(read.table("data/chore_data/merged.file"))

## save data as a file
write.table(parsed.data, file="data/chore_data/merged.file.parsed", col.names=TRUE, row.names=FALSE, quote=FALSE, append=FALSE)
