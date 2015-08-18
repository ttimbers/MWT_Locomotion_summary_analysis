##load MWT data
##example input at command line
##rscript bin/Column_identification_command.R "/Users/catrinaloucks/Documents/PhD/MWT_Locomotion_summary_analysis/data/chore_data/merged.file" 

main <- function() {
  
  args <- commandArgs(trailingOnly = TRUE)
  file <- args[1]
  controlStrain <- args[2]
  
  ## Check if required packages are installed; if they are they will be loaded; if not they will be installed and loaded
  
  if(!require("ggplot2")) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  if(!require("grid")) {
    install.packages("grid")
    library(fmsb)
  }
  if(!require("plyr")) {
    install.packages("plyr")
    library(plyr)
  }
  if(!require("stringr")) {
    install.packages("stringr")
    library(stringr)
  }
  if(!require("asbio")) {
    install.packages("asbio")
    library(asbio)
  }
  if(!require("fmsb")) {
    install.packages("fmsb")
    library(fmsb)
  }
  
  ##use function to extract column names and change time column from factor to numeric
  parsed.data  <- extract.col(read.table(file))
  
  ## save data as a file
  write.table(parsed.data, file=paste(file,".parsed", sep=""), col.names=TRUE, row.names=FALSE, quote=FALSE, append=FALSE)
    
  ## check if control strain is valid; if invalid, prompt user to input another strain
  ## and repeat check until valid strain is given.
  ## If setControlStrain does not return a factor, then the control strain given was not found
  ## in parsed.data and is thus invalid
  while (!class(setControlStrain(controlStrain, parsed.data)) == "factor") {
    cat("Please enter a valid control strain (without quotations): ")
    controlStrain <- readLines(file("stdin"),1)
  }
  
  ## make control strain the first factor so it is plotted first 
  parsed.data$strain <- setControlStrain(controlStrain, parsed.data)

  ## call function to call speed vs. time
  plot.speed.time(parsed.data)
  
  ##=========================================================================================================
  ## BODY SIZE PLOTS
  ##=========================================================================================================
  
  ## use function to get length, width, and area of each worm, averaged over 60 to 70s
  mean.size.data <- mean.size(parsed.data, 60, 70)
  
  ## make and save plot of worm area (box plot overlayed with violin plot + jittered points)
  makeBoxPlot(mean.size.data, "area", expression(Area~(mm^{2})))
  
  ## make and save plot of worm length (box plot overlayed with violin plot + jittered points)
  makeBoxPlot(mean.size.data, "length", "Length (mm)")
  
  ## make and save plot of worm width (box plot overlayed with violin plot + jittered points)
  makeBoxPlot(mean.size.data, "width", "Width (mm)")
  
  ##=========================================================================================================
  ## PATHLENGTH PLOT
  ##=========================================================================================================
  
  ## get pathlength data for each worm from 530 to 590s
  pathlength.data <- aggregatePathlength(parsed.data, 530, 590)
  
  ## make and save plot of worm pathlength (box plot overlayed with violin plot + jittered points)
  makeBoxPlot(pathlength.data, "pathlength", "Pathlength (mm)", "from 530 to 590s")
  
  ##=========================================================================================================
  ## TOTAL DISTANCE PLOT
  ##=========================================================================================================
  
  ## use function to get total distance data for each worm (from 530s to 590s)
  distance.data <- aggregateDistance(parsed.data, 530, 590)
  
  ## make and save plot of worm distance travelled (box plot overlayed with violin plot + jittered points)
  makeBoxPlot(distance.data, "distance", "Distance (mm)", "from 530 to 590s")
  
  ##=========================================================================================================
  ## PATH PLOT
  ##=========================================================================================================
  
  ## make dataframe with adjusted path data from 100 to 160s (where (x,y) is shifted to (0,0) 
  ## at the start of the time interval), and from 530 to 590s
  adjusted.path.data <- rbind(adjusted.path(parsed.data, 100, 160), 
                              adjusted.path(parsed.data, 530, 590))
  
  ## replace duplicate IDs between plates with unique IDs
  adjusted.path.data <- uniqueID(adjusted.path.data)
  
  ## make and save path plot of each worm, separated by strain and time period
  plot.path(adjusted.path.data)
  
  ##=========================================================================================================
  ## RADAR PLOT (MEDIAN)
  ##=========================================================================================================
  
  ## save radar plot of medians of each strain
  makeRadarPlots(mean.size.data, pathlength.data, distance.data)
  
}


##=========================================================================================================
## FUNCTION TO PARSE DATA
##=========================================================================================================

##function for creating choreography output file with column names

extract.col <- function(data){
  ## split up column V1 into date, plate, time and strain 
  date <- str_extract(data$V1, "[0-9]{8}")
  plate <- str_extract(data$V1, "[0-9]{8}_[0-9]{6}")
  time <- str_extract(data$V1, ":[0-9]+[.][0-9]+")
  time <- sub(":", "", time)
  strain <- str_extract(data$V1,"[A-Za-z]+[-]?[0-9]+")
  
  ## combine new columns with merged file
  new.data <- cbind(date, plate, time, strain, data[,2:dim(data)[2]])  
  
  ##rename columns  
  colnames(new.data) <- c("date", "plate", "time", "strain", "frame", "ID", "persistance", "area", "speed", "angularspeed", "length", "rellength", "width", "relwidth", "aspect", "relaspect", "midline", "morphwidth", "kink", "bias", "pathlen", "curve", "dir", "loc_x", "loc_y", "vel_x", "vel_y", "orient", "crab")
  
  ##replace time column (factor) with time as numeric
  new.data$time  <- as.numeric(levels(new.data$time))[new.data$time]
  
  return(new.data)
  
}

##=========================================================================================================
## CONTROL STRAIN FUNCTION
##=========================================================================================================

## INPUT:   cstrain = control strain as a string
##          parsedData = output of extract.col; a dataframe with strain data, for which the column is named
##                       as "strain"
## OUTPUT:  Returns the strain factor of parsedData with given control strain as first level
##
## If control strain is not found in parsed data, function will not return a factor and will print
## an error message.
setControlStrain <- function(cstrain, parsedData) {
  out <- tryCatch(
{
  strainLevels <- levels(parsedData$strain)                  ## get strains in parsed.data
  if (!cstrain %in% strainLevels) stop()                      ## if control strain is not in strains throw stop()
  strainLevels <- strainLevels[strainLevels != cstrain]       ## otherwise drop cstrain from strains
  strainLevels <- append(cstrain, strainLevels)               ## and re-add at start of strains
  
  factor(parsedData$strain, levels = strainLevels)           ## returns parsed.data strain factor (upon return(out))
},
error=function(cond) {                                        ## catch the stop()
  message(paste(cstrain, "is not a valid strain."))           
  message(paste("Valid strains include:", toString(strainLevels)))      ## print valid strains for user, do not return a factor
}
  )    
return(out)
}

##=========================================================================================================
## FUNCTION FOR PLOTTING TIME VS SPEED
##=========================================================================================================

plot.speed.time <- function(dataframe) {
  
  ##plot speed decay over time  
  ##bin into time intervals to make it quicker to plot (average speed over every 20s for 10 min)
  
  ##divide time into intervals (e.g. 20-40) to the last time point
  cut1 <- cut(dataframe$time, breaks=seq(0, max(dataframe$time), by = 20))
  
  ##extract intervals as the max of the interval (e.g. 40 from 20-40)
  time.interval <- as.numeric(str_extract(cut1, "[1-9]{1}[0-9]+"))
  
  dataframe.tint <- dataframe
  
  ##replace time column with the time interval (upper limit of time period)
  dataframe.tint$time <- time.interval
  
  ##get rid of data from 0-40s of the experiment (sometimes the tracker doesn't start tracking 
  ##until 15s into the experiment)
  dataframe.tint  <- dataframe.tint[which(dataframe.tint$time>40),]
  
  ##average over each plate for each time period
  speed.tint.plate <- ddply(dataframe.tint,.(strain,time,plate),summarise,speed=mean(speed, na.rm=TRUE))
  ##average over each strain for each time period
  speed.tint.plate.strain <- ddply(speed.tint.plate,.(strain,time),summarise,N=length(speed),mean.speed=mean(speed),sd=sd(speed), se=sd/sqrt(N))
  
  ##make plot with error bars
  g  <- ggplot(speed.tint.plate.strain, aes(x = time, y = mean.speed, colour = strain)) + 
    geom_errorbar(aes(ymin=mean.speed-se, ymax=mean.speed+se), width=.1) +
    geom_line(aes(group = strain)) + geom_point() +
    labs(x="Time", y="Speed") +
    theme_bw()
  
  ##save plot
  ggsave(file="results/speedVtime.pdf", g, height = 3, width = 5)
}

##=========================================================================================================
## BODY SIZE FUNCTION
##=========================================================================================================

## SUMMARY: given parsed data return df with mean area, length, and width of each worm
## INPUT: parsedData = data with appropriate column names, with information for worm area, length, width, and time,
##                     as well as worm ID, strain and plate.
##                     These columns should be named as "area", "length", "width", "time", "ID", 
##                     "strain", and "plate", respectively (without quotations).
##        minT = lower limit of time interval to average over
##        maxT = upper limit of time interval to average over
## 
## OUTPUT: A dataframe where rows consist of unique combinations of worm ID, plate, and strain
##         (ie each row has data for a single worm).
##         Columns include ID, plate, strain, 
##         as well as the length, width, and area averaged over the time interval specified.
##
## Input data will be subsetted to the time interval specified. 
## The width, length, and area of each worm will be averaged over this time interval.
## In order to do this for each worm, the data is aggregated by worm ID, strain, and plate
## This is necessary as worm IDs are not unique between plates and strains.
mean.size <- function(parsedData, minT, maxT) {
  
  ## subset parsed data to times between minT and maxT
  time.subset <- parsedData[parsedData$time < maxT & parsedData$time > minT, ]
  
  ## aggregate mean area, length, and width, grouped by ID, strain, and plate
  mean.subset <- aggregate(cbind(area, length, width) ~ ID + strain + plate, time.subset, mean)  
  
  return(mean.subset)
  
}

##=========================================================================================================
## PATHLENGTH FUNCTIONS
##=========================================================================================================

## SUMMARY: Given a a vector of pathlengths, return change in pathlength between end and beginning of vector
##          eg: find pathlength from 530s to 590s by subtracting pathlength at 530s from pathlength at 590s
## INPUT: pathlens = vector of numbers, where each number is a pathlength. The first number should be
##                   the first pathlength, and the last number should be the last pathlength.
## OUTPUT: Last pathlength - first pathlength (change in pathlength), or NA if any pathlengths are missing.
##
## If any pathlengths are missing in the vector, the change in pathlength cannot be calculated, 
## as the next number after the missing pathlength will begin at 0.
pathlength <- function(pathlens) {
  
  if (any(is.na(pathlens))) {    ## if there are any missing pathlengths, we cannot find the pathlength over the time interval,
    return(NA)                   ## as the next pathlengths after the NA value(s) will start at 0
  }
  
  else {
    return(pathlens[length(pathlens)] - pathlens[1])  ## subtract final pathlength from initial pathlength
  }
}

## SUMMARY: given parsed data return data frame with pathlength for each worm over specified time interval
## INPUT: parsedData = data with appropriate column names, with information for worm pathlength, ID, strain and plate.
##                     These columns should be named as "pathlength", "ID", "strain", and "plate", respectively.
##        minT = lower limit of time interval to find pathlength over
##        maxT = upper limit of time interval to find pathlength over
## 
## OUTPUT: A dataframe where rows consist of unique combinations of worm ID, plate, and strain
##         (ie each row has data for a single worm).
##         Columns include ID, plate, strain, 
##         as well as the pathlength calculated over the time interval specified.
##
## Input data will be subsetted to the time interval specified. 
## The pathlength for each worm will be calculated over this time interval.
## In order to do this for each worm, the data is aggregated by worm ID, strain, and plate
## This is necessary as worm IDs are not unique between plates and strains.
## The helper function pathlength is used to calculate pathlength.

aggregatePathlength <- function(parsedData, minT, maxT) {
  
  ## subset parsed data to times between minT and max T
  time.subset <- parsedData[parsedData$time > minT & parsedData$time < maxT, ]
  
  ## aggregate data with pathlength function, grouping by ID, strain, and plate
  pathlength.output <- ddply(time.subset, c("ID", "strain", "plate"), summarise,
                             pathlength = pathlength(pathlen))
  
  ## drop rows with NA pathlengths
  pathlength.output <- na.omit(pathlength.output)
  
  return(pathlength.output)
  
}

##=========================================================================================================
## FUNCTIONS FOR TOTAL DISTANCE TRAVELLED
##=========================================================================================================

## SUMMARY: Given matrix of x and y points, return the total distance between each consecutive point
## INPUT: xy = matrix with x values in first column, and y values in 2nd column,
##             where each row corresponds to a point (x,y)
## OUTPUT: total distance between each consecutive point
totalDistance <- function(xy) {
  
  previous.x <- xy[1,1]      ## initiate previous x and y as first x and y values
  previous.y <- xy[1,2]     
  total <- 0                 ## initiate distance as 0
  
  for (i in 2:nrow(xy)) { ## use a for loop to go through each row of matrix (corresponding to an x,y point)
    ## and calculate euclidean distance between each point and the previous point
    ## start at 2 because we initialized previous.x and previous.y with the first values
    
    diff.x <- xy[i,1] - previous.x  ## get difference between current x position and previous x position
    diff.y <- xy[i,2] - previous.y
    
    total <- total + sqrt((diff.x)^2 + (diff.y)^2)  ## calculate diagonal of x and y difference (euclidean)
    ## and add to total distance
    
    previous.x <- xy[i,1]  ## set current x as previous x
    previous.y <- xy[i,2]  ## set current y as previous y
    
  }
  return(total)
}

## SUMMARY: given parsed data return data frame with total distance travelled over specified time interval
## INPUT: parsedData = data with appropriate column names, with information for worm x-location, y-location, 
##                     ID, strain and plate.
##                     These columns should be named as "loc_x", "loc_y", "ID", "strain", and "plate", respectively.
##        minT = lower limit of time interval to find distance travelled over
##        maxT = upper limit of time interval to find distance travelled over
## 
## OUTPUT: A dataframe where rows consist of unique combinations of worm ID, plate, and strain
##         (ie each row has data for a single worm).
##         Columns include ID, plate, strain, 
##         as well as the distance travelled, calculated over the time interval specified.
##
## Input data will be subsetted to the time interval specified. 
## The distance travelled for each worm will be calculated over this time interval.
## In order to do this for each worm, the data is aggregated by worm ID, strain, and plate
## This is necessary as worm IDs are not unique between plates and strains.
## The helper function totalDistance is used to calculate the distance travelled.
aggregateDistance <- function(parsedData, minT, maxT) {
  
  ## subset parsed data to times between minT and maxT seconds
  time.subset <- parsedData[parsedData$time > minT & parsedData$time < maxT, ]
  
  ## aggregate data with distance function, grouping by ID, strain, and plate
  aggDist.output <- ddply(time.subset, c("ID", "strain", "plate"), summarise,
                          distance = totalDistance(cbind(loc_x,loc_y)))
  
  return(aggDist.output)
  
}

##=========================================================================================================
## BOXPLOT/VIOLINPLOT/JITTERED POINT PLOT FUNCTION
##=========================================================================================================

## SUMMARY: Generates figure with boxplot, violin plot and jittered points for a given observation plotted against strain,
##          using the specified y-axis label, and an optional subtitle.
## INPUT: dataframe = A dataframe with a strain column titled "strain" and another column with a specified observation.
##        observation = the observation to be plotted against strain (given as a string). Examples include "pathlength" and "width".
##        ylabel = The label for the y-axis, as a string.
##                 Also accepts expressions (eg: expression(Area~(mm^{2}))).
##        subtitle = subtitle for plot; this argument is optional.
## OUTPUT: saves plot in results folder as plot_observation.pdf
makeBoxPlot <- function(dataframe, observation, ylabel, subtitle) {
  
  ## Capitalize the observation name, for example "area" to "Area"
  ## Approach: get first letter, capitalize it, paste it with the rest of observation string
  ## This is used when making the plot title
  capitalizedObservation <- paste(toupper(substr(observation, 1, 1)),          
                                  substr(observation, 2, nchar(observation)),  
                                  sep = "")                                    
  
  g <- ggplot(dataframe, aes_string(x = "strain", y = observation)) + ## plot observation against strain
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background white
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3)) +  ## change the y-axis label font to black, make larger, and move away from axis
    labs(x="Strain", y= ylabel) +   ## set x and y labels
    geom_violin(alpha=0.8, color="gray", fill='#F0FFFF') +  ## overlay violin plot    
    geom_boxplot(outlier.size = 0)+
    geom_jitter(alpha = 0.7, position = position_jitter(width = 0.05), size = 1.5, colour="gray50") +  ## overlay jitter plot
    scale_x_discrete(labels=  ## overlay x axis labels with # of observations
                       paste(levels(dataframe$strain),
                             "\n(n=",
                             table(dataframe$strain),
                             ")", 
                             sep=""))
  
  if (missing(subtitle)) {                                      ## if subtitle argument is missing
    g <- g + ggtitle(paste("Worm", capitalizedObservation))     ## simply make title
  } else {
    g <- g + ggtitle(bquote(atop(.(paste("Worm", capitalizedObservation)), atop(.(subtitle), ""))))
  }                                         ## otherwise make title with subtitle
                                            ## note that bquote is used to get the title and subtitle values
                                            ## otherwise ggtitle uses them as strings and does not refer to the object values
  
  #save plot
  ggsave(file=paste("results/plot_", observation, ".pdf", sep=""), g, height = 5)
}

##=========================================================================================================
## PATHPLOT FUNCTIONS
##=========================================================================================================

## SUMMARY: given vector of numbers, subtract first number from every number in vector
## INPUT: vector of class numeric
## OUTPUT: vector of class numeric
## This is a helper function used to shift worm positions to start from 0.
## "von" stands for vector of numbers
adjust.n <- function(von) {
  von <- von - von[1]    
  return(von)
}

## SUMMARY: given parsed data, return a dataframe with adjusted x and y positions of each worm so that its
##          initial position is (0,0), for the time interval specified
## INPUT: parsedData = data with appropriate column names, with information for worm x-location, y-location, 
##                     ID, strain and plate.
##                     These columns should be named as "loc_x", "loc_y", "ID", "strain", and "plate", respectively.
##        minT = lower limit of time interval to adjust positions over
##        maxT = upper limit of time interval to adjust positions over
adjusted.path <- function(parsedData, minT, maxT) {
  
  ## subset parsed data to times between t1 and t2 seconds
  time.subset <- parsedData[parsedData$time > minT & parsedData$time < maxT, ]
  
  # summarize dataframe, by shifting x and y values to start from 0 for each worm (grouped by ID, plate, and strain)
  adjusted.path.output <- ddply(time.subset, cbind("ID", "plate", "strain"), summarize,
                                adj_x = adjust.n(loc_x),
                                adj_y = adjust.n(loc_y))
  
  # Add column with info on time period
  adjusted.path.output$timeperiod <- paste(minT, "s to ", maxT, "s", sep = "")
  
  return(adjusted.path.output)
}

## SUMMARY: Replaces duplicate worm IDs between plates with new IDs
## INPUT: adj.path.output = parsed data with adjusted x and y locations, plate, and strain
##                          columns should be named as "adj_x", "adj_y", "plate", and "strain", respectively.
## OUTPUT: the input dataframe with duplicate IDs replaced with random IDs
## This is necessary to give each worm a unique colour on the plot (by ID), as well as to count the number of unique worms plotted.
## Note that time period is not included as a grouping factor when finding duplicate IDs, as the same worm (and thus same ID) may be
## tracked in multiple time periods.
uniqueID <- function(adj.path.output) {
  
  ## group by ID, plate and strain, and aggregate ID by mean (IDs should be identical in each grouping)
  groups <- ddply(adj.path.output, cbind("ID", "plate", "strain"), summarize, ID = mean(ID))  
  
  ## find aggregated combinations of ID + plate + strain that have duplicate IDs (different plates might have duplicate IDs)
  ## note that we do not include timeperiod here, as the same worm may be tracked at different times giving it the same ID
  duplicateRows <- groups[duplicated(groups$ID),]  
  
  ## if there are duplicate IDs, replace the IDs in adj.path.output with a new unique ID (for each grouping of plate+strain+id)
  if (nrow(duplicateRows) > 0) {              
    
    numberDuplicates <- nrow(duplicateRows)
    
    for (i in 1:numberDuplicates) {  
      
      duplicateRow <- duplicateRows[i,]
      plate <- duplicateRow$plate
      strain <- duplicateRow$strain
      ID <- duplicateRow$ID
      
      adj.path.output[adj.path.output$plate == plate & adj.path.output$strain == strain & adj.path.output$ID == ID,]$ID <- runif(1)
    }
    
    ## use recursion to check if any of the newly assigned random IDs are duplicates
    uniqueID(adj.path.output)
    
  } else {
    
    ## return adj.path.output
    return(adj.path.output)
  }
}

## SUMMARY: given dataframe including adjusted x and y locations, strain, and time period, plot worm locations.
##          with separate plots for each time period and strain. 
## INPUT: adj.path.output = dataframe with adjusted x locations, adjusted y locations, strain, and timeperiod, named as 
##                         "adj_x", "adj_y", "strain", and "timeperiod", respectively. 
## OUTPUT: plots worm paths for each time period and strain, and saves plot in results folder
plot.path <- function(adj.path.output) {
  
  ## change ID to factor so each unique ID (representing a unique worm) can have a distinct colour in ggplot2
  adj.path.output$ID <- as.factor(adj.path.output$ID)
  
  ## make DF with number of worms for each grouping of strain and timeperiod
  pathObsN <- ddply(adj.path.output, cbind("strain", "timeperiod"), summarize, n = paste("n=", length(unique(ID))))
  
  g <- ggplot(data=adj.path.output, aes(x=adj_x, y=adj_y)) + 
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background white
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3), ## change the y-axis label font to black, make larger, and move away from axis
          aspect.ratio = 1, ## set aspect ratio to 1
          panel.margin = unit(2, "lines")) +     ## add margin around facet panels
    ggtitle("Path Plot") +  ## set title
    labs(x = "Relative x position (mm)", 
         y = "Relative y position (mm)") +
    coord_cartesian(xlim = c(-10, 10), ylim=c(-10, 10)) +   ## limit the x and y axes ranges to a constant
    geom_point(size = 0.5, aes(colour=ID)) +  ## overlay points that show worm path
    guides(colour=FALSE) + ## don't show legend for worm ID
    facet_grid(strain ~ timeperiod) +     ## group data by strain and time period then plot each unique grouping
    geom_text(data=pathObsN, aes(x=0, y=-9, label=n),   ## overlay number of worms
              colour="black", size = 3)
  
  ##save plot
  ggsave(file="results/path_plot.pdf", g)
}

##=========================================================================================================
## RADARPLOT FUNCTIONS
##=========================================================================================================

makeRadarPlots <- function(mean.size.output, aggPath.output, aggDist.output) {
  
  ## get strains from mean.size.output (could have used path/distance dataframes, should all be the same)
  strainLevels <- levels(mean.size.output$strain)
  
  ## we will create a dataframe with features as columns (ie width, pathlength, distance, speed, etc.), 
  ## which will have the median values for each strain.
  ## here we initialize the columns as empty
  ## we also make a strain column so we can keep track of which medians belong to which strain
  strain <- c()
  width <- c()
  length <- c()
  area <- c()
  pathlength <- c()
  distance <- c()
  
  ## then we loop through the strains, and add median values to the columns (as well as the strain)
  for (i in 1:length(strainLevels)) {
    
    sizes <- mean.size.output[mean.size.output$strain == strainLevels[i],]
    pathlens <- aggPath.output[aggPath.output$strain == strainLevels[i],]
    distances <- aggDist.output[aggDist.output$strain == strainLevels[i],]
    
    strain <- c(strain, strainLevels[i])
    width <- c(width, median(sizes$width))
    length <- c(length, median(sizes$length))
    area <- c(area, median(sizes$area))
    pathlength <- c(pathlength, median(pathlens$pathlen))
    distance <- c(distance, median(distances$distance))
  }
  
  ## construct the dataframe with median values for each feature and the strain
  df <- data.frame(strain, width, length, area, pathlength, distance)
  
  ## construct a dataframe with the maximum and minimum values for the radar plot (see maxmin in ?radarchart)
  maxmindf <- data.frame(
    width = c(0.5, 0),
    length = c(1.2, 0),
    area = c(0.25, 0),
    pathlength = c(0.25, 0),
    distance = c(4, 0))
  
  ## write to PDF
  pdf("results/radar_plot.pdf")
  
  ## n2mfrow(length(strainLevels)) automatically chooses good values for mfrow based on # of strains
  ## margins are specified so that plots are not cutoff, and the title is not too high
  ## xpd = TRUE is needed so that peripheral labels are not cutoff
  par(mfrow=rev(n2mfrow(length(strainLevels))), mar=c(8,2,8,2), xpd = TRUE)

  ## apply radarchart function to each row of the df (where each row has data for 1 strain)
  ## we access the features using df[i,-1], where the -1 drops the strain name
  ## we combine the df with the maxmindf to make a new df:
  ## first row is the maximums, 2nd row is minimums, and 3rd row is features to plot
  lapply(1:length(strainLevels), function(i) {
    radarchart(rbind(maxmindf, df[i,-1]), 
               axistype = 2,               ## see ?radarchart
               seg = 5, 
               centerzero = TRUE, 
               palcex = 0.9,
               title = paste(toString(df[i,1]), "Medians", sep = " "),  ## get the strain name from the df
               vlabels = c("Width", "Length        ", "Area", "Pathlength", "         Distance"))
    ## Note: the labels are manually specified with whitespace so that they do not overlap with the chart. 
    ## These must be adjusted if the number of features plotted changes.
  })
  
  dev.off()
}

main()