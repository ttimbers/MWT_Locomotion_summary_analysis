##load MWT data
##example input at command line
##rscript bin/Column_identification_command.R "/Users/catrinaloucks/Documents/PhD/MWT_Locomotion_summary_analysis/data/chore_data/merged.file" 

main <- function() {
  
  args <- commandArgs(trailingOnly = TRUE)
  file <- args[1]
  
  require(ggplot2)
  require(plyr)
  require(stringr)
  require(gridExtra)
  require(asbio)
  
  ##using function to extract column names and change time column from factor to numeric
  parsed.data  <- extract.col(read.table(file))
  
  ## save data as a file
  write.table(parsed.data, file=paste(file,".parsed", sep=""), col.names=TRUE, row.names=FALSE, quote=FALSE, append=FALSE)
  
  ## call function to call speed vs. time
  plot.speed.time(parsed.data)
  
  ##=========================================================================================================
  ## BODY SIZE PLOTS
  ##=========================================================================================================
  
  ## use function to get mean size data for each worm (mean size data from 60 to 70s)
  mean.size.data <- mean.size(parsed.data)
  
  ## make and save violin plot of worm area with jittered points (using mean size data)
  violinplot.area(mean.size.data)
  
  ## make and save violin plot of worm length with jittered points (using mean size data)
  violinplot.length(mean.size.data)
  
  ## make and save violin plot of worm width with jittered points (using mean size data)
  violinplot.width(mean.size.data)
  
  ##=========================================================================================================
  ## PATHLENGTH PLOT
  ##=========================================================================================================
  
  ## use function to get pathlength data for each worm (pathlength over 530 to 590s)
  mean.pathlength.data <- mean.pathlength(parsed.data)
  
  ## make and save violin plot of worm pathlength data with jittered points
  violinplot.pathlength(mean.pathlength.data)
  
  ##=========================================================================================================
  ## PATH PLOT
  ##=========================================================================================================
  
  ## use function to change worm x and y positions to start from 0 (over 530 to 590s)
  adjusted.path.data <- adjusted.path(parsed.data)
  
  ## make and save path plot of each worm from 530 to 590s (separated by strain, starting at (0,0))
  plot.strains(adjusted.path.data)
  
}


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

##function for plotting time vs. speed

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
## MEDIAN CONFIDENCE INTERVAL HELPER FUNCTIONS
##=========================================================================================================

## given list of numbers return upper boundary of 95% confidence interval for the median (using ci.median method)
errorUpper <- function(x){
  ci <- ci.median(x)
  text.ci <- ci[[2]]
  upper <- text.ci[3]
  return(upper)
} 

## given list of numbers return lower boundary of 95% confidence interval for the median (using ci.median method)
errorLower <- function(x){ 
  ci <- ci.median(x)
  text.ci <- ci[[2]]
  lower <- text.ci[2]
  return(lower)
} 

##=========================================================================================================
## BODY SIZE FUNCTIONS
##=========================================================================================================

## given parsed data return df with mean area, length, and width (from 60-70s) of each worm (including strain)
mean.size <- function(dataframe) {
  
  ## subset parsed data to times between 60 seconds and 70 seconds
  time.subset <- dataframe[dataframe$time < 70 & dataframe$time > 60, ]
  
  ## aggregate mean area, length, and width with each worm (ID), retaining strain and plate info
  mean.subset <- aggregate(cbind(area, length, width) ~ ID + strain + plate, time.subset, mean)  
  
  ## get levels of strain
  strainLevels <- levels(mean.subset$strain)
  
  ## if strains includes n2, make this the first factor in levels so it is plotted first
  if ("n2" %in% strainLevels) {                     
    strainLevels <- strainLevels[strainLevels != "n2"]         # remove n2
    strainLevels <- append("n2", strainLevels)   # readd at start
    levels(mean.subset$strain) <- strainLevels
  }
  
  ## if strains includes N2, make this the first factor in levels so it is plotted first
  if ("N2" %in% strainLevels) {                     
    strainLevels <- strainLevels[strainLevels != "N2"]          # remove N2
    strainLevels <- append("N2", strainLevels)                  # readd at start
    levels(mean.subset$strain) <- strainLevels
  }
  
  return(mean.subset)

}

## given size means, make body area violin plot
violinplot.area <- function(mean.size.output) {
  
  g <- ggplot(mean.size.output, aes(x = strain, y = area)) + ## plot lengths
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background white
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3)) +  ## change the y-axis label font to black, make larger, and move away from axis
#           panel.border = element_rect(colour = "gray", fill=NA, size=0.8),
#           panel.grid.major.y = element_line(colour = "gray", size=0.5),
#           panel.grid.minor.y = element_line(colour = "gray", size = 0.5)) +
    ggtitle("Violin Plot of Worm Area") +            ## set title
    labs(x="Strain", y=expression(Area ~ (mm^{2}))) +     ## label the x and y axes 
    geom_violin(alpha=0.8, color="gray", fill='#F0FFFF') +  ## overlay violin plot    
    geom_jitter(alpha = 0.5, position = position_jitter(width = 0.05), size = 3, colour="gray50") +  ## overlay jitter plot
    scale_x_discrete(labels=  ## overlay x axis labels with # of observations
                       paste(levels(mean.size.output$strain),
                             "\n(n=",
                             table(mean.size.output$strain),
                             ")",    ## add number of observations to label on 2nd line
                             sep="")) +  
    stat_summary(fun.ymax = errorUpper, fun.ymin = errorLower, geom = "linerange", size=3.5, colour="black" ) + 
    stat_summary(fun.y=median, geom="point", size=3, color="white")
  
  #save plot
  ggsave(file="results/violinplot_area.pdf", g)
}

## given size means, make body length violin plot
violinplot.length <- function(mean.size.output) {
  
  g <- ggplot(mean.size.output, aes(x = strain, y = length)) + ## plot lengths
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background white
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3)) +  ## change the y-axis label font to black, make larger, and move away from axis
#           panel.border = element_rect(colour = "gray", fill=NA, size=0.8),
#           panel.grid.major.y = element_line(colour = "gray", size=0.5),
#           panel.grid.minor.y = element_line(colour = "gray", size = 0.5)) +
    ggtitle("Violin Plot of Worm Length") +            ## set title
    labs(x="Strain", y="Length (mm)") +     ## label the x and y axes     
    geom_violin(alpha=0.8, color="gray", fill='#F0FFFF') +  ## overlay violin plot    
    geom_jitter(alpha = 0.5, position = position_jitter(width = 0.05), size = 3, colour="gray50") +  ## overlay jitter plot
    scale_x_discrete(labels=  ## overlay x axis labels with # of observations
                       paste(levels(mean.size.output$strain),
                             "\n(n=",table(mean.size.output$strain),")",    ## add number of observations to label on 2nd line
                             sep="")) +  
    stat_summary(fun.ymax = errorUpper, fun.ymin = errorLower, geom = "linerange", size=3.5, colour="black" ) + 
    stat_summary(fun.y=median, geom="point", size=3, color="white")
  
  ##save plot
  ggsave(file="results/violinplot_length.pdf", g)
}

## make body width violin plot
violinplot.width <- function(mean.size.output) {
  
  g <- ggplot(mean.size.output, aes(x = strain, y = width)) + ## plot lengths
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background white
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3)) +  ## change the y-axis label font to black, make larger, and move away from axis
    #           panel.border = element_rect(colour = "gray", fill=NA, size=0.8),
    #           panel.grid.major.y = element_line(colour = "gray", size=0.5),
    #           panel.grid.minor.y = element_line(colour = "gray", size = 0.5)) +
    ggtitle("Violin Plot of Worm Width") +            ## set title
    labs(x="Strain", y="Width (mm)") +     ## label the x and y axes     
    geom_violin(alpha=0.8, color="gray", fill='#F0FFFF') +  ## overlay violin plot    
    geom_jitter(alpha = 0.5, position = position_jitter(width = 0.05), size = 3, colour="gray50") +  ## overlay jitter plot
    scale_x_discrete(labels=  ## overlay x axis labels with # of observations
                       paste(levels(mean.size.output$strain),
                             "\n(n=",table(mean.size.output$strain),")",    ## add number of observations to label on 2nd line
                             sep="")) +  
    stat_summary(fun.ymax = errorUpper, fun.ymin = errorLower, geom = "linerange", size=3.5, colour="black" ) + 
    stat_summary(fun.y=median, geom="point", size=3, color="white")
  
  ##save plot
  ggsave(file="results/violinplot_width.pdf", g)
}

##=========================================================================================================
## PATHLENGTH FUNCTIONS
##=========================================================================================================

## Given matrix or df of loc_x and loc_y, return total distance
## quicker implementation
pathlength <- function(xy) {
  
  previous.x <- xy[1,1]      ## initiate previous x and y as first x and y values
  previous.y <- xy[1,2]     
  total <- 0                 ## initiate pathlength as 0
  
  for (i in 1:nrow(xy)) { ## use a for loop to go through each row of matrix (corresponding to an x,y point)
    ## and calculate euclidean distance between each point and the previous point
    
    diff.x <- xy[i,1] - previous.x  ## get difference between current x position and previous x position
    diff.y <- xy[i,2] - previous.y
    
    total <- total + sqrt((diff.x)^2 + (diff.y)^2)  ## calculate diagonal of x and y difference (euclidean)
    ## and add to total pathlength
    
    previous.x <- xy[i,1]  ## set current x as previous x
    previous.y <- xy[i,2]  ## set current y as previous y
    
  }
  return(total)
}

## slower implementation (uses base dist function to find distance between every point, not just consecutive points)
#   pathlength <- function(xy) {
#     out <- as.matrix(dist(xy))
#     sum(out[row(out) - col(out) == 1])
#   }

## given parsed data return data frame with mean pathlength (from 530 - 590s) for each worm, with ID, strain, and plate
mean.pathlength <- function(dataframe) {
  
  ## subset parsed data to times between 530 and 590 seconds
  time.subset <- dataframe[dataframe$time > 530 & dataframe$time < 590, ]
  
  ## aggregate data with pathlength function, grouping by ID, strain, and plate
  pathlength.output <- ddply(time.subset, c("ID", "strain", "plate"), summarise,
                pathlength = pathlength(cbind(loc_x,loc_y)))
  
  ## get levels of strain
  strainLevels <- levels(pathlength.output$strain)
  
  ## if strains includes n2, make this the first factor in levels so it is plotted first
  if ("n2" %in% strainLevels) {                     
    strainLevels <- strainLevels[strainLevels != "n2"]         # remove n2
    strainLevels <- append("n2", strainLevels)                 # readd at start
    levels(pathlength.output$strain) <- strainLevels
  }
  
  ## if strains includes N2, make this the first factor in levels so it is plotted first
  if ("N2" %in% strainLevels) {                     
    strainLevels <- strainLevels[strainLevels != "N2"]          # remove N2
    strainLevels <- append("N2", strainLevels)                  # readd at start
    levels(pathlength.output$strain) <- strainLevels
  }
  
  return(pathlength.output)
  
}

## given mean pathlength data make violin plot
violinplot.pathlength <- function(mean.pathlength.output) {
  
  g <- ggplot(mean.pathlength.output, aes(x = strain, y = pathlength)) + ## plot pathlengths
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background white
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3)) + ## change the y-axis label font to black, make larger, and move away from axis
    #           panel.border = element_rect(colour = "gray", fill=NA, size=0.8),
    #           panel.grid.major.y = element_line(colour = "gray", size=0.5),
    #           panel.grid.minor.y = element_line(colour = "gray", size = 0.5)) +
    ggtitle("Violin Plot of Worm Pathlength") +            ## set title
    labs(x="Strain", y= "Pathlength from 530s to 590s (mm)") +     ## label the x and y axes 
    geom_violin(alpha=0.8, color="gray", fill='#F0FFFF') +  ## overlay violin plot
    geom_jitter(alpha = 0.5, position = position_jitter(width = 0.05), size = 3, colour="gray50") +  ## overlay jitter plot
    scale_x_discrete(labels=  ## overlay x axis labels with # of observations
                       paste(levels(mean.pathlength.output$strain),
                             "\n(n=",table(mean.pathlength.output$strain),")",    ## add number of observations to label on 2nd line
                             sep="")) +  
    stat_summary(fun.ymax = errorUpper, fun.ymin = errorLower, geom = "linerange", size=3.5, colour="black" ) + 
    stat_summary(fun.y=median, geom="point", size=3, color="white")
  
  ##save plot
  ggsave(file="results/violinplot_pathlength.pdf", g)
}

##=========================================================================================================
## PATHPLOT FUNCTIONS
##=========================================================================================================

## given dataframe with x locations, adjust initial x to 0 and following x-s accordingly
adjust.x <- function(df.x) {
  df.x <- df.x - df.x[1]      ## subtract initial x from every x in dataframe
  return(df.x)
}

## given dataframe with y locations, adjust initial y to 0 and following y's accordingly
adjust.y <- function(df.y) {
  df.y <- df.y - df.y[1]     ## subtract initial y from every y in dataframe
  return(df.y)
}

## given parsed data return dataframe with adjusted x and y locations of each worm from 530 to 590s
## grouped by ID, strain, and plate.
## The x and y locations are adjusted for each worm so that it's initial position is (0,0)
## and following positions are adjusted accordingly
adjusted.path <- function(dataframe) {
  
  ## subset parsed data to times between 530 and 590 seconds
  time.subset <- dataframe[dataframe$time > 530 & dataframe$time < 590, ]
  
  adjusted.path.output <- ddply(time.subset, cbind("ID", "plate", "strain"), transform,
        adj_x = adjust.x(loc_x),
        adj_y = adjust.y(loc_y))
  
  return(adjusted.path.output)
}

## given dataframe of a single strain with adjusted x and y locations, replace duplicate IDs between plates with unique IDs
uniqueID <- function(toPlot) {
  
  ## group by ID, plate and strain, and aggregate ID by mean (IDs should be identical in each grouping)
  groups <- ddply(toPlot, cbind("ID", "plate", "strain"), summarize, ID = mean(ID))  

  ## find aggregated combinations of ID + plate + strain that have duplicate IDs (different plates might have duplicate IDs)
  duplicateRows <- groups[duplicated(groups$ID),]  
  
  ## if there are duplicate IDs, replace the IDs in toPlot with a new unique ID (for each grouping of plate+strain+id)
  if (nrow(duplicateRows) > 0) {              
    
    numberDuplicates <- nrow(duplicateRows)
    
    for (i in 1:numberDuplicates) {  
      
      duplicateRow <- duplicateRows[i,]
      plate <- duplicateRow$plate
      strain <- duplicateRow$strain
      ID <- duplicateRow$ID
  
      toPlot[toPlot$plate == plate & toPlot$strain == strain & toPlot$ID == ID,]$ID <- runif(1)
    }
    
    ## use recursion to check if any of the newly assigned random IDs are duplicates
    uniqueID(toPlot)
    
  } else {
    
    ## change ID to factor so each unique ID (representing a unique worm) can have a distinct colour in ggplot2
    toPlot$ID <- as.factor(toPlot$ID)
    
    ## return toPlot
    return(toPlot)
  }
}

## given dataframe of a single strain with adjusted x and y locations, plot worm paths starting from (0,0)
plot.path <- function(toPlot) {
  
  ## replace duplicate IDs between plates with unique IDs so each worm plotted can have a distinct colour
  toPlot <- uniqueID(toPlot)
  
  g <- ggplot(data=toPlot, aes(x=adj_x, y=adj_y)) + 
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background white
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3), ## change the y-axis label font to black, make larger, and move away from axis
          aspect.ratio = 1) + ## set aspect ratio to 1
    ggtitle(paste(unique(toPlot$strain), "Path Plot")) +  ## set title
    labs(x = 
           paste("Relative x position (mm)", 
                 "\n(n=" ,length(unique(toPlot$ID)), ")",
                 sep=""), 
         y = "Relative y position (mm)") +
    coord_cartesian(xlim = c(-8, 8), ylim=c(-8, 8)) +   ## limit the x and y axes ranges to a constant
    geom_point(size = 0.5, aes(colour=ID)) +  ## overlay points that show worm path
    guides(colour=FALSE) ## don't show legend for worm ID
}

## given parsed data with adjusted x and y locations for ALL strains, make plots for all strains and save as single file
plot.strains <- function(adjusted.path.output) {
  
  ## get levels of strain
  strainLevels <- levels(adjusted.path.output$strain)
  
  ## if strains includes n2, make this the first factor in levels so it is plotted first
  if ("n2" %in% strainLevels) {                     
    strainLevels <- strainLevels[strainLevels != "n2"]         # remove n2
    strainLevels <- append("n2", strainLevels)                 # readd at start
    levels(adjusted.path.output$strain) <- strainLevels        # update levels with new order
  }
  
  ## if strains includes N2, make this the first factor in levels so it is plotted first
  if ("N2" %in% strainLevels) {                     
    strainLevels <- strainLevels[strainLevels != "N2"]         # remove N2
    strainLevels <- append("N2", strainLevels)                 # readd at start
    levels(adjusted.path.output$strain) <- strainLevels        # update levels with new order
  }
  
  plotList <- list()  #initialize list of plots as empty
  
  ## create path plots for each strain
  for (i in 1:length(strainLevels)) {
    toPlot <- adjusted.path.output[adjusted.path.output$strain == strainLevels[i],]   # subset adjusted path data for strain
    plotName <- paste("plot", i, sep="")   # make arbitrary unique plot name
    assign(plotName, plot.path(toPlot))    # assign path plot of specific strain to plot name
    plotList[[i]] <- get(plotName)         # add plot to list of plots
  }
  
  ## make arguments (with list of plots) to be arranged by gridExtra
  arrangeArgs <- c(plotList, ncol=2)
  
  ## make plot with N2 as first plot
  g <- do.call(arrangeGrob, arrangeArgs) 
  
  ##save plot
  ggsave(file="results/path_plot.pdf", g)
  
}

main()