##load MWT data
##example input at command line
##rscript bin/Column_identification_command.R "/Users/catrinaloucks/Documents/PhD/MWT_Locomotion_summary_analysis/data/chore_data/merged.file" 

main <- function() {
  
  args <- commandArgs(trailingOnly = TRUE)
  file <- args[1]
  
  require(ggplot2)
  require(plyr)
  
  ##using function to extract column names and change time column from factor to numeric
  parsed.data  <- extract.col(read.table(file))
  
  ## save data as a file
  write.table(parsed.data, file=paste(file,".parsed", sep=""), col.names=TRUE, row.names=FALSE, quote=FALSE, append=FALSE)
  
  ## call function to call speed vs. time
  plot.speed.time(parsed.data)
  
  ## use function to get mean size data for each worm (mean size data from 60 to 70s)
  mean.size.data <- mean.size(parsed.data)
  
  ## make and save violin plot of worm area with jittered points (using mean size data)
  violinplot.area(mean.size.data)
  
  ## make and save violin plot of worm length with jittered points (using mean size data)
  violinplot.length(mean.size.data)
  
  ## make and save violin plot of worm width with jittered points (using mean size data)
  violinplot.width(mean.size.data)
  
  ## use function to get pathlength data for each worm (pathlength over 530 to 590s)
  mean.pathlength.data <- mean.pathlength(parsed.data)
  
  ## make and save violin plot of worm pathlength data with jittered points
  violinplot.pathlength(mean.pathlength.data)
}


##function for creating choreography output file with column names

extract.col <- function(data){
  ## split up column V1 into date, plate, time and strain 
  library(stringr)
  date <- str_extract(data$V1, "[0-9]{8}")
  plate <- str_extract(data$V1, "[0-9]{8}_[0-9]{6}")
  time <- str_extract(data$V1, "[0-9]+[.][0-9]+")
  strain <- str_extract(data$V1,"[A-Za-z]+[-]?[0-9]+")
  
  ## combine new columns with merged file
  new.data <- cbind(date, plate, time, strain, data[,2:dim(data)[2]])  
  
  ##rename columns  
  colnames(new.data) <- c("date", "plate", "time", "strain", "frame", "ID", "number", "goodnumber", "persistance", "area", "speed", "angularspeed", "length", "rellength", "width", "relwidth", "aspect", "relaspect", "midline", "morphwidth", "kink", "bias", "pathlen", "curve", "dir", "loc_x", "loc_y", "vel_x", "vel_y", "orient", "crab")
  
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

## given parsed data return df with mean area, length, and width (from 60-70s) of each worm (including strain)
## Question: ask about order of aggregation (run with plate then withplate <- mean.size(parsed.data), vs
##                                     run without plate then withoutplate <- mean.size(parsed.data))
mean.size <- function(parsed) {
  
  ## subset parsed data to times between 60 seconds and 70 seconds
  time.subset <- parsed[parsed$time < 70 & parsed$time > 60, ]
  
  ## aggregate mean area, length, and width with each worm (ID), retaining strain and plate info
  mean.subset <- aggregate(cbind(area, length, width) ~ ID + strain + plate, time.subset, mean)  
  
  return(mean.subset)

}

## given size means, make body area violin plot
violinplot.area <- function(mean.size.output) {
  
  g <- ggplot(mean.size.output, aes(x = strain, y = area)) + ## plot lengths
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background grey
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3)) + ## change the y-axis label font to black, make larger, and move away from axis
    ggtitle("Violin Plot of Worm Area") +            ## set title
    labs(x="Strain", y=expression(Area ~ (mm^{2}))) +     ## label the x and y axes 
    geom_violin(alpha=0.5, color="gray", fill='#F0FFFF') +  ## overlay violin plot
    geom_jitter(alpha = 0.5, position = position_jitter(width = 0.05), size = 3) +  ## overlay jitter plot
    geom_errorbar(stat = "hline", yintercept = "median", width=0.4,aes(ymax=..y..,ymin=..y..)) ## overlay mean line
  
  ##save plot
  ggsave(file="results/violinplot_area.pdf", g, height = 3, width = 5)
}

## given size means, make body length violin plot
violinplot.length <- function(mean.size.output) {
  
  g <- ggplot(mean.size.output, aes(x = strain, y = length)) + ## plot lengths
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background grey
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3)) + ## change the y-axis label font to black, make larger, and move away from axis
    ggtitle("Violin Plot of Worm Length") +            ## set title
    labs(x="Strain", y="Length (mm)") +     ## label the x and y axes 
    geom_violin(alpha=0.5, color="gray", fill='#F0FFFF') +  ## overlay violin plot
    geom_jitter(alpha = 0.5, position = position_jitter(width = 0.05), size = 3) +  ## overlay jitter plot
    geom_errorbar(stat = "hline", yintercept = "median", width=0.4,aes(ymax=..y..,ymin=..y..)) ## overlay mean line
  
  ##save plot
  ggsave(file="results/violinplot_length.pdf", g, height = 3, width = 5)
}

## make body width violin plot
violinplot.width <- function(mean.size.output) {
  
  ## make plot
  g <- ggplot(mean.size.output, aes(x = strain, y = width)) + ## plot widths
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background grey
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3)) + ## change the y-axis label font to black, make larger, and move away from axis
    ggtitle("Violin Plot of Worm Width") +            ## set title
    labs(x="Strain", y="Width (mm)") +     ## label the x and y axes 
    geom_violin(alpha=0.5, color="gray", fill='#F0FFFF') +  ## overlay violin plot
    geom_jitter(alpha = 0.5, position = position_jitter(width = 0.05), size = 3) + ## overlay jitter plot
    geom_errorbar(stat = "hline", yintercept = "median", width=0.4,aes(ymax=..y..,ymin=..y..)) ## overlay mean line
  
  ##save plot
  ggsave(file="results/violinplot_width.pdf", g, height = 3, width = 5)
}

## given parsed data return data frame with mean pathlength (from 530 - 590s) for each worm, with ID, strain, and plate
mean.pathlength <- function(parsed) {
  
#   ## nested function: given matrix or df of loc_x and loc_y, return total distance
#   ## note: this implementation is noticeably slower, uses base dist function to find distance between every point, not just consecutive points
#   pathlength <- function(xy) {
#     out <- as.matrix(dist(xy))
#     sum(out[row(out) - col(out) == 1])
#   }
  
  ## nested function: given matrix or df of loc_x and loc_y, return total distance
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
  
  ## subset parsed data to times between 530 and 590 seconds
  time.subset <- parsed[parsed$time > 530 & parsed$time < 590, ]
  
  ## aggregate data with pathlength function, grouping by ID, strain, and plate
  pathlength.subset <- ddply(time.subset, c("ID", "strain", "plate"), summarise,
                pathlength = pathlength(cbind(loc_x,loc_y)))
  
  return(pathlength.subset)
  
}

## given mean pathlength data make violin plot
violinplot.pathlength <- function(mean.pathlength) {
  
  g <- ggplot(mean.pathlength, aes(x = strain, y = pathlength)) + ## plot pathlengths
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background grey
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3)) + ## change the y-axis label font to black, make larger, and move away from axis
    ggtitle("Violin Plot of Worm Pathlength") +            ## set title
    labs(x="Strain", y= "Pathlength from 530s to 590s (mm)") +     ## label the x and y axes 
    geom_violin(alpha=0.5, color="gray", fill='#F0FFFF') +  ## overlay violin plot
    geom_jitter(alpha = 0.5, position = position_jitter(width = 0.05), size = 3) +  ## overlay jitter plot
    geom_errorbar(stat = "hline", yintercept = "median", width=0.4,aes(ymax=..y..,ymin=..y..)) ## overlay mean line
  
  ##save plot
  ggsave(file="results/violinplot_pathlength.pdf", g, height = 3, width = 5)
}

main()