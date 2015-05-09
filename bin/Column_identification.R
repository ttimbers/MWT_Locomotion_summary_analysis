##load MWT data
##example input at command line
##rscript bin/Column_identification_command.R "/Users/catrinaloucks/Documents/PhD/MWT_Locomotion_summary_analysis/data/chore_data/merged.file" 

main <- function() {
  
  args <- commandArgs(TRUE)
  
  ##using function to extract column names
  parsed.data  <- extract.col(read.table("args[1]"))
  
  ## save data as a file
  write.table(parsed.data, file="args[1].parsed", col.names=TRUE, row.names=FALSE, quote=FALSE, append=FALSE)
  
  ## call script to call speed vs. time

  ## use function to get mean size data for each worm (mean size data from 60 to 70s)
  mean.size.data <- mean.size(parsed.data)
  
  ## use mean size data to make area violin plot (with jittered points)
  violinplot.area(mean.size.data)
  
  ## use mean size data to make length violin plot (with jittered points)
  violinplot.length(mean.size.data)
  
  ## use mean size data to make width violin plot (with jittered points)
  violinplot.width(mean.size.data)
  
  ## use mean size data to make area box plot
  boxplot.area(mean.size.data)
  
  ## use mean size data to make length box plot
  boxplot.length(mean.size.data)
  
  ## use mean size data to make width box plot
  boxplot.width(mean.size.data)
  
  
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
  new.data$time  <- as.numeric(levels(parsed.data$time))[parsed.data$time]
  
  return(new.data)
  
}

##function for plotting time vs. speed

plot.speed.time <- function(parsed.data) {
  
  ##plot speed decay over time  
  ##bin into time intervals to make it quicker to plot (average speed over every 20s for 10 min)
  
  ##divide time into intervals (e.g. 20-40) to the last time point
  cut1 <- cut(parsed.data$time, breaks=seq(0, max(parsed.data$time), by = 20))
  
  ##extract intervals as the max of the interval (e.g. 40 from 20-40)
  time.interval <- as.numeric(str_extract(cut1, "[1-9]{1}[0-9]+"))
  
  parsed.data.tint <- parsed.data
  
  ##replace time column with the time interval (upper limit of time period)
  parsed.data.tint$time <- time.interval
  
  ##get rid of data from 0-40s of the experiment (sometimes the tracker doesn't start tracking 
  ##until 15s into the experiment)
  parsed.data.tint  <- parsed.data.tint[which(parsed.data.tint$time>40),]
  
  library(plyr)
  ##average over each plate for each time period
  speed.tint.plate <- ddply(parsed.data.tint,.(strain,time,plate),summarise,speed=mean(speed, na.rm=TRUE))
  ##average over each strain for each time period
  speed.tint.plate.strain <- ddply(speed.tint.plate,.(strain,time),summarise,N=length(speed),mean.speed=mean(speed),sd=sd(speed), se=sd/sqrt(N))
  
 
  ##make plot with error bars
  require(ggplot2)
  g  <- ggplot(speed.tint.plate.strain, aes(x = time, y = mean.speed, colour = strain)) + 
    geom_errorbar(aes(ymin=mean.speed-se, ymax=mean.speed+se), width=.1) +
    geom_line(aes(group = strain)) + geom_point() +
    labs(x="Time", y="Speed") +
    theme_bw()
  
  g
  
  
  
}

## given parsed data make table with mean area, length, and width (from 60-70s) of each worm (including strain)
mean.size <- function(parsed) {
  
  ##replace time column (factor) with time as numeric
  parsed.data$time  <- as.numeric(levels(parsed$time))[parsed$time]
  
  ## subset parsed data to times between 60 seconds and 70 seconds
  time.subset <- parsed[parsed$time < 70 & parsed$time > 60, ]
  
  ## aggregate mean area, length, and width of each worm with each strain
  mean.subset <- aggregate(cbind(area, length, width) ~ ID + strain, time.subset, mean)

}

## given means, make body area violin plot
## TODO: add axis labels with units, fix title
violinplot.area <- function(mean.subset) {
  
  ## initiate ggplot
  g <- ggplot(mean.subset, aes(x = strain, y = area))
  
  ## make violin plot of area for each strain
  g <- g+ggtitle("Violin Plot of Worm Area")          ## not sure if title is good or even needed..
  g <- g+theme(plot.title = element_text(size=20, face="bold", vjust=2))
  g <- g+geom_violin(alpha=0.5, color="gray") + geom_jitter(alpha = 0.5, position = position_jitter(width = 0.1))
  
  g
}

## given means, make body length violin plot
## TODO: add axis labels with units, fix title
violinplot.length <- function(mean.subset) {
  
  ## initiate ggplot
  g <- ggplot(mean.subset, aes(x = strain, y = length))
  
  ## make violin plot of length for each strain
  g <- g+ggtitle("Violin Plot of Worm Length")
  g <- g+theme(plot.title = element_text(size=20, face="bold", vjust=2))
  g <- g+geom_violin(alpha=0.5, color="gray") + geom_jitter(alpha = 0.5, position = position_jitter(width = 0.1))
  
  g
}

## make body width violin plot
## TODO: add axis labels with units, fix title
violinplot.width <- function(mean.subset) {
  
  ## initiate ggplot
  g <- ggplot(mean.subset, aes(x = strain, y = width))
  
  ## make violin plot of length for each strain
  g <- g+ggtitle("Violin Plot of Worm Width")
  g <- g+theme(plot.title = element_text(size=20, face="bold", vjust=2))
  g <- g+geom_violin(alpha=0.5, color="gray") + geom_jitter(alpha = 0.5, position = position_jitter(width = 0.1))
  
  g
}

## make area boxplot
## TODO: add axis labels with units, fix title
boxplot.area <- function(mean.subset) {
  
  #initiate ggplot
  g <- ggplot(mean.subset, aes(x = strain, y = area))
  
  ## make boxplot
  g <- g+ggtitle("Boxplot of Worm Area")
  g <- g+geom_boxplot()  
  
  g
}

## make length boxplot
## TODO: add axis labels with units, fix title
boxplot.length <- function(mean.subset) {
  
  #initiate ggplot
  g <- ggplot(mean.subset, aes(x = strain, y = length))
  
  ## make boxplot
  g <- g+ggtitle("Boxplot of Worm Length")
  g <- g+geom_boxplot()  
  
  g
}

## make width boxplot
## TODO: add axis labels with units, fix title
boxplot.width <- function(mean.subset) {
  
  #initiate ggplot
  g <- ggplot(mean.subset, aes(x = strain, y = width))
  
  ## make boxplot
  g <- g+ggtitle("Boxplot of Worm Width")
  g <- g+geom_boxplot()  
  
  g
}





##save plot
pdf(file="/Users/michelleroux/Documents/Jesse/2015-01-19/Speed_decay.pdf", width=5, height=5)
plot(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[3])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[3])], pch=18, xlab="Time(s)",ylab="Speed(mm/s)", ylim = c(0,1.4))
points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[2])],col=2,pch=18)
points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[1])],col=4,pch=18)
points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[4])],col=5,pch=18)
points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[5])],col=8,pch=18)
##plot the error bars (standard error)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[3])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[3])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[3])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[3])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[3])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[3])])
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[2])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[2])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[2])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[2])], col=2)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[1])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[1])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[1])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[1])], col=4)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[4])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[4])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[4])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[4])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[4])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[4])], col=5)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[5])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[5])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[5])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[5])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[5])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[5])], col=8)
##plot the legend
legend("topright",bty="n",y.intersp=1,c(as.character(strains[1]),as.character(strains[2]),as.character(strains[3]), as.character(strains[4]), as.character(strains[5])),col=c(1,2,4,5,8),pch=(18))
dev.off()



##summarize body area
df.area <- ddply(plate.data.tint,.(strain,time,plate),summarise,area=mean(area))
df.area <- df.area[which(df.area$time==40),]
df.area.agg <- ddply(df.area,.(strain, time),summarise,N=length(area),mean.area=mean(area),sd=sd(area), se=sd/sqrt(N))
##look at the plot
boxplot(df.area$area~df.area$strain, xlab="Strain",ylab="Body size (mm^2)", ylim = c(0,11))
##save the plot 
pdf("/Users/michelleroux/Documents/Jesse/2015-01-19/body_size_area.pdf", width=6, height=6)
boxplot(df.area$area~df.area$strain, xlab="Strain",ylab="Body size (mm^2)", ylim = c(0,11))
dev.off()

##summarize length
df.length <- ddply(plate.data.tint,.(strain,time,plate),summarise,length=mean(midline))
df.length <- df.length[which(df.length$time==40),]
df.length.agg <- ddply(df.length,.(strain, time),summarise,N=length(length),mean.length=mean(length),sd=sd(length), se=sd/sqrt(N))

##plot the data
boxplot(df.length$length~df.length$strain, xlab="Strain",ylab="Body length (mm)", ylim = c(0,9.5))
##save the data
pdf("/Users/michelleroux/Documents/Jesse/2015-01-19/body_size_length.pdf", width=6, height=6)
boxplot(df.length$length~df.length$strain, xlab="Strain",ylab="Body length (mm)", ylim = c(0,9.5))
dev.off()

##summarize width
df.width <- ddply(plate.data.tint,.(strain,time,plate),summarise,width=mean(morphwidth))
df.width <- df.width[which(df.width$time==40),]
df.width.agg <- ddply(df.width,.(strain, time),summarise,N=length(width),mean.width=mean(width),sd=sd(width), se=sd/sqrt(N))

##plot the data
boxplot(df.width$width~df.width$strain, xlab="Strain",ylab="Body width (mm)")
##save the data
pdf("/Users/michelleroux/Documents/Jesse/2015-01-19/body_size_width.pdf", width=6, height=6)
boxplot(df.width$width~df.width$strain, xlab="Strain",ylab="Body width (mm)")
dev.off()

pdf("/Users/michelleroux/Documents/Jesse/2015-01-19/body_size_all_measures.pdf", width=6, height=12)
par(mfrow=c(3,1))
boxplot(df.area$area~df.area$strain, xlab="Strain",ylab="Body size (mm^2)", ylim = c(0,11))
boxplot(df.length$length~df.length$strain, xlab="Strain",ylab="Body length (mm)", ylim = c(0,9.5))
boxplot(df.width$width~df.width$strain, xlab="Strain",ylab="Body width (mm)")
dev.off()

main()