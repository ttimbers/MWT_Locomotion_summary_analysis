## DEPRECATED CODE


## =======================================================================================
## OLD TOTAL DISTANCE FUNCTION
## =======================================================================================

# slower implementation (uses base dist function to find distance between every point, not just consecutive points)
  totalDistance <- function(xy) {
    out <- as.matrix(dist(xy))
    sum(out[row(out) - col(out) == 1])
  }

## =======================================================================================
## OLD PATH PLOT FUNCTIONS
## =======================================================================================

## Old implementation where I didn't know about facet wrapping, and subsetted manually to make
## separate plots by strain

## for each strain subset, plots were generated "on the fly" in the "main" pathplot function: 
## plot.strains() and arranged using the gridExtra package

## Also, the timeperiod was kept track of for each plot by making it the plot object name
## rather than adding it directly to the dataframe as a variable and values
## (see plot.strains())

## Previous calls that were made in main() using this code:

#         ## use function to change worm x and y positions from 100 to 160s to start from 0
#         adjusted.path.data.start <- adjusted.path(parsed.data, 100, 160)

#         ## use function to change worm x and y positions from 530 to 560s to start from 0
#         adjusted.path.data.end <- adjusted.path(parsed.data, 530, 560)

#         ## make and save path plot of each worm (separated by strain, starting at (0,0))
#         plot.strains(adjusted.path.data.start, adjusted.path.data.end)

if(!require("gridExtra")) {
  install.packages("gridExtra")
  library(gridExtra)
}

## given parsed data return dataframe with adjusted x and y locations of each worm from 530 to 590s
## grouped by ID, strain, and plate.
## The x and y locations are adjusted for each worm so that it's initial position is (0,0)
## and following positions are adjusted accordingly
adjusted.path <- function(dataframe, t1, t2) {
  
  ## subset parsed data to times between t1 and t2 seconds
  time.subset <- dataframe[dataframe$time > t1 & dataframe$time < t2, ]
  
  ## transform dataframe, by shifting x and y values to start from 0 for each worm (grouped by ID, plate, and strain)
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
## second argument will be "start" or "end"; indicating if the plot is for 100s to 160s or for 530s to 590s
plot.path <- function(toPlot, string) {
  
  ## replace duplicate IDs between plates with unique IDs so each worm plotted can have a distinct colour
  toPlot <- uniqueID(toPlot)
  
  if ("start" == string) {
    timeperiod <- "from 100s to 160s"
  }
  
  if ("end" == string) {
    timeperiod <- "from 530s to 590s"
  }
  
  g <- ggplot(data=toPlot, aes(x=adj_x, y=adj_y)) + 
    theme(plot.title = element_text(size=20, face="bold", vjust=2), ## make the plot title larger and higher
          panel.background = element_rect(fill = "white"), ## make the plot background white
          axis.text.x=element_text(colour="black", size = 12), ## change the x-axis values font to black
          axis.text.y=element_text(colour="black", size = 12), ## change the y-axis values font to black and make larger
          axis.title.x = element_text(size = 16, vjust = -0.2), ## change the x-axis label font to black, make larger, and move away from axis
          axis.title.y = element_text(size = 16, vjust = 1.3), ## change the y-axis label font to black, make larger, and move away from axis
          aspect.ratio = 1) + ## set aspect ratio to 1
    ggtitle(bquote(atop(.(paste(unique(toPlot$strain), "Path Plot")), atop(.(timeperiod), "")))) +
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
plot.strains <- function(path.output.start, path.output.end) {
  
  ## get strain levels
  strainLevels <- levels(path.output.start$strain) 
  
  #initialize list of plots for the start period (100 - 160s) as empty
  startplotList <- list()  
  
  ## initalize list of plots for the end period (530s to 590s) as empty
  endplotList <- list()  
  
  ## create path plots for each strain from 100s to 160s
  for (i in 1:length(strainLevels)) {
    toPlot <- path.output.start[path.output.start$strain == strainLevels[i],]   # subset adjusted path data for strain
    plotName <- paste("startplot", i, sep="")   # make arbitrary unique plot name
    assign(plotName, plot.path(toPlot, "start"))    # assign path plot of specific strain to plot name
    startplotList[[i]] <- get(plotName)         # add plot to list of plots
  }
  
  ## create path plots for each strain from 530s to 590s
  for (i in 1:length(strainLevels)) {
    toPlot <- path.output.end[path.output.end$strain == strainLevels[i],]   # subset adjusted path data for strain
    plotName <- paste("endplot", i, sep="")   # make arbitrary unique plot name
    assign(plotName, plot.path(toPlot, "end"))    # assign path plot of specific strain to plot name
    endplotList[[i]] <- get(plotName)         # add plot to list of plots
  }
  
  ## add start plots and end plot lists together
  plotList <- c(startplotList, endplotList)
  
  ## make arguments (with list of plots) to be arranged by gridExtra
  arrangeArgs <- c(plotList, ncol=2)
  
  ## make plot with N2 as first plot
  g <- do.call(arrangeGrob, arrangeArgs) 
  
  ##save plot
  ggsave(file="results/path_plot.pdf", g)
  
}

## =======================================================================================
## OLD SIZE FUNCTION (before giving it arguments for grouping variables and variables to aggregate over)
## =======================================================================================

## Example call:
## use function to get length, width, and area of each worm, averaged over 60 to 70s
mean.size.data <- mean.size(parsed.data, 60, 70)

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
## OLD MEDIAN CONFIDENCE INTERVALS
##=========================================================================================================

## Old package used to make median confidence intervals; unfortunately package was not updated to latest R
## version and no longer works.

require(fmsb)

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

## add median point + median error bars to a ggplot
g <- g + stat_summary(fun.ymax = errorUpper, fun.ymin = errorLower, geom = "linerange", size=3.5, colour="black" ) +    ## add error bar for median confidence interval (95%)
  stat_summary(fun.y=median, geom="point", size=2, color="white") ## add a median point

##=========================================================================================================
## MEAN AGGREGATION FUNCTION
##=========================================================================================================

## SUMMARY: Subsets dataframe to given time interval, then aggregates dataframe, 
##          grouped by given variables and aggregated over given variables, 
##          using mean() as the aggregate function.
## INPUT: parsedData = dataframe with appropriate column names
##        minT = lower limit of time interval to average over
##        maxT = upper limit of time interval to average over
##        toAverage = vector of column names to be averaged (example: cbind("area", "length", "width"))
##        toGroup = vector of column names to group data by (example: cbind"ID", "plate", "strain")
##        timeName = name of the time column in the dataframe (example: "time")
## 
## OUTPUT: A dataframe where rows consist of unique combinations of grouping variables.
##         Columns include the grouping variables, and the columns which were aggregated by mean().
##
## For our purposes, we will use this to find the mean worm length, width, and area from 60-70s, grouped by
## ID strain and plate to access each worm.

aggregateMean <- function(parsedData, minT, maxT, toAverage, toGroup, timeName) {
  
  ## subset parsed data to times between minT and maxT
  time.subset <- parsedData[parsedData[[timeName]] < maxT & parsedData[[timeName]] > minT,]  
  
  return(mean.subset)
  
  ## generate string specifying variables for ddply to aggregate over, using mean to aggregate
  ## for example turn c("1", "2") into "1 = mean(1), 2 = mean(2)"
  toAverageString <- paste(toAverage, " = mean(", toAverage, ")", sep = "", collapse = ", ")
  
  ## Use parse to create unevaluated 
  ## Then use eval to evaluate the ddply function with correct arguments
  ## Attempted to use do.call() and/or get instead of eval(parse(...)), but couldn't get a solution...
  mean.subset <- eval(parse(text = paste("ddply(time.subset, toGroup, here(summarize),", toAverageString, ")")))
  
  return(mean.subset)
  
}


##=========================================================================================================
## OLD AGGREGATE PATHLENGTH FUNCTION
##=========================================================================================================

pathlength.data <- aggregatePathlength(parsed.data, 530, 590)

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
## OLD AGGREGATE DISTANCE FUNCTION
##=========================================================================================================

distance.data <- aggregateDistance(parsed.data, 530, 590)

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
## OLD ADJUST PATH FUNCTION
##=========================================================================================================


## make dataframe with adjusted path data from 100 to 160s (where (x,y) is shifted to (0,0) 
## at the start of the time interval), and from 530 to 590s
adjusted.path.data <- rbind(adjusted.path(parsed.data, 100, 160), 
                            adjusted.path(parsed.data, 530, 590))


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
  print(str(time.subset))
  
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
