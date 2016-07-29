library(rstudioapi) #package that sets WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set wd
require(plyr)
library(Hmisc)
library(png)


# Load in GLATSOS detections
load("Input/THB_FilteredGlatosDetectionData.RData") # data stored in an object called 'query3'

# Create labels for the y-axis - these are the GLATOS arrays sorted in order from north to south
plotLabels = unique(query3$Array)[c(9,8,5,4,3,1,2,6,7,10,11,12,15,13,14)]

# Add a numeric column to the dataframe for plotting purposes. The call to plot uses the numeric values to plot the data and then the 3-letter array names are used as the labels
plotLabels = data.frame(PlotLabel = plotLabels, LabelNum = c(1:15), stringsAsFactors = FALSE)

# Add the plot labels to the detections file
query3 = merge(query3, plotLabels, by.x = "Array", by.y = "PlotLabel")

# Changed the labels for DRM and TBT because I used these short forms in the paper for the two different lake trout populations (not necessary for you Andrew)
plotLabels$PlotLabel[c(7,12)] = c("TB", "DI")

# Loops through every unique fish (tagID in my study because I did not reuse tags) and creates a plot showing the detection history of that fish - writes it to a folder called ""IndividualPlots" within the "Results" folder
for(i in sort(unique(query3$TagID))){
    #subset out detections for fish i
    query4 = query3[query3$TagID == i,]
    #create the png file that will be written to (can make other image file types also, e.g., tiff, jpeg)
    png(paste("Results/IndividualPlots/",i,".png", sep = ""), height = 1000, width = 1000, pointsize = 30)
        # set the margins of the plot (mar = plot margins, oma = outer margins)
        par(mar = c(2.5,3,0,0), oma = c(1,1.5,1,1), xpd = TRUE)
        # plot the numeric value of the GLATOS array (from the plotLabels created above and then merged into the detections dataframe) versus time
        plot(query4$DateUTC, query4$LabelNum, yaxt = 'n', xaxt = "n", ylab = "", xlab = "Date", ylim = c(1,15), xlim = c(min(query3$DateUTC), max(as.POSIXct("2015-05-01"))))
        # Increases the line thickness of the box around the plot - not necessary on PC
        box(lwd = 2)
        # Adds the 3-letter codes as y-axis labels
        axis(2, at = c(1:15), labels = plotLabels$PlotLabel, las = 1)
        # Adds the time labels to the x-axis
        axis(1, at = c(as.POSIXct("2013-01-01"), as.POSIXct("2013-07-01"), as.POSIXct("2014-01-01"), as.POSIXct("2014-07-01"), as.POSIXct("2015-01-01")), labels = c(as.POSIXct("2013-01-01"), as.POSIXct("2013-07-01"), as.POSIXct("2014-01-01"), as.POSIXct("2014-07-01"), as.POSIXct("2015-01-01")), las = 1)
        # Adds x and y axis titles
        mtext("Receiver line", side = 2.1, line = 3, cex = 1.1)
        mtext("Date", side = 1, line = 2.2, cex = 1.1)
        # Adds the FishID to the top of the plot
        mtext(paste(i), side = 3, line = -1.5, cex = 1.1)
        # Adds an arrow identifying the TB spawning array
        arrows(as.POSIXct("2012-08-10"),7, pch = 17, as.POSIXct("2012-09-05"), 7, lwd = 5)
    # Closes the plot device (you won't actually see the plot window when you write directly to image file) and finishes writing the image file
    dev.off()
}
