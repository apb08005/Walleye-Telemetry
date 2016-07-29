#Creating a filtered GLATOS Rdata file

library(rstudioapi) #package that sets WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set wd

#Read in data and get a feel for what's included
GLATOSDetections2015 = read.csv("Input//LEWAE_detectionsWithLocs_20151022_200134.csv", as.is = TRUE)
head(GLATOSDetections2015)
ReleaseLocations = unique(GLATOSDetections2015$release_location)
arrays = unique(GLATOSDetections2015$glatos_array)

###########################################################################################################

#Check to see that each individual has > 5 detections
table = as.data.frame(table(GLATOSDetections2015$animal_id))
table.2 = table[table$Freq >= 5,] #if table.2 is smaller, then remove the fish comprising the difference
#All individuals had at least five detections, so no cuts were made here

###########################################################################################################

#Check min lags to reduce false detections
lags = seq(1800,7200,450)

for (i in lags){
  if (i == lags[1]){
    results = nrow(GLATOSDetections2015[GLATOSDetections2015$min_lag <= i,])
  }else{
    temp = nrow(GLATOSDetections2015[GLATOSDetections2015$min_lag <= i,])
    results = c(results, temp)
  }
}

plot(results)  

#Based on plot make the cutoff conservative, but not such that we lose greater than ~ 10%
GLATOSDetections2015.2 = GLATOSDetections2015[GLATOSDetections2015$min_lag <= 3600,]

##########################################################################################################

#Make sure columns are in desired format
str(GLATOSDetections2015.2)

#Notice that detection_timestamp_utc is in characters rather than posixct, other than that looks fine
GLATOSDetections2015.2$detection_timestamp_utc = as.POSIXct(GLATOSDetections2015.2$detection_timestamp_utc, "GMT") #success!


#create rdata file
save(GLATOSDetections2015.2, file = "Results//GLATOSDetections2015")
