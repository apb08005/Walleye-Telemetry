library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######################################################################################################################
##2014 DATA

#Calculating time from from to last detection for each individual, first clean up data

detect = read.csv("Input/VUE_Export2014.csv", as.is = TRUE, header = TRUE) #Detections from 2014 VPS array

table.1 = data.frame(table(detect$Transmitter)) #gives # of observations for each transmitter ID
table.2 = data.frame(table.1[table.1$Freq >= 5,]) #removes any transmitter ID's with fewer than five detections


#Makes new detections dataframe that only includes those transmitters that had 5 or more observations
for(i in table.2$Var1){
  if(i == table.2$Var1[1]){
    detect.1 = detect[detect$Transmitter == i,]
  }else{
    tempo = detect[detect$Transmitter == i,]
    detect.1 = rbind(detect.1, tempo)
  }
}

#Makes a dataframe consisting of only the first detection for each transmitter
uniquefish = unique(detect.1$Transmitter)
first = function(x) head(x, 1)
for(i in uniquefish){
  if(i == uniquefish[1]){
    detect1 = first(detect.1[detect.1$Transmitter == i,])
  }else{
    tempo = first(detect.1[detect.1$Transmitter == i,])
    detect1 = rbind(detect1,tempo)
  }
} 
nrow(detect1)

#Makes a dataframe of only the last detection for each transmitter
last = function(x) tail(x, 1)
for(i in uniquefish){
  if(i == uniquefish[1]){
    detect2 = last(detect[detect$Transmitter == i,])
  }else{
    tempo = last(detect[detect$Transmitter == i,])
    detect2 = rbind(detect2,tempo)
  }
} 
nrow(detect2)

#creates a dataframe with columns for transmitter name, time at first detection, and time at last detection 
detect3 = data.frame(Transmitter = uniquefish, First = detect1$ï..Date.and.Time..UTC., Last = detect2$ï..Date.and.Time..UTC.)

detect3$Transmitter = as.character(detect3$Transmitter)

#False detections often result in shortened transmitter ID's so I'm removing transmitters with other than 14 characters
detect4 = detect3[nchar(detect3$Transmitter) == 14,]
detect4$First = as.POSIXct(detect4$First, tz = "GMT") #changing times from characters to usable format
detect4$Last = as.POSIXct(detect4$Last, tz = "GMT")

detect5 = detect4[detect4$First != detect4$Last,] #removing transmitters where no time elapsed between first and last detection
nrow(detect5)

detect6 = detect5
detect6$TP = detect6$Last-detect6$First #creating a column for the time between first and last detection
hist(as.numeric(detect6$TP))

#Creating a vector that consists of only the third portion of the full transmitter ID
for(i in detect6$Transmitter){
  if(i == detect6$Transmitter[1]){
    ID = strsplit(detect6$Transmitter[detect6$Transmitter == i], "-")[[1]][3]
  }else{
    tempo = strsplit(detect6$Transmitter[detect6$Transmitter == i], "-")[[1]][3]
    ID = c(ID,tempo)
  }
}
length(ID)

detect7 = detect6
detect7$ID = as.numeric(ID) #Adding the vector to our dataframe so we can subset with it

#All ID's greater than 60000 are synctags (not actual fish) so I'm subsetting them out
detect8 = detect7[detect7$ID < 60000,] 
hist(as.numeric(detect8$TP), xlab = "Time From First to Last Detection (mins)", main = "Histogram of TP (2014)")
head(detect8)

######################################################################################################################
#Calculating the number of days detected and residence index

#Narrows the detection dataframe to only include transmitter ID's which made it through previous analysis
for(i in detect8$Transmitter){
  if(i == detect8$Transmitter[1]){
    detect.b = detect.1[detect.1$Transmitter == i,]
  }else{
    tempo = detect.1[detect.1$Transmitter == i,]
    detect.b = rbind(detect.b, tempo)
  }
}

detect.b$ï..Date.and.Time..UTC. = as.POSIXct(detect.b$ï..Date.and.Time..UTC., tz = "GMT") 
detect.b$days = trunc(detect.b$ï..Date.and.Time..UTC., units = "days") #truncates timestamp to nearest day in new col

#For each transmitter is counts the number of unique days on which that transmitter was detected, extremely inefficient
for(i in detect.b$Transmitter){
  if(i == detect.b$Transmitter[1]){
    DD = length(unique(detect.b$days[detect.b$Transmitter == i]))
  }else{
    tempo = length(unique(detect.b$days[detect.b$Transmitter == i]))
    DD = c(DD,tempo)
    }
}

#trying to do the same as above but more efficiently. Hint:Do this instead
days.detect = function(x) length(unique(x))
result = data.frame(tapply(detect.b$days, detect.b$Transmitter, days.detect))

#Add DD as a column to detect8
detect8$DD = result$tapply.detect.b.days..detect.b.Transmitter..days.detect.
detect8$TP_days = as.numeric(ceiling(detect8$TP/1440))

#Calculate RI
detect8$RI = detect8$DD/detect8$TP_days

#Give it a look!
par(mfrow = c(1,3))
hist(detect8$DD, xlab = "Number of Days Detected", main = "DD 2014")
hist(detect8$TP_days, xlab = "Days From First to Last Detection", main = "TP 2014")
hist(detect8$RI, xlab = "DD/TP", main = "RI 2014")

par(mfrow = c(1,1))
pairs(detect8[,c(6,7,8)])

######################################################################################################################
##2015 DATA

#Calculating time from from to last detection for each individual, first clean up data

detect = read.csv("Input/VUE_Export2015.csv", as.is = TRUE, header = TRUE) #Detections from 2015 VPS array

table.1 = data.frame(table(detect$Transmitter)) #gives # of observations for each transmitter ID
table.2 = data.frame(table.1[table.1$Freq >= 5,]) #removes any transmitter ID's with fewer than five detections


#Makes new detections dataframe that only includes those transmitters that had 5 or more observations
for(i in table.2$Var1){
  if(i == table.2$Var1[1]){
    detect.1 = detect[detect$Transmitter == i,]
  }else{
    tempo = detect[detect$Transmitter == i,]
    detect.1 = rbind(detect.1, tempo)
  }
}

#Makes a dataframe consisting of only the first detection for each transmitter
uniquefish = unique(detect.1$Transmitter)
first = function(x) head(x, 1)
for(i in uniquefish){
  if(i == uniquefish[1]){
    detect1 = first(detect.1[detect.1$Transmitter == i,])
  }else{
    tempo = first(detect.1[detect.1$Transmitter == i,])
    detect1 = rbind(detect1,tempo)
  }
} 
nrow(detect1)

#Makes a dataframe of only the last detection for each transmitter
last = function(x) tail(x, 1)
for(i in uniquefish){
  if(i == uniquefish[1]){
    detect2 = last(detect[detect$Transmitter == i,])
  }else{
    tempo = last(detect[detect$Transmitter == i,])
    detect2 = rbind(detect2,tempo)
  }
} 
nrow(detect2)

#creates a dataframe with columns for transmitter name, time at first detection, and time at last detection 
detect3 = data.frame(Transmitter = uniquefish, First = detect1$ï..Date.and.Time..UTC., Last = detect2$ï..Date.and.Time..UTC.)

detect3$Transmitter = as.character(detect3$Transmitter)

#False detections often result in shortened transmitter ID's so I'm removing transmitters with other than 14 characters
detect4 = detect3[nchar(detect3$Transmitter) == 14,]
detect4$First = as.POSIXct(detect4$First, tz = "GMT") #changing times from characters to usable format
detect4$Last = as.POSIXct(detect4$Last, tz = "GMT")

detect5 = detect4[detect4$First != detect4$Last,] #removing transmitters where no time elapsed between first and last detection
nrow(detect5)

detect6 = detect5
detect6$TP = detect6$Last-detect6$First #creating a column for the time between first and last detection
hist(as.numeric(detect6$TP))

#Creating a vector that consists of only the third portion of the full transmitter ID
for(i in detect6$Transmitter){
  if(i == detect6$Transmitter[1]){
    ID = strsplit(detect6$Transmitter[detect6$Transmitter == i], "-")[[1]][3]
  }else{
    tempo = strsplit(detect6$Transmitter[detect6$Transmitter == i], "-")[[1]][3]
    ID = c(ID,tempo)
  }
}
length(ID)

detect7 = detect6
detect7$ID = as.numeric(ID) #Adding the vector to our dataframe so we can subset with it

#All ID's greater than 60000 are synctags (not actual fish) so I'm subsetting them out
detect8 = detect7[detect7$ID < 60000,] 
hist(as.numeric(detect8$TP))
head(detect8)

######################################################################################################################
#Calculating the number of days detected and residence index

#Narrows the detection dataframe to only include transmitter ID's which made it through previous analysis
for(i in detect8$Transmitter){
  if(i == detect8$Transmitter[1]){
    detect.b = detect.1[detect.1$Transmitter == i,]
  }else{
    tempo = detect.1[detect.1$Transmitter == i,]
    detect.b = rbind(detect.b, tempo)
  }
}

detect.b$ï..Date.and.Time..UTC. = as.POSIXct(detect.b$ï..Date.and.Time..UTC., tz = "GMT") 
detect.b$days = trunc(detect.b$ï..Date.and.Time..UTC., units = "days") #truncates timestamp to nearest day in new col

#For each transmitter is counts the number of unique days on which that transmitter was detected, extremely inefficient
for(i in detect.b$Transmitter){
  if(i == detect.b$Transmitter[1]){
    DD = length(unique(detect.b$days[detect.b$Transmitter == i]))
  }else{
    tempo = length(unique(detect.b$days[detect.b$Transmitter == i]))
    DD = c(DD,tempo)
  }
}

#trying to do the same as above but more efficiently. Hint:Do this instead
days.detect = function(x) length(unique(x))
result = data.frame(tapply(detect.b$days, detect.b$Transmitter, days.detect))

#Add DD as a column to detect8
detect8$DD = result$tapply.detect.b.days..detect.b.Transmitter..days.detect.
detect8$TP_days = as.numeric(ceiling(detect8$TP/1440))

#Calculate RI
detect8$RI = detect8$DD/detect8$TP_days

#Give it a look!
par(mfrow = c(1,3))
hist(detect8$DD, xlab = "Number of Days Detected", main = "DD 2015")
hist(detect8$TP_days, xlab = "Days From First to Last Detection", main = "TP 2015")
hist(detect8$RI, xlab = "DD/TP", main = "RI 2015")

par(mfrow = c(1,1))
pairs(detect8[,c(6,7,8)])

#################################################################################################################
#2016 Data

#Calculating time from from to last detection for each individual, first clean up data

detect = read.csv("Input/VUE_Export2016_Incomplete.csv", as.is = TRUE, header = TRUE) #Detections from 2015 VPS array

table.1 = data.frame(table(detect$Transmitter)) #gives # of observations for each transmitter ID
table.2 = data.frame(table.1[table.1$Freq >= 5,]) #removes any transmitter ID's with fewer than five detections


#Makes new detections dataframe that only includes those transmitters that had 5 or more observations
for(i in table.2$Var1){
  if(i == table.2$Var1[1]){
    detect.1 = detect[detect$Transmitter == i,]
  }else{
    tempo = detect[detect$Transmitter == i,]
    detect.1 = rbind(detect.1, tempo)
  }
}

#Makes a dataframe consisting of only the first detection for each transmitter
uniquefish = unique(detect.1$Transmitter)
first = function(x) head(x, 1)
for(i in uniquefish){
  if(i == uniquefish[1]){
    detect1 = first(detect.1[detect.1$Transmitter == i,])
  }else{
    tempo = first(detect.1[detect.1$Transmitter == i,])
    detect1 = rbind(detect1,tempo)
  }
} 
nrow(detect1)

#Makes a dataframe of only the last detection for each transmitter
last = function(x) tail(x, 1)
for(i in uniquefish){
  if(i == uniquefish[1]){
    detect2 = last(detect[detect$Transmitter == i,])
  }else{
    tempo = last(detect[detect$Transmitter == i,])
    detect2 = rbind(detect2,tempo)
  }
} 
nrow(detect2)

#creates a dataframe with columns for transmitter name, time at first detection, and time at last detection 
detect3 = data.frame(Transmitter = uniquefish, First = detect1$ï..Date.and.Time..UTC., Last = detect2$ï..Date.and.Time..UTC.)

detect3$Transmitter = as.character(detect3$Transmitter)

#False detections often result in shortened transmitter ID's so I'm removing transmitters with other than 14 characters
detect4 = detect3[nchar(detect3$Transmitter) == 14,]
detect4$First = as.POSIXct(detect4$First, tz = "GMT") #changing times from characters to usable format
detect4$Last = as.POSIXct(detect4$Last, tz = "GMT")

detect5 = detect4[detect4$First != detect4$Last,] #removing transmitters where no time elapsed between first and last detection
nrow(detect5)

detect6 = detect5
detect6$TP = detect6$Last-detect6$First #creating a column for the time between first and last detection
hist(as.numeric(detect6$TP))

#Creating a vector that consists of only the third portion of the full transmitter ID
for(i in detect6$Transmitter){
  if(i == detect6$Transmitter[1]){
    ID = strsplit(detect6$Transmitter[detect6$Transmitter == i], "-")[[1]][3]
  }else{
    tempo = strsplit(detect6$Transmitter[detect6$Transmitter == i], "-")[[1]][3]
    ID = c(ID,tempo)
  }
}
length(ID)

detect7 = detect6
detect7$ID = as.numeric(ID) #Adding the vector to our dataframe so we can subset with it

#All ID's greater than 60000 are synctags (not actual fish) so I'm subsetting them out
detect8 = detect7[detect7$ID < 60000,] 
hist(as.numeric(detect8$TP))
head(detect8)

######################################################################################################################
#Calculating the number of days detected and residence index

#Narrows the detection dataframe to only include transmitter ID's which made it through previous analysis
for(i in detect8$Transmitter){
  if(i == detect8$Transmitter[1]){
    detect.b = detect.1[detect.1$Transmitter == i,]
  }else{
    tempo = detect.1[detect.1$Transmitter == i,]
    detect.b = rbind(detect.b, tempo)
  }
}

detect.b$ï..Date.and.Time..UTC. = as.POSIXct(detect.b$ï..Date.and.Time..UTC., tz = "GMT") 
detect.b$days = trunc(detect.b$ï..Date.and.Time..UTC., units = "days") #truncates timestamp to nearest day in new col

#For each transmitter is counts the number of unique days on which that transmitter was detected, extremely inefficient
for(i in detect.b$Transmitter){
  if(i == detect.b$Transmitter[1]){
    DD = length(unique(detect.b$days[detect.b$Transmitter == i]))
  }else{
    tempo = length(unique(detect.b$days[detect.b$Transmitter == i]))
    DD = c(DD,tempo)
  }
}

#trying to do the same as above but more efficiently. Hint:Do this instead
days.detect = function(x) length(unique(x))
result = data.frame(tapply(detect.b$days, detect.b$Transmitter, days.detect))

#Add DD as a column to detect8
detect8$DD = result$tapply.detect.b.days..detect.b.Transmitter..days.detect.
detect8$TP_days = as.numeric(ceiling(detect8$TP/24))

#Calculate RI
detect8$RI = detect8$DD/detect8$TP_days

#Give it a look!
par(mfrow = c(1,3))
hist(detect8$DD, xlab = "Number of Days Detected", main = "DD 2016")
hist(detect8$TP_days, xlab = "Days From First to Last Detection", main = "TP 2016")
hist(detect8$RI, xlab = "DD/TP", main = "RI 2016")

par(mfrow = c(1,1))
pairs(detect8[,c(6,7,8)])
