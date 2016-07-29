#This script was written to combine all HOBO logger CSV outputs into one data.frame with a POSIXct time format. This ended up needing a bunch of nifty but slightly confusing features so I'd be happy to explain what's going on more thoroughly than what's in the annotations.

library(rstudioapi) #package that sets your WD to the filepath where you save the script. Feel free to use or not use, but I find this makes my life a little easier. file=file.choose() will not work with this script.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set wd. Again, feel free to do this part manually

# Create a list of csv files you want to add to the database, all of your HOBO logger csv exports should be included
tables = list.files("Input", pattern = ".csv", full.names = TRUE) #this creates a list of files in the WD's Input folder with .csv in the name. You could also create the list manually: list = c("filepath1", "filepath2", etc.)

#Reformats to POSIXct datetime and combines the HOBO outputs to one csv file in the Results section of the WD. This makes it easier to deal with in R (making comparisons and plots etc.) and prevents Excel from fucking it up
for(j in tables){ #You should learn basic loops if unfamiliar
  if(j == tables[1]){ #for the first file in WD. . .
    combined = read.csv(j, header=TRUE, skip = 1, as.is = TRUE) #create a DF based on first file that later files will be appended to
    names(combined) = c("dumb", "Date_Time", "Temp", "x", "x1", "x2", "x3", "x4") #Name columns, this step is important because different loggers will have different column names by default and the code won't work if that isn't remedied
    combined$Date_Time = as.POSIXct(combined$Date_Time, tz = "GMT", "%m/%d/%y %I:%M:%S %p") #converts dates from character to a usable date format. See: ?as.POSIXct for more info
    combined$logger = substr(j, nchar(j)-6, nchar(j)-4) #creates a column that gives the loggers a unique identifier, unless you use the same naming scheme as me you will need to modify this line. Doing something like this will be necessary to make comparisons between loggers
  }else{ #For all subsequent files in the WD: tables[2] to tables[n]
    tempo = read.csv(j, header=TRUE, skip = 1, as.is = TRUE) #creates a temporary df for each j in tables beyond tables[1]
    names(tempo) = c("dumb", "Date_Time", "Temp", "x", "x1", "x2", "x3", "x4") #names need to match those in combined
    tempo$Date_Time = as.POSIXct(tempo$Date_Time, tz = "GMT", "%m/%d/%y %I:%M:%S %p") #same as for combined
    tempo$logger = substr(j, nchar(j)-6, nchar(j)-4) #same as in combined
    combined = rbind(combined, tempo) #appends tempo to combined, this loops for all files in tables
  }
  final = subset(combined, select = c(Date_Time,Temp,logger))
}

plot(final$Date_Time, final$Temp) #Need to cut out observations from before and after deployment, this helps visualize
head(final)
tail(final)
cleanup = final[final$Date_Time > "2016-03-23 12:00:00" & final$Date_Time < "2016-05-31 08:00:00",]
plot(cleanup$Date_Time, cleanup$Temp)
write.csv(cleanup, "Results/HOBOsUnite.csv") #creates a csv file of combined loggers and puts it in WD's results folder
saveRDS(cleanup, file = "Results/HOBOsUnite.rds")

