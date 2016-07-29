library(rstudioapi) #package that sets WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set wd


library(RSQLite)

FishBioData = read.csv("Tables//FishBioData.csv", as.is=TRUE, na.string="")



# Create a list of csv files you want to add to the database
tables = list.files("Tables", pattern = ".csv", full.names = TRUE)

# List of position files, which are in a different folder
positions = list.files("Tables/Positions", pattern = ".csv", full.names = TRUE)

#List of VUE detection files
VUEDetections = list.files("Tables/VUEDetections", pattern = ".csv", full.names = TRUE)

# Creates connection to a database file. If the database file does not exist, it creates one
con = dbConnect(dbDriver("SQLite"), dbname="Database/TestDatabase.db")

# Loop through the csv files and adds each as a separate table in the db file
for(i in tables){
    # Read in ith csv file
    temp = read.csv(i, header = TRUE, as.is = TRUE, na.strings="")
 
    # Create a new column for use in querying out the data - necessary because tags were reused so date range must be included the relational key
    if(i == "Tables/FishBioData.csv"){
        # New column created
        temp$LAST_DATE_FOR_QUERY = temp$RECAP_DATE
        # If the tag has not been recaptured, the "LAST_DATE_FOR_QUERY" is todays date (i.e., the current system time)
        temp[is.na(temp$RECAP_DATE) == TRUE, "LAST_DATE_FOR_QUERY"] = as.character(format(Sys.time(), "%Y-%m-%d")) 
    }
    
    dbWriteTable(con, name = substr(basename(i), 1, nchar(basename(i))-4), value = temp, row.names = NA, overwrite = TRUE, append = FALSE)

}

#########################################################################################################################

# Combines the position files into a single dataframe
for(j in positions){
    if(j == positions[1]){
        combined = read.csv(j, header = TRUE, as.is = TRUE)  
   }else{
        temp = read.csv(j, header = TRUE, as.is = TRUE)
        combined = rbind(combined, temp)
   }
}

# Separates fish detections from sync tag detections and adds them as two separate tables in the db file
FishPositions = combined[combined$TRANSMITTER %in% FishBioData$TRANS_ID,]

 dbWriteTable(con, name = "FishPositions", value = FishPositions, row.names = NA, overwrite = TRUE, append = FALSE)
 
SyncPositions = combined[combined$TRANSMITTER >= 60000,]

 dbWriteTable(con, name = "SyncPositions", value = SyncPositions, row.names = NA, overwrite = TRUE, append = FALSE)

#########################################################################################################################
 
 # Combines the position files into a single dataframe
 for(j in VUEDetections){
   if(j == VUEDetections[1]){
     combined2 = read.csv(j, header = TRUE, as.is = TRUE)  
   }else{
     temp2 = read.csv(j, header = TRUE, as.is = TRUE)
     combined2 = rbind(combined2, temp2)
   }
 }

 # Separates fish detections from sync tag detections and adds them as two separate tables in the db file
 FishVUEDetections = combined2[combined2$Transmitter %in% FishBioData$PINGERFULLID,]
 
 dbWriteTable(con, name = "FishVUEDetections", value = FishVUEDetections, row.names = NA, overwrite = TRUE, append = FALSE)
 
 
 
#creates report
sink(file = paste("Database/DatabaseCreationReport_", format(Sys.time(), "%Y%m%d"),".txt", sep = "")) 
    cat("Database Tables: ", dbListTables(con), "\n")

    for(k in dbListTables(con)){
        cat("\n", "\n")
        cat(k, "\n")
        cat("Number of records: ", as.numeric(dbGetQuery(con, sprintf("select count(*) FROM %s", k))), "\n")
        cat("Fields: ", "\n\t")
        cat(dbListFields(con, k), sep = "\n\t")
    }
sink()



query = dbGetQuery(con, "select * FROM FishPositions FP, FishBioData FBD WHERE FP.TRANSMITTER = FBD.TRANS_ID AND FP.DATETIME BETWEEN FBD.TAG_DATE AND FBD.LAST_DATE_FOR_QUERY")
query2 = query[,c(21,1:20,23:28)]

query2$DATETIME = as.POSIXct(query2$DATETIME, tz= "GMT")
query2$SURGERY_START = as.POSIXct(query2$SURGERY_START, tz= "GMT")
query2$SURGERY_END = as.POSIXct(query2$SURGERY_END, tz= "GMT")
query2$RELEASED = as.POSIXct(query2$RELEASED, tz= "GMT")
query2$TAG_DATE = as.POSIXct(query2$TAG_DATE, tz= "GMT")
query2$RECAP_DATE = as.POSIXct(query2$RECAP_DATE, tz= "GMT")

save(query2, file = "RDataFiles//FullPositions2015.RData")
