library(rstudioapi) #package that sets WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set wd
specSheets = list.files("Input", pattern = "TagSheet", full.names = TRUE) #create vector of sheet names we want to combine
drmSheet = read.csv("Input/DRM14_ VPS-Spec-Binder_20141202.csv", as.is = TRUE, header = TRUE) #Read in other spec sheet
drmSheet$ID = ifelse(is.na(drmSheet$ID) == TRUE, drmSheet$ID, paste0(drmSheet$Channel,"-", drmSheet$Coding.ID, "-", drmSheet$ID)) #create Pinger full ID column using paste
drmSheet$ID.1 = ifelse(is.na(drmSheet$ID.1) == TRUE, drmSheet$ID.1, paste0(drmSheet$Channel,"-", drmSheet$Coding.ID, "-", drmSheet$ID.1))
drmSheet = drmSheet[,c(1,3,4,7,8,2,9,10,11,12,13,14,15,16,17)] #selecting and ordering columns in drmsheet to match VPS Workbook
walleye2012 = read.csv("Input\\Walleye2012TagSpecs.csv", as.is=TRUE) #read in other tag spec sheet

for(i in specSheets){
    if(i == specSheets[1]){
        results = read.csv(i, header = TRUE, as.is=TRUE)   
    }else{
        temp = read.csv(i, header = TRUE, as.is=TRUE)
        results = rbind(results, temp)
    }
}

results2 = results[,c(2,1,6,13,14,7,34,36,37,38)]
results2$ID.1 = NA
results2$Name = NA
results2$ID.2 = NA
results2$ID.2 = NA
results2$Type.2 = NA
results2$Slope.2 = NA
results2$Intercept.2 = NA

results2 = results2[,c(1:5,12,6,11,7,9,10,13,14,15,16)]

names(results2) = c("Serial", "Order#", "Family", "MinDelay", "MaxDelay", "Name", "PingerFullID", "SensorFullID1", "Type1", "Slope1", "Intercept1", "SensorFullID2", "Type2", "Slope2", "Intercept2")

names(drmSheet) = c("Serial", "Order#", "Family", "MinDelay", "MaxDelay", "Name", "PingerFullID", "SensorFullID1", "Type1", "Slope1", "Intercept1", "SensorFullID2", "Type2", "Slope2", "Intercept2")

names(walleye2012) = c("Serial", "Order#", "Family", "MinDelay", "MaxDelay", "Name", "PingerFullID", "SensorFullID1", "Type1", "Slope1", "Intercept1", "SensorFullID2", "Type2", "Slope2", "Intercept2")

resultsCombined = rbind(results2, drmSheet, walleye2012) ##LOOK AT ME!! THERE ARE NA VALUES FOR PINGERFULLID!! Last 231 rows in the drm spreadsheet are pressure tags? ID number is one column to the right



write.csv(resultsCombined, "Results/CombinedSpecSheet.csv", row.names = FALSE, na = "")


head(resultsCombined)

range(as.numeric(unlist(strsplit(results$VUE.Tag.ID, "-"))[seq(3,length(results$VUE.Tag.ID),3)]))

unique(results$VUE.Tag.ID)
head(results[is.na(as.numeric(unlist(strsplit(results$VUE.Tag.ID, "-"))[seq(3,length(results$VUE.Tag.ID),3)])),])

numeric = resultsCombined$PingerFullID
numeric = strsplit(numeric, "-")
numeric = unlist(numeric)
numeric = numeric[seq(3, length(numeric),3)]
numeric = as.numeric(numeric, na.rm=TRUE)
resultsCombined$transmitter = numeric #didn't actually do it because of NA problem, but pretty sure this works,
resultsCombined = resultsCombined[resultsCombined$transmitter < 65000,]
resultsCombined = resultsCombined[,c(1:15)]



write.csv(resultsCombined, "Results/CombinedSpecSheet.csv", row.names = FALSE)
summary(numeric)
 
length(numeric)
nrow(resultsCombined)
range(numeric, na.rm=TRUE)
