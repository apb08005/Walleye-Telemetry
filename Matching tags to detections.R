library(rstudioapi) #package that sets WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set wd

DetectionsVPS2015 = read.csv("Input//VUE_Export2015.csv", as.is=TRUE)
Transmitters = read.csv("Input//CombinedSpecSheet.csv", as.is=TRUE) #Combined spec sheets in WalleyeSpecSheets script

DetectionsVPS2015$match1 = match(DetectionsVPS2015$Transmitter, Transmitters$PingerFullID, nomatch = 0)
DetectionsVPS2015$match2 = match(DetectionsVPS2015$Transmitter, Transmitters$SensorFullID1, nomatch = 0)
DetectionsVPS2015$match = DetectionsVPS2015$match1+DetectionsVPS2015$match2
matched = DetectionsVPS2015[DetectionsVPS2015$match > 0,]   
notmatched = DetectionsVPS2015[DetectionsVPS2015$match == 0,]

notmatched.table = as.data.frame(table(notmatched$Transmitter))
notmatched.table

UniqueFish = DetectionsVPS2015[unique(DetectionsVPS2015$Transmitter),]
head(DetectionsVPS2015, 100)
nrow(UniqueFish)

temp62 = notmatched[notmatched$Transmitter == "A69-1601-62012",]
plot(temp62$Longitude, temp62$Latitude)
str(temp62)

head(notmatched)



fishy = unique(DetectionsVPS2015$Transmitter)

for(i in fishy){
  if(i == fishy[1]){
    Rows = nrow(i)
  }else{
    temp = nrow(i)
    combined = c(Rows, temp)
  }
}



table

head(DetectionsVPS2015)  

