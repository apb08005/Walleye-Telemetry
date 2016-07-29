library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

VUE2014 = read.csv("Input/VUE_Export2014.csv", as.is = TRUE, header = TRUE)

table.1 = data.frame(table(VUE2014$Transmitter)) #gives # of observations for each transmitter ID
table.2 = data.frame(table.1[table.1$Freq >= 5,]) #removes any transmitter ID's with fewer than five detections


#Makes new detections dataframe that only includes those transmitters that had 5 or more observations
for(i in table.2$Var1){
  if(i == table.2$Var1[1]){
    Vue2014.1 = VUE2014[VUE2014$Transmitter == i,]
  }else{
    tempo = VUE2014[VUE2014$Transmitter == i,]
    Vue2014.1 = rbind(Vue2014.1, tempo)
  }
}

#False detections often result in shortened transmitter ID's so I'm removing transmitters with other than 14 characters
VUE2014.2 = Vue2014.1[nchar(Vue2014.1$Transmitter) == 14,]

#All ID's greater than 60000 are synctags (not actual fish) so I'm subsetting them out
for(i in VUE2014.2$Transmitter){
  if(i == VUE2014.2$Transmitter[1]){
    ID = strsplit(VUE2014.2$Transmitter[VUE2014.2$Transmitter == i], "-")[[1]][3]
  }else{
    tempo = strsplit(VUE2014.2$Transmitter[VUE2014.2$Transmitter == i], "-")[[1]][3]
    ID = c(ID,tempo)
  }
}
length(ID)
VUE2014.3 = VUE2014.2[VUE2014.2$ID < 60000,] 

#Create a daily detection frequency on the reef complex over duration of deployment
sequence = seq(from = min(query2$DATETIME), to = max(query2$DATETIME), by = "day") #creates sequence from first to lst to last detection in 1 one day increments
for(i in sequence){
  if(i == sequence[1]){
    detectfreq = nrow(query2[query2$DATETIME < (i+(60*60*24)),]) - nrow(query2[query2$DATETIME < i,]) #60*60*24 = number of seconds in a day, therefore i+60*60*24 = sequence[i+1]
  }else{
    temp = nrow(query2[query2$DATETIME < (i+(60*60*24)),]) - nrow(query2[query2$DATETIME <  i,])
    detectfreq = c(detectfreq, temp)
  }
} 
plot(sequence, detectfreq)


#same concept but for frequency of occurence of unique fish
for(i in sequence){
  if(i == sequence[1]){
    detectfreq.2 = length(unique(query2$Fish.ID[query2$DATETIME < (i+(60*60*24)) & query2$DATETIME >= i])) #60*60*24 = number of seconds in a day, therefore i+60*60*24 = sequence[i+1]
  }else{
    temp = length(unique(query2$Fish.ID[query2$DATETIME < (i+(60*60*24)) & query2$DATETIME >= i]))
    detectfreq.2 = c(detectfreq.2, temp)
  }
} 
plot(sequence, detectfreq.2, xlab = "", ylab = "Number of Unique Fish on Reef", main = "2014")