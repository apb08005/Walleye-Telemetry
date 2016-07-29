library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################################################################################################################
#2016 analysis
eggs = read.csv("Input/Reorganized_ErrorCheckedEggs_2016.csv")
eggs$Date = as.POSIXct(eggs$Date, tz = "gmt")
str(eggs)
eggs$Site_Name = as.character(eggs$Site_Name)
head(eggs)
by(eggs[,2], eggs$Site_Name, summary)

forplot = aggregate(eggs$Eggs_Day, by = list(Site_Name = eggs$Site_Name, Date = eggs$Date), FUN = mean, na.rm = TRUE)
names(forplot) = c("Site", "Date", "Eggs/Day/Subsample")

plot(forplot$Date, forplot$`Eggs/Day/Subsample`, type = "n", ylab = "Egg CPUE", xlab = "Date", main = "2016")
reefs = c("R2", "R5", "R7", "R9", "R10", "R20", "R24")
for (i in 1:length(reefs)) lines(x = forplot$Date[forplot$Site == reefs[i]], y = forplot$`Eggs/Day/Subsample`[forplot$Site == reefs[i]], col = i)
legend("topright", c("Niagara", "Round", "Crib", "Toussaint (North)", "Toussaint (South)", "Cone", "Locust"), col = 1:7, lty = 1)

#####################################################################################################################
#2014 analysis
eggs = read.csv("Input/Reorganized_ErrorCheckedEggs_2014.csv")
eggs$Date = as.factor(eggs$Date)
str(eggs)
eggs$Site_Name = as.character(eggs$Site_Name)
head(eggs)
by(eggs[,2], eggs$Site_Name, summary)

forplot = aggregate(eggs$Eggs_Day, by = list(Site_Name = eggs$Site_Name, Date = eggs$Date), FUN = mean, na.rm = TRUE)
names(forplot) = c("Site", "Date", "Eggs/Day/Subsample")
forplot$Date = as.POSIXct(forplot$Date, tz = "GMT")

plot(forplot$Date, forplot$`Eggs/Day/Subsample`, type = "n", ylab = "Egg CPUE", xlab = "Date", main = "2014")
reefs = c("R2", "R5", "R7", "R9", "R10", "R20", "R24")
for (i in 1:length(reefs)) lines(x = forplot$Date[forplot$Site == reefs[i]], y = forplot$`Eggs/Day/Subsample`[forplot$Site == reefs[i]], col = i)
legend("topright", c("Niagara", "Round", "Crib", "Toussaint (North)", "Toussaint (South)", "Cone", "Locust"), col = 1:7, lty = 1)

