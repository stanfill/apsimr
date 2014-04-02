#This illustrates how to conduct a sensitivity analysis using the apsimr package

library(apsimr)
exe <-" \"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\" "
wd <- "C:/Users/Sta36z/Documents/APSIM"
toRun <- "Nitrogen Cycle.apsim"
results <- apsimr(exe, wd, files = toRun)

#Create variable that is the total NO3 over all layers
results$TotalNO3<-rowSums(results[,14:20])
results$TotalNH4<-rowSums(results[,21:27])
results$TotalUrea<-rowSums(results[,35:41])

#Plot total NO3 as a function of time
dataToPlot<-melt(results,id.vars="Date",measure.vars=c("TotalNO3","TotalNH4","TotalUrea"),
                 variable.name="GHG")
qplot(Date,value,data=dataToPlot,geom='line',colour=GHG,size=I(2))
