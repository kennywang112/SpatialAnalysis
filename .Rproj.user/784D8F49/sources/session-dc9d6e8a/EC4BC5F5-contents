library(tidyverse)

Ethnicity <- read.csv("./Data/Camden/tables/KS201EW_oa11.csv")
Rooms <- read.csv("./Data/Camden/tables/KS403EW_oa11.csv")
Qualifications <-read.csv("./Data/Camden/tables/KS501EW_oa11.csv")
Employment <-read.csv("./Data/Camden/tables/KS601EW_oa11.csv")

Ethnicity <- Ethnicity[, c(1, 21)]
Rooms <- Rooms[, c(1, 13)]
Employment <- Employment[, c(1, 20)]
Qualifications <- Qualifications[, c(1, 20)]

names(Ethnicity)<- c("OA", "White_British") 
names(Rooms)<- c("OA", "Low_Occupancy") 
names(Employment)<- c("OA", "Unemployed") 
names(Qualifications)<- c("OA", "Qualification")


Ethnicity%>%
  merge(Rooms, by="OA")%>%
  merge(Employment, by="OA")%>%
  merge(Qualifications, by="OA") -> Census.Data

# write.csv(Census.Data, "practical_data.csv", row.names=F)

Census.Data%>%head()

mean(Census.Data$Unemployed)
median(Census.Data$Unemployed)
range(Census.Data$Unemployed)
names(Census.Data)

library(vioplot)
vioplot(Census.Data$Unemployed, Census.Data$Qualification, Census.Data$White_British, Census.Data$Low_Occupancy, ylim=c(0,100),
        col = "dodgerblue", rectCol="dodgerblue3", colMed="dodgerblue4", names=c("Unemployed", "Qualifications", "White British", "Occupancy"))

# bubble plot
# a bubble plot with a dotted regression line
symbols(Census.Data$Unemployed, Census.Data$Qualification,
        circles = Census.Data$White_British,
        fg="white", bg ="purple", inches = 0.2, xlab="% in full time employmented", ylab="% With a Qualification") +
  abline(lm(Census.Data$Qualification~ Census.Data$Unemployed), col="red", lwd=2, lty=2)


