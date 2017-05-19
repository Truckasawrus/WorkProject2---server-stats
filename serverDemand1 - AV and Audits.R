library(plyr)
library(dplyr)
library(ggplot2)
library(quantmod)
library(reshape2)
library(scales)
library(DataCombine)
library(timeDate)
library(tidyr)
library(lubridate)
library(chron)


#READ IN SQL TAS REPORTS, CONVERT DATE COLUMN FROM FACTOR

SLAStats<-read.csv("SLA 5-2 to 5-5.csv", header = T, stringsAsFactors = F)

SLAStats$Report.Date<-as.Date(SLAStats$Report.Date,"%d/%m/%Y")


# CONVERT, ROUND, REFORMAT TIME COLUMNS -TAS AUDIT START/END - RUN CONVERSION THEN REFORMAT THEN CONVERSION AGAIN 

SLAStats$TAS.Audit.Started<-as.POSIXct(SLAStats$TAS.Audit.Started, format="%H:%M")

SLAStats$TAS.Audit.Started<-format(strptime("1970-01-01", "%Y-%m-%d", tz="UTC")
                                   + round(as.numeric(SLAStats$TAS.Audit.Started)/900)*900,"%H:%M")

SLAStats$TAS.Audit.Completed<-as.POSIXct(SLAStats$TAS.Audit.Completed, format="%H:%M")

SLAStats$TAS.Audit.Completed<-format(strptime("1970-01-01", "%Y-%m-%d", tz="UTC")
                                     + round(as.numeric(SLAStats$TAS.Audit.Completed)/900)*900,"%H:%M")


##CONVERT, ROUND, REFORMAT TIME COLUMNS -AV SYNC START/END - RUN CONVERSION THEN REFORMAT THEN CONVERSION AGAIN

SLAStats$AV.Sync.Start.Time<-as.POSIXct(SLAStats$AV.Sync.Start.Time, format="%H:%M")

SLAStats$AV.Sync.Start.Time<-format(strptime("1970-01-01", "%Y-%m-%d", tz="UTC")
                                    + round(as.numeric(SLAStats$AV.Sync.Start.Time)/900)*900,"%H:%M")

SLAStats$AV.Sync.End.Time<-as.POSIXct(SLAStats$AV.Sync.End.Time, format="%H:%M")

SLAStats$AV.Sync.End.Time<-format(strptime("1970-01-01", "%Y-%m-%d", tz="UTC")
                                    + round(as.numeric(SLAStats$AV.Sync.End.Time)/900)*900,"%H:%M")


##SET HALF HOUR BINS FOR TAS AUDIT START AND END TIMES

SLAStats$TAS.Audit.Startedbin<-cut(SLAStats$TAS.Audit.Started,breaks ="30 mins") 

SLAStats$TAS.Audit.Completedbin<-cut(SLAStats$TAS.Audit.Completed,breaks ="30 mins")


##SET HALF HOUR BINS FOR AV SYNC START AND END TIMES

SLAStats$AV.Sync.Startedbin<-cut(SLAStats$AV.Sync.Start.Time,breaks ="30 mins")

SLAStats$AV.Sync.Completedbin<-cut(SLAStats$AV.Sync.End.Time,breaks ="30 mins")

##ADD CATEGORY (AVSYNC PROCESS OR TAS AUDIT PROCESS)




##myTable<-table(cut(SLAStats$TAS.Audit.Started,breaks ="30 mins"))

##df<-data.frame(myTable)




ggplot(SLAStats) +
  geom_segment(aes(x = AV.SQL.Server, y = AV.Sync.Start.Time
                   ,xend = AV.SQL.Server
                   ,yend=AV.Sync.End.Time
                   ,color = EnterpriseID, size = 1))+
  scale_y_datetime(breaks = date_breaks("30 mins"),    
                   labels = date_format("%H:%M")) + # change to "%H:%M"
  coord_flip() +
  facet_grid(.~Report.Date)+
  xlab("") + ylab("") +
  theme_bw() + 
  theme(axis.text=element_text(size= 10),
        axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(SLAStats) +
  geom_segment(aes(x = AV.SQL.Server, y = TAS.Audit.Started
                   ,xend = AV.SQL.Server
                   ,yend= TAS.Audit.Completed
                   ,color = EnterpriseID, size = 1))+
  scale_y_datetime(breaks = date_breaks("hour"),    # change to "15 mins"
                   labels = date_format("%H:%M:%S")) + # change to "%H:%M"
  coord_flip() +
  facet_grid(.~Report.Date)+
  xlab("") + ylab("") +
  theme_bw() + 
  theme(axis.text=element_text(size= 10),
        axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


