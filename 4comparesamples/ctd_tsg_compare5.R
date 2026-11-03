#' @title match processed data from underway flow to CTD data and plot TSG vs CTD data
#' plot TSG vs CTD data
#' 
#' @description  read table CTD_table3.csv created by function tsg_ctd.R and 
#' read processed data from underway flow, match and plot TSG vs CTD data
#'
#' @param  TSG processed files directory (pathprocessed)
#' 
#' @output is in the \4comparesamples folder and includes:
#' - CTD_TSG_QAT.csv-  table:CTD data at a certain depth near surface per event with time and position matched with TSG data 
#' - TSG_ctdtable.RData
#' - XXX_TSGCTDplot.jpeg - plots TSG vs CTD data for each variable per event and per day
#'
#' @author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

read.ctd_tsg_compare4<- function(pathprocessed){

ctdall <- data.frame(read.csv("4comparesamples/CTD_table3.csv", header = TRUE))


filestsg <- list.files(path= pathprocessed, pattern = 'TSGdata.*\\.csv', full.names = TRUE)
filestsgnmea <- list.files(path= pathprocessed, pattern = 'TSGposition.*\\.csv', full.names = TRUE)


TSG_ctdtable <- data.frame(timeTSG = "NA",timectd = "NA", 
                              CTDlat = 0,
                              CTDlon = 0,
                              TSGsal = 0, ctdsal1= 0,  
                              TSGChl = 0, ctdChl= 0,
                              TSGOxy = 0, ctdoxy1= 0,
                              TSGtemp = 0,ctdtemp1= 0,
                              TSGcon = 0, ctdcon= 0,
                              TSGUV = 0, ctdUV = 0,
                                TSGpH = 0, ctdpH = 0,
                              event = 0,
                              ctdpres = 0
) 
x=1
for (g in 1:length(filestsg)){
tsgall <- data.frame(read.csv(filestsg[g], header = TRUE))
tsgnmea <- data.frame(read.csv(filestsgnmea[g], header = TRUE))

ctdallmin <- ctdall %>% mutate(match_time = floor_date(as.POSIXct(timectd, tz = "UTC"), "minute"))
tsgall$match_time <- floor_date(ymd_hms(tsgall$time, tz = "UTC"), unit = "minute")

# 2. Join them using the new minute-level key
join_tsgctd <- inner_join(ctdallmin, tsgall, by = "match_time")

if (is.null(join_tsgctd) || length(join_tsgctd) == 0) {
  break
}

TSG_ctdtable2 <- data.frame(timeTSG = join_tsgctd$time,timectd = join_tsgctd$timectd, 
                              CTDlat = join_tsgctd$lat,
                              CTDlon = join_tsgctd$lon,
                              TSGsal = join_tsgctd$salinity, ctdsal1= join_tsgctd$ctdsal1,  
                              TSGChl = join_tsgctd$Fluorescence, ctdChl= join_tsgctd$ctdCh,
                              TSGOxy = join_tsgctd$O2Concentration_ml_L, ctdoxy1= join_tsgctd$ctdoxy1,
                              TSGtemp = join_tsgctd$Temperature_TSG_ITS_90,ctdtemp1= join_tsgctd$ctdtemp1,
                              TSGcon = join_tsgctd$Conductivity_S_m, ctdcon= join_tsgctd$ctdcon,
                              TSGUV = join_tsgctd$FluorescenceUV, ctdUV = join_tsgctd$ctdUV,
                              TSGpH = join_tsgctd$pH, ctdpH = join_tsgctd$ctdpH,
                              event = join_tsgctd$event,
                              ctdpres = join_tsgctd$ctdpres   )

TSG_ctdtable <- rbind(TSG_ctdtable, TSG_ctdtable2)
}

#remove first row of zeros from TSG_ctdtable 
TSG_ctdtable <- TSG_ctdtable[-1, ]

      
write.csv(TSG_ctdtable, file = "4comparesamples/CTD_TSG_QAT.csv")
save(TSG_ctdtable, file = "4comparesamples/TSG_ctdtable.RData")
#TSG_ctdtable <- data.frame(read.csv("4comparesamples/CTD_TSG_QAT.csv", header = TRUE))

#############SALTS

jpeg(paste0(getwd(),"/4comparesamples/salt_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$ctdsal1, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdsal1 )),round(max(TSG_ctdtable$ctdsal1) )),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="salinity, green=CTD, red=TSG", ylab="salinity",xlab="time",xaxt="n")
points(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$TSGsal, col = "dark red", pch = 19, bg="red", cex=2)
#axis.POSIXct(1, at = seq(min(TSG_ctdtable$timeTSG),max(TSG_ctdtable$timeTSG),by="day"), format = "%b-%d", las=2)
axis.Date(1, at = seq(as.Date(TSG_ctdtable$timeTSG[1]), max(as.Date(TSG_ctdtable$timeTSG))+1, "days"))
text(as.Date(TSG_ctdtable$timeTSG[seq(1, length(TSG_ctdtable$TSGsal), by=11)])+0.25,
     TSG_ctdtable$TSGsal[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
#axis(side = 1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
# Close the pdf file
dev.off()

jpeg(paste0(getwd(),"/4comparesamples/salevent_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(TSG_ctdtable$event,TSG_ctdtable$ctdsal1, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdsal1 )),round(max(TSG_ctdtable$ctdsal1) )),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="salinity, green=CTD, red=TSG", ylab="salinity",xlab="event",xaxt="n", 
     font.main=1, cex.main=2, cex.lab=1.5)
points(TSG_ctdtable$event,TSG_ctdtable$TSGsal, col = "dark red", pch = 19, bg="red", cex=2)
text(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]+0.25,
     TSG_ctdtable$ctdsal1[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
axis(side = 1, at = round(min(as.numeric(TSG_ctdtable$event),na.rm = TRUE )):round(max(as.numeric(TSG_ctdtable$event),na.rm = TRUE )))
# Close the pdf file
dev.off()

#############TEMP

jpeg(paste0(getwd(),"/4comparesamples/temp_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$ctdtemp1, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdtemp1 )),round(max(TSG_ctdtable$ctdtemp1) )),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="Temperature, green=CTD, red=TSG", ylab="temperature",xlab="time",xaxt="n")
points(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$TSGtemp, col = "dark red", pch = 19, bg="red", cex=2)
#axis.POSIXct(1, at = seq(min(TSG_ctdtable$timeTSG),max(TSG_ctdtable$timeTSG),by="day"), format = "%b-%d", las=2)
axis.Date(1, at = seq(as.Date(TSG_ctdtable$timeTSG[1]), max(as.Date(TSG_ctdtable$timeTSG))+1, "days"))
text(as.Date(TSG_ctdtable$timeTSG[seq(1, length(TSG_ctdtable$TSGsal), by=11)])+0.25,
     TSG_ctdtable$ctdtemp1[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
#axis(side = 1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
# Close the pdf file
dev.off()


jpeg(paste0(getwd(),"/4comparesamples/tempevent_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(TSG_ctdtable$event,TSG_ctdtable$ctdtemp1, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdtemp1 )),round(max(TSG_ctdtable$ctdtemp1) )),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="Temperature, green=CTD, red=TSG", ylab="temperature",xlab="event",xaxt="n",
     font.main=1, cex.main=2, cex.lab=1.5)
points(TSG_ctdtable$event,TSG_ctdtable$TSGtemp, col = "dark red", pch = 19, bg="red", cex=2)
text(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]+0.25,
     TSG_ctdtable$ctdtemp1[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
axis(side = 1, at = round(min(as.numeric(TSG_ctdtable$event),na.rm = TRUE )):round(max(as.numeric(TSG_ctdtable$event),na.rm = TRUE )))
# Close the pdf file
dev.off()

#############OXY

jpeg(paste0(getwd(),"/4comparesamples/oxy_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$ctdoxy1, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$TSGOxy,na.rm = TRUE))-0.5,round(max(TSG_ctdtable$ctdoxy1))+0.5),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="Oxygen Concentration, green=CTD, red=TSG", ylab="oxygen",xlab="time",xaxt="n")
points(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$TSGOxy, col = "dark red", pch = 19, bg="red", cex=2)
#axis.POSIXct(1, at = seq(min(TSG_ctdtable$timeTSG),max(TSG_ctdtable$timeTSG),by="day"), format = "%b-%d", las=2)
axis.Date(1, at = seq(as.Date(TSG_ctdtable$timeTSG[1]), max(as.Date(TSG_ctdtable$timeTSG))+1, "days"))
text(as.Date(TSG_ctdtable$timeTSG[seq(1, length(TSG_ctdtable$TSGsal), by=11)])+0.25,
     TSG_ctdtable$ctdoxy1[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
#axis(side = 1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
# Close the pdf file
dev.off()


jpeg(paste0(getwd(),"/4comparesamples/oxyevent_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(TSG_ctdtable$event,TSG_ctdtable$ctdoxy1, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$TSGOxy,na.rm = TRUE)-0.5),round(max(TSG_ctdtable$ctdoxy1))+0.5),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="Oxygen Concentration, green=CTD, red=TSG", ylab="oxygen",xlab="event",xaxt="n",
     font.main=1, cex.main=2, cex.lab=1.5)
points(TSG_ctdtable$event,TSG_ctdtable$TSGOxy, col = "dark red", pch = 19, bg="red", cex=2)
text(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]+0.25,
     TSG_ctdtable$ctdoxy1[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
axis(side = 1, at = round(min(as.numeric(TSG_ctdtable$event),na.rm = TRUE )):round(max(as.numeric(TSG_ctdtable$event),na.rm = TRUE )))
# Close the pdf file
dev.off()

###############CHL

jpeg(paste0(getwd(),"/4comparesamples/chl_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$ctdChl, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdChl ))-2,round(max(TSG_ctdtable$ctdChl) )),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="Chlorophyl, green=CTD, red=TSG", ylab="Chlorophyl",xlab="time",xaxt="n")
points(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$TSGChl, col = "dark red", pch = 19, bg="red", cex=2)
#axis.POSIXct(1, at = seq(min(TSG_ctdtable$timeTSG),max(TSG_ctdtable$timeTSG),by="day"), format = "%b-%d", las=2)
axis.Date(1, at = seq(as.Date(TSG_ctdtable$timeTSG[1]), max(as.Date(TSG_ctdtable$timeTSG))+1, "days"))
text(as.Date(TSG_ctdtable$timeTSG[seq(1, length(TSG_ctdtable$TSGsal), by=11)])+0.25,
     TSG_ctdtable$ctdChl[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
#axis(side = 1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
# Close the pdf file
dev.off()


jpeg(paste0(getwd(),"/4comparesamples/chlevent_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(TSG_ctdtable$event,TSG_ctdtable$ctdChl, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdChl ))-2,round(max(TSG_ctdtable$TSGChl) )),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="Chlorophyl, green=CTD, red=TSG", ylab="Chlorophyl",xlab="event",xaxt="n",
     font.main=1, cex.main=2, cex.lab=1.5)
points(TSG_ctdtable$event,TSG_ctdtable$TSGChl, col = "dark red", pch = 19, bg="red", cex=2)
text(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]+0.25,
     TSG_ctdtable$ctdChl[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
axis(side = 1, at = round(min(as.numeric(TSG_ctdtable$event),na.rm = TRUE )):round(max(as.numeric(TSG_ctdtable$event),na.rm = TRUE )))
# Close the pdf file
dev.off()

#############Conductivity

jpeg(paste0(getwd(),"/4comparesamples/con_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$ctdcon, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdcon ))-0.5,round(max(TSG_ctdtable$ctdcon))+0.5),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="Conductivity, green=CTD, red=TSG", ylab="conductivity",xlab="time",xaxt="n",
     font.main=1, cex.main=2, cex.lab=1.5)
points(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$TSGcon, col = "dark red", pch = 19, bg="red", cex=2)
#axis.POSIXct(1, at = seq(min(TSG_ctdtable$timeTSG),max(TSG_ctdtable$timeTSG),by="day"), format = "%b-%d", las=2)
axis.Date(1, at = seq(as.Date(TSG_ctdtable$timeTSG[1]), max(as.Date(TSG_ctdtable$timeTSG))+1, "days"))
text(as.Date(TSG_ctdtable$timeTSG[seq(1, length(TSG_ctdtable$TSGsal), by=11)])+0.25,
     TSG_ctdtable$ctdcon[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
#axis(side = 1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
# Close the pdf file
dev.off()


jpeg(paste0(getwd(),"/4comparesamples/conevent_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(TSG_ctdtable$event,TSG_ctdtable$ctdcon, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdcon ))-0.5,round(max(TSG_ctdtable$ctdcon))+0.5),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="Conductivity, green=CTD, red=TSG", ylab="conductivity",xlab="event",xaxt="n",
     font.main=1, cex.main=2, cex.lab=1.5)
points(TSG_ctdtable$event,TSG_ctdtable$TSGcon, col = "dark red", pch = 19, bg="red", cex=2)
text(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]+0.25,
     TSG_ctdtable$ctdcon[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
axis(side = 1, at = round(min(as.numeric(TSG_ctdtable$event),na.rm = TRUE )):round(max(as.numeric(TSG_ctdtable$event),na.rm = TRUE )))
# Close the pdf file
dev.off()

#############pH

jpeg(paste0(getwd(),"/4comparesamples/pH_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$ctdpH, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdpH ))-1,round(max(TSG_ctdtable$ctdpH) )+1),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="pH, green=CTD, red=TSG", ylab="pH",xlab="time",xaxt="n")
points(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$TSGpH, col = "dark red", pch = 19, bg="red", cex=2)
#axis.POSIXct(1, at = seq(min(TSG_ctdtable$timeTSG),max(TSG_ctdtable$timeTSG),by="day"), format = "%b-%d", las=2)
axis.Date(1, at = seq(as.Date(TSG_ctdtable$timeTSG[1]), max(as.Date(TSG_ctdtable$timeTSG))+1, "days"))
text(as.Date(TSG_ctdtable$timeTSG[seq(1, length(TSG_ctdtable$TSGsal), by=11)])+0.25,
     TSG_ctdtable$ctdpH[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=14)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
#axis(side = 1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
# Close the pdf file
dev.off()


jpeg(paste0(getwd(),"/4comparesamples/pHevent_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(TSG_ctdtable$event,TSG_ctdtable$ctdpH, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdpH ))-1,round(max(TSG_ctdtable$ctdpH))+1),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="pH, green=CTD, red=TSG", ylab="pH",xlab="event",xaxt="n",
     font.main=1, cex.main=2, cex.lab=1.5)
points(TSG_ctdtable$event,TSG_ctdtable$TSGpH, col = "dark red", pch = 19, bg="red", cex=2)
text(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]+0.25,
     TSG_ctdtable$ctdpH[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
axis(side = 1, at = round(min(as.numeric(TSG_ctdtable$event),na.rm = TRUE )):round(max(as.numeric(TSG_ctdtable$event),na.rm = TRUE )))
# Close the pdf file
dev.off()

#############UV

jpeg(paste0(getwd(),"/4comparesamples/UV_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$ctdUV , col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdUV ))-1,round(max(TSG_ctdtable$TSGUV))+0.5),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="CDOM, green=CTD, red=TSG", ylab="CDOM",xlab="time",xaxt="n")
points(as.Date(TSG_ctdtable$timeTSG),TSG_ctdtable$TSGUV, col = "dark red", pch = 19, bg="red", cex=2)
#axis.POSIXct(1, at = seq(min(TSG_ctdtable$timeTSG),max(TSG_ctdtable$timeTSG),by="day"), format = "%b-%d", las=2)
axis.Date(1, at = seq(as.Date(TSG_ctdtable$timeTSG[1]), max(as.Date(TSG_ctdtable$timeTSG))+1, "days"))
text(as.Date(TSG_ctdtable$timeTSG[seq(1, length(TSG_ctdtable$TSGsal), by=11)])+0.25,
     TSG_ctdtable$ctdUV [seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
#axis(side = 1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
# Close the pdf file
dev.off()


jpeg(paste0(getwd(),"/4comparesamples/UVevent_TSGCTDplot.jpeg"), width = 1150, height = 750)
# 2. Create a plot
plot(TSG_ctdtable$event,TSG_ctdtable$ctdUV, col = "green", pch = 19, bg="green", cex=4, 
     ylim=c(round(min(TSG_ctdtable$ctdUV ))-1,round(max(TSG_ctdtable$TSGUV))+0.5),
     #xlim=c(min(TSG_ctdtable$timeTSG ),max(TSG_ctdtable$timeTSG) ),
     main="CDOM, green=CTD, red=TSG", ylab="CDOM",xlab="event",xaxt="n",
     font.main=1, cex.main=2, cex.lab=1.5)
points(TSG_ctdtable$event,TSG_ctdtable$TSGUV, col = "dark red", pch = 19, bg="red", cex=2)
text(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]+0.25,
     TSG_ctdtable$ctdUV[seq(1, length(TSG_ctdtable$TSGsal), by=11)], 
     labels = as.character(TSG_ctdtable$event[seq(1, length(TSG_ctdtable$TSGsal), by=11)]), 
     offset = 100, cex = 1, col = NULL,font = 20)
axis(side = 1, at = round(min(as.numeric(TSG_ctdtable$event),na.rm = TRUE )):round(max(as.numeric(TSG_ctdtable$event),na.rm = TRUE )))
# Close the pdf file
dev.off()

}


