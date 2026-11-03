#' @title read elog Flow-Through files and processed data  from underway flow through system, 
#' match the samples from the TSG to the TSG data and create tables and plots
#' 
#' @description  read elog Flow-Through files and processed data from underway flow through system, 
#' match the samples from the TSG to the TSG data and create tables and plots.  
#' The TSG data is averaged per minute (12 values) and matched with sample data
#' Does not include PCO2 data
#' pathbottle - directory of analysed Sample data taken from the TSG 
# file names start with the variable type:     
# - 'Oxy.*\\.csv  (convert .dat file to csv, 8 header lines)
# - 'Chl.*\\.xlsx' (excel, 18 header lines)
# - 'Salts.*\\.csv' (csv, 4 header lines)
#' 
#' @param  ELOG TSG logs directory (pathrawdata) and bottle data (pathbottle) directory
#' 
#' @output is in the \4comparesamples folder and includes:
# - Bottle_TSG_QAT.csv - table: TSG data average of a minute (12 values) matched with sample data 
# - TSG_bottletable2.csv - table: TSG data when the sample was taken, 12 readings for the minute time of sample 
# - TSG_samples.csv -  table: time, date, lat, lon, ID of each sample 
# - chl2tsgplot.jpeg, oxy2tsgplot.jpeg, salt2sgplot.jpeg - plots of TSG vs sample data
#'
#' @author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

# > output_dir <- pathout
# > pathrawdata <- pathelog
# > pathbottle <- samples_directory

read.elog_tsg <- function(output_dir, pathrawdata, pathbottle){
  #(pathout, pathelog, samples_directory)
  output_dir <- pathout
  pathrawdata <- pathelog
  pathbottle <-  samples_directory
 
  library(dplyr)
  
  fileslog <- list.files(path= pathrawdata, pattern = '*\\.log', full.names = TRUE) #list of log files with path
  TSG_samples <- data.frame(time = "NA",
                                 date = "NA",
                                 POSIXctdate = "NA",
                                 lat = 0,
                                 lon = 0, 
                                 sampleID = 0) 
  
  TSG_bottletable <- data.frame(timebottle = "NA",
                            timeTSG = "NA",
                            lat = 0,
                            lon = 0,
                            sampleID = 0,
                            TSGsal = 0,
                            TSGChl = 0,
                            TSGOxy = 0,
                            TSGtemp = 0,
                            TSGcon = 0,
                            TSGUV = 0,
                            TSGpH = 0
                            ) 
  #bottleposix <- data.frame(time = "NA")
  y=1
  x=1
  for (i in fileslog){ print(i)
  d <- readLines(i, warn = FALSE)
  df <- data.frame(d)
  sampleID <- df$d[14]
  sampleID <- strsplit(sampleID, split = ': ')
  sampleID <- sampleID[[1]][2]
  ss <- df$d[17]
  ssp <- strsplit(ss, split = ':')
  gps <- ssp[[1]][2]
  sgps <- strsplit(gps, split = " | ")
  sdate <- sgps[[1]][2]
  stime <- sgps[[1]][4]
  xtime <- paste0(str_sub(stime, 1, 2), ":",str_sub(stime, 3, 4),":",str_sub(stime, 5, -1))
  xdatetime <- paste0(sdate, " ", xtime)
  #strptime(x, "%m/%d/%y %H:%M:%S")
  datepos <- as.POSIXct(xdatetime, tz = 'UTC')
  
  slatdeg <- as.numeric(sgps[[1]][6])
  slondeg <- as.numeric(sgps[[1]][10])
  slatdec <- as.numeric(sgps[[1]][7])/60
  slondec <- as.numeric(sgps[[1]][11])/60 
  slatdecdeg <- slatdec + slatdeg
  slondecdeg <- slondec + slondeg
  
  TSG_samples[y,1] <- xtime
  TSG_samples[y,2] <- sdate
  TSG_samples[y,3] <- xdatetime
  TSG_samples[y,4] <- slatdecdeg
  TSG_samples[y,5] <- slondecdeg
  TSG_samples[y,6] <- sampleID
  
  y=y+1
  
  }
  
  filestsg <- list.files(path= pathprocessed, pattern = 'TSGdata.*\\.csv', full.names = TRUE)
  #filestsgpco2 <- list.files(path= pathprocessed, pattern = 'PCO2data.*\\.csv', full.names = TRUE)
  
  
  for (g in filestsg){
    tsgall <- data.frame(read.csv(g, header = TRUE))
    
    TSG_samplesmin <- TSG_samples %>% mutate(match_time = floor_date(as.POSIXct(POSIXctdate, tz = "UTC"), "minute"))
    tsgall$match_time <- floor_date(ymd_hms(tsgall$time, tz = "UTC"), unit = "minute")
    
    # 2. Join them using the new minute-level key
    join_tsgctd <- inner_join(TSG_samplesmin, tsgall, by = "match_time")
    
    TSG_bottletable2 <- data.frame(timebottle = join_tsgctd$POSIXctdate,
                                  timeTSG = join_tsgctd$time.x,
                                  lat = join_tsgctd$lat,
                                  lon = join_tsgctd$lon,
                                  sampleID = join_tsgctd$sampleID,
                                  TSGsal = join_tsgctd$salinity_PSU,
                                  TSGChl = join_tsgctd$Fluorescence,
                                  TSGOxy = join_tsgctd$O2Concentration_ml_L,
                                  TSGtemp = join_tsgctd$Temperature_TSG_ITS_90,
                                  TSGcon = join_tsgctd$Conductivity_S_m,
                                  TSGUV = join_tsgctd$FluorescenceUV,
                                  TSGpH = join_tsgctd$pH
    ) 
    
    TSG_bottletable <- rbind(TSG_bottletable, TSG_bottletable2)
    
  }
  
  tsg_sample_file <- file.path(output_dir, "TSG_samples.csv")
  write.csv(TSG_samples, file = tsg_sample_file)
  
  tsg_bottle_file <- file.path(output_dir, "TSG_bottletable2.csv")
  write.csv(TSG_bottletable, file = tsg_bottle_file)

  ####################
  fileOXY <- list.files(path= pathbottle, pattern = 'Oxy.*\\.csv', full.names = TRUE) #list of log files with path
  fileChl <- list.files(path= pathbottle, pattern = 'Chl.*\\.xlsx', full.names = TRUE) #list of log files with path
  fileSal <- list.files(path= pathbottle, pattern = 'Salts.*\\.csv', full.names = TRUE, all.files=FALSE) #list of log files with path
  
  
  OXY_samples <-  data.frame(read.csv(fileOXY, header = TRUE, sep = ",", skip = 8, stringsAsFactors = FALSE))
  Chl_samples <-  data.frame(read_excel(fileChl, col_names = TRUE, skip = 18))
  Sal_samples <-  data.frame(read.csv(fileSal, header = TRUE, sep = ",", skip = 0, stringsAsFactors = FALSE))
  #Sal_samples <- Sal_samples[-c(1), ]
  
  TSG_samples$oxy1 = c(1:length(TSG_samples$time))
  TSG_samples$oxy2 = c(1:length(TSG_samples$time))
  TSG_samples$chl1 = c(1:length(TSG_samples$time))
  TSG_samples$chl2 = c(1:length(TSG_samples$time))
  TSG_samples$sal  = c(1:length(TSG_samples$time))
  TSG_samples$oxyTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$chlTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$salTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$tempTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$conTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$UVTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$pHTSGmean  = c(1:length(TSG_samples$time))
                      
  for (k in 1:length(TSG_samples$time)){
    
    matchoxy1 <- match(paste0(TSG_samples$sampleID[k], "_1"), OXY_samples$Sample)
    #matchoxy2 <- match(paste0(TSG_samples$sampleID[k], "_2"), OXY_samples$Sample)
    matchchl1 <- match(TSG_samples$sampleID[k], Chl_samples$I.D.)
    matchsal1 <- match(as.numeric(TSG_samples$sampleID[k]), as.numeric(Sal_samples$Bottle.Label))
   # matchTSG <-  match(TSG_bottletable$sampleID, as.numeric(TSG_samples$sampleID[k]))
    matchTSG <-  which(TSG_bottletable$sampleID==as.numeric(TSG_samples$sampleID[k]))
    
    TSG_oxymean <- mean(TSG_bottletable$TSGOxy[matchTSG], na.rm = TRUE)
    TSG_chlmean <- mean(TSG_bottletable$TSGChl[matchTSG], na.rm = TRUE)
    TSG_salmean <- mean(TSG_bottletable$TSGsal[matchTSG], na.rm = TRUE)
    TSG_tempmean <- mean(TSG_bottletable$TSGtemp[matchTSG], na.rm = TRUE)
    TSG_conmean <- mean(TSG_bottletable$TSGcon[matchTSG], na.rm = TRUE)
    TSG_UVmean <- mean(TSG_bottletable$TSGUV[matchTSG], na.rm = TRUE)
    TSG_pHmean <- mean(TSG_bottletable$TSGpH[matchTSG], na.rm = TRUE)
    
    # adding a new column to the data frame using $ symbol
    TSG_samples$oxy1[k] = OXY_samples$O2_Concentration.ml.l.[matchoxy1]
    #TSG_samples$oxy2[k] = OXY_samples$O2_Concentration.ml.l.[matchoxy2]
    TSG_samples$chl1[k] = Chl_samples$CHL.[matchchl1]
    TSG_samples$chl2[k] = Chl_samples$CHL.[matchchl1+1]
    TSG_samples$sal[k] = Sal_samples$Calculated.Salinity[matchsal1]
    TSG_samples$oxyTSGmean[k]  = TSG_oxymean
    TSG_samples$chlTSGmean[k]  = TSG_chlmean
    TSG_samples$salTSGmean[k]  = TSG_salmean
    
    TSG_samples$tempTSGmean[k]  = TSG_tempmean
    TSG_samples$conTSGmean[k]  = TSG_conmean
    TSG_samples$UVTSGmean[k]  = TSG_UVmean
    TSG_samples$pHTSGmean[k]  = TSG_pHmean
    
  }
  
  write_delim(TSG_samples, file = "4comparesamples/Bottle_TSG_QAT", delim = ",")
  write.csv(TSG_samples, file = "4comparesamples/Bottle_TSG_QAT.csv")
 TSG_samples <- data.frame(read.csv("4comparesamples/Bottle_TSG_QAT.csv", stringsAsFactors = FALSE))
  # 1. Open jpeg file, set the path, name and the image size
  jpeg(filename = paste0(getwd() ,"/4comparesamples/salt2sgplot.jpeg"), width = 1150, height = 750)
  # 2. Create a plot
  plot(TSG_samples$sampleID,TSG_samples$sal, col = "green", pch = 19, bg="green", cex=4, 
       ylim=c(round(min(as.numeric(TSG_samples$salTSGmean),na.rm = TRUE ))-1.5,round(max(as.numeric(TSG_samples$salTSGmean),na.rm = TRUE ))),
       main="Salinity, green=sample, red=TSG", font.main=1, cex.main=2, ylab="salinity",xlab="sample ID ",xaxt="n", font.lab=2, cex.lab=1.5)
  points(TSG_samples$sampleID,TSG_samples$salTSGmean, col = "dark red", pch = 19, bg="red", cex=2)
  axis(side = 1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
  # 3. Close the pdf file
  dev.off()
  
  # 1. Open jpeg file, set the path, name and the image size
  jpeg(paste0(getwd(),"/4comparesamples/oxy2tsgplot.jpeg"), width = 1150, height = 750)
  # 2. Create a plot
  plot(TSG_samples$sampleID,TSG_samples$oxy1, col = "green", pch = 19, bg="green", cex=3, ylim=c(4.5,round(max(as.numeric(TSG_samples$oxy1),na.rm = TRUE ))+0.8),
       main="Oxygen Concentration, green/blue=sample, red=TSG", font.main=1, cex.main=2, ylab="O2 Concentration (ml-L)",xlab="sample ID",xaxt="n", font.lab=2, cex.lab=1.5)
  axis(1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
  #points(TSG_samples$sampleID,TSG_samples$oxy2, col = "blue", pch = 19, bg="blue", cex=2)
  points(TSG_samples$sampleID,TSG_samples$oxyTSGmean, col = "dark red", pch = 19, bg="red", cex=2)
  # 3. Close the pdf file
  dev.off()
  
  # 1. Open jpeg file, set the path, name and the image size
  jpeg(paste0(getwd(),"/4comparesamples/chl2tsgplot.jpeg"), width = 1150, height = 750)
  # 2. Create a plot
  plot(TSG_samples$sampleID,TSG_samples$chl1, col = "green", pch = 19, bg="green", cex=3, ylim=c(0,round(max(as.numeric(TSG_samples$chlTSGmean),na.rm = TRUE ))+1.5),
       main="Chlorophyl, green/blue=sample, red=TSG",font.main=1, cex.main=2,  ylab="Chlorophyl",xlab="sample ID",xaxt="n", font.lab=2, cex.lab=1.5)
  axis(1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
  points(TSG_samples$sampleID,TSG_samples$chl2, col = "blue", pch = 19, bg="blue", cex=2)
  points(TSG_samples$sampleID,TSG_samples$chlTSGmean, col = "dark red", pch = 19, bg="red", cex=2)
  
  # 3.Close the pdf file
  dev.off()

}
