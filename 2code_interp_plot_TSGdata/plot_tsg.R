#' @title plot each variable vs longitude and map station locations 
#' from interpdataall data frame created by TSG_interp.R 
#' 
#' @description creates and saves a plot of each variable vs longitude, a map of station locations 
#' from interpdataall data frame created by TSG_interp.R 
#' containing interpolated data from the BIO underway flow through system
#' 
#' @param interpdataallplot interpdataall data frame created by TSG_interp.R containing interpolated data and date-time,
#' and longitude and latitude limits for plots (lonlim,latlim)
#' 
#' @output a plot each variable vs longitude, and map station locations 
#' saved as jpeg in the folder \2code_interp_plot_TSGdata\hourly_TSG_dataplots
#' 
#' @author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada, Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

plot.tsg <- function(output_dir, interpdataallplot, lonlim, latlim){
  
  #Plot map of TSG data

  interpdataallplot$longitude <- interpdataallplot$longitude*-1 #change longitude to negative
 
  # 1. Open jpeg file, set the path, name and the image size
  output_file = file.path(output_dir, "TSG_map.jpeg")
  jpeg(output_file, width = 1150, height = 750)
  mapPlot(coastlineWorldFine, 
          projection="+proj=merc",
          col="lightgray", 
          longitudelim=lonlim, 
          latitudelim=latlim, lonlabels = TRUE,latlabels = TRUE,  )
  
  mapPoints(longitude = interpdataallplot$longitude,
            latitude = interpdataallplot$latitude, 
            pch = 4,cex = 0.5, col="blue")
  #mtext("TSG hourly positions", csi=5)
  mtext("TSG hourly positions")
  
  # Close the pdf file
  dev.off()
  
  #plot data by Longitude
  varnames <- colnames(interpdataallplot)
  varnum <- (3:(length(interpdataallplot)-1))
  
  #replace bad data with NA
  interpdataallplot[,3][interpdataallplot[,3] < 1] <- NA
  interpdataallplot[,4][interpdataallplot[,4] < 1] <- NA 
  interpdataallplot[,5][interpdataallplot[,5] < 1] <- NA
  interpdataallplot[,6][interpdataallplot[,6] > 2500] <- NA
  # interpdataallplot[,7][interpdataallplot[,7] > 959] <- NA
  # interpdataallplot[,9][interpdataallplot[,9] > 959] <- NA
  # interpdataallplot[,12][interpdataallplot[,12] > 959] <- NA
  
  for (i in varnum){
       
    # 1. Open jpeg file, set the path, name and the image size
    filename = paste0("TSG_",varnames[i],"_long.jpeg")
    output_file = file.path(output_dir, filename)
    jpeg(output_file, width = 1150, height = 750)
    
    # 2. Create a plot
    plot(interpdataallplot$longitude,interpdataallplot[,i],  
         main= paste0("TSG ", varnames[i], " hourly"), ylab=varnames[i],xlab="Longitude ",xaxt="n") 
    axis(side = 1, at = round(min(interpdataallplot$longitude,na.rm = TRUE )):round(max(interpdataallplot$longitude,na.rm = TRUE )))
    # Close the pdf file
    dev.off()
   
  }
  
  graphics.off()

}
