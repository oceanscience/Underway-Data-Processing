
#TSG configuration file for Anderra Optode Sensor
#StartupInfo	4831F	591	Mode	AADI Smart Sensor Terminal Protocol	RS232 
#Protocol Version	3	Config Version	12
# Feb 2016
#Product Number	4831F
#Serial Number	591
#SW ID	1940031
#SW Version	4	9	1
#Salinity[PSU]	0.00
#PhaseCoef	0.000000E+00	1.000000E+00	0.000000E+00	0.000000E+00
#FoilID	1207M

read.tsgcalphase <- function(tsgdata) {
  #rm(list=ls())
  
  FCA <- c(-3.176837E-06,	-6.413872E-06,	1.743939E-03,	
          -1.904569E-01,	7.106918E-04,	-5.010922E-07,	1.056089E+01,	
          -6.135393E-02,	1.165880E-04,	-3.871131E-07,	-3.042181E+02,	
          2.469668E+00,	-7.050199E-03,	-2.616267E-05)
  
  FoilCoefA <- as.data.frame(FCA, row.names = NULL,
                             stringsAsFactors = FALSE, col.names = NULL)
  FCB <- c(9.131562E-07,	3.768247E+03,	-4.024900E+01,	1.506045E-01,
           1.097636E-03,	-1.384983E-05,	-3.184898E-07,	0.000000E+00,	
           0.000000E+00,	0.000000E+00,	0.000000E+00,	0.000000E+00,	
           0.000000E+00,	0.000000E+00)
  
  FoilCoefB	<- as.data.frame(FCB, row.names = NULL,
                             stringsAsFactors = FALSE, col.names = NULL)
  
  FPT<- c(1,0,0,	0,1,2,0,1,2,3,0,1,2,3,4,0,1,2,3,4,5,0,0,0,0,0,0,0)
  FoilPolyDegT	<- as.data.frame(FPT, row.names = NULL,
                             stringsAsFactors = FALSE, col.names = NULL)
  
  FPO<- c(4,5,4,3,3,3,2,2,2,2,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
  FoilPolyDegO	<- as.data.frame(FPO, row.names = NULL,
                             stringsAsFactors = FALSE, col.names = NULL)
  SVUFC<- c(3.046782E-03,1.293621E-04,2.499496E-06,2.340326E+02,-3.365579E-01,
                  -5.374688E+01,	4.557863E+0)
  SVUFoilCoef <- as.data.frame(SVUFC, row.names = NULL,
                                stringsAsFactors = FALSE, col.names = NULL)
    
  ConcCoef<- c(0.000000E+00,	1.000000E+00)
  
  NomAirPress<- 1013.25  #[hPa]
  
  NomAirMix	<- 0.20946
  
  CalDataSat <- c(31.12,9.91)  #[Deg]
  
  CalDataAPress	<- 939.141   #[hPa]
  
  CalDataZero	<- 19.996  #[Deg]
  
  O2Concentration <- 0
  O2Concentrationcomp <- 0
  O2ConcentrationSV <- 0
  o2sat_sw=0 
  o2percentsat=0
  o2percentsatSV=0
  #	      time	Temperature_intake_ITS_90	Temperature_TSG_ITS_90	Conductivity_S_m	FluorescenceUV	pH	Fluorescence	calphase	salinity_PSU
  
  #11000	10/8/2022 15:17	999	15.28211	3.984185	1.6117	8.216	1.4042	31.6849817	31.9796
  #1    	10/9/2022 0:00	999	19.67218	4.63014	1.3919	8.281	1.3309	31.040293	33.9429
  
  for (y in 1:length(tsgdata$calphase)) {
    TSGtemp <- tsgdata$Temperature_TSG_ITS_90[y]
    calph <- tsgdata$calphase[y]
    sal <- tsgdata$salinity_PSU[y]
    
    #Based on the calibrated phase (CalPhase) and temperature the 
    #partial pressure of O2 is calculated by use of a two dimensional polynomial:
    # where the polynomial coefficients C0 to C13 are stored in the property FoilCoefA, 
    # C14 to C27 are stored in FoilCoefB, 
    # The temperature exponents, m0..27, are stored as FoilPolyDeg,
    # phase exponents, n0..27,are stored as FoilPolyDegO.
    
    DeltaPhaseA <- 0
    DeltaPhaseB <- 0
    for (i in 1:length(FCA)){
      DeltaPhaseA[i] <- FCA[i] * TSGtemp^FPT[i] *calph^FPO[i] 
    }
    
    for (i in 1:length(FCB)){
      DeltaPhaseB[i] <- FCB[i] * TSGtemp^FPT[i+14] *calph^FPO[i+14] 
    }
    
    DP <- c(DeltaPhaseA, DeltaPhaseB)
    DeltaPhase <- as.data.frame(DP, row.names = NULL,
                                 stringsAsFactors = FALSE, col.names = NULL)
    DeltaPhasetotal <- DP[1]
    DPsize <-  length(DP)-1
    for (i in 1:DPsize){
      DeltaPhasetotal <-  DeltaPhasetotal+DP[i+1]
    }
    
    # From the partial pressure the air saturation is then calculated as:
    #  where NomAirPress is a property for the nominal air pressure, usually 1013.25 hPa, 
    #  and NomAirMix is the nominal O2 content in air, by default 0.20946.
    
    Pvapour <- exp(52.57-(6690.9/(TSGtemp+273.15))-(4.681*log(TSGtemp+273.15)))
    airSaturation <- (DeltaPhasetotal*100)/((NomAirPress-Pvapour)*NomAirMix)
    
    Ts = log((298.15-TSGtemp)/(273.15+TSGtemp))  #  scaled temperature, Temperature, °C
    
    A0 = 2.00856 
    B0 = -6.24097e-3
    A1 = 3.22400 
    B1 = -6.93498e-3
    A2 = 3.99063 
    B2 = -6.90358e-3
    A3 = 4.80299 
    B3 = -4.29155e-3
    A4 = 9.78188e-1 
    A5 = 1.71069
    C0 = -3.11680e-7 
    
    C <-exp(A0+(A1*Ts)+(A2*Ts^2)+(A3*Ts^3)+(A4*Ts^4)+(A5*Ts^5)+ C0*sal^2)    #sal*(B0+B1*Ts+(B2*Ts^2)+(B3*Ts^3)) +
    
    O2Concentration[y] <- (C*44.659*airSaturation)/100   #uMol
    
    B0 = -6.24097e-3 
    C0 = -3.11680e-7
    B1 = -6.93498e-3
    B2 = -6.90358e-3
    B3 = -4.29155e-3
    
    O2Concentrationcomp[y] <- O2Concentration[y]*exp(sal*(B0+ B1*Ts+ B2*Ts^2+B3*Ts^3)+C0*sal^2)
    
    C0 = 3.046782E-03
    C1 = 1.293621E-04	
    C2 = 2.499496E-06	
    C3 = 2.340326E+02	
    C4 = -3.365579E-01	
    C5 = -5.374688E+01	
    C6 = 4.557863E+00
    C00 = -3.11680e-7 
    P0 = C3 + C4*TSGtemp
    Pc = C5 + C6*calph
    Ksv = C0+C1*TSGtemp+C2*TSGtemp^2
    O2ConcentrationSV[y] <- ((P0/Pc)-1)/Ksv
    O2ConcentrationSV[y] <- O2ConcentrationSV[y]*exp(sal*(B0+ B1*Ts+ B2*Ts^2+B3*Ts^3)+C00*sal^2)
    
    library(oce)
    library(csasAtlPhys)
    
    o2sat_sw[y]=swSatO2(TSGtemp,sal)*44.6589; #% factor to convert ml/l to uM/L
    
    o2percentsat[y]=100*O2Concentrationcomp/o2sat_sw;
    o2percentsatSV[y]=100*O2ConcentrationSV/o2sat_sw;
  }
  df <- data.frame(O2Concentrationcomp=O2Concentrationcomp, O2ConcentrationSV=O2ConcentrationSV, 
                   o2sat_sw =o2sat_sw,o2percentsat=o2percentsat,o2percentsatSV=o2percentsatSV)
  df
  #o2percentsat=100*335/o2sat; 
}