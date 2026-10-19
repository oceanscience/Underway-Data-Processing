# Underway-Data-Processing
Processing and plotting underway data and comparing with CTD and sample data

After cloning the project, make a copy of the .REnviron_sample file and rename
it to .REnviron.

Modify the settings in the .REnviron for the specific cruise:
* set the directory TSG_Input_Directory path to where the TSG files are saved.
* set the Processed_Directory to where you wish the processed files to be written to, don't write them to the Underway-Data-Processing project directory.
* set the Cruise_Number for the mission, e.g "DY18402"

Input Data:  
*  Copy the elog files for TSG sampling into the elog folder 
*  Copy the oxy, salts, Chl bottle data into the bottle folder
*  Copy the ODF ctd downward data into the ctd folder
*  Copy the TSG data csv logs into the Raw folder
*  delete the first lines of the file when the flow rate was zero, also the TSG water can be turned off for cleaning, maintenance or while in port, but logging continues confirm in ELOG or other logs when there is a data spike or missing data.
*  If there is pCO2 data this is copied to the Raw folder and some lines in the scripts may need to be uncommented.

Bottle data:
In read_elog_tsg3.R edit the number of lines to skip in the header of each file and the pattern to search for files 

oxygen:  
•	save the oxy file as csv, delete a bunch of sample data that is not from the TSG for the script to run faster.
•	The calibration coefficients may have changed ask OETS for the coefficients and place them in the code TSG_optode_SatO2_4.R.  This was never done in the past so you have to go back and rerun the code with the correct coefficients for each mission.

salts: 
•	concatenate the salts data into on csv files, delete a bunch of sample data that is not from the TSG for the script to run faster.

chlorophyl:  
•	concatenate the chl into one xls file, delete a bunch of sample data that is not from the TSG for the script to run faster. 
•	Chl data has 2 sheets in the excel file delete the first sheet “”rawValues” 
•	Remove the _1a, _1, _2  and _2a from the sample IDs
•	Remove the sample IDs that have no Chl and Phaeo values ie the sample ID with “a”


