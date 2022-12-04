# MasterThesis_JohSche_12-2022

///////////////////////////////////////////////

CONTENT DESCRIPTION:

- The seven pre-processed data sets for the individual study areas (XXX_Data_08092022.txt). Sources are linked at the bottom of this document. 
- The "irfnnhs.r" script contains the functions for the calculation of the impulse response functions and does not need to be run. Make sure to save it in the same working directory as the "JoSche_MA_Script_122022.R" script (source: KIRCHNER, J. W. (2022): Impulse Response Functions for Nonlinear, Nonstationary, and Heterogeneous Systems, Estimated by Deconvolution and Demixing of Noisy Time Series. Sensors 22(9): 3291. DOI: 10.3390/s22093291).
- The "JoSche_MA_Script_122022.R" script contains the code for all Models and Figures presented within the final thesis "Comparative Analysis of In-Stream Nitrate Behaviour During Low-Flow Conditions Across Landscapes" which were calculated within RStudio. 

///////////////////////////////////////////////

RUN THE CODE:

1- Save both scripts ("irfnnhs.r" and "JoSche_MA_Script_122022.R") in the same working directory.

2- create a folder called "inputs" within the working directory you created in Step 1 and add all seven data sets (XXX_Data_08092022.txt) to this folder.

3- Open the "JoSche_MA_Script_122022.R" script.

4- Install necessary packages.

5- Run the Script (if you want to save time or have <= 16 GB of RAM, read NOTES 1 and 2)

///////////////////////////////////////////////

NOTE 1 (RAM):

Running the entire "JoSche_MA_Script_122022" script may take 5-10 hours depending on RAM availability and the CPU. If not enough RAM (< 32 GB) is available, the script might(!) crash during the calculation of the random forest models (in some cases it may work, so it is worth a try as long as you have at least 16 GB of RAM). If it crashes, run the models one-by-one (lines 1300-1420). Empty unused RAM in-between models if necessary. 
The models will not run on systems with <= 8 GB of RAM (tested on Windows 11). In this case, run lines 1-1294 and lines 1677-3978. This will skip Random Forest model calculations entirely.

///////////////////////////////////////////////

NOTE 2 (RUNTIME):

If you want to run the script faster without skipping the random forest models: 
Run lines 1 - 1420
and Lines 1548 - 4200
This way you do not calculate the mean minimal depth of the random forest models (reduces runtime by ~80-90%).

///////////////////////////////////////////////

SOURCES:

ACC: United States Geological Survey (2022): https://waterdata.usgs.gov/monitoring-location/01654000/#parameterCode=00065&period=P7D

HND & SBH: Terrestrial Environmental Observatories (2022): https://ddp.tereno.net/ddp/dispatch?sosurls=UFZ,https%3A%2F%2Fwebapp.ufz.de%2Fdmp%2Fsos%2Ftereno%2Fsos&autozoom=lomeparu

NNC: United States Geological Survey (2022): https://waterdata.usgs.gov/nwis/uv?site_no=07144780&legacy=1

SMC: United States Geological Survey (2022): https://waterdata.usgs.gov/monitoring-location/01632900/#parameterCode=00065&period=P7D

TMR: United States Geological Survey (2022): https://waterdata.usgs.gov/monitoring-location/04080798/#parameterCode=00065&period=P7D

WEB: Not publicly available.
