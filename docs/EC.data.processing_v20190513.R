# Script name: EC.data.processing.R (v20190204)
#
# Created on: Nov 2, 2018
#
# Last modified on: Feb 04, 2019
#
# Created by: Adam Green (a9green@uwaterloo.ca)
#             Myroslava Khomik (mkhomik@uwaterloo.ca)
#
# Script modification history: see end of script. If you modify anything, please update the
#                            "Last modified on" field above and also add a brief outline of
#                             the modification made to the script in the "MODIFICATION HISTORY"
#                             section located at the end of this script.
#
# INSTRUCTIONS: Before you run this script, please read the "USER INFO" section &
#               enter required information in the "USER INPUT" section.




######################  USER INFO #################################
## WHAT DOES THIS SCRIPT DO?
# This script processes EC-tower flux data, based on our lab's old Excel sheets that
# were used before Nov 2018, with modifications from literature-available R-scripts. For
# a list of references, see end of script and comments throughout.  The script goes through
# the following steps:
#           a) Input data fluxes (NEE, Qe and Qh) are quality checked: filtered for records used
#              for each half hour and any detected and user-defined outliers.
#           b) footprint is calculated from Kljun et al 2015 (uses her FFP R-functions)
#           c) filters out qa/qc'd fluxes (NEE, latent and sensible heat) based on footprint
#           d) Gapfills NEE using REddyProc R-package (Wurtzler et al 2018),
#              which also computes U-star threshold
#           e) filters qa/qc'd Qh and Qe fluxes based on REddyProc derived u-star (50%-median)
#           f) calculates energy budget closure and Bowen ratios
#           g) closes Energy budget and recalculates closure and Bowen ratio and provides
#              closed Qe and Qh fluxes
#           h) gapfills Qg with Qnet ratio
#           i) calculates PET using Preistly-Taylor equation
#           j) calculates ET from Qe and gapfills using relationship between nongapfilled ET and PET
#           h) recalculate energy balance and gapfill, where can, sensible heat flux
#           i) calculates daily sums and means of C, energy and ET fluxes over the measurement seson
#           j) provides some visual plots of hh and daily flux variables
#
## WHAT DOES THIS SCRIPT OUTPUT?
#  This script outputs 27 files in total, as follows: 10 .pdf files with plots; 8 .csv files
#  with backed-up data at different stages of calculations/analysis in the script;
#  2 R-files (.rds) with data for footprint plots; 7 text (.txt) with data summaries and
#  regression model results. Note the files will have infront of each
#  file name the site name and year of measurement, in addition to the following name identifiers:
#     1) Input.data.stats.txt  - this is basic stats on the distribution of each
#                     variable in the original Input.data file
#     2) FFP.data.no.NA.subset.backup.csv  - backup of data after Kljun footprint analysis,
#                                            but before footprint climatology analysis.
#     3) FFP.climatology.rds  - backup of results from footrping.climatology, used for 3D plots
#     4) FFP.rds - backup of results for the 2-D plots from footprint-run.
#     5) Input.data.w.FFP.backup.csv  - backup of data that has footprint filters
#     6) uStarThAnnual.txt  - text file with saved u-star estimates from REddyProc runs where had
#                             enough data to run the u*-distribution estimates (i.e. 5%, 50%, 95%)
#     7) Input.4.REddy.backup.csv - subset of data used in REddyProc part of the script, where
#                                   gapfill and partition C-fluxes.
#     8) Combined.FFP.NEE.backup.csv  - backup of data after REddyProc run (i.e. after NEE has
#                                       been gapfilled and partitioned into GPP and Reco)
#     9) ET.user.PET.line.txt  - text file with saved output from linear regression of ET.user vs PET,
#                                 used to gapfill ET data. ET.user is the ET filtered by user defined
#                                 u*-threshold.
#    10) ET.u05.PET.line.txt  - text file with saved output from linear regression of ET.u05 vs PET,
#                                used to gapfill ET data.
#    11) ET.u50.PET.line.txt  - text file with saved output from linear regression of ET.u50 vs PET,
#                                used to gapfill ET data.
#    12) ET.u95.PET.line.txt  - text file with saved output from linear regression of ET.u95 vs PET,
#                                used to gapfill ET data.
#    13) Master.backup.csv - has all the half hourly variables calculated in this script
#    14) QA.QC.GAPFILLED.EC.Fluxes.LONG.csv  - subset of Master file with select variable columns
#                                              which may be useful for students/postdoc in further
#                                              site data analysis and flux interpretation. Some of
#                                              the C-flux column names have been simplified.
#    15) QA.QC.GAPFILLED.EC.Fluxes.SHORT.csv - subset of Master file with fewer select variables
#                                              compared to LONG (subset above (i.e. only user defined
#                                              fluxes and U50 fluxes and associated filters).
#    16) Annual.Flux.totals.txt - text files with total NEE, Reco and GPP in g of C/m2 for the
#                                 entire season measured and ET in mm/m2 (if whole year (365/366),
#                                 then annual, othrewise summed over fewer days)
#    17) Daily.Fluxes.csv  - file with final clean and gapfilled NEE, Reco, GPP and ET variables
#                            presented as daily sums of the available half-hours in the Master file.
#                            In units of g C for CO2 and mm for ET. And also daily mean Energy fluxes
#                            (i.e. Qnet, gapfilled Qg, Qe and Qh)
#    Ten (10) pdf-files with select figures from different secions of the script:
#                       i)    Outlier.mean.filter.plots.pdf
#                       ii)   records.mean.user.plots.pdf
#                       iii)  Footprint.plots.pdf
#                       iv)   Footprint.filtered.fluxes.pdf
#                       v)    ReddyProc.plots.Annual.pdf
#                       vi)   ReddyProc.plots.userU.pdf
#                       Vii)  Ufiltered.Energy.Fluxes.pdf
#                       viii) Gapfilled.Energy.Fluxes.pdf
#                       ix)   Daily.Flux.plots.pdf
#                       x)    Final.Long.data.plots.pdf
#
#
## WHAT INPUT IS REQUIRED FROM THE USER TO RUN THE SCRIPT?
#      See "USER input" section on lines (138 to 233 in scriot below).  The user will also
#      need to interact with the script at different times. For example when running for the
#      first time on your PC, you may need to uncomment the "require" packages lines(251-271)
#      to install R-packages needed to run this script. Otherwise if you already have them,
#      "library" will work. When installing the "EBImage" package from a special link provided
#      by Kljun et al (this package is needed for running the footpring calculations in this
#      script), you may be prompted in the "output terminal" to update (a/s/n) packages, just
#      type "a" in the Terminal for "all" (s= some and n = none).
#      There is also an optional user-filtering section (lines ####), which requries you to first
#      look at a scatter plot of your flux data and then decide what max and min limits you want to
#      use to filter out the outliers (all data outside your chosen min-max range).
#      Finally there are 4 "Break-point" sections (by default commented out, in the script below),
#      which allow you to run the script in sections at a time and then come back to your analysis, at that "Break"
#      such that you don't have to rerun the lines above, but simply read-in the saved/backed-up
#      files and start working on them from then onwards. Break1=lines 746-763, Break2 = 802-818,
#      Break3 = lines 943-956, Break4 = lines 1237-1252.
#
#      NOTE:  Kljun FOOTPRINT analysis can take an hour or few to complete!
#      NOTE2: ReddyProc requires at least 3 months of NEE data to properly gapfill and partition.
#
#      REFERENCE used to develop this script are listed below (lines 3614-3639)
#
#      HISTORY OF SCRIPT MODIFICATION BEGINS ON line 3642.


####################################################################################
############################# CLEAR YOUR WORK SPACE TO CONTINUE ####################
####################################################################################
#The following line will clear your Global Environment space in R,
#so please save any previous work before running the script
rm(list =ls())
#
# The following will clear your plots window
graphics.off()



###########################################################################################
#############################  START OF USER INPUT SECTION ################################
###########################################################################################
# *****************************************************************************************
# PLEASE UPDATE THE REQUIRED (*) INFO MANUALLY IN THE LINES BELOW. (O) = optional fields.
# *****************************************************************************************
#
# (*) What is your working directory (i.e. folder where the scripts are located which you will run,
#     including this one). NOTE: if you copy paste from File-Explorer to replace "\" with "/"
working.dir <- "C:\Users\Kessel\Downloads\REddyProc-master\Nikanotee_Upland_2017"
#
# (*) What is the name of the .csv file that yaou will Input for analysis?
file.name = "Nikanotee_Upland_2017_ReddyProc_Input.csv"
#
# (*) What is the name of the address of the folder where the Input data file is located?
Input.directory = "C:\Users\Kessel\Downloads\REddyProc-master\Nikanotee_Upland_2017"
#
# (*) What is the address of the folder where you want to store the output and backup data files?
#     Make sure if you copy paste from File-Explorer to
#     replace "\" with "/"
Output.directory <-"C:\Users\Kessel\Downloads\REddyProc-master\Nikanotee_Upland_2017"

# (*) At which frequency was the data collected each half hour (ex.10 Hz)?
data.Hz = 10
#
# (*) What is the max distance encompassing your study site in each of the
#     following compass directions? (in meters (m))
#     This is used for filtering calculated footprints.
#     North (337.6 to 360 and 0 to 22.5 degrees)
N = 90
#     North-East (22.6 to 67.5 degrees)
NE = 155
#     East (67.6 to 112.5 degrees)
E = 150
#     South-East (112.6 to 157.5 degrees)
SE = 182
#     South (157.6 to 202.5 degrees)
S = 192
#     South-West (202.6 to 247.5 degrees)
SW = 200
#     West (247.6 to 292.5 degrees)
W = 125
#     North-West (292.6 to 337.5 degrees)
NW = 135
#
#     (Comment/Uncomment) if notprovided in the input file
# (*) At what height did you measure fluxes (i.e. displacement height) in meters (m)
zm = 2.0  #(i.e. z-d)
#
# (*) What is the roughness length for your site, in meters (m)?
#     Please enter "NA" if not known. Needed to estimate roughness sublayer. Note
#     that to calculate the footpring you need to specify either z0 or mean wind speed.
#     If both are given, then the script uses z0.
z0 = "NA"
#
# (O) Which percent source area are you interested in for the footprint? (%)
#     This optional input variable, percent of source area from 10%-90%. Can take on a
#     single value (ex. "90") or as a sequence of increasing values (ex. "seq(10, 40, 60, 80)").
#     Default is set to (10,10,80). In our Hydromet lab, we are typically insterested in the
#     distance from which 80% of the flux comes from. So it is set at 80% below. You can
#     modify this field as you like. see Kljun et al 2015 for more details.
#     Set to "NaN" or "NULL" if don't want this option and will run in default mode.
r.Kljun = 80
#
# (*) From what year are your site data you are about to analyze?
measurement.year = 2017
#
# (*) How many days did you measure for? (ex. full year = 356 or leap = 366)
measurement.days = 91
#
# (*) What's the name of your site? This will be used as identifier in the name of the final output
#     file. Make it something memorable and unique, if you are processing several files from the
#     same site. Note that the measurement year specified above will also be included in the
#     fname of the output file.
site.name = "Nikanotee_Upland"
#
# (*) What are your site's Latitude (Lat) and Longitude (Lon) in degrees?
site.Lat =  56.9311
site.Lon = -111.4172
#
# (*) By how many hours is the time of your data offset in comparison to GMT?
time.zone = -7
#
# (O) What is the Ustar threshold you determined manually, if applicable? Please see lines
#     ######### ADD LINES for more info
U.Threshold.user = 0.1
#
# (*) What percent of Energy Balance closure would you accept?
#     (will be used to filter out fluxes, anything below to reject)
EN.balance.closure = 80
#
#
#*************************************************************************************
######################################################################################
################## END OF USER INPUT SECTION (but see notes above) ###################
######################################################################################





######################################################################################
############ !! BELOW SCRIPT IS NOT FOR USER MODIFICATION !! #########################
######################################################################################
#########################  START OF ANALYSIS SCRIPT ##################################
######################################################################################
#
#### SET WORKING DIRECTORY ###
##############################
setwd(working.dir)



#### INSTALL R-packages ###
###########################
#These are needed to run this script
# #install.packages
# require(fields)
# require(plot3D)
# require(stats)
# require(dplyr)
# require(data.table)
# #below ones are esp for Kljun et al 2015 footprint code
# source("https://bioconductor.org/biocLite.R")
# biocLite("EBImage")
# require(EBImage)
# require(spatialfil)
# #below are for NEE partition and gapfilling - REddyProc
# require(purrr)
# require(REddyProc)
# require(forcats)
# require(zoo)
# require(spam)
# require(lattice)

# If you already have the above packages, then you can simply run the lines below.
library(plot3D)
library(stats)
library(dplyr)
library(data.table)
#below ones are esp for Kljun et al 2015 footprint code
library(spatialfil)
library(purrr)
library(REddyProc) #for NEE partition and gapfilling - REddyProc
library(forcats)
library(zoo)
library(fields) # apparently fields package needs this one to run.
library(spam)
library(lattice)




#### LIST of CONSTANTS  USED IN THIS SCRIPT ###
###############################################
#von Karmen Constant, unitless
k=0.41
#acceleration due to gravity meters/second
g= 9.81
#specific heat of air at constant pressure (and temp in Kelvin) in J/kg/K
Cp.air.K = 1005
#canopy surface resistance in seconds/meter
r.surface = 0
#bulk aerodynamic resistance for water (seconds/meter)
r.water = 1
#latent heat of vapourization in Mj/kg
lamda.vap = 2.45
# molar mass of CO2
molarMass.CO2 <- 12.011




#### IMPORT DATA SET ###
########################
Input.file.path <- paste(Input.directory, file.name, sep="/")
Input.data <- read.csv(Input.file.path, header=TRUE, na.strings="-9999")
str(Input.data)
names(Input.data)
#
#some descriptive stats on Input.data
summary(Input.data)
#save above stats to a text file for future reference
final.file.name <- paste(site.name, measurement.year, "Input.data.stats.txt", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
sink(File.name.path)
print(str(Input.data))
print(names(Input.data))
print(summary(Input.data))
sink()  # returns output to the console
#
#Check to see if zm exists in file and if not add zm to input file, as defined by user above.
if(exists("zm")){Input.data$zm<- zm}




#### QC/QA: FILTER BY RECORDS  ###
##################################
#filter out fluxes by number of half hourly records
#calculate number records you should have in each half hour, based on data collection
#frequency (data.Hz) specified by user (assume user knows how often data is measured
#by tower)
records = data.Hz*60*30
#
#create filter to keep only rows where have at least 85% of full half hourly records
Input.data$records.filter <- ifelse(Input.data$used.records >= records*.85, 1, 0)
#
#create filtered NEE, Qe and Qh columns, where filter fluxes by records-filter
Input.data$NEE.records.filtered <- Input.data$NEE*Input.data$records.filter
Input.data$Qh.records.filtered <- Input.data$Qh*Input.data$records.filter
Input.data$Qe.records.filtered <- Input.data$Qe*Input.data$records.filter




#### QC/QA: FILTER NEE, Qe and Qh FOR OUTLIERS BASED ON NEIGHBOURING NEE-MEANS/STDEV###
#######################################################################################
#check NEE distributions
hist(Input.data$NEE.records.filtered)
plot(NEE.records.filtered~DoY, data=Input.data)
#
#count the number of hh available 5hours before and after each point
Input.data$NEE.count.lead.5 <- rollapply(Input.data$NEE.records.filtered,  width=list(-1:-5), FUN = function(x) sum(!is.na(x)),
                                         fill=NA)
Input.data$NEE.count.lag.5 <- rollapply(Input.data$NEE.records.filtered,  width=list(1:5), FUN = function(x) sum(!is.na(x)),
                                        fill=NA)
#check if will have at least 4/5 values out of the 5 hh to compute mean and sd (80%)
Input.data$f.count.lead.5 <- ifelse(Input.data$NEE.count.lead.5<4|is.na(Input.data$NEE.count.lead.5), 0, 1)
Input.data$f.count.lag.5 <- ifelse(Input.data$NEE.count.lag.5<4|is.na(Input.data$NEE.count.lag.5), 0, 1)
#
#if have at least 4/5 hh, calculate the means and standard deviations (sd) for 5 half hours above and below each point,
#which will be used to check flagged outlier points to see if they are really outliers
#(i.e. bigger than 3.5 times the sd).
Input.data$NEE.mean.lead.5 <- ifelse(Input.data$f.count.lead.5==1, rollapply(Input.data$NEE.records.filtered,  width=list(-1:-5), mean, na.rm=TRUE, fill=NA), NA)
Input.data$NEE.sd.lead.5 <- ifelse(Input.data$f.count.lead.5==1, rollapply(Input.data$NEE.records.filtered,  width=list(-1:-5), sd, na.rm=TRUE, fill=NA), NA)
#
Input.data$NEE.mean.lag.5 <- ifelse(Input.data$f.count.lag.5==1, rollapply(Input.data$NEE.records.filtered,  width=list(1:5), mean, na.rm=TRUE, fill=NA), NA)
Input.data$NEE.sd.lag.5 <- ifelse(Input.data$f.count.lag.5==1, rollapply(Input.data$NEE.records.filtered,  width=list(1:5), sd, na.rm=TRUE, fill=NA), NA)
#
#create sd.ranges for each case of lag and lead cases, and when there is enough data
Input.data$NEE.sd.lag.5.max <-3.5*Input.data$NEE.sd.lag.5 + Input.data$NEE.mean.lag.5
Input.data$NEE.sd.lag.5.min <-Input.data$NEE.mean.lag.5 - 3.5*Input.data$NEE.sd.lag.5
Input.data$NEE.sd.lead.5.max <- 3.5*Input.data$NEE.sd.lead.5 + Input.data$NEE.mean.lead.5
Input.data$NEE.sd.lead.5.min <- Input.data$NEE.mean.lead.5-3.5*Input.data$NEE.sd.lag.5
#
#Do same as above, but this time for n=10 half-hours
#count the number of hh available 10 hours before and 10 hh after each point
Input.data$NEE.count.lead.10 <- rollapply(Input.data$NEE.records.filtered,  width=list(-1:-10), FUN = function(x) sum(!is.na(x)),
                                          fill=NA)
Input.data$NEE.count.lag.10 <- rollapply(Input.data$NEE.records.filtered,  width=list(1:10), FUN = function(x) sum(!is.na(x)),
                                         fill=NA)
#check if will have at least 8/10 values out of the 10 hh to compute mean and sd (80%)
Input.data$f.count.lead.10 <- ifelse(Input.data$NEE.count.lead.10<8|is.na(Input.data$NEE.count.lead.10), 0, 1)
Input.data$f.count.lag.10 <- ifelse(Input.data$NEE.count.lag.10<8|is.na(Input.data$NEE.count.lag.10), 0, 1)
#
#if have at least 8/10 hh, calculate the means and standard deviations (sd) for 10 half hours above and 10 hh below each point,
#which will be used to check flagged outlier points to see if they are really outliers
#(i.e. bigger than 3.5 times the sd).
Input.data$NEE.mean.lead.10 <- ifelse(Input.data$f.count.lead.10==1, rollapply(Input.data$NEE.records.filtered,  width=list(-1:-10), mean, na.rm=TRUE, fill=NA), NA)
Input.data$NEE.sd.lead.10 <- ifelse(Input.data$f.count.lead.10==1, rollapply(Input.data$NEE.records.filtered,  width=list(-1:-10), sd, na.rm=TRUE, fill=NA), NA)
#
Input.data$NEE.mean.lag.10 <- ifelse(Input.data$f.count.lag.10==1, rollapply(Input.data$NEE.records.filtered,  width=list(1:10), mean, na.rm=TRUE, fill=NA), NA)
Input.data$NEE.sd.lag.10 <- ifelse(Input.data$f.count.lag.10==1, rollapply(Input.data$NEE.records.filtered,  width=list(1:10), sd, na.rm=TRUE, fill=NA), NA)
#
#create sd.ranges for each case of lag and lead cases, and when there is enough data
Input.data$NEE.sd.lag.10.max <-3.5*Input.data$NEE.sd.lag.10 + Input.data$NEE.mean.lag.10
Input.data$NEE.sd.lag.10.min <-Input.data$NEE.mean.lag.10 - 3.5*Input.data$NEE.sd.lag.10
Input.data$NEE.sd.lead.10.max <- 3.5*Input.data$NEE.sd.lead.10 + Input.data$NEE.mean.lead.10
Input.data$NEE.sd.lead.10.min <- Input.data$NEE.mean.lead.10-3.5*Input.data$NEE.sd.lag.10
#
#
#filter out NEE by comparing each value to neighbouring 5hh or 10 hh mean+/- 3.5*sd (chose 3.5sd
#based on discussion in Aubinet et al "Eddy Covariance" book, pp.68-69). If value is outside of
#the acceptable range, then flag as 1, else flag=0. Zero also if there were not enough hh to cross
#calculate the means/sd to cross-check with.
Input.data$NEE.mean.filter <-  ifelse(Input.data$f.count.lag.5==1,
                                      ifelse(Input.data$NEE.records.filtered<Input.data$NEE.sd.lag.5.max
                                       & Input.data$NEE.records.filtered>Input.data$NEE.sd.lag.5.min , 0 , 1),

                                  ifelse(Input.data$f.count.lag.10==1,
                                      ifelse(Input.data$NEE.records.filtered<Input.data$NEE.sd.lag.10.max
                                      & Input.data$NEE.records.filtered>Input.data$NEE.sd.lag.10.min, 0, 1),

                                  ifelse(Input.data$f.count.lead.5==1,
                                      ifelse(Input.data$NEE.records.filtered<Input.data$NEE.sd.lead.5.max
                                      & Input.data$NEE.records.filtered>Input.data$NEE.sd.lead.5.min , 0 , 1),

                                  ifelse(Input.data$f.count.lead.10==1,
                                      ifelse(Input.data$NEE.records.filtered<Input.data$NEE.sd.lead.10.max
                                      & Input.data$NEE.records.filtered>Input.data$NEE.sd.lead.10.min, 0, 1), NA))))
#
#filter out data based on above outlier.flag, if flag=1 or NA, then NEE set to NA, else existing NEE value is retained.
Input.data$NEE.mean.filtered <-  ifelse(Input.data$NEE.mean.filter==1|is.na(Input.data$NEE.mean.filter), NA, Input.data$NEE.records.filtered)
#
#also remove associated Qh and Qe half hours
Input.data$Qh.mean.filtered <-  ifelse(Input.data$NEE.mean.filter==1|is.na(Input.data$NEE.mean.filter), NA, Input.data$Qh.records.filtered)
Input.data$Qe.mean.filtered <-  ifelse(Input.data$NEE.mean.filter==1|is.na(Input.data$NEE.mean.filter), NA, Input.data$Qe.records.filtered)
#
##cross check filtered NEE data
plot(NEE.records.filtered~DoY, data=Input.data, main="black=old, green=new")
points(NEE.mean.filtered~DoY, data=Input.data, col="green")
#
##cross check filtered Qh adn Qe data
plot(Qh.records.filtered~DoY, data=Input.data, col= "orange", main="orange=old, green=new")
points(Qh.mean.filtered~DoY, data=Input.data, col="green")
plot(Qe.records.filtered~DoY, data=Input.data,col="blue", main="blue - removed, green=kept")
points(Qe.mean.filtered~DoY, data=Input.data, col="green")
#
#
#save plots to pdf.file
final.file.name <- paste(site.name, measurement.year, "Outlier.mean.filter.plots.pdf", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
pdf(file=File.name.path)
plot(NEE.records.filtered~DoY, data=Input.data, main="black=old, green=new")
points(NEE.mean.filtered~DoY, data=Input.data, col="green")
plot(Qh.records.filtered~DoY, data=Input.data, col= "orange", main="orange=old, green=new")
points(Qh.mean.filtered~DoY, data=Input.data, col="green")
plot(Qe.records.filtered~DoY, data=Input.data,col="blue", main="blue - removed, green=kept")
points(Qe.mean.filtered~DoY, data=Input.data, col="green")
#
dev.off()



#### OPTIONAL QA/QC - FILTER OUT FLUXES FURTHER OUTLIERS BASED ON USER DEFINED LIMITS ###
#########################################################################################
#comment out if you don't want to use this part of the script, or simply skip to the next section
#which is "VPD calculation section".
#
#Define your limits based on best-judgement and knowledge of site
#check flux distributions to help you decide
hist(Input.data$NEE.mean.filtered)
plot(NEE.mean.filtered~DoY, data=Input.data)
# Based on above plots set the upper and lower limits for NEE
NEE.outlier.upper.limit = 30
NEE.outlier.lower.limit = -40
#
hist(Input.data$Qh.mean.filtered)
plot(Qh.mean.filtered~DoY, data=Input.data)
# Based on above plots set the upper and lower limits for Qh
Qh.outlier.upper.limit = 300
Qh.outlier.lower.limit = -100
#
hist(Input.data$Qe.mean.filtered)
plot(Qe.mean.filtered~DoY, data=Input.data)
# Based on above plots set the upper and lower limits for Qe
Qe.outlier.upper.limit = 500
Qe.outlier.lower.limit = -100
#
#Now re-filter all flux values based on above limits, by defining a flag-variable for each and then
#filtering by that flag
#if NEE is outside of the user-defined outlier limits, flag that value as 1, else zero.
Input.data$NEE.outlier.user.flag <- ifelse(Input.data$NEE.mean.filtered < NEE.outlier.upper.limit &
                                        Input.data$NEE.mean.filtered > NEE.outlier.lower.limit, 0, 1)
#
Input.data$Qh.outlier.user.flag <- ifelse(Input.data$Qh.mean.filtered < Qh.outlier.upper.limit &
                                             Input.data$Qh.mean.filtered > Qh.outlier.lower.limit, 0, 1)
#
Input.data$Qe.outlier.user.flag <- ifelse(Input.data$Qe.mean.filtered < Qe.outlier.upper.limit &
                                             Input.data$Qe.mean.filtered > Qe.outlier.lower.limit, 0, 1)
#
#final filter by which flues will be filtered out for outliers, as defined by user
Input.data$NEE.no.outliers2 <-  ifelse(Input.data$NEE.outlier.user.flag==1, NA, Input.data$NEE.mean.filtered)
Input.data$Qh.no.outliers2 <-  ifelse(Input.data$Qh.outlier.user.flag==1, NA, Input.data$Qh.mean.filtered)
Input.data$Qe.no.outliers2 <-  ifelse(Input.data$Qe.outlier.user.flag==1, NA, Input.data$Qe.mean.filtered)
#
#cross check filtered data
hist(Input.data$NEE.no.outliers2)
plot(NEE.no.outliers2~DoY, data=Input.data)
#
hist(Input.data$Qh.no.outliers2)
plot(Qh.no.outliers2~DoY, data=Input.data)
#
hist(Input.data$Qe.no.outliers2)
plot(Qe.no.outliers2~DoY, data=Input.data)
#
#Create final filtered NEE value (flux.filtered) which will work with from here on. So this NEE.filtered
# has been filtered for hh-record.numbers, mean.filter adn optionally user-defined filters.
Input.data$NEE.filtered <- Input.data$NEE.no.outliers2
Input.data$Qh.filtered <- Input.data$Qh.no.outliers2
Input.data$Qe.filtered <- Input.data$Qe.no.outliers2
#
#double check values
plot(NEE~DoY, data=Input.data, main="black=old, green=new NEE timeseries")
points(NEE.filtered~DoY, data=Input.data, col="green")
#
plot(Qh~DoY, data=Input.data, col="orange", main="orange=old, green=new Qh timeseries")
points(Input.data$Qh.filtered~DoY, data=Input.data, col="green")
#
plot(Qe~DoY, data=Input.data, col="blue", main="blue=old, green=new Qe timeseries")
points(Qe.filtered~DoY, data=Input.data, col="green")
#
#save plots to pdf.file
final.file.name <- paste(site.name, measurement.year, "records.mean.user.filter.plots.pdf", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
pdf(file=File.name.path)
plot(NEE~DoY, data=Input.data, main="black=old, green=new NEE timeseries")
points(NEE.filtered~DoY, data=Input.data, col="green")
plot(Qh~DoY, data=Input.data, col="orange", main="orange=old, green=new Qh timeseries")
points(Input.data$Qh.filtered~DoY, data=Input.data, col="green")
plot(Qe~DoY, data=Input.data, col="blue", main="blue=old, green=new Qe timeseries")
points(Qe.filtered~DoY, data=Input.data, col="green")
dev.off()




#######  CALCULATE VPD   #####################
##############################################
#calculate Vapour Pressure Deficit, VPD, (using: Saturated vapour pressure (es) in kPa;
#and actual vapour pressure (ea) in kPa):
Input.data$es <- 0.6108*exp((17.269*Input.data$Tair)/(Input.data$Tair+237.3)) # Tair in oC
Input.data$ea = (Input.data$rH*Input.data$es/100)
Input.data$VPD <- Input.data$es-Input.data$ea # in kPa
# set values below zero to zero
Input.data$VPD[Input.data$VPD < 0] <- 0
#
#check VPD (in kPa)
hist(Input.data$VPD)
max(Input.data$VPD, na.rm=T)
min(Input.data$VPD, na.rm=T)





######  CALCULATE ATMOSPHERIC STABILITY HEIGHT (L)  ######
##########################################################
#calculate atompsheric stability, L (m), based on equation 13 in Abassi et al 2017,
#but for air density do not use the 100 multiplier they list in their equaltion 3.
#calculate latent heat of vapourization of water in J/kg, where Tair in oC
Input.data$Lvap = 2.5*10^6-2361*Input.data$Tair
#calculate speficif humidity (kg/kg), ea and Pair are in kPa
Input.data$spec.hum = 0.6108*Input.data$ea/(Input.data$Pressure/1000)
#calculate, Ra, gas constant of moist air (J/kg/K)
Input.data$Ra = 287*(1+0.608*Input.data$spec.hum)
#calculate density of air, q.air (kg/m3), where Pair is in Pascals, Tair in C
Input.data$q.air = (Input.data$Pressure/(Input.data$Ra*(273.16+Input.data$Tair)))
#calculate virtual air temperature, Tav in K
Input.data$Tav = (Input.data$Tair+273.16)*(1+0.61*Input.data$spec.hum)
#calculate Monin-Obukhov length, L (m) = eq 13 of Abassi et al 2017
Input.data$L = -1*(Input.data$Ustar)^3*(Input.data$q.air)*(Input.data$Tav)/
                      (k*g*(
                        (Input.data$Qh.filtered/Cp.air.K)+
                          0.61*(Input.data$Tair+273.16)*Input.data$Qe.filtered/
                          Input.data$Lvap))
#cross check L values
hist(Input.data$L)
max(Input.data$L, na.rm=T)
min(Input.data$L, na.rm=T)
plot(L~DoY, data=Input.data)




#### CALCULATE BOUNDARY LAYER HEIGHT (h.PBL) ###
################################################
#this calculation is based on code from Campbell-Scientific EasyFlux PC script,
#which is based on data in the L and h colums of Table I in Kljun et al (2004)
Input.data$h.PBL <- ifelse (
  !is.na(Input.data$Ustar)& !is.na(Input.data$WS)& !is.na(Input.data$L),
    ifelse(Input.data$L <= -1013.3, 1000,
           ifelse(Input.data$L <=-650 & Input.data$L>-1013.3,
                  1200-200*((Input.data$L+650)/(-1013.3+650)),
                  ifelse(Input.data$L <=-30 & Input.data$L>-650,
                         1500-300*((Input.data$L+30)/(-650+30)),
                         ifelse(Input.data$L <=-5 & Input.data$L>-30,
                                2000-500*((Input.data$L+5)/(-30+5)),
                                ifelse(Input.data$L <= 0 & Input.data$L > -5,
                                       2000+20*(Input.data$L+5),
            ifelse(Input.data$L >0 & Input.data$L<84,
                   200-((84-Input.data$L)*(50/46)),
                   ifelse(Input.data$L >=84 & Input.data$L<130,
                          200+50*((Input.data$L-84)/(130-84)),
                          ifelse(Input.data$L >=130 & Input.data$L<1000,
                                 250+550*((Input.data$L-130)/(1000-130)),
                                 ifelse(Input.data$L >=1000 & Input.data$L<1316.4,
                                        800+200*((Input.data$L-1000)/(1316.4-1000)),
                                        ifelse(Input.data$L>1316.4, 1000, NA)))))))))),
                                        NA)
#cross check h values
hist(Input.data$h.PBL)
max(Input.data$h.PBL, na.rm=T)
min(Input.data$h.PBL, na.rm=T)
plot(h.PBL~DoY, data = Input.data, main="h.PBL (m) timeseries")
plot(h.PBL~DoY, data = subset(Input.data, Rg<10), main="h.PBL (m) timeseries - night")
plot(h.PBL~DoY, data = subset(Input.data, Rg>10), main="h.PBL (m) timeseries - day")





#### CALCULATE FOOTRPINTS - Kljun method ###
############################################
#for more details on this method read Kljun et al (2015)
#code adapted from "http://footprint.kljun.net/download.php" [November 2018]
#
#select columns from Input.data set that will be used in footprint analysis
FFP.variables <- c("Year", "DoY", "Hour", "WD", "WS", "Ustar", "sigmav", "L", "zm", "h.PBL")
#create subset of Input.data, with only FFP.variables
FFP.data <- Input.data[FFP.variables]
#keep only rows that have non missing values across all variables
FFP.data.no.NA <- na.omit(FFP.data)
#
#keep rows where h.PBL > zm and h.PBL >10m
FFP.data.no.NA.subset <- subset(FFP.data.no.NA, h.PBL > zm & h.PBL > 10)
#
#test for conditions needed for Kljun's script (ex. zm/L =>-15.5)
FFP.data.no.NA.subset$zm.L <- FFP.data.no.NA.subset$zm/FFP.data.no.NA.subset$L
min(FFP.data.no.NA.subset$zm.L)
#keep rows where zm/L is >= -15.5
FFP.data.no.NA.subset <- subset(FFP.data.no.NA.subset, zm.L >= -15.5)
#double check
min(FFP.data.no.NA.subset$zm.L)
#
#create place holders for columns where data will be output: Max footprint distance (FFPMax)
#and footprint distance from which 80% of flux comes from (FFP_80):
for (i in 1:length(FFP.data.no.NA.subset$Year)){
  FFP.data.no.NA.subset$FFPMax[i]<-NA
  FFP.data.no.NA.subset$FFP_80[i]<-NA}
#
#CALL-UP Kljun et al (2016) FFP function which will be used in the analysis below.
#Note that the R-script containing these functions should be stored in your current
#working directory, in order for the code below to work.
source("calc_footprint_FFP.R")
#
#CALCULATE INDIVIDUAL FOOTPRINTS (hh), using calc_footpring_FFP function (from Kljun et al 2015)
#NOTE: This bit of code can take several hours to run for dataset of about 10,000 rows or more.
for (i in 1:length(FFP.data.no.NA.subset$Year)){
  FFP <- calc_footprint_FFP(zm=FFP.data.no.NA.subset$zm[i],
                            z0=NA,
                            umean = FFP.data.no.NA.subset$WS[i],
                            h=FFP.data.no.NA.subset$h.PBL[i],
                            ol=FFP.data.no.NA.subset$L[i],
                            sigmav=FFP.data.no.NA.subset$sigmav[i],
                            ustar=FFP.data.no.NA.subset$Ustar[i],
                            wind_dir=NULL,
                            r= r.Kljun, nx = NULL,
                            rslayer = 1, crop = NULL)
  FFP.data.no.NA.subset$FFPMax[i]<-FFP$x_ci_max
  FFP.data.no.NA.subset$FFP_80[i]<-max(unlist(FFP$xr))
}
#
#The next steps saves the FFP object (which has the data for plotting 2D footprint maps),
#as a R-file (i.e. in R-data-format), which can be accessed at a later date for any future
#replotting and processing without the need to rerun the above step.
final.file.name <- paste(site.name, measurement.year, "FFP.rds", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
saveRDS(FFP, file = File.name.path)
#
#
#Filter individual half hourly 80%-source footprint distances, based on the user-defined
#distances in each compass direction.
#create the filter to be used for directional filtering ("WD.filter")
FFP.data.no.NA.subset$WD.filter <-
ifelse(FFP.data.no.NA.subset$WD > 0 & FFP.data.no.NA.subset$WD <= 22.5,
               FFP.data.no.NA.subset$WD.filter <- N,

        ifelse(FFP.data.no.NA.subset$WD > 22.5 & FFP.data.no.NA.subset$WD <= 67.5,
               FFP.data.no.NA.subset$WD.filter <- NE,

        ifelse(FFP.data.no.NA.subset$WD > 67.5 & FFP.data.no.NA.subset$WD <= 112.5,
               FFP.data.no.NA.subset$WD.filter <- E,

        ifelse(FFP.data.no.NA.subset$WD > 112.5 & FFP.data.no.NA.subset$WD <= 157.5,
               FFP.data.no.NA.subset$WD.filter <- SE,

        ifelse(FFP.data.no.NA.subset$WD > 157.5 & FFP.data.no.NA.subset$WD <= 202.5,
              FFP.data.no.NA.subset$WD.filter <- S,

        ifelse(FFP.data.no.NA.subset$WD > 202.5 & FFP.data.no.NA.subset$WD <= 247.5,
              FFP.data.no.NA.subset$WD.filter <- SW,

        ifelse(FFP.data.no.NA.subset$WD > 247.5 & FFP.data.no.NA.subset$WD <= 292.5,
              FFP.data.no.NA.subset$WD.filter <- W,

        ifelse(FFP.data.no.NA.subset$WD > 292.5 & FFP.data.no.NA.subset$WD <= 337.5,
              FFP.data.no.NA.subset$WD.filter <- NW,

        ifelse(FFP.data.no.NA.subset$WD > 337.5 & FFP.data.no.NA.subset$WD <= 360,
              FFP.data.no.NA.subset$WD.filter <- N, NA
       )))))))))
#
#WD.filter will next be used to filter out distances outside of site boundaries
FFP.data.no.NA.subset$footprint.filter <-
  ifelse(FFP.data.no.NA.subset$FFP_80 <= FFP.data.no.NA.subset$WD.filter, 1, 0)
#
#filtered footprint at 80% source distance and within site boundaries
FFP.data.no.NA.subset$filtered.FFP.80 <-
  FFP.data.no.NA.subset$FFP_80 * FFP.data.no.NA.subset$footprint.filter
#
#filtered 80% footprint distance cross check, max should be within site boundaries!
hist(FFP.data.no.NA.subset$filtered.FFP.80)
max(FFP.data.no.NA.subset$filtered.FFP.80, na.rm=T)
min(FFP.data.no.NA.subset$filtered.FFP.80, na.rm=T)
#
#backup FFP.data.no.NA.subset
final.file.name <- paste(site.name, measurement.year, "FFP.data.no.NA.subset.backup.csv", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
write.csv(FFP.data.no.NA.subset, file=File.name.path)
#
#
############ OPTIONAL SCRIPT BREAK POINT 1: START #####
######################################################
#if you ran all of the above script, the output-till-here was saved as
#"FFP.data.no.NA.subset.backup.csv" in your Output directory. So you could stop here and
#come back to the analysis, without the need to rerun the footprint analysis. To do this
#you can uncomment the lines of code below and import that backed-up file manually to
#continue processing the script below. You will also need to load the R-object FFP needed
#for plotting footprints below.
#
#TO IMPORT backed-up file COMMENT/UNCOMMENT the 6 lines below:
# final.file.name <- paste(site.name, measurement.year, "FFP.data.no.NA.subset.backup.csv", sep=".")
# backup.file.path <- paste(Output.directory, final.file.name, sep="/")
# FFP.data.no.NA.subset <- read.csv(backup.file.path, header=TRUE, na.strings="NA")
# final.file.name <- paste(site.name, measurement.year, "FFP.rds", sep=".")
# backup.file.path <- paste(Output.directory, final.file.name, sep="/")
# FFP <- readRDS(backup.file.path)
#
########### END of OPTIONAL SCRIPT BREAK POINT 1 #####
#
#
#
#CALCULATE FOOTPRINT CLIMATOLOGY, using calc_footpring_FFP_climatology function from
#Kljun et al (2015). This function takes on several rows of data and computes mean
#footprint over that time range. Again, note that this bit of code may take an hour to run.
#
#subset the data to allow for no errors in the annual footprint calculation
#keep only rows that have non missing values across all variables
FFP.data.no.NA.subset$filtered.FFP.80[FFP.data.no.NA.subset$filtered.FFP.80 == 0] <- NA
FSub <- na.omit(FFP.data.no.NA.subset)
#
#
#Call the function that proocesses that annual footprint based on the subsetted data
source("calc_footprint_FFP_climatology.R")
#run analysis
FFP.climatology <- calc_footprint_FFP_climatology (zm=FSub$zm,
                                                   z0=NA,
                                                   umean=FSub$WS,
                                                   h=FSub$h.PBL,
                                                   ol=FSub$L,
                                                   sigmav=FSub$sigmav,
                                                   ustar=FSub$Ustar,
                                                   wind_dir=FSub$WD,
                                                   domain = c(-200, 200, -200, 200),
                                                   dx = NULL,
                                                   dy = NULL,
                nx = NULL, ny = NULL, r = NULL, rslayer = 1, smooth_data = NULL,
                crop = NULL, pulse = NULL, fig = 0)
#
#The next steps saves the FFP.climatology object (which has the data for plotting footprint maps),
#as a R-file (i.e. in R-data-format), which can be accessed at a later date for any future
#replotting and processing without the need to rerun the above step.
final.file.name <- paste(site.name, measurement.year, "FFP.climatology.rds", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
saveRDS(FFP.climatology, file = File.name.path)
#
#
############ OPTIONAL SCRIPT BREAK POINT 2: START #####
######################################################
#if you ran the above FFP.climatology script, the output-till-here was saved as
#"FFP.climatology.rds" in your Output directory. So you could stop here and
#come back to the analysis, without the need to rerun the footprint climatology. To do this
#you can uncomment the lines of code below and import that backed-up file manually to
#continue processing the script below.
#
#TO IMPORT backed-up file COMMENT/UNCOMMENT the 6 lines below:
# final.file.name <- paste(site.name, measurement.year, "FFP.climatology.rds", sep=".")
# backup.file.path <- paste(Output.directory, final.file.name, sep="/")
# FFP.climatology <- readRDS(backup.file.path)
# final.file.name <- paste(site.name, measurement.year, "FFP.rds", sep=".")
# backup.file.path <- paste(Output.directory, final.file.name, sep="/")
# FFP <- readRDS(backup.file.path)
#
########### END of OPTIONAL SCRIPT BREAK POINT 2 #####
#
#
#
#Visualize footprint outputs, from Individual footprint results (FFP) - 2D
#Figure 1F
plot(FFP$x_ci,FFP$f_ci, type="l", main = "Figure 1F: individual footprint")
#
# Figure 2F
ffp_x <- c(FFP$x_2d)
ffp_y <- c(FFP$y_2d)
ffp_f <- c(FFP$f_2d)
quilt.plot(ffp_x,ffp_y,ffp_f,nx=1000,ny=1000, xlim=c(0,200),ylim=c(-100,100),
           add.legend=TRUE)
for (i in 1:8) lines(unlist(FFP$xr[i]),unlist(FFP$yr[i]), type="l", col="red")
#
#Next, visualize footprint climatology output (FFP.climatology) - 3D
# Figure 3F
#Sets the line colour and line widths for Figure 3F
cols<-c("firebrick1","firebrick3","firebrick4","red","red1","red2","red3","red4")
wid<-c(0.5,0.5,1,1,2,2,3,3)
#calculates the min and max distance for the footprint to scale the image
ydist1<-max(round((ceiling(FFP.climatology$yr[[8]])*2)/50)*50,na.rm=TRUE)
xdist1<-max(round((ceiling(FFP.climatology$xr[[8]])*2)/50)*50,na.rm=TRUE)
ydist2<-min(round((floor(FFP.climatology$yr[[8]])*2)/50)*50,na.rm=TRUE)
xdist2<-min(round((floor(FFP.climatology$xr[[8]])*2)/50)*50,na.rm=TRUE)
#plots figure
image.plot(FFP.climatology$x_2d[1,]*2,FFP.climatology$y_2d[,1]*2, FFP.climatology$fclim_2d,
                         xlim=c(xdist2,xdist1),ylim=c(ydist2,ydist1),xlab="Distance (m)",ylab="Distance (m)",main="Flux Footprint (10%-80%)")
for (i in 1:8) lines(FFP.climatology$xr[[i]]*2,FFP.climatology$yr[[i]]*2, type="l", col=cols[i],lwd=wid[i])
#

#
#
#####  save plots to pdf.file ####
final.file.name <- paste(site.name, measurement.year, "Footprint.plots.pdf", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
pdf(file=File.name.path)
plot(FFP$x_ci,FFP$f_ci, type="l", main = "Figure 1F: individual footprint")
ffp_x <- c(FFP$x_2d)
ffp_y <- c(FFP$y_2d)
ffp_f <- c(FFP$f_2d)
quilt.plot(ffp_x,ffp_y,ffp_f,nx=1000,ny=1000, xlim=c(0,200),ylim=c(-100,100),
           add.legend=TRUE)
for (i in 1:8) lines(unlist(FFP$xr[i]),unlist(FFP$yr[i]), type="l", col="red")
cols<-c("firebrick1","firebrick3","firebrick4","red","red1","red2","red3","red4")
wid<-c(0.5,0.5,1,1,2,2,3,3)
ydist1<-max(round((ceiling(FFP.climatology$yr[[8]])*2)/50)*50,na.rm=TRUE)
xdist1<-max(round((ceiling(FFP.climatology$xr[[8]])*2)/50)*50,na.rm=TRUE)
ydist2<-min(round((floor(FFP.climatology$yr[[8]])*2)/50)*50,na.rm=TRUE)
xdist2<-min(round((floor(FFP.climatology$xr[[8]])*2)/50)*50,na.rm=TRUE)
image.plot(FFP.climatology$x_2d[1,]*2,FFP.climatology$y_2d[,1]*2, FFP.climatology$fclim_2d,
           xlim=c(xdist2,xdist1),ylim=c(ydist2,ydist1),xlab="Distance (m)",ylab="Distance (m)",main="Flux Footprint (10%-80%)")
for (i in 1:8) lines(FFP.climatology$xr[[i]]*2,FFP.climatology$yr[[i]]*2, type="l", col=cols[i],lwd=wid[i])
surf3D(FFP.climatology$x_2d,FFP.climatology$y_2d, FFP.climatology$fclim_2d,
       xlim=c(-20,20),ylim=c(-20,20), theta = 90, phi=90, main="Figure 4F: 3D climatology")
dev.off()





#### QA/QC: FILTER FLUXES BASED ON FOOTPRINT ###
################################################
#filter out NEE, latent (Qe) and sensible heat (Qh) fluxes based on footprint
#output from above analysis
#
#merge the original Input.data file with the footprint output file FFP.data.no.NA.subset
Input.data.w.FFP <- merge(x=Input.data,y=FFP.data.no.NA.subset, all.x = TRUE)
#
#filter out fluxes, to make sure they come from within 80% of the calculated footprint
Input.data.w.FFP$NEE.footfiltered <- ifelse(is.na(Input.data.w.FFP$NEE.filtered), NA,
                                            ifelse(Input.data.w.FFP$footprint.filter==1,
                                                   Input.data.w.FFP$NEE.filtered,
                                                   NA))
mean(Input.data.w.FFP$NEE.footfiltered, na.rm=TRUE)
mean(Input.data.w.FFP$NEE.filtered, na.rm=TRUE)
#
Input.data.w.FFP$Qh.footfiltered <- ifelse(is.na(Input.data.w.FFP$Qh.filtered), NA,
                                           ifelse(Input.data.w.FFP$footprint.filter==1,
                                                  Input.data.w.FFP$Qh.filtered,
                                                  NA))
mean(Input.data.w.FFP$Qh.footfiltered, na.rm=TRUE)
mean(Input.data.w.FFP$Qh.filtered, na.rm=TRUE)
#
Input.data.w.FFP$Qe.footfiltered <- ifelse(is.na(Input.data.w.FFP$Qe.filtered), NA,
                                           ifelse(Input.data.w.FFP$footprint.filter==1,
                                                  Input.data.w.FFP$Qe.filtered,
                                                  NA))
mean(Input.data.w.FFP$Qe.footfiltered, na.rm=TRUE)
mean(Input.data.w.FFP$Qe.filtered, na.rm=TRUE)
#
#double check filtered values
plot(NEE.filtered~DoY, data=Input.data.w.FFP, col="green", main="green=filtered, red=footprint filtered")
points(NEE.footfiltered~DoY, data=Input.data.w.FFP, col="red")
#
plot(Qh.filtered~DoY, data=Input.data.w.FFP, col="orange", main="orange=filtered, black=footprint filtered")
points(Qh.footfiltered~DoY, data=Input.data.w.FFP, col="black")
#
plot(Qe.filtered~DoY, data=Input.data.w.FFP, col="blue", main="blue=filtered, red=footprint filtered")
points(Qe.footfiltered~DoY, data=Input.data.w.FFP, col="red")
#
#backup Input.data.w.FFP
final.file.name <- paste(site.name, measurement.year, "Input.data.w.FFP.backup.csv", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
write.csv(Input.data.w.FFP, file=File.name.path)
#
#####  save plots to pdf.file ####
final.file.name <- paste(site.name, measurement.year, "Footprint.filtered.fluxes.plots.pdf", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
pdf(file=File.name.path)
plot(NEE.filtered~DoY, data=Input.data.w.FFP, col="green", main="green=filtered, red=footprint filtered")
points(NEE.footfiltered~DoY, data=Input.data.w.FFP, col="red")
plot(Qh.filtered~DoY, data=Input.data.w.FFP, col="orange", main="orange=filtered, black=footprint filtered")
points(Qh.footfiltered~DoY, data=Input.data.w.FFP)
plot(Qe.filtered~DoY, data=Input.data.w.FFP, col="blue", main="blue=filtered, red=footprint filtered")
points(Qe.footfiltered~DoY, data=Input.data.w.FFP, col="red")
dev.off()





############ OPTIONAL SCRIPT BREAK POINT 3: START #########################
###########################################################################
#if you ran the above script this far, the output-till-here was saved as
#"Input.data.w.FFP.backup.csv" in your Output directory. So you could stop here and
#come back to the analysis, without the need to rerun al the footprint analysis. To do this
#you can uncomment the lines of code below and import that backed-up file manually to
#continue processing the script below.
#
#TO IMPORT backed-up file COMMENT/UNCOMMENT the three lines below:
#final.file.name <- paste(site.name, measurement.year, "Input.data.w.FFP.backup.csv", sep=".")
#backup.file.path <- paste(Output.directory, final.file.name, sep="/")
#Input.data.w.FFP <- read.csv(backup.file.path, header=TRUE, na.strings="NA")
#
########### END of OPTIONAL SCRIPT BREAK POINT 3 #################################





#### PARTITION AND GAPFILL CARBON FLUXES (NEE) ###
##################################################
#partitioning and gapfilling of NEE is done using the REddy Proc package from
# Wurtzler et al (2018). See their paper for more details.
#
#select columns from Input.data.w.FFP that will be used in REddyProc analysis
Input.4.Reddy.variables <- c("Year", "DoY", "Hour", "Ustar", "VPD", "Tair",
                             "rH", "Rg", "NEE.footfiltered", "zm" )
#
#create subset of Input.data, with only FFP.variables
Input.4.Reddy <- Input.data.w.FFP[Input.4.Reddy.variables]
Input.4.Reddy$NEE <- Input.4.Reddy$NEE.footfiltered
#
#cross check input variables: Rg (W/m2), VPD (hPa), NEE (umolCO2/m2/s), Tair (oC)
# Rg and VPD must be positive; NEE must be between -50 and 100 and be positive (i.e.
# source when Rg<10, so night; VPD in hPa)
#
#Rg
hist(Input.4.Reddy$Rg)
#note REddyProc accepts Rg above zero W/m2
max(Input.4.Reddy$Rg, na.rm = TRUE)
min(Input.4.Reddy$Rg, na.rm = TRUE)
#if any Rg is below zero, set to zero
Input.4.Reddy$Rg <- ifelse(is.na(Input.4.Reddy$Rg), NA,
                             ifelse(Input.4.Reddy$Rg <0, 0, Input.4.Reddy$Rg))
#double check Rg<0
min(Input.4.Reddy$Rg, na.rm = TRUE)
plot(Rg~DoY, data=Input.4.Reddy, col="red", main="Rg vs DoY", ylab="Rg (W/m2)")
#
#
#NEE
hist(Input.4.Reddy$NEE)
#note REddyProc accepts NEE in the range of -50 to 100 umol CO2/m2/s
max(Input.4.Reddy$NEE, na.rm = TRUE)
min(Input.4.Reddy$NEE, na.rm = TRUE)
plot(NEE~DoY, data=Input.4.Reddy, col="green", main="NEE vs DoY", ylab="NEE (umol CO2/m2/s)")
#check if have negative NEE when Rg<0 (so uptake at night)
plot(NEE~Rg, data=Input.4.Reddy)
plot(NEE~Rg, data=subset(Input.4.Reddy, Rg<10), main="NEE when Rg<10")
#set negative nighttime NEE (uptake) to NA, when Rg is less than 10 W/m2
#add a filter column to identify where NEE was set to NA (i.e. filter Rg.10.f = 1, else zero)
Input.4.Reddy$Rg.10.f <- ifelse(Input.4.Reddy$Rg <10 & Input.4.Reddy$NEE <0, 1, 0)
Input.4.Reddy$NEE.Rg.f <- ifelse(Input.4.Reddy$Rg <10 & Input.4.Reddy$NEE <0,
                            NA, Input.4.Reddy$NEE)
#check visually
plot(NEE~Rg, data=Input.4.Reddy, main="after Rg filter of NEE")
points(NEE.Rg.f~Rg, data=Input.4.Reddy, col="green")
#replace NEE column with Rg filtered NEE in Input.4.Reddy
Input.4.Reddy$NEE <- Input.4.Reddy$NEE.Rg.f

# VPD
hist(Input.4.Reddy$VPD)
#note: REddy Proc accepts VPD in the range of 0 to 50 hPa
max(Input.4.Reddy$VPD, na.rm = TRUE)
min(Input.4.Reddy$VPD, na.rm = TRUE)
#convert to hPa from kPa
Input.4.Reddy$VPD <- Input.4.Reddy$VPD*100
# filter out VPD above 50 as NA
Input.4.Reddy$VPD <- ifelse(is.na(Input.4.Reddy$VPD), NA,
                              ifelse(Input.4.Reddy$VPD > 50, NA, Input.4.Reddy$VPD))
max(Input.4.Reddy$VPD, na.rm = TRUE)
plot(VPD~DoY, data=Input.4.Reddy, col="blue", main="VPD vs DoY", ylab="VPD (hPa)")
#
# Backup Input.4.Reddy in case need to run a check on ustars, to determine manually
#backup Input.data.w.FFP
final.file.name <- paste(site.name, measurement.year, "Input.4.Reddy.backup.csv", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
write.csv(Input.4.Reddy, file=File.name.path)
#
#
# User defined U-star threshold, by plotting nightime NEE vs ustar for different small
# temperature bins.
#
################################################################################################
###################        USER DEFINED U-STAR DETERMINATION       #############################
################################################################################################
# AT THIS POINT COULD GO AWAY AND USE ABOVE DATA (Input.4.REddy.csv) TO SEE WHAT SORT OF U-star
# the data suggests. TO MANUALLY CHECK, USE A DIFFERENT R-SCRIPT, called Manual.U.Star.CHeck.R.
# then can input in the user section above the user defined u-star for analysis below.
################################################################################################
#
# BACK to regular script
########### start of REddyProc script ##########
#
#+++ Add time stamp in POSIX time format
EddyDataWithPosix.F <- fConvertTimeToPosix(Input.4.Reddy, 'YDH',Year.s = 'Year'
                                           ,Day.s = 'DoY',Hour.s = 'Hour',
                                           TName.s = "DateTime", tz="GMT")
#
#+++ Initalize R5 reference class sEddyProc for post-processing of eddy data
#+++ with the variables needed for post-processing later
EddyProc.C <- sEddyProc$new('siteName',EddyDataWithPosix.F ,
                            c('NEE','Rg','Tair','VPD', 'Ustar'))
#
#plot raw data
EddyProc.C$sPlotFingerprintY('NEE', Year.i = measurement.year)
#
#
############################################################################################
#####  THE NEXT BIT OF SCRIPT CALCULATES C-FLUXES USING USER SPECIFIC U-star Thershold  ####
############################################################################################
EddyProc.C$sMDSGapFillAfterUstar(FluxVar.s="NEE",
                                 UstarVar.s = "Ustar", UstarThres.df = U.Threshold.user,
                                 UstarSuffix.s = "userUstar", FlagEntryAfterLowTurbulence.b = FALSE,
                                 isFilterDayTime = FALSE, swThr = 10,
                                 RgColName = "Rg")
grep("^NEE.*_f$", colnames( EddyProc.C$sExportResults()), value = TRUE )
uStarScens <- c("userUstar")
meanNEE <- sapply(uStarScens, function(uStarScen){
  varName <- paste0("NEE_",uStarScen,"_f")
  mean( EddyProc.C$sExportResults()[[varName]], na.rm = TRUE)
})
print(meanNEE)
#plot finger plot to visualize gapfilled NEE data. Below we chose U50 filtered values
EddyProc.C$sPlotFingerprintY('NEE_userUstar_f', Year.i = measurement.year)
#
#Next partition NEE into GPP and Reco, first check if Tair and VPD is present without gaps
#if have gaps, then gapfill. Also adjust daylight timing based on latitude and longitude of
#site location.
EddyProc.C$sSetLocationInfo(Lat_deg.n = site.Lat, Long_deg.n = site.Lon,
                            TimeZone_h.n = time.zone)
EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('VPD', FillAll.b = FALSE)
#
EddyProc.C$sMRFluxPartition(FluxVar.s = "NEE_userUstar_f",
                            QFFluxVar.s = "NEE_userUstar_fqc", QFFluxValue.n = 0,
                            TempVar.s = "Tair_f", QFTempVar.s = "Tair_fqc",
                            QFTempValue.n = 0, RadVar.s = "Rg", T_ref.n = 273.15 +
                              15, Suffix.s = "userUstar", debug.l = list(useLocaltime.b = FALSE),
                            parsE0Regression = list())
#
#results stored in columes Reco and GPP_f, modified by respective u* suffix
grep("GPP.*_f$|Reco",names(EddyProc.C$sExportResults()), value = TRUE)
#
#plot finger plots to visualize data - GPP and Reco
EddyProc.C$sPlotFingerprintY('GPP_userUstar_f', Year.i = measurement.year)
EddyProc.C$sPlotFingerprintY('Reco_userUstar', Year.i = measurement.year)
#
# pass results to a datatable
FilledEddyData.F.userUstar <- EddyProc.C$sExportResults()
Combined.FFP.NEE.userUstar <- cbind(Input.data.w.FFP, FilledEddyData.F.userUstar)
#
#####  save plots to pdf.file ####
final.file.name <- paste(site.name, measurement.year, "REddyProc.plots.UserU.pdf", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
pdf(file=File.name.path)
EddyProc.C$sPlotFingerprintY('NEE', Year.i = measurement.year)
EddyProc.C$sPlotFingerprintY('NEE_userUstar_f', Year.i = measurement.year)
EddyProc.C$sPlotFingerprintY('GPP_userUstar_f', Year.i = measurement.year)
EddyProc.C$sPlotFingerprintY('Reco_userUstar', Year.i = measurement.year)
dev.off()




########################################################################################
########## U-Star determination by REddy Proc  #########################################
########################################################################################
#
#If have enough hh-data for different temperature bins, then can
#calculate Ustar thresholds based on all data points and show the 5%, 50%, 95% th values
#based on the distribution. The columns will be labelled with U05, U50, U95 accordingly
#
########### start of REddyProc script ##########
#+++ Add time stamp in POSIX time format
EddyDataWithPosix.F <- fConvertTimeToPosix(Input.4.Reddy, 'YDH',Year.s = 'Year'
                                           ,Day.s = 'DoY',Hour.s = 'Hour',
                                           TName.s = "DateTime", tz="GMT")
#
#+++ Initalize R5 reference class sEddyProc for post-processing of eddy data
#+++ with the variables needed for post-processing later
EddyProc.C <- sEddyProc$new('siteName',EddyDataWithPosix.F ,
                            c('NEE','Rg','Tair','VPD', 'Ustar'))
#
#plot raw data
EddyProc.C$sPlotFingerprintY('NEE', Year.i = measurement.year)
#
#calculate Ustar thresholds based on all data points and show the 5%, 50%, 95% th values
#based on the distribution. The columns will be labelled with U05, U50, U95 accordingly
uStarTh <- EddyProc.C$sEstUstarThresholdDistribution(
  nSample = 10L, probs = c(0.05, 0.5, 0.95))
uStarTh%>%
  filter(aggregationMode == "year") %>%
  select(uStar, "5%", "50%", "95%")
select(uStarTh, -seasonYear)
#
#warnings()  #check, if needed
#
#filter NEE by U-star
uStarThAnnual <- usGetAnnualSeasonUStarMap(uStarTh)[-2]
uStarSuffixes <- colnames(uStarThAnnual)[-1]
print(uStarThAnnual)
#
#save above Ustar stats to a text file for future reference
final.file.name <- paste(site.name, measurement.year, "uStarThAnnual.txt", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
sink(File.name.path)
print(uStarThAnnual)
sink()  # returns output to the console
#
EddyProc.C$sMDSGapFillAfterUStarDistr('NEE',
                                      UstarThres.df = uStarThAnnual,
                                      UstarSuffix.V.s = uStarSuffixes,
                                      FillAll = TRUE)
#
#creates column names
grep("NEE_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
grep("NEE_.*_fsd$",names(EddyProc.C$sExportResults()), value = TRUE)
#
#plot finger plot to visualize gapfilled NEE data. Below we chose U50 filtered values
EddyProc.C$sPlotFingerprintY('NEE_U50_f', Year.i = measurement.year)
#
#Next partition NEE into GPP and Reco, first check if Tair and VPD is present without gaps
#if have gaps, then gapfill. Also adjust daylight timing based on latitude and longitude of
#site location.
EddyProc.C$sSetLocationInfo(Lat_deg.n = site.Lat, Long_deg.n = site.Lon,
                            TimeZone_h.n = time.zone)
EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('VPD', FillAll.b = FALSE)
#
#night time partitioning approach
resPart <- lapply(uStarSuffixes, function(suffix){
  EddyProc.C$sMRFluxPartition(Suffix.s = suffix)
})
#
#results stored in columes Reco and GPP_f, modified by respective u* suffix
grep("GPP.*_f$|Reco",names(EddyProc.C$sExportResults()), value = TRUE)
#
#plot finger plots to visualize data - GPP_U50 & Reco_U50
EddyProc.C$sPlotFingerprintY('GPP_U50_f', Year.i = measurement.year)
EddyProc.C$sPlotFingerprintY('Reco_U50', Year.i = measurement.year)
#
#pass results to a datatable
FilledEddyData.F <- EddyProc.C$sExportResults()
#
#rename some columns
GPPAgg <- sapply( uStarSuffixes, function(suffix) {
  GPPHalfHour <- FilledEddyData.F[[paste0("GPP_",suffix,"_f")]]
  mean(GPPHalfHour, na.rm = TRUE)
})
#estimated mean aggregated GPP in umol CO2/m2/s
print(GPPAgg)
#
#convert above means from units of umol of CO2 per m2 to g of C per m2 and
#estimate totals based on mean for the whole measurement period.
GPPAgg.gC.m2.yr <- GPPAgg * 1e-6 * molarMass.CO2 * 3600*24*measurement.days
print(GPPAgg.gC.m2.yr)
#
#estimate uncertainty in annual flux total
uncertaintyGPP <-(max(GPPAgg) - min(GPPAgg)) / median(GPPAgg)
print(uncertaintyGPP)
#
#merge the manual ustar REddyProc analysis and footprint analysis output
# (i.e. Combined.FFP.NEE.userUstar) with the multi-ustar estimated REddyProc analysis
# (i.e. FilledEddyData.F) into a single data table called "CombineData.FFP.NEE"
Combined.FFP.NEE <- cbind(Combined.FFP.NEE.userUstar, FilledEddyData.F)
#
#backup Combined.FFP.NEE into a .csv file
final.file.name <- paste(site.name, measurement.year, "Combined.FFP.NEE.backup.csv", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
write.csv(Combined.FFP.NEE, file=File.name.path)
#
#####  save plots to pdf.file ####
final.file.name <- paste(site.name, measurement.year, "REddyProc.plots.Annual.pdf", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
pdf(file=File.name.path)
EddyProc.C$sPlotFingerprintY('NEE', Year.i = measurement.year)
EddyProc.C$sPlotFingerprintY('NEE_U50_f', Year.i = measurement.year)
EddyProc.C$sPlotFingerprintY('GPP_U50_f', Year.i = measurement.year)
EddyProc.C$sPlotFingerprintY('Reco_U50', Year.i = measurement.year)
dev.off()




########### OPTIONAL SCRIPT BREAK POINT 4: START #####
######################################################
#if you ran all of the above script (footprint and NEE-partition/fill modules),
#their combined output-till-here was saved as "Combined.FFP.NEE.backup.csv" in your
#working directory. So you could come back to the script at this point, without the
#need to rerun the footprint analysis or REddyProc package. To do this
#you can uncomment the line of code below and import that backed-up file manually to
#continue processing the data using the rest of the script below. Note that the
#backedup file should be located in your current working directory, for this to work.
#
#TO IMPORT the backed-up file (comment/uncomment the 3 lines below)
# final.file.name <- paste(site.name, measurement.year, "Combined.FFP.NEE.backup.csv", sep=".")
# backup.file.path <- paste(Output.directory, final.file.name, sep="/")
# Combined.FFP.NEE <- read.csv(backup.file.path, header=TRUE, na.strings="NA")
#
########### END of OPTIONAL SCRIPT BREAK POINT 4 #####




#### QA/QC: FILTER OUT SENSIBLE AND LATENT HEAT FLUX BY differernt U*-thresholds ###
####################################################################################
#Create Ustar filters to filter data by
#User u-star threshold filter
Combined.FFP.NEE$Ustar.user.Thr.filter <- ifelse(Combined.FFP.NEE$Ustar <  U.Threshold.user, 0, 1)
# U05 Ustar Threshold (from Annual estimates) filter, these can be used for flux uncertainty estimates
Combined.FFP.NEE$Ustar.U05.Thr.filter <- ifelse(Combined.FFP.NEE$Ustar < Combined.FFP.NEE$Ustar_U05_Thres,
                                                0, 1)
# U50 Ustar Threshold (from Annual estimates) filter
Combined.FFP.NEE$Ustar.U50.Thr.filter <- ifelse(Combined.FFP.NEE$Ustar < Combined.FFP.NEE$Ustar_U50_Thres,
                                            0, 1)
# U95 Ustar Threshold (from Annual estimates) filter
Combined.FFP.NEE$Ustar.U95.Thr.filter <- ifelse(Combined.FFP.NEE$Ustar < Combined.FFP.NEE$Ustar_U95_Thres,
                                                0, 1)

#filter EN fluxes by the different u-star thresholds
Combined.FFP.NEE$Qh.user.filtered <- Combined.FFP.NEE$Qh.footfiltered*Combined.FFP.NEE$Ustar.user.Thr.filter
Combined.FFP.NEE$Qh.u05.filtered <- Combined.FFP.NEE$Qh.footfiltered*Combined.FFP.NEE$Ustar.U05.Thr.filter
Combined.FFP.NEE$Qh.u50.filtered <- Combined.FFP.NEE$Qh.footfiltered*Combined.FFP.NEE$Ustar.U50.Thr.filter
Combined.FFP.NEE$Qh.u95.filtered <- Combined.FFP.NEE$Qh.footfiltered*Combined.FFP.NEE$Ustar.U95.Thr.filter
#
Combined.FFP.NEE$Qe.user.filtered <- Combined.FFP.NEE$Qe.footfiltered*Combined.FFP.NEE$Ustar.user.Thr.filter
Combined.FFP.NEE$Qe.u05.filtered <- Combined.FFP.NEE$Qe.footfiltered*Combined.FFP.NEE$Ustar.U05.Thr.filter
Combined.FFP.NEE$Qe.u50.filtered <- Combined.FFP.NEE$Qe.footfiltered*Combined.FFP.NEE$Ustar.U50.Thr.filter
Combined.FFP.NEE$Qe.u95.filtered <- Combined.FFP.NEE$Qe.footfiltered*Combined.FFP.NEE$Ustar.U95.Thr.filter
#
#visually check filtered data
plot(Qh.user.filtered~DoY, data=Combined.FFP.NEE,
     main="sensible heat flux W/m2 filtered by u*-U50", sub="user=org, 05=blk, 50=grn, 95=brwn",
     col="orange", mai=c(1,1,1,1))
points(Qh.u05.filtered~DoY, data=Combined.FFP.NEE)
points(Qh.u50.filtered~DoY, data=Combined.FFP.NEE, col="green")
points(Qh.u95.filtered~DoY, data=Combined.FFP.NEE, col="brown")
#
plot(Qe.u50.filtered~DoY, data=Combined.FFP.NEE, main="latent heat flux W/m2 filtered by u*-U50", sub="blue=U50, black=user U", col="blue")
points(Qe.user.filtered~DoY, data=Combined.FFP.NEE)
#
plot(Qh.footfiltered~DoY, data=Combined.FFP.NEE,
     col="orange", main="orange=footfiltered, purple=u50.filtered")
points(Qh.u50.filtered~DoY, data=Combined.FFP.NEE, col="purple")
plot(Qh.footfiltered~Qh.u50.filtered, data=Combined.FFP.NEE, col="black", main="footfiltered vs u50.filtered")
plot(Qh.footfiltered~Qh.user.filtered, data=Combined.FFP.NEE, col="black", main="footfiltered vs u.user.filtered")
#
plot(Qe.footfiltered~DoY, data=Combined.FFP.NEE, col="blue", main="blue=footfiltered, purple=u50.filtered")
points(Qe.u50.filtered~DoY, data=Combined.FFP.NEE, col="purple")
plot(Qe.footfiltered~Qe.u50.filtered, data=Combined.FFP.NEE, col="black", main="footfiltered vs u50.filtered")
plot(Qe.footfiltered~Qe.user.filtered, data=Combined.FFP.NEE, col="black", main="footfiltered vs user.filtered")
#
#####  save plots to pdf.file ####
final.file.name <- paste(site.name, measurement.year, "Ufiltered.Energy.Fluxes.pdf", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
pdf(file=File.name.path)
plot(Qh.user.filtered~DoY, data=Combined.FFP.NEE,
     main="sensible heat flux W/m2 filtered by u*-U50", sub="user=org, 05=blk, 50=grn, 95=brwn",
     col="orange", mai=c(1,1,1,1))
points(Qh.u05.filtered~DoY, data=Combined.FFP.NEE)
points(Qh.u50.filtered~DoY, data=Combined.FFP.NEE, col="green")
points(Qh.u95.filtered~DoY, data=Combined.FFP.NEE, col="brown")
plot(Qe.u50.filtered~DoY, data=Combined.FFP.NEE, main="latent heat flux W/m2 filtered by u*-U50", sub="blue=U50, black=user U", col="blue")
points(Qe.user.filtered~DoY, data=Combined.FFP.NEE)
plot(Qh.footfiltered~DoY, data=Combined.FFP.NEE,
     col="orange", main="orange=footfiltered, purple=u50.filtered")
points(Qh.u50.filtered~DoY, data=Combined.FFP.NEE, col="purple")
plot(Qh.footfiltered~Qh.u50.filtered, data=Combined.FFP.NEE, col="black", main="footfiltered vs u50.filtered")
plot(Qh.footfiltered~Qh.user.filtered, data=Combined.FFP.NEE, col="black", main="footfiltered vs u.user.filtered")
plot(Qe.footfiltered~DoY, data=Combined.FFP.NEE, col="blue", main="blue=footfiltered, purple=u50.filtered")
points(Qe.u50.filtered~DoY, data=Combined.FFP.NEE, col="purple")
plot(Qe.footfiltered~Qe.u50.filtered, data=Combined.FFP.NEE, col="black", main="footfiltered vs u50.filtered")
plot(Qe.footfiltered~Qe.user.filtered, data=Combined.FFP.NEE, col="black", main="footfiltered vs user.filtered")
dev.off()





#### CALCULATE CLOSURE, using different u-star filtered EN fluxes ###
#####################################################################
#Calculate energy budget closure which will be used to filter out EC-energy fluxes
#below are all in Watts/m2
# USER- U CLOSURE
Combined.FFP.NEE$closure.ratio.initial.user <- ifelse(is.na(Combined.FFP.NEE$Qe.user.filtered)|
                                    is.na(Combined.FFP.NEE$Qh.user.filtered)|
                                     is.na(Combined.FFP.NEE$Qnet)|
                                      is.na(Combined.FFP.NEE$Qg),
                                     NA,
                                   (Combined.FFP.NEE$Qe.user.filtered+Combined.FFP.NEE$Qh.user.filtered)/
                                        (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg))
#
min(Combined.FFP.NEE$closure.ratio.initial.user, na.rm = TRUE)
max(Combined.FFP.NEE$closure.ratio.initial.user, na.rm = TRUE)
plot(closure.ratio.initial.user~DoY, data=Combined.FFP.NEE)
#
#calculate total initial closure in Watts
Combined.FFP.NEE$Total.initial.closure.user.Watts <- ifelse(is.na(Combined.FFP.NEE$Qe.user.filtered)|
                                                   is.na(Combined.FFP.NEE$Qh.user.filtered)|
                                                   is.na(Combined.FFP.NEE$Qnet)|
                                                   is.na(Combined.FFP.NEE$Qg),
                                                 NA,
                                                 Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qe.user.filtered-
                                                   Combined.FFP.NEE$Qh.user.filtered-Combined.FFP.NEE$Qg)
min(Combined.FFP.NEE$Total.initial.closure.user.Watts, na.rm = TRUE)
max(Combined.FFP.NEE$Total.initial.closure.user.Watts, na.rm = TRUE)
plot(Total.initial.closure.user.Watts~DoY, data=Combined.FFP.NEE)
#
# U05 CLOSURE ratio
Combined.FFP.NEE$closure.ratio.initial.u05 <- ifelse(is.na(Combined.FFP.NEE$Qe.u05.filtered)|
                                                   is.na(Combined.FFP.NEE$Qh.u05.filtered)|
                                                   is.na(Combined.FFP.NEE$Qnet)|
                                                   is.na(Combined.FFP.NEE$Qg),
                                                 NA,
                                                 (Combined.FFP.NEE$Qe.u05.filtered+Combined.FFP.NEE$Qh.u05.filtered)/
                                                   (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg))
#
min(Combined.FFP.NEE$closure.ratio.initial.u05, na.rm = TRUE)
max(Combined.FFP.NEE$closure.ratio.initial.u05, na.rm = TRUE)
plot(closure.ratio.initial.u05~DoY, data=Combined.FFP.NEE)
#
#calculate total initial closure U05 energy difference in Watts
Combined.FFP.NEE$Total.initial.closure.u05.Watts <- ifelse(is.na(Combined.FFP.NEE$Qe.u05.filtered)|
                                                         is.na(Combined.FFP.NEE$Qh.u05.filtered)|
                                                         is.na(Combined.FFP.NEE$Qnet)|
                                                         is.na(Combined.FFP.NEE$Qg),
                                                       NA,
                                                       Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qe.u05.filtered-
                                                         Combined.FFP.NEE$Qh.u05.filtered-Combined.FFP.NEE$Qg)
min(Combined.FFP.NEE$Total.initial.closure.u05.Watts, na.rm = TRUE)
max(Combined.FFP.NEE$Total.initial.closure.u05.Watts, na.rm = TRUE)
plot(Total.initial.closure.u05.Watts~DoY, data=Combined.FFP.NEE)
#
# U50 CLOSURE ratio
Combined.FFP.NEE$closure.ratio.initial.u50 <- ifelse(is.na(Combined.FFP.NEE$Qe.u50.filtered)|
                                                   is.na(Combined.FFP.NEE$Qh.u50.filtered)|
                                                   is.na(Combined.FFP.NEE$Qnet)|
                                                   is.na(Combined.FFP.NEE$Qg),
                                                 NA,
                                                 (Combined.FFP.NEE$Qe.u50.filtered+Combined.FFP.NEE$Qh.u50.filtered)/
                                                   (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg))
#
min(Combined.FFP.NEE$closure.ratio.initial.u50, na.rm = TRUE)
max(Combined.FFP.NEE$closure.ratio.initial.u50, na.rm = TRUE)
plot(closure.ratio.initial.u50~DoY, data=Combined.FFP.NEE)
#
#calculate total initial closure in Watts
Combined.FFP.NEE$Total.initial.closure.u50.Watts <- ifelse(is.na(Combined.FFP.NEE$Qe.u50.filtered)|
                                                         is.na(Combined.FFP.NEE$Qh.u50.filtered)|
                                                         is.na(Combined.FFP.NEE$Qnet)|
                                                         is.na(Combined.FFP.NEE$Qg),
                                                       NA,
                                                       Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qe.u50.filtered-
                                                         Combined.FFP.NEE$Qh.u50.filtered-Combined.FFP.NEE$Qg)
min(Combined.FFP.NEE$Total.initial.closure.u50.Watts, na.rm = TRUE)
max(Combined.FFP.NEE$Total.initial.closure.u50.Watts, na.rm = TRUE)
plot(Total.initial.closure.u50.Watts~DoY, data=Combined.FFP.NEE)
#
# U95 EN CLOSURE Ratio
Combined.FFP.NEE$closure.ratio.initial.u95 <- ifelse(is.na(Combined.FFP.NEE$Qe.u95.filtered)|
                                                   is.na(Combined.FFP.NEE$Qh.u95.filtered)|
                                                   is.na(Combined.FFP.NEE$Qnet)|
                                                   is.na(Combined.FFP.NEE$Qg),
                                                 NA,
                                                 (Combined.FFP.NEE$Qe.u95.filtered+Combined.FFP.NEE$Qh.u95.filtered)/
                                                   (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg))
#
min(Combined.FFP.NEE$closure.ratio.initial.u95, na.rm = TRUE)
max(Combined.FFP.NEE$closure.ratio.initial.u95, na.rm = TRUE)
plot(closure.ratio.initial.u95~DoY, data=Combined.FFP.NEE)
#
#calculate total initial closure U95 in Watts
Combined.FFP.NEE$Total.initial.closure.u95.Watts <- ifelse(is.na(Combined.FFP.NEE$Qe.u95.filtered)|
                                                         is.na(Combined.FFP.NEE$Qh.u95.filtered)|
                                                         is.na(Combined.FFP.NEE$Qnet)|
                                                         is.na(Combined.FFP.NEE$Qg),
                                                       NA,
                                                       Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qe.u95.filtered-
                                                         Combined.FFP.NEE$Qh.u95.filtered-Combined.FFP.NEE$Qg)
min(Combined.FFP.NEE$Total.initial.closure.u95.Watts, na.rm = TRUE)
max(Combined.FFP.NEE$Total.initial.closure.u95.Watts, na.rm = TRUE)
plot(Total.initial.closure.u95.Watts~DoY, data=Combined.FFP.NEE)





#### Bowen Ratio, using different u-star threshold filtered EN flux data ###
############################################################################
#  NOTE: this is the first estimate, below will refine the Bowen estimate ##
############################################################################
#
# USER U star
Combined.FFP.NEE$Bowen.user <- Combined.FFP.NEE$Qh.user.filtered/Combined.FFP.NEE$Qe.user.filtered
hist(Combined.FFP.NEE$Bowen.user)
plot(Bowen.user~DoY, data=Combined.FFP.NEE, main="Bowen.user ratio", col="brown")
#
#filter Bowen to be within -15 to +15
Combined.FFP.NEE$Bowen.user <- ifelse(Combined.FFP.NEE$Bowen.user <= 15 & Combined.FFP.NEE$Bowen.user >= -15,
                                      Combined.FFP.NEE$Bowen.user, NA)
min(Combined.FFP.NEE$Bowen.user, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.user, na.rm = TRUE)
plot(Bowen.user~DoY, data=Combined.FFP.NEE, col="brown")
#
# U05 star
Combined.FFP.NEE$Bowen.u05 <- Combined.FFP.NEE$Qh.u05.filtered/Combined.FFP.NEE$Qe.u05.filtered
hist(Combined.FFP.NEE$Bowen.u05)
plot(Bowen.u05~DoY, data=Combined.FFP.NEE, main="Bowen ratio u05", col="brown")
#
#filter Bowen to be within -15 to +15
Combined.FFP.NEE$Bowen.u05 <- ifelse(Combined.FFP.NEE$Bowen.u05 <= 15 & Combined.FFP.NEE$Bowen.u05 >= -15,
                                 Combined.FFP.NEE$Bowen.u05, NA)
min(Combined.FFP.NEE$Bowen.u05, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.u05, na.rm = TRUE)
plot(Bowen.u05~DoY, data=Combined.FFP.NEE, col="brown")
#
# U50 star
Combined.FFP.NEE$Bowen.u50 <- Combined.FFP.NEE$Qh.u50.filtered/Combined.FFP.NEE$Qe.u50.filtered
hist(Combined.FFP.NEE$Bowen.u50)
plot(Bowen.u50~DoY, data=Combined.FFP.NEE, main="Bowen u50 ratio", col="brown")
#
#filter Bowen to be within -15 to +15
Combined.FFP.NEE$Bowen.u50 <- ifelse(Combined.FFP.NEE$Bowen.u50 <= 15 & Combined.FFP.NEE$Bowen.u50 >= -15,
                                 Combined.FFP.NEE$Bowen.u50, NA)
min(Combined.FFP.NEE$Bowen.u50, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.u50, na.rm = TRUE)
plot(Bowen.u50~DoY, data=Combined.FFP.NEE, col="brown")
#
# U95 star
Combined.FFP.NEE$Bowen.u95 <- Combined.FFP.NEE$Qh.u95.filtered/Combined.FFP.NEE$Qe.u95.filtered
hist(Combined.FFP.NEE$Bowen.u95)
plot(Bowen.u95~DoY, data=Combined.FFP.NEE, main="Bowen u95 ratio", col="brown")
#
#filter Bowen to be within -15 to +15
Combined.FFP.NEE$Bowen.u95 <- ifelse(Combined.FFP.NEE$Bowen.u95 <= 15 & Combined.FFP.NEE$Bowen.u95 >= -15,
                                 Combined.FFP.NEE$Bowen.u95, NA)
min(Combined.FFP.NEE$Bowen.u95, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.u95, na.rm = TRUE)
plot(Bowen.u95~DoY, data=Combined.FFP.NEE, col="brown")




#### Calculating Latent and Sensible heat closure ###
#####################################################
#
#user U star Thr
#Closed latent heat flux Qe (Watts/m2)
Combined.FFP.NEE$Qe.closed.try1.user <- ifelse(is.na(Combined.FFP.NEE$Qg),
                                  (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qnet*.10)/
                                (1+Combined.FFP.NEE$Bowen.user),
                                (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg)/
                                  (1+Combined.FFP.NEE$Bowen.user))
#
min(Combined.FFP.NEE$Qe.closed.try1.user, na.rm = TRUE)
max(Combined.FFP.NEE$Qe.closed.try1.user, na.rm = TRUE)
hist(Combined.FFP.NEE$Qe.closed.try1.user)
plot(Qe.closed.try1.user~DoY, data=Combined.FFP.NEE, col="blue", main="Qe.closed try 1.userU")
#
#filter Qe.closed.try1 to be within -1000 to +1000 W/m2 (realistic range) and to be
# within 1.2 times of #absolute Qnet value.  The result will be our final Qe.closed value.
# note: if user filter was used above, the +/-1000 may be redundant here.
Combined.FFP.NEE$Qe.closed.user <- ifelse(Combined.FFP.NEE$Qe.closed.try1.user <= 1000 &
                                       Combined.FFP.NEE$Qe.closed.try1.user >= -1000 &
                                       abs(Combined.FFP.NEE$Qe.closed.try1.user)<=1.2*abs(Combined.FFP.NEE$Qnet),
                                 Combined.FFP.NEE$Qe.closed.try1.user, NA)
# cross check
min(Combined.FFP.NEE$Qe.closed.user, na.rm = TRUE)
max(Combined.FFP.NEE$Qe.closed.user, na.rm = TRUE)
hist(Combined.FFP.NEE$Qe.closed.user)
plot(Qe.closed.user~DoY, data=Combined.FFP.NEE, col="blue", main="Qe.closed.userU")
#
#U05 star Thr
#Closed latent heat flux Qe (Watts/m2)
Combined.FFP.NEE$Qe.closed.try1.u05 <- ifelse(is.na(Combined.FFP.NEE$Qg),
                                               (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qnet*.10)/
                                                 (1+Combined.FFP.NEE$Bowen.u05),
                                               (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg)/
                                                 (1+Combined.FFP.NEE$Bowen.u05))
#
min(Combined.FFP.NEE$Qe.closed.try1.u05, na.rm = TRUE)
max(Combined.FFP.NEE$Qe.closed.try1.u05, na.rm = TRUE)
hist(Combined.FFP.NEE$Qe.closed.try1.u05)
plot(Qe.closed.try1.u05~DoY, data=Combined.FFP.NEE, col="blue", main="Qe.closed try 1.userU")
#
#filter Qe.closed.try1 to be within -1000 to +1000 W/m2 (realistic range) and to be
# within 1.2 times of #absolute Qnet value.  The result will be our final Qe.closed value.
# note: if user filter was used above, the +/-1000 may be redundant here.
Combined.FFP.NEE$Qe.closed.u05 <- ifelse(Combined.FFP.NEE$Qe.closed.try1.u05 <= 1000 &
                                            Combined.FFP.NEE$Qe.closed.try1.u05 >= -1000 &
                                            abs(Combined.FFP.NEE$Qe.closed.try1.u05)<=1.2*abs(Combined.FFP.NEE$Qnet),
                                          Combined.FFP.NEE$Qe.closed.try1.u05, NA)
# cross check
min(Combined.FFP.NEE$Qe.closed.u05, na.rm = TRUE)
max(Combined.FFP.NEE$Qe.closed.u05, na.rm = TRUE)
hist(Combined.FFP.NEE$Qe.closed.u05)
plot(Qe.closed.u05~DoY, data=Combined.FFP.NEE, col="blue", main="Qe.closed.U05")
#
#U50 star Thr
#Closed latent heat flux Qe (Watts/m2)
Combined.FFP.NEE$Qe.closed.try1.u50 <- ifelse(is.na(Combined.FFP.NEE$Qg),
                                               (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qnet*.10)/
                                                 (1+Combined.FFP.NEE$Bowen.u50),
                                               (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg)/
                                                 (1+Combined.FFP.NEE$Bowen.u50))
#
min(Combined.FFP.NEE$Qe.closed.try1.u50, na.rm = TRUE)
max(Combined.FFP.NEE$Qe.closed.try1.u50, na.rm = TRUE)
hist(Combined.FFP.NEE$Qe.closed.try1.u50)
plot(Qe.closed.try1.u50~DoY, data=Combined.FFP.NEE, col="blue", main="Qe.closed try 1.u50")
#
#filter Qe.closed.try1 to be within -1000 to +1000 W/m2 (realistic range) and to be
# within 1.2 times of #absolute Qnet value.  The result will be our final Qe.closed value.
# note: if user filter was used above, the +/-1000 may be redundant here.
Combined.FFP.NEE$Qe.closed.u50 <- ifelse(Combined.FFP.NEE$Qe.closed.try1.u50 <= 1000 &
                                            Combined.FFP.NEE$Qe.closed.try1.u50 >= -1000 &
                                            abs(Combined.FFP.NEE$Qe.closed.try1.u50)<=1.2*abs(Combined.FFP.NEE$Qnet),
                                          Combined.FFP.NEE$Qe.closed.try1.u50, NA)
#cross check
min(Combined.FFP.NEE$Qe.closed.u50, na.rm = TRUE)
max(Combined.FFP.NEE$Qe.closed.u50, na.rm = TRUE)
hist(Combined.FFP.NEE$Qe.closed.u50)
plot(Qe.closed.u50~DoY, data=Combined.FFP.NEE, col="blue", main="Qe.closed.u50")
#
#U95 star Thr
#Closed latent heat flux Qe (Watts/m2)
Combined.FFP.NEE$Qe.closed.try1.u95 <- ifelse(is.na(Combined.FFP.NEE$Qg),
                                               (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qnet*.10)/
                                                 (1+Combined.FFP.NEE$Bowen.u95),
                                               (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg)/
                                                 (1+Combined.FFP.NEE$Bowen.u95))
#
min(Combined.FFP.NEE$Qe.closed.try1.u95, na.rm = TRUE)
max(Combined.FFP.NEE$Qe.closed.try1.u95, na.rm = TRUE)
hist(Combined.FFP.NEE$Qe.closed.try1.u95)
plot(Qe.closed.try1.u95~DoY, data=Combined.FFP.NEE, col="blue", main="Qe.closed try 1.u95")
#
#filter Qe.closed.try1 to be within -1000 to +1000 W/m2 (realistic range) and to be
#within 1.2 times of #absolute Qnet value.  The result will be our final Qe.closed value.
#note: if user filter was used above, the +/-1000 may be redundant here.
Combined.FFP.NEE$Qe.closed.u95 <- ifelse(Combined.FFP.NEE$Qe.closed.try1.u95 <= 1000 &
                                            Combined.FFP.NEE$Qe.closed.try1.u95 >= -1000 &
                                            abs(Combined.FFP.NEE$Qe.closed.try1.u95)<=1.2*abs(Combined.FFP.NEE$Qnet),
                                          Combined.FFP.NEE$Qe.closed.try1.u95, NA)
#cross check
min(Combined.FFP.NEE$Qe.closed.u95, na.rm = TRUE)
max(Combined.FFP.NEE$Qe.closed.u95, na.rm = TRUE)
hist(Combined.FFP.NEE$Qe.closed.u95)
plot(Qe.closed.u95~DoY, data=Combined.FFP.NEE, col="blue", main="Qe.closed.u95")
#
#
#User U star Thr
#Closed sensible heat flux Qh (Watts/m2)
Combined.FFP.NEE$Qh.closed.try1.user <- ifelse(is.na(Combined.FFP.NEE$Qg),
                                   Combined.FFP.NEE$Bowen.user*
                                     (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qnet*.10)/
                                       (1+Combined.FFP.NEE$Bowen.user),
                                   Combined.FFP.NEE$Bowen.user*
                                     (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg)/
                                       (1+Combined.FFP.NEE$Bowen.user))
#
min(Combined.FFP.NEE$Qh.closed.try1.user, na.rm = TRUE)
max(Combined.FFP.NEE$Qh.closed.try1.user, na.rm = TRUE)
hist(Combined.FFP.NEE$Qh.closed.try1.user)
plot(Qh.closed.try1.user~DoY, data=Combined.FFP.NEE, col="red", main="Qh.closed try 1 user")
#
#filter Qh.closed.try1 to be within -1000 to +1000 W/m2 (realistic range) and to be
#within 1.2 times of #absolute Qnet value.  The result will be our final Qe.closed value.
#note: if user filter was used above, the +/-1000 may be redundant here.
Combined.FFP.NEE$Qh.closed.user <- ifelse(Combined.FFP.NEE$Qh.closed.try1.user <= 1000 &
                                       Combined.FFP.NEE$Qh.closed.try1.user >= -1000 &
                                       abs(Combined.FFP.NEE$Qh.closed.try1.user)<=1.2*abs(Combined.FFP.NEE$Qnet),
                                     Combined.FFP.NEE$Qh.closed.try1.user, NA)
#cross check
min(Combined.FFP.NEE$Qh.closed.user, na.rm = TRUE)
max(Combined.FFP.NEE$Qh.closed.user, na.rm = TRUE)
hist(Combined.FFP.NEE$Qh.closed.user)
plot(Qh.closed.user~DoY, data=Combined.FFP.NEE, col="orange", main="Qh.closed.user")
#
#U05 star Thr
#Closed sensible heat flux Qh (Watts/m2)
Combined.FFP.NEE$Qh.closed.try1.u05 <- ifelse(is.na(Combined.FFP.NEE$Qg),
                                               Combined.FFP.NEE$Bowen.u05*
                                                 (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qnet*.10)/
                                                 (1+Combined.FFP.NEE$Bowen.u05),
                                               Combined.FFP.NEE$Bowen.u05*
                                                 (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg)/
                                                 (1+Combined.FFP.NEE$Bowen.u05))
#
min(Combined.FFP.NEE$Qh.closed.try1.u05, na.rm = TRUE)
max(Combined.FFP.NEE$Qh.closed.try1.u05, na.rm = TRUE)
hist(Combined.FFP.NEE$Qh.closed.try1.u05)
plot(Qh.closed.try1.u05~DoY, data=Combined.FFP.NEE, col="red", main="Qh.closed try 1 u05")
#
#filter Qh.closed.try1 to be within -1000 to +1000 W/m2 (realistic range) and to be
# within 1.2 times of #absolute Qnet value.  The result will be our final Qe.closed value.
# note: if user filter was used above, the +/-1000 may be redundant here.
Combined.FFP.NEE$Qh.closed.u05 <- ifelse(Combined.FFP.NEE$Qh.closed.try1.u05 <= 1000 &
                                            Combined.FFP.NEE$Qh.closed.try1.u05 >= -1000 &
                                            abs(Combined.FFP.NEE$Qh.closed.try1.u05)<=1.2*abs(Combined.FFP.NEE$Qnet),
                                          Combined.FFP.NEE$Qh.closed.try1.u05, NA)
# cross check
min(Combined.FFP.NEE$Qh.closed.u05, na.rm = TRUE)
max(Combined.FFP.NEE$Qh.closed.u05, na.rm = TRUE)
hist(Combined.FFP.NEE$Qh.closed.u05)
plot(Qh.closed.u05~DoY, data=Combined.FFP.NEE, col="orange", main="Qh.closed.u05")
#
# U50 star Thr
#Closed sensible heat flux Qh (Watts/m2)
Combined.FFP.NEE$Qh.closed.try1.u50 <- ifelse(is.na(Combined.FFP.NEE$Qg),
                                               Combined.FFP.NEE$Bowen.u50*
                                                 (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qnet*.10)/
                                                 (1+Combined.FFP.NEE$Bowen.u50),
                                               Combined.FFP.NEE$Bowen.u50*
                                                 (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg)/
                                                 (1+Combined.FFP.NEE$Bowen.u50))
#
min(Combined.FFP.NEE$Qh.closed.try1.u50, na.rm = TRUE)
max(Combined.FFP.NEE$Qh.closed.try1.u50, na.rm = TRUE)
hist(Combined.FFP.NEE$Qh.closed.try1.u50)
plot(Qh.closed.try1.u50~DoY, data=Combined.FFP.NEE, col="red", main="Qh.closed try 1 u50")
#
#filter Qh.closed.try1 to be within -1000 to +1000 W/m2 (realistic range) and to be
# within 1.2 times of #absolute Qnet value.  The result will be our final Qe.closed value.
# note: if user filter was used above, the +/-1000 may be redundant here.
Combined.FFP.NEE$Qh.closed.u50 <- ifelse(Combined.FFP.NEE$Qh.closed.try1.u50 <= 1000 &
                                            Combined.FFP.NEE$Qh.closed.try1.u50 >= -1000 &
                                            abs(Combined.FFP.NEE$Qh.closed.try1.u50)<=1.2*abs(Combined.FFP.NEE$Qnet),
                                          Combined.FFP.NEE$Qh.closed.try1.u50, NA)
# cross check
min(Combined.FFP.NEE$Qh.closed.u50, na.rm = TRUE)
max(Combined.FFP.NEE$Qh.closed.u50, na.rm = TRUE)
hist(Combined.FFP.NEE$Qh.closed.u50)
plot(Qh.closed.u50~DoY, data=Combined.FFP.NEE, col="orange", main="Qh.closed.u50")
#
# u95 star Thr
#Closed sensible heat flux Qh (Watts/m2)
Combined.FFP.NEE$Qh.closed.try1.u95 <- ifelse(is.na(Combined.FFP.NEE$Qg),
                                               Combined.FFP.NEE$Bowen.u95*
                                                 (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qnet*.10)/
                                                 (1+Combined.FFP.NEE$Bowen.u95),
                                               Combined.FFP.NEE$Bowen.u95*
                                                 (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg)/
                                                 (1+Combined.FFP.NEE$Bowen.u95))
#
min(Combined.FFP.NEE$Qh.closed.try1.u95, na.rm = TRUE)
max(Combined.FFP.NEE$Qh.closed.try1.u95, na.rm = TRUE)
hist(Combined.FFP.NEE$Qh.closed.try1.u95)
plot(Qh.closed.try1.u95~DoY, data=Combined.FFP.NEE, col="red", main="Qh.closed try 1 u95")
#
#filter Qh.closed.try1 to be within -1000 to +1000 W/m2 (realistic range) and to be
# within 1.2 times of #absolute Qnet value.  The result will be our final Qe.closed value.
# note: if user filter was used above, the +/-1000 may be redundant here.
Combined.FFP.NEE$Qh.closed.u95 <- ifelse(Combined.FFP.NEE$Qh.closed.try1.u95 <= 1000 &
                                            Combined.FFP.NEE$Qh.closed.try1.u95 >= -1000 &
                                            abs(Combined.FFP.NEE$Qh.closed.try1.u95)<=1.2*abs(Combined.FFP.NEE$Qnet),
                                          Combined.FFP.NEE$Qh.closed.try1.u95, NA)
# cross check
min(Combined.FFP.NEE$Qh.closed.u95, na.rm = TRUE)
max(Combined.FFP.NEE$Qh.closed.u95, na.rm = TRUE)
hist(Combined.FFP.NEE$Qh.closed.u95)
plot(Qh.closed.u95~DoY, data=Combined.FFP.NEE, col="orange", main="Qh.closed.u95")
#




#### Gapfilling missing Qg with Qnet ###
########################################
Combined.FFP.NEE$Qg.gf <- ifelse(is.na(Combined.FFP.NEE$Qg),
                                 Combined.FFP.NEE$Qnet*.1,
                                 Combined.FFP.NEE$Qg)
min(Combined.FFP.NEE$Qg.gf, na.rm = TRUE)
max(Combined.FFP.NEE$Qg.gf, na.rm = TRUE)
hist(Combined.FFP.NEE$Qg.gf)
plot(Qg.gf~DoY, data=Combined.FFP.NEE, col="darkgoldenrod", main="Qg.gf gapfilled Qg")
#
#create column that flags all filled Qg values (i.e. when flag=1)
Combined.FFP.NEE$Qg.gf.flag <- ifelse(is.na(Combined.FFP.NEE$Qg), 1, 0)




#### RE-CALCULATE CLOSURE, for the various u-star Thresholds ###
################################################################
#Calculate energy budget closure which will be used to filter out EC-energy fluxes
#below are all in Watts/m2
#
#User Ustar
Combined.FFP.NEE$closure.ratio.final.user <- ifelse(
                                     is.na(Combined.FFP.NEE$Qe.closed.user)|
                                     is.na(Combined.FFP.NEE$Qh.closed.user)|
                                     is.na(Combined.FFP.NEE$Qnet)|
                                     is.na(Combined.FFP.NEE$Qg.gf),
                                   NA,
                                   (Combined.FFP.NEE$Qe.closed.user+Combined.FFP.NEE$Qh.closed.user)/
                                     (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg.gf))
#
min(Combined.FFP.NEE$closure.ratio.final.user, na.rm = TRUE)
max(Combined.FFP.NEE$closure.ratio.final.user, na.rm = TRUE)
plot(closure.ratio.final.user~DoY, data=Combined.FFP.NEE, main="final closure ratio userU")
summary(Combined.FFP.NEE$closure.ratio.final.user)
#
#re-calculate total closure
Combined.FFP.NEE$Total.final.closure.user.Watts <- ifelse(is.na(Combined.FFP.NEE$Qe.closed.user)|
                                                         is.na(Combined.FFP.NEE$Qh.closed.user)|
                                                         is.na(Combined.FFP.NEE$Qnet)|
                                                         is.na(Combined.FFP.NEE$Qg.gf),
                                                       NA,
                                                       Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qe.closed.user-
                                                         Combined.FFP.NEE$Qh.closed.user-Combined.FFP.NEE$Qg.gf)
#
min(Combined.FFP.NEE$Total.final.closure.user.Watts, na.rm = TRUE)
max(Combined.FFP.NEE$Total.final.closure.user.Watts, na.rm = TRUE)
plot(Total.final.closure.user.Watts~DoY, data=Combined.FFP.NEE, main = "Total final closure user in Watts")
#
# U05 Thr
Combined.FFP.NEE$closure.ratio.final.u05 <- ifelse(
  is.na(Combined.FFP.NEE$Qe.closed.u05)|
    is.na(Combined.FFP.NEE$Qh.closed.u05)|
    is.na(Combined.FFP.NEE$Qnet)|
    is.na(Combined.FFP.NEE$Qg.gf),
  NA,
  (Combined.FFP.NEE$Qe.closed.u05+Combined.FFP.NEE$Qh.closed.u05)/
    (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg.gf))
#
min(Combined.FFP.NEE$closure.ratio.final.u05, na.rm = TRUE)
max(Combined.FFP.NEE$closure.ratio.final.u05, na.rm = TRUE)
plot(closure.ratio.final.u05~DoY, data=Combined.FFP.NEE, main="final closure ratio u05U")
summary(Combined.FFP.NEE$closure.ratio.final.u05)
#
#re-calculate total closure
Combined.FFP.NEE$Total.final.closure.u05.Watts <- ifelse(is.na(Combined.FFP.NEE$Qe.closed.u05)|
                                                            is.na(Combined.FFP.NEE$Qh.closed.u05)|
                                                            is.na(Combined.FFP.NEE$Qnet)|
                                                            is.na(Combined.FFP.NEE$Qg.gf),
                                                          NA,
                                                          Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qe.closed.u05-
                                                            Combined.FFP.NEE$Qh.closed.u05-Combined.FFP.NEE$Qg.gf)
#
min(Combined.FFP.NEE$Total.final.closure.u05.Watts, na.rm = TRUE)
max(Combined.FFP.NEE$Total.final.closure.u05.Watts, na.rm = TRUE)
plot(Total.final.closure.u05.Watts~DoY, data=Combined.FFP.NEE, main = "Total final closure u05 in Watts")
#
# U50 Thr
Combined.FFP.NEE$closure.ratio.final.u50 <- ifelse(
  is.na(Combined.FFP.NEE$Qe.closed.u50)|
    is.na(Combined.FFP.NEE$Qh.closed.u50)|
    is.na(Combined.FFP.NEE$Qnet)|
    is.na(Combined.FFP.NEE$Qg.gf),
  NA,
  (Combined.FFP.NEE$Qe.closed.u50+Combined.FFP.NEE$Qh.closed.u50)/
    (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg.gf))
#
min(Combined.FFP.NEE$closure.ratio.final.u50, na.rm = TRUE)
max(Combined.FFP.NEE$closure.ratio.final.u50, na.rm = TRUE)
plot(closure.ratio.final.u50~DoY, data=Combined.FFP.NEE, main="final closure ratio u50U")
summary(Combined.FFP.NEE$closure.ratio.final.u50)
#
#re-calculate total closure
Combined.FFP.NEE$Total.final.closure.u50.Watts <- ifelse(is.na(Combined.FFP.NEE$Qe.closed.u50)|
                                                            is.na(Combined.FFP.NEE$Qh.closed.u50)|
                                                            is.na(Combined.FFP.NEE$Qnet)|
                                                            is.na(Combined.FFP.NEE$Qg.gf),
                                                          NA,
                                                          Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qe.closed.u50-
                                                            Combined.FFP.NEE$Qh.closed.u50-Combined.FFP.NEE$Qg.gf)
#
min(Combined.FFP.NEE$Total.final.closure.u50.Watts, na.rm = TRUE)
max(Combined.FFP.NEE$Total.final.closure.u50.Watts, na.rm = TRUE)
plot(Total.final.closure.u50.Watts~DoY, data=Combined.FFP.NEE, main = "Total final closure u50 in Watts")
#
# U95 Thr
Combined.FFP.NEE$closure.ratio.final.u95 <- ifelse(
  is.na(Combined.FFP.NEE$Qe.closed.u95)|
    is.na(Combined.FFP.NEE$Qh.closed.u95)|
    is.na(Combined.FFP.NEE$Qnet)|
    is.na(Combined.FFP.NEE$Qg.gf),
  NA,
  (Combined.FFP.NEE$Qe.closed.u95+Combined.FFP.NEE$Qh.closed.u95)/
    (Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qg.gf))
#
min(Combined.FFP.NEE$closure.ratio.final.u95, na.rm = TRUE)
max(Combined.FFP.NEE$closure.ratio.final.u95, na.rm = TRUE)
plot(closure.ratio.final.u95~DoY, data=Combined.FFP.NEE, main="final closure ratio u95U")
summary(Combined.FFP.NEE$closure.ratio.final.u95)
#
#re-calculate total closure
Combined.FFP.NEE$Total.final.closure.u95.Watts <- ifelse(is.na(Combined.FFP.NEE$Qe.closed.u95)|
                                                            is.na(Combined.FFP.NEE$Qh.closed.u95)|
                                                            is.na(Combined.FFP.NEE$Qnet)|
                                                            is.na(Combined.FFP.NEE$Qg.gf),
                                                          NA,
                                                          Combined.FFP.NEE$Qnet-Combined.FFP.NEE$Qe.closed.u95-
                                                            Combined.FFP.NEE$Qh.closed.u95-Combined.FFP.NEE$Qg.gf)
#
min(Combined.FFP.NEE$Total.final.closure.u95.Watts, na.rm = TRUE)
max(Combined.FFP.NEE$Total.final.closure.u95.Watts, na.rm = TRUE)
plot(Total.final.closure.u95.Watts~DoY, data=Combined.FFP.NEE, main = "Total final closure u95 in Watts")





#### RECALCULATE BOWEN RATIO for the different u-star Thresholds ###
####################################################################
# User U star Thr
Combined.FFP.NEE$Bowen.user <- Combined.FFP.NEE$Qh.closed.user/Combined.FFP.NEE$Qe.closed.user
hist(Combined.FFP.NEE$Bowen.user)
plot(Bowen.user~DoY, data=Combined.FFP.NEE, main="recalculated Bowen ratio userU", col="brown")
#
#filter Bowen to be within -15 to +15
Combined.FFP.NEE$Bowen.user <- ifelse(Combined.FFP.NEE$Bowen.user <= 15 & Combined.FFP.NEE$Bowen.user >= -15,
                                 Combined.FFP.NEE$Bowen.user, NA)
min(Combined.FFP.NEE$Bowen.user, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.user, na.rm = TRUE)
plot(Bowen.user~DoY, data=Combined.FFP.NEE, col="brown", main ="recaculated Bowen.user ratio within +/- 15")
#
# U05 star Thr
Combined.FFP.NEE$Bowen.u05 <- Combined.FFP.NEE$Qh.closed.u05/Combined.FFP.NEE$Qe.closed.u05
hist(Combined.FFP.NEE$Bowen.u05)
plot(Bowen.u05~DoY, data=Combined.FFP.NEE, main="recalculated Bowen ratio u05U", col="brown")
#
#filter Bowen to be within -15 to +15
Combined.FFP.NEE$Bowen.u05 <- ifelse(Combined.FFP.NEE$Bowen.u05 <= 15 & Combined.FFP.NEE$Bowen.u05 >= -15,
                                      Combined.FFP.NEE$Bowen.u05, NA)
min(Combined.FFP.NEE$Bowen.u05, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.u05, na.rm = TRUE)
plot(Bowen.u05~DoY, data=Combined.FFP.NEE, col="brown", main ="recaculated Bowen.u05 ratio within +/- 15")
#
# U50 star Thr
Combined.FFP.NEE$Bowen.u50 <- Combined.FFP.NEE$Qh.closed.u50/Combined.FFP.NEE$Qe.closed.u50
hist(Combined.FFP.NEE$Bowen.u50)
plot(Bowen.u50~DoY, data=Combined.FFP.NEE, main="recalculated Bowen ratio u50U", col="brown")
#
#filter Bowen to be within -15 to +15
Combined.FFP.NEE$Bowen.u50 <- ifelse(Combined.FFP.NEE$Bowen.u50 <= 15 & Combined.FFP.NEE$Bowen.u50 >= -15,
                                      Combined.FFP.NEE$Bowen.u50, NA)
min(Combined.FFP.NEE$Bowen.u50, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.u50, na.rm = TRUE)
plot(Bowen.u50~DoY, data=Combined.FFP.NEE, col="brown", main ="recaculated Bowen.u50 ratio within +/- 15")
#
# U95 star Thr
Combined.FFP.NEE$Bowen.u95 <- Combined.FFP.NEE$Qh.closed.u95/Combined.FFP.NEE$Qe.closed.u95
hist(Combined.FFP.NEE$Bowen.u95)
plot(Bowen.u95~DoY, data=Combined.FFP.NEE, main="recalculated Bowen ratio u95U", col="brown")
#
#filter Bowen to be within -15 to +15
Combined.FFP.NEE$Bowen.u95 <- ifelse(Combined.FFP.NEE$Bowen.u95 <= 15 & Combined.FFP.NEE$Bowen.u95 >= -15,
                                      Combined.FFP.NEE$Bowen.u95, NA)
min(Combined.FFP.NEE$Bowen.u95, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.u95, na.rm = TRUE)
plot(Bowen.u95~DoY, data=Combined.FFP.NEE, col="brown", main ="recaculated Bowen.u95 ratio within +/- 15")





#### CALCULATE POTENTIAL EVAPOTRANSPIRATION (PET) ###
#####################################################
#PET in millimetres per half hour (mm/hh) is calculated using Priestly-Taylor equation
#create column with atmospheric pressure in kPa
Combined.FFP.NEE$P.kPa <- Combined.FFP.NEE$Pressure/1000
#
#Pyschometeric Constant (kPa/oC), NOTE: input Pressure must be in kPa
Combined.FFP.NEE$pysch <- 0.0006647*Combined.FFP.NEE$P.kPa
#
#Slope of the saturation vapour pressure (kPa/oC), input Tair in Celsius
Combined.FFP.NEE$slopsat <- 4098*0.6108*exp(
                                (17.27*Combined.FFP.NEE$Tair)/(Combined.FFP.NEE$Tair+237.3))/
                          ((Combined.FFP.NEE$Tair+237.2)^2)
#
#Convert energies from W/m2/hh to MJ/m2/30min
Combined.FFP.NEE$Qnet.mj <- Combined.FFP.NEE$Qnet/1000000*60*30
Combined.FFP.NEE$Qg.mj <- Combined.FFP.NEE$Qg.gf/1000000*60*30
#
#Calculates specific heat of air in MJ/kg/oC, pressure in kPa
Combined.FFP.NEE$Cp.air.C <- (Combined.FFP.NEE$pysch*0.622*2.45)/
  (Combined.FFP.NEE$P.kPa)
#
#Calculate PET using Preistly-Tailor (PT) method ####
#following this link [2018]: http://bioma.jrc.ec.europa.eu/components/componentstools/evapotranspiration/help/Priestley-Taylor.html
#
#Bowen ration for ET.PM
Combined.FFP.NEE$Bowen.PT.user<-ifelse(is.na(Combined.FFP.NEE$Bowen.user),mean(Combined.FFP.NEE$Bowen.user, na.rm=TRUE),Combined.FFP.NEE$Bowen.user)
Combined.FFP.NEE$Bowen.PT.u05<-ifelse(is.na(Combined.FFP.NEE$Bowen.u05),mean(Combined.FFP.NEE$Bowen.u05, na.rm=TRUE),Combined.FFP.NEE$Bowen.u05)
Combined.FFP.NEE$Bowen.PT.u50<-ifelse(is.na(Combined.FFP.NEE$Bowen.u50),mean(Combined.FFP.NEE$Bowen.u50, na.rm=TRUE),Combined.FFP.NEE$Bowen.u50)
Combined.FFP.NEE$Bowen.PT.u95<-ifelse(is.na(Combined.FFP.NEE$Bowen.u95),mean(Combined.FFP.NEE$Bowen.u95, na.rm=TRUE),Combined.FFP.NEE$Bowen.u95)
#
#filter Bowen.PT to be positive
Combined.FFP.NEE$Bowen.PT.user <- ifelse(Combined.FFP.NEE$Bowen.PT.user <= 15 & Combined.FFP.NEE$Bowen.PT.user >= 0,
                                 Combined.FFP.NEE$Bowen.PT.user, NA)
min(Combined.FFP.NEE$Bowen.PT.user, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.PT.user, na.rm = TRUE)
plot(Bowen.PT.user~DoY, data=Combined.FFP.NEE, col="brown", main = "Bowen.PT.user")
#
Combined.FFP.NEE$Bowen.PT.u05 <- ifelse(Combined.FFP.NEE$Bowen.PT.u05 <= 15 & Combined.FFP.NEE$Bowen.PT.u05 >= 0,
                                    Combined.FFP.NEE$Bowen.PT.u05, NA)
min(Combined.FFP.NEE$Bowen.PT.u05, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.PT.u05, na.rm = TRUE)
plot(Bowen.PT.u05~DoY, data=Combined.FFP.NEE, col="brown", main = "Bowen.PT.u05")
#
Combined.FFP.NEE$Bowen.PT.u50 <- ifelse(Combined.FFP.NEE$Bowen.PT.u50 <= 15 & Combined.FFP.NEE$Bowen.PT.u50 >= 0,
                                    Combined.FFP.NEE$Bowen.PT.u50, NA)
min(Combined.FFP.NEE$Bowen.PT.u50, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.PT.u50, na.rm = TRUE)
plot(Bowen.PT.u50~DoY, data=Combined.FFP.NEE, col="brown", main = "Bowen.PT.u50")
#
Combined.FFP.NEE$Bowen.PT.u95 <- ifelse(Combined.FFP.NEE$Bowen.PT.u95 <= 15 & Combined.FFP.NEE$Bowen.PT.u95 >= 0,
                                    Combined.FFP.NEE$Bowen.PT.u95, NA)
min(Combined.FFP.NEE$Bowen.PT.u95, na.rm = TRUE)
max(Combined.FFP.NEE$Bowen.PT.u95, na.rm = TRUE)
plot(Bowen.PT.u95~DoY, data=Combined.FFP.NEE, col="brown", main = "Bowen.PT.u95")
#
#calculate P-T coefficient
#Combined.FFP.NEE$PTCoef<-((1+(Combined.FFP.NEE$pysch/Combined.FFP.NEE$slopsat))/(1+Combined.FFP.NEE$Bowen.PT))
#set constant
Combined.FFP.NEE$PTCoef<-1.26
#
#calculate PET.PT in mm/hh
Combined.FFP.NEE$PET.PT <-(1/2.45)*((Combined.FFP.NEE$slopsat*(Combined.FFP.NEE$Qnet.mj - Combined.FFP.NEE$Qg.mj))/(Combined.FFP.NEE$pysch+Combined.FFP.NEE$slopsat))*Combined.FFP.NEE$PTCoef
#
#check PET.PT
hist(Combined.FFP.NEE$PET.PT)
max(Combined.FFP.NEE$PET.PT, na.rm = TRUE)
min(Combined.FFP.NEE$PET.PT, na.rm = TRUE)
#if PET is negative, set to zero
Combined.FFP.NEE$PET.PT <- ifelse(Combined.FFP.NEE$PET.PT < 0, 0, Combined.FFP.NEE$PET.PT)
#check filter
min(Combined.FFP.NEE$PET.PT, na.rm = TRUE)
#plot PET timeseries
plot(PET.PT~DoY, data=Combined.FFP.NEE, main="PET timeseries")





#### GAPFILL ET with ET/PET RELATIONSHIP for each u-star option ###
###################################################################
#User U star
#convert Qe from W/m2/hh to Mj/m2/hh to mm/hh (ET)
Combined.FFP.NEE$ET.user <- Combined.FFP.NEE$Qe.closed.user/1000000*60*30/2.45
hist(Combined.FFP.NEE$ET.user)
max(Combined.FFP.NEE$ET.user, na.rm = TRUE)
min(Combined.FFP.NEE$ET.user, na.rm = TRUE)
#if ET is negative, set to zero
Combined.FFP.NEE$ET.user <- ifelse(Combined.FFP.NEE$ET.user < 0, 0,
                              Combined.FFP.NEE$ET.user)
#check filter
min(Combined.FFP.NEE$ET.user, na.rm = TRUE)
#plot ET timeseries
plot(ET.user~DoY, data=Combined.FFP.NEE, main="ET.user timeseries")
#
#check the relationship scatter between ET and PET
#first check linear
plot(ET.user~PET.PT, data=Combined.FFP.NEE, main="scatter plot of ET.user vs PET.PT", ylab="ET mm/hh", xlab="PET mm/hh")
#also check ln(ET) vs PET (may want to use this one, if ET vs PET looks too heteroscedastic)
#plot(log(ET)~PET.PT, data=Combined.FFP.NEE, main="scatter plot of lnET vs PET", ylab="ET mm/hh", xlab="PET mm/hh")
#
#fit lenear regression to ET vS PET scatter
ET.PET.line <- lm(ET.user ~ 0+PET.PT, data=Combined.FFP.NEE)
#
#regression summary
summary(ET.PET.line)
#
#save above regression stats to a text file for future reference
final.file.name <- paste(site.name, measurement.year, "ET.user.PET.line.txt", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
sink(File.name.path)
print(ET.PET.line)
print(summary(ET.PET.line)$r.squared)
print(summary(ET.PET.line)$adj.r.squared)
print(summary(ET.PET.line))
sink()  # returns output to the console
#
#Look up variable names in above regression
attributes(summary(ET.PET.line))
#
#create slope and intercept values to use in script below
#slope value of above regression line
ET.PET.line.m <- ET.PET.line$coeff[[1]]

#
#Look up R2 and adjusted R2 of above relationship
summary(ET.PET.line)$r.squared
summary(ET.PET.line)$adj.r.squared
#
#Calculate predicted ET from PET, based on above fitted linear regression
Combined.FFP.NEE$ET.user.pred <- ET.PET.line.m*Combined.FFP.NEE$PET.PT
#check ET.pred
hist(Combined.FFP.NEE$ET.user.pred)
max(Combined.FFP.NEE$ET.user.pred, na.rm = TRUE)
min(Combined.FFP.NEE$ET.user.pred, na.rm = TRUE)
#set negative predicted ET values to zero
Combined.FFP.NEE$ET.user.pred <-
  ifelse(Combined.FFP.NEE$ET.user.pred < 0, 0, Combined.FFP.NEE$ET.user.pred)
#check filter
min(Combined.FFP.NEE$ET.user.pred, na.rm = TRUE)
#plot ET timeseries
plot(ET.user.pred~DoY, data=Combined.FFP.NEE, main="ET.pred timeseries")
#
#gapfill ET with pred.ET
Combined.FFP.NEE$ET.user.gf <- ifelse(is.na(Combined.FFP.NEE$ET.user),
                                 ifelse(is.na(Combined.FFP.NEE$ET.user.pred), NA,
                                        Combined.FFP.NEE$ET.user.pred),
                                 Combined.FFP.NEE$ET.user)
#check gapfilled ET
hist(Combined.FFP.NEE$ET.user.gf)
max(Combined.FFP.NEE$ET.user.gf, na.rm = TRUE)
min(Combined.FFP.NEE$ET.user.gf, na.rm = TRUE)
plot(ET.user.gf~DoY, data=Combined.FFP.NEE, main="ET.user.gf timeseries")
#
#
#u05 U star
#convert Qe from W/m2/hh to Mj/m2/hh to mm/hh (ET)
Combined.FFP.NEE$ET.u05 <- Combined.FFP.NEE$Qe.closed.u05/1000000*60*30/2.45
hist(Combined.FFP.NEE$ET.u05)
max(Combined.FFP.NEE$ET.u05, na.rm = TRUE)
min(Combined.FFP.NEE$ET.u05, na.rm = TRUE)
#if ET is negative, set to zero
Combined.FFP.NEE$ET.u05 <- ifelse(Combined.FFP.NEE$ET.u05 < 0, 0,
                                   Combined.FFP.NEE$ET.u05)
#check filter
min(Combined.FFP.NEE$ET.u05, na.rm = TRUE)
#plot ET timeseries
plot(ET.u05~DoY, data=Combined.FFP.NEE, main="ET.u05 timeseries")
#
#check the relationship scatter between ET and PET
#first check linear
plot(ET.u05~PET.PT, data=Combined.FFP.NEE, main="scatter plot of ET.u05 vs PET.PT", ylab="ET mm/hh", xlab="PET mm/hh")
#also check ln(ET) vs PET (may want to use this one, if ET vs PET looks too heteroscedastic)
#plot(log(ET)~PET.PT, data=Combined.FFP.NEE, main="scatter plot of lnET vs PET", ylab="ET mm/hh", xlab="PET mm/hh")
#
#fit lenear regression to ET vS PET scatter
ET.PET.line <- lm(ET.u05 ~ 0+PET.PT, data=Combined.FFP.NEE)
#
#regression summary
summary(ET.PET.line)
#
#save above regression stats to a text file for future reference
final.file.name <- paste(site.name, measurement.year, "ET.u05.PET.line.txt", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
sink(File.name.path)
print(ET.PET.line)
print(summary(ET.PET.line)$r.squared)
print(summary(ET.PET.line)$adj.r.squared)
print(summary(ET.PET.line))
sink()  # returns output to the console
#
#Look up variable names in above regression
attributes(summary(ET.PET.line))
#
#create slope and intercept values to use in script below
#slope value of above regression line
ET.PET.line.m <- ET.PET.line$coeff[[1]]

#
#Look up R2 and adjusted R2 of above relationship
summary(ET.PET.line)$r.squared
summary(ET.PET.line)$adj.r.squared
#
#Calculate predicted ET from PET, based on above fitted linear regression
Combined.FFP.NEE$ET.u05.pred <- ET.PET.line.m*Combined.FFP.NEE$PET.PT
#check ET.pred
hist(Combined.FFP.NEE$ET.u05.pred)
max(Combined.FFP.NEE$ET.u05.pred, na.rm = TRUE)
min(Combined.FFP.NEE$ET.u05.pred, na.rm = TRUE)
#set negative predicted ET values to zero
Combined.FFP.NEE$ET.u05.pred <-
  ifelse(Combined.FFP.NEE$ET.u05.pred < 0, 0, Combined.FFP.NEE$ET.u05.pred)
#check filter
min(Combined.FFP.NEE$ET.u05.pred, na.rm = TRUE)
#plot ET timeseries
plot(ET.u05.pred~DoY, data=Combined.FFP.NEE, main="ET.pred timeseries")
#
#gapfill ET with pred.ET
Combined.FFP.NEE$ET.u05.gf <- ifelse(is.na(Combined.FFP.NEE$ET.u05),
                                      ifelse(is.na(Combined.FFP.NEE$ET.u05.pred), NA,
                                             Combined.FFP.NEE$ET.u05.pred),
                                      Combined.FFP.NEE$ET.u05)
#check gapfilled ET
hist(Combined.FFP.NEE$ET.u05.gf)
max(Combined.FFP.NEE$ET.u05.gf, na.rm = TRUE)
min(Combined.FFP.NEE$ET.u05.gf, na.rm = TRUE)
plot(ET.u05.gf~DoY, data=Combined.FFP.NEE, main="ET.u05.gf timeseries")
#
#u50 U star
#convert Qe from W/m2/hh to Mj/m2/hh to mm/hh (ET)
Combined.FFP.NEE$ET.u50 <- Combined.FFP.NEE$Qe.closed.u50/1000000*60*30/2.45
hist(Combined.FFP.NEE$ET.u50)
max(Combined.FFP.NEE$ET.u50, na.rm = TRUE)
min(Combined.FFP.NEE$ET.u50, na.rm = TRUE)
#if ET is negative, set to zero
Combined.FFP.NEE$ET.u50 <- ifelse(Combined.FFP.NEE$ET.u50 < 0, 0,
                                   Combined.FFP.NEE$ET.u50)
#check filter
min(Combined.FFP.NEE$ET.u50, na.rm = TRUE)
#plot ET timeseries
plot(ET.u50~DoY, data=Combined.FFP.NEE, main="ET.u50 timeseries")
#
#check the relationship scatter between ET and PET
#first check linear
plot(ET.u50~PET.PT, data=Combined.FFP.NEE, main="scatter plot of ET.u50 vs PET.PT", ylab="ET mm/hh", xlab="PET mm/hh")
#also check ln(ET) vs PET (may want to use this one, if ET vs PET looks too heteroscedastic)
#plot(log(ET)~PET.PT, data=Combined.FFP.NEE, main="scatter plot of lnET vs PET", ylab="ET mm/hh", xlab="PET mm/hh")
#
#fit lenear regression to ET vS PET scatter
ET.PET.line <- lm(ET.u50 ~ 0+ PET.PT, data=Combined.FFP.NEE)
#
#regression summary
summary(ET.PET.line)
#
#save above regression stats to a text file for future reference
final.file.name <- paste(site.name, measurement.year, "ET.u50.PET.line.txt", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
sink(File.name.path)
print(ET.PET.line)
print(summary(ET.PET.line)$r.squared)
print(summary(ET.PET.line)$adj.r.squared)
print(summary(ET.PET.line))
sink()  # returns output to the console
#
#Look up variable names in above regression
attributes(summary(ET.PET.line))
#
#create slope and intercept values to use in script below
#slope value of above regression line
ET.PET.line.m <- ET.PET.line$coeff[[1]]

#
#Look up R2 and adjusted R2 of above relationship
summary(ET.PET.line)$r.squared
summary(ET.PET.line)$adj.r.squared
#
#Calculate predicted ET from PET, based on above fitted linear regression
Combined.FFP.NEE$ET.u50.pred <- ET.PET.line.m*Combined.FFP.NEE$PET.PT
#check ET.pred
hist(Combined.FFP.NEE$ET.u50.pred)
max(Combined.FFP.NEE$ET.u50.pred, na.rm = TRUE)
min(Combined.FFP.NEE$ET.u50.pred, na.rm = TRUE)
#set negative predicted ET values to zero
Combined.FFP.NEE$ET.u50.pred <-
  ifelse(Combined.FFP.NEE$ET.u50.pred < 0, 0, Combined.FFP.NEE$ET.u50.pred)
#check filter
min(Combined.FFP.NEE$ET.u50.pred, na.rm = TRUE)
#plot ET timeseries
plot(ET.u50.pred~DoY, data=Combined.FFP.NEE, main="ET.pred timeseries")
#
#gapfill ET with pred.ET
Combined.FFP.NEE$ET.u50.gf <- ifelse(is.na(Combined.FFP.NEE$ET.u50),
                                      ifelse(is.na(Combined.FFP.NEE$ET.u50.pred), NA,
                                             Combined.FFP.NEE$ET.u50.pred),
                                      Combined.FFP.NEE$ET.u50)
#check gapfilled ET
hist(Combined.FFP.NEE$ET.u50.gf)
max(Combined.FFP.NEE$ET.u50.gf, na.rm = TRUE)
min(Combined.FFP.NEE$ET.u50.gf, na.rm = TRUE)
plot(ET.u50.gf~DoY, data=Combined.FFP.NEE, main="ET.u50.gf timeseries")
#
#u95 U star
#convert Qe from W/m2/hh to Mj/m2/hh to mm/hh (ET)
Combined.FFP.NEE$ET.u95 <- Combined.FFP.NEE$Qe.closed.u95/1000000*60*30/2.45
hist(Combined.FFP.NEE$ET.u95)
max(Combined.FFP.NEE$ET.u95, na.rm = TRUE)
min(Combined.FFP.NEE$ET.u95, na.rm = TRUE)
#if ET is negative, set to zero
Combined.FFP.NEE$ET.u95 <- ifelse(Combined.FFP.NEE$ET.u95 < 0, 0,
                                   Combined.FFP.NEE$ET.u95)
#check filter
min(Combined.FFP.NEE$ET.u95, na.rm = TRUE)
#plot ET timeseries
plot(ET.u95~DoY, data=Combined.FFP.NEE, main="ET.u95 timeseries")
#
#check the relationship scatter between ET and PET
#first check linear
plot(ET.u95~PET.PT, data=Combined.FFP.NEE, main="scatter plot of ET.u95 vs PET.PT", ylab="ET mm/hh", xlab="PET mm/hh")
#also check ln(ET) vs PET (may want to use this one, if ET vs PET looks too heteroscedastic)
#plot(log(ET)~PET.PT, data=Combined.FFP.NEE, main="scatter plot of lnET vs PET", ylab="ET mm/hh", xlab="PET mm/hh")
#
#fit lenear regression to ET vS PET scatter
ET.PET.line <- lm(ET.u95 ~ 0+PET.PT, data=Combined.FFP.NEE)
#
#regression summary
summary(ET.PET.line)
#
#save above regression stats to a text file for future reference
final.file.name <- paste(site.name, measurement.year, "ET.u95.PET.line.txt", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
sink(File.name.path)
print(ET.PET.line)
print(summary(ET.PET.line)$r.squared)
print(summary(ET.PET.line)$adj.r.squared)
print(summary(ET.PET.line))
sink()  # returns output to the console
#
#Look up variable names in above regression
attributes(summary(ET.PET.line))
#
#create slope and intercept values to use in script below
#slope value of above regression line
ET.PET.line.m <- ET.PET.line$coeff[[1]]

#
#Look up R2 and adjusted R2 of above relationship
summary(ET.PET.line)$r.squared
summary(ET.PET.line)$adj.r.squared
#
#Calculate predicted ET from PET, based on above fitted linear regression
Combined.FFP.NEE$ET.u95.pred <- ET.PET.line.m*Combined.FFP.NEE$PET.PT
#check ET.pred
hist(Combined.FFP.NEE$ET.u95.pred)
max(Combined.FFP.NEE$ET.u95.pred, na.rm = TRUE)
min(Combined.FFP.NEE$ET.u95.pred, na.rm = TRUE)
#set negative predicted ET values to zero
Combined.FFP.NEE$ET.u95.pred <-
  ifelse(Combined.FFP.NEE$ET.u95.pred < 0, 0, Combined.FFP.NEE$ET.u95.pred)
#check filter
min(Combined.FFP.NEE$ET.u95.pred, na.rm = TRUE)
#plot ET timeseries
plot(ET.u95.pred~DoY, data=Combined.FFP.NEE, main="ET.pred timeseries")
#
#gapfill ET with pred.ET
Combined.FFP.NEE$ET.u95.gf <- ifelse(is.na(Combined.FFP.NEE$ET.u95),
                                      ifelse(is.na(Combined.FFP.NEE$ET.u95.pred), NA,
                                             Combined.FFP.NEE$ET.u95.pred),
                                      Combined.FFP.NEE$ET.u95)
#check gapfilled ET
hist(Combined.FFP.NEE$ET.u95.gf)
max(Combined.FFP.NEE$ET.u95.gf, na.rm = TRUE)
min(Combined.FFP.NEE$ET.u95.gf, na.rm = TRUE)
plot(ET.u95.gf~DoY, data=Combined.FFP.NEE, main="ET.u95.gf timeseries")
#




#### GAPFILL Qe and Qh ###########
##################################
Combined.FFP.NEE$Qe.user.gf = ifelse(is.na(Combined.FFP.NEE$ET.user.gf), NA,
                                       Combined.FFP.NEE$ET.user.gf*2.5*1000000/1800)
Combined.FFP.NEE$Qe.u05.gf = ifelse(is.na(Combined.FFP.NEE$ET.u05.gf), NA,
                                     Combined.FFP.NEE$ET.u05.gf*2.5*1000000/1800)
Combined.FFP.NEE$Qe.u50.gf = ifelse(is.na(Combined.FFP.NEE$ET.u50.gf), NA,
                                     Combined.FFP.NEE$ET.u50.gf*2.5*1000000/1800)
Combined.FFP.NEE$Qe.u95.gf = ifelse(is.na(Combined.FFP.NEE$ET.u95.gf), NA,
                                     Combined.FFP.NEE$ET.u95.gf*2.5*1000000/1800)
#
Combined.FFP.NEE$Qh.user.gf = ifelse(is.na(Combined.FFP.NEE$Qnet) |
                                  is.na(Combined.FFP.NEE$Qg.gf) |
                                  is.na(Combined.FFP.NEE$Qe.user.gf), NA,
                                         ifelse(is.na(Combined.FFP.NEE$Qh.closed.user),
                                                       Combined.FFP.NEE$Qnet-
                                                        Combined.FFP.NEE$Qg.gf -
                                                          Combined.FFP.NEE$Qe.user.gf,
                                          Combined.FFP.NEE$Qh.closed.user))
Combined.FFP.NEE$Qh.u05.gf = ifelse(is.na(Combined.FFP.NEE$Qnet) |
                                       is.na(Combined.FFP.NEE$Qg.gf) |
                                       is.na(Combined.FFP.NEE$Qe.u05.gf), NA,
                                     ifelse(is.na(Combined.FFP.NEE$Qh.closed.u05),
                                            Combined.FFP.NEE$Qnet-
                                              Combined.FFP.NEE$Qg.gf -
                                              Combined.FFP.NEE$Qe.u05.gf,
                                            Combined.FFP.NEE$Qh.closed.u05))
Combined.FFP.NEE$Qh.u50.gf = ifelse(is.na(Combined.FFP.NEE$Qnet) |
                                       is.na(Combined.FFP.NEE$Qg.gf) |
                                       is.na(Combined.FFP.NEE$Qe.u50.gf), NA,
                                     ifelse(is.na(Combined.FFP.NEE$Qh.closed.u50),
                                            Combined.FFP.NEE$Qnet-
                                              Combined.FFP.NEE$Qg.gf -
                                              Combined.FFP.NEE$Qe.u50.gf,
                                            Combined.FFP.NEE$Qh.closed.u50))
Combined.FFP.NEE$Qh.u95.gf = ifelse(is.na(Combined.FFP.NEE$Qnet) |
                                       is.na(Combined.FFP.NEE$Qg.gf) |
                                       is.na(Combined.FFP.NEE$Qe.u95.gf), NA,
                                     ifelse(is.na(Combined.FFP.NEE$Qh.closed.u95),
                                            Combined.FFP.NEE$Qnet-
                                              Combined.FFP.NEE$Qg.gf -
                                              Combined.FFP.NEE$Qe.u95.gf,
                                            Combined.FFP.NEE$Qh.closed.u95))
#check values
plot(Qh.closed.user~DoY, data=Combined.FFP.NEE, col="orange", main="User U: orange=closed, purple=ufiltered, pink=Qh.gf")
points(Qh.user.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qh.user.gf~DoY, data=Combined.FFP.NEE, col="pink")
#
plot(Qh.closed.u05~DoY, data=Combined.FFP.NEE, col="orange", main="U05: orange=closed, purple=ufiltered, pink=Qh.gf")
points(Qh.u05.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qh.u05.gf~DoY, data=Combined.FFP.NEE, col="pink")
#
plot(Qh.closed.u50~DoY, data=Combined.FFP.NEE, col="orange", main="U50: orange=closed, purple=ufiltered, pink=Qh.gf")
points(Qh.u50.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qh.u50.gf~DoY, data=Combined.FFP.NEE, col="pink")
#
plot(Qh.closed.u95~DoY, data=Combined.FFP.NEE, col="orange", main="U95: orange=closed, purple=ufiltered, pink=Qh.gf")
points(Qh.u95.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qh.u95.gf~DoY, data=Combined.FFP.NEE, col="pink")
#
plot(Qh.user.gf~DoY, data=Combined.FFP.NEE, col="orange", main=".gf: org=user, prpl=u05, pnk=u50, blk=u95")
points(Qh.u05.gf~DoY, data=Combined.FFP.NEE, col="purple")
points(Qh.u50.gf~DoY, data=Combined.FFP.NEE, col="pink")
points(Qh.u95.gf~DoY, data=Combined.FFP.NEE)
#
#
plot(Qe.closed.user~DoY, data=Combined.FFP.NEE, col="blue", main="User U: blue=closed, purple=ufiltered, pink=Qe.gf")
points(Qe.user.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qe.user.gf~DoY, data=Combined.FFP.NEE, col="pink")
#
plot(Qe.closed.u05~DoY, data=Combined.FFP.NEE, col="blue", main="U05: blue=closed, purple=ufiltered, pink=Qe.gf")
points(Qe.u05.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qe.u05.gf~DoY, data=Combined.FFP.NEE, col="pink")
#
plot(Qe.closed.u50~DoY, data=Combined.FFP.NEE, col="blue", main="U50: blue=closed, purple=ufiltered, pink=Qe.gf")
points(Qe.u50.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qe.u50.gf~DoY, data=Combined.FFP.NEE, col="pink")
#
plot(Qe.closed.u95~DoY, data=Combined.FFP.NEE, col="blue", main="U95: blue=closed, purple=ufiltered, pink=Qe.gf")
points(Qe.u95.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qe.u95.gf~DoY, data=Combined.FFP.NEE, col="pink")
#
plot(Qe.user.gf~DoY, data=Combined.FFP.NEE, col="blue", main=".gf: ble=user, prpl=u05, pnk=u50, blk=u95")
points(Qe.u05.gf~DoY, data=Combined.FFP.NEE, col="purple")
points(Qe.u50.gf~DoY, data=Combined.FFP.NEE, col="pink")
points(Qe.u95.gf~DoY, data=Combined.FFP.NEE)
#
#
plot(Qnet~DoY, data=Combined.FFP.NEE, col="gold", main="ENERGY BALANCE timeseries - user U",
     sub="golg=Qnet, blue=Qe.gf, orange=Qh.gf, brown=Qg.gf")
points(Qe.user.gf~DoY, data=Combined.FFP.NEE, col="blue")
points(Qh.user.gf~DoY, data=Combined.FFP.NEE, col="orange")
points(Qg.gf~DoY, data=Combined.FFP.NEE, col="brown")
#
plot(Qnet~DoY, data=Combined.FFP.NEE, col="gold", main="ENERGY BALANCE timeseries - 05 U",
     sub="golg=Qnet, blue=Qe.gf, orange=Qh.gf, brown=Qg.gf")
points(Qe.u05.gf~DoY, data=Combined.FFP.NEE, col="blue")
points(Qh.u05.gf~DoY, data=Combined.FFP.NEE, col="orange")
points(Qg.gf~DoY, data=Combined.FFP.NEE, col="brown")
#
plot(Qnet~DoY, data=Combined.FFP.NEE, col="gold", main="ENERGY BALANCE timeseries - 50 U",
     sub="golg=Qnet, blue=Qe.gf, orange=Qh.gf, brown=Qg.gf")
points(Qe.u50.gf~DoY, data=Combined.FFP.NEE, col="blue")
points(Qh.u50.gf~DoY, data=Combined.FFP.NEE, col="orange")
points(Qg.gf~DoY, data=Combined.FFP.NEE, col="brown")
#
plot(Qnet~DoY, data=Combined.FFP.NEE, col="gold", main="ENERGY BALANCE timeseries - 95 U",
     sub="golg=Qnet, blue=Qe.gf, orange=Qh.gf, brown=Qg.gf")
points(Qe.u95.gf~DoY, data=Combined.FFP.NEE, col="blue")
points(Qh.u95.gf~DoY, data=Combined.FFP.NEE, col="orange")
points(Qg.gf~DoY, data=Combined.FFP.NEE, col="brown")
#
#
#####  save plots to pdf.file ####
final.file.name <- paste(site.name, measurement.year, "Gapfilled.Energy.Fluxes.pdf", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
pdf(file=File.name.path)
plot(Qh.closed.user~DoY, data=Combined.FFP.NEE, col="orange", main="User U: orange=closed, purple=ufiltered, pink=Qh.gf")
points(Qh.user.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qh.user.gf~DoY, data=Combined.FFP.NEE, col="pink")
plot(Qh.closed.u05~DoY, data=Combined.FFP.NEE, col="orange", main="U05: orange=closed, purple=ufiltered, pink=Qh.gf")
points(Qh.u05.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qh.u05.gf~DoY, data=Combined.FFP.NEE, col="pink")
plot(Qh.closed.u50~DoY, data=Combined.FFP.NEE, col="orange", main="U50: orange=closed, purple=ufiltered, pink=Qh.gf")
points(Qh.u50.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qh.u50.gf~DoY, data=Combined.FFP.NEE, col="pink")
plot(Qh.closed.u95~DoY, data=Combined.FFP.NEE, col="orange", main="U95: orange=closed, purple=ufiltered, pink=Qh.gf")
points(Qh.u95.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qh.u95.gf~DoY, data=Combined.FFP.NEE, col="pink")
plot(Qh.user.gf~DoY, data=Combined.FFP.NEE, col="orange", main=".gf: org=user, prpl=u05, pnk=u50, blk=u95")
points(Qh.u05.gf~DoY, data=Combined.FFP.NEE, col="purple")
points(Qh.u50.gf~DoY, data=Combined.FFP.NEE, col="pink")
points(Qh.u95.gf~DoY, data=Combined.FFP.NEE)
plot(Qe.closed.user~DoY, data=Combined.FFP.NEE, col="blue", main="User U: blue=closed, purple=ufiltered, pink=Qe.gf")
points(Qe.user.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qe.user.gf~DoY, data=Combined.FFP.NEE, col="pink")
plot(Qe.closed.u05~DoY, data=Combined.FFP.NEE, col="blue", main="U05: blue=closed, purple=ufiltered, pink=Qe.gf")
points(Qe.u05.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qe.u05.gf~DoY, data=Combined.FFP.NEE, col="pink")
plot(Qe.closed.u50~DoY, data=Combined.FFP.NEE, col="blue", main="U50: blue=closed, purple=ufiltered, pink=Qe.gf")
points(Qe.u50.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qe.u50.gf~DoY, data=Combined.FFP.NEE, col="pink")
plot(Qe.closed.u95~DoY, data=Combined.FFP.NEE, col="blue", main="U95: blue=closed, purple=ufiltered, pink=Qe.gf")
points(Qe.u95.filtered~DoY, data=Combined.FFP.NEE, col="purple")
points(Qe.u95.gf~DoY, data=Combined.FFP.NEE, col="pink")
plot(Qe.user.gf~DoY, data=Combined.FFP.NEE, col="blue", main=".gf: ble=user, prpl=u05, pnk=u50, blk=u95")
points(Qe.u05.gf~DoY, data=Combined.FFP.NEE, col="purple")
points(Qe.u50.gf~DoY, data=Combined.FFP.NEE, col="pink")
points(Qe.u95.gf~DoY, data=Combined.FFP.NEE)
plot(Qnet~DoY, data=Combined.FFP.NEE, col="gold", main="ENERGY BALANCE timeseries - user U",
     sub="golg=Qnet, blue=Qe.gf, orange=Qh.gf, brown=Qg.gf")
points(Qe.user.gf~DoY, data=Combined.FFP.NEE, col="blue")
points(Qh.user.gf~DoY, data=Combined.FFP.NEE, col="orange")
points(Qg.gf~DoY, data=Combined.FFP.NEE, col="brown")
plot(Qnet~DoY, data=Combined.FFP.NEE, col="gold", main="ENERGY BALANCE timeseries - 05 U",
     sub="golg=Qnet, blue=Qe.gf, orange=Qh.gf, brown=Qg.gf")
points(Qe.u05.gf~DoY, data=Combined.FFP.NEE, col="blue")
points(Qh.u05.gf~DoY, data=Combined.FFP.NEE, col="orange")
points(Qg.gf~DoY, data=Combined.FFP.NEE, col="brown")
plot(Qnet~DoY, data=Combined.FFP.NEE, col="gold", main="ENERGY BALANCE timeseries - 50 U",
     sub="golg=Qnet, blue=Qe.gf, orange=Qh.gf, brown=Qg.gf")
points(Qe.u50.gf~DoY, data=Combined.FFP.NEE, col="blue")
points(Qh.u50.gf~DoY, data=Combined.FFP.NEE, col="orange")
points(Qg.gf~DoY, data=Combined.FFP.NEE, col="brown")
plot(Qnet~DoY, data=Combined.FFP.NEE, col="gold", main="ENERGY BALANCE timeseries - 95 U",
     sub="golg=Qnet, blue=Qe.gf, orange=Qh.gf, brown=Qg.gf")
points(Qe.u95.gf~DoY, data=Combined.FFP.NEE, col="blue")
points(Qh.u95.gf~DoY, data=Combined.FFP.NEE, col="orange")
points(Qg.gf~DoY, data=Combined.FFP.NEE, col="brown")
dev.off()




#### CALCULATE WUE in gC/m2/mm for different U star Thr values #######
######################################################################
#calculate water use efficiency as gram of Carbon gain per mm of water loss
#so from EC data it will be GPP in grams C per m2 divided by ET.gf if mm per hh
#calculate GPP in g C/m2, derived from different U star Thr
# User U star
Combined.FFP.NEE$GPP.user.gC.m2.hh <- Combined.FFP.NEE$GPP_userUstar_f*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$GPP.user.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$GPP.user.gC.m2.hh , na.rm = TRUE)
#set negative GPP, set as zero
Combined.FFP.NEE$GPP.user.gC.m2.hh <-
  ifelse(Combined.FFP.NEE$GPP.user.gC.m2.hh < 0, 0, Combined.FFP.NEE$GPP.user.gC.m2.hh)
#check filter
min(Combined.FFP.NEE$GPP.user.gC.m2.hh , na.rm = TRUE)
#GPP timeseries
plot(GPP.user.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="GPP.user.gC.m2.hh  timeseries")
#
plot(GPP.user.gC.m2.hh ~ Rg, data=Combined.FFP.NEE, main="GPP.user.gC.m2.hh   vs Rg")
# GPP vs ET
plot(GPP.user.gC.m2.hh~ET.user.gf, data=Combined.FFP.NEE, main="user U: GPP vs ET - in gr/mm")
#fit lenear regression to GPP vs ET scatter
WUE.line <- lm(GPP.user.gC.m2.hh~ET.user.gf, data=Combined.FFP.NEE)
#regression summary
summary(WUE.line)
#slope of above regression, so estimated overall WUE in gC/gwater
WUE.line.m <- WUE.line$coeff[[2]]
print(WUE.line.m)
#
#CALCULATED WUE in g C/g water per hh
#gram of water = mm of water
Combined.FFP.NEE$WUE.user <- ifelse(Combined.FFP.NEE$GPP.user.gC.m2.hh > 0 & Combined.FFP.NEE$ET.user.gf > 0,
                                Combined.FFP.NEE$GPP.user.gC.m2.hh/Combined.FFP.NEE$ET.user.gf, NA)
#check WUE values
max(Combined.FFP.NEE$WUE.user, na.rm = TRUE)
min(Combined.FFP.NEE$WUE.user, na.rm = TRUE)
hist(Combined.FFP.NEE$WUE.user)
plot(WUE.user ~ DoY, data=Combined.FFP.NEE, main="WUE timeseries")
#subset WUE to within range of most values as shown in plots above
Combined.FFP.NEE$WUE.user.qc <- ifelse(Combined.FFP.NEE$WUE.user > 0 & Combined.FFP.NEE$WUE.user < 10,
                                                         Combined.FFP.NEE$WUE.user, NA)
#
max(Combined.FFP.NEE$WUE.user.qc, na.rm = TRUE)
min(Combined.FFP.NEE$WUE.user.qc, na.rm = TRUE)
hist(Combined.FFP.NEE$WUE.user.qc)
plot(WUE.user.qc ~ DoY, data=Combined.FFP.NEE, main="WUE.subset timeseries")
mean(Combined.FFP.NEE$WUE.user.qc,na.rm=TRUE)
median(Combined.FFP.NEE$WUE.user.qc, na.rm=TRUE)
sd(Combined.FFP.NEE$WUE.user.qc, na.rm=TRUE)
#
#
# 05 U star
Combined.FFP.NEE$GPP.u05.gC.m2.hh <- Combined.FFP.NEE$GPP_U05_f*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$GPP.u05.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$GPP.u05.gC.m2.hh , na.rm = TRUE)
#set negative GPP, set as zero
Combined.FFP.NEE$GPP.u05.gC.m2.hh <-
  ifelse(Combined.FFP.NEE$GPP.u05.gC.m2.hh < 0, 0, Combined.FFP.NEE$GPP.u05.gC.m2.hh)
#check filter
min(Combined.FFP.NEE$GPP.u05.gC.m2.hh , na.rm = TRUE)
#GPP timeseries
plot(GPP.u05.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="GPP.u05.gC.m2.hh  timeseries")
#
plot(GPP.u05.gC.m2.hh ~ Rg, data=Combined.FFP.NEE, main="GPP.u05.gC.m2.hh   vs Rg")
# GPP vs ET
plot(GPP.u05.gC.m2.hh~ET.u05.gf, data=Combined.FFP.NEE, main="05 U: GPP vs ET - in gr/mm")
#fit lenear regression to GPP vs ET scatter
WUE.line <- lm(GPP.u05.gC.m2.hh~ET.u05.gf, data=Combined.FFP.NEE)
#regression summary
summary(WUE.line)
#slope of above regression, so estimated overall WUE in gC/gwater
WUE.line.m <- WUE.line$coeff[[2]]
print(WUE.line.m)
#
#CALCULATED WUE in g C/g water per hh
#gram of water = mm of water
Combined.FFP.NEE$WUE.u05 <- ifelse(Combined.FFP.NEE$GPP.u05.gC.m2.hh > 0 & Combined.FFP.NEE$ET.u05.gf > 0,
                                    Combined.FFP.NEE$GPP.u05.gC.m2.hh/Combined.FFP.NEE$ET.u05.gf, NA)
#check WUE values
max(Combined.FFP.NEE$WUE.u05, na.rm = TRUE)
min(Combined.FFP.NEE$WUE.u05, na.rm = TRUE)
hist(Combined.FFP.NEE$WUE.u05)
plot(WUE.u05 ~ DoY, data=Combined.FFP.NEE, main="WUE timeseries")
#subset WUE to within range of most values as shown in plots above
Combined.FFP.NEE$WUE.u05.qc <- ifelse(Combined.FFP.NEE$WUE.u05 > 0 & Combined.FFP.NEE$WUE.u05 < 10,
                                       Combined.FFP.NEE$WUE.u05, NA)
#
max(Combined.FFP.NEE$WUE.u05.qc, na.rm = TRUE)
min(Combined.FFP.NEE$WUE.u05.qc, na.rm = TRUE)
hist(Combined.FFP.NEE$WUE.u05.qc)
plot(WUE.u05.qc ~ DoY, data=Combined.FFP.NEE, main="WUE.subset timeseries")
mean(Combined.FFP.NEE$WUE.u05.qc,na.rm=TRUE)
median(Combined.FFP.NEE$WUE.u05.qc, na.rm=TRUE)
sd(Combined.FFP.NEE$WUE.u05.qc, na.rm=TRUE)
#
# 50 U star
Combined.FFP.NEE$GPP.u50.gC.m2.hh <- Combined.FFP.NEE$GPP_U50_f*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$GPP.u50.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$GPP.u50.gC.m2.hh , na.rm = TRUE)
#set negative GPP, set as zero
Combined.FFP.NEE$GPP.u50.gC.m2.hh <-
  ifelse(Combined.FFP.NEE$GPP.u50.gC.m2.hh < 0, 0, Combined.FFP.NEE$GPP.u50.gC.m2.hh)
#check filter
min(Combined.FFP.NEE$GPP.u50.gC.m2.hh , na.rm = TRUE)
#GPP timeseries
plot(GPP.u50.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="GPP.u50.gC.m2.hh  timeseries")
#
plot(GPP.u50.gC.m2.hh ~ Rg, data=Combined.FFP.NEE, main="GPP.u50.gC.m2.hh   vs Rg")
# GPP vs ET
plot(GPP.u50.gC.m2.hh~ET.u50.gf, data=Combined.FFP.NEE, main="50 U: GPP vs ET - in gr/mm")
#fit lenear regression to GPP vs ET scatter
WUE.line <- lm(GPP.u50.gC.m2.hh~ET.u50.gf, data=Combined.FFP.NEE)
#regression summary
summary(WUE.line)
#slope of above regression, so estimated overall WUE in gC/gwater
WUE.line.m <- WUE.line$coeff[[2]]
print(WUE.line.m)
#
#CALCULATED WUE in g C/g water per hh
#gram of water = mm of water
Combined.FFP.NEE$WUE.u50 <- ifelse(Combined.FFP.NEE$GPP.u50.gC.m2.hh > 0 & Combined.FFP.NEE$ET.u50.gf > 0,
                                    Combined.FFP.NEE$GPP.u50.gC.m2.hh/Combined.FFP.NEE$ET.u50.gf, NA)
#check WUE values
max(Combined.FFP.NEE$WUE.u50, na.rm = TRUE)
min(Combined.FFP.NEE$WUE.u50, na.rm = TRUE)
hist(Combined.FFP.NEE$WUE.u50)
plot(WUE.u50 ~ DoY, data=Combined.FFP.NEE, main="WUE timeseries")
#subset WUE to within range of most values as shown in plots above
Combined.FFP.NEE$WUE.u50.qc <- ifelse(Combined.FFP.NEE$WUE.u50 > 0 & Combined.FFP.NEE$WUE.u50 < 10,
                                       Combined.FFP.NEE$WUE.u50, NA)
#
max(Combined.FFP.NEE$WUE.u50.qc, na.rm = TRUE)
min(Combined.FFP.NEE$WUE.u50.qc, na.rm = TRUE)
hist(Combined.FFP.NEE$WUE.u50.qc)
plot(WUE.u50.qc ~ DoY, data=Combined.FFP.NEE, main="WUE.subset timeseries")
mean(Combined.FFP.NEE$WUE.u50.qc,na.rm=TRUE)
median(Combined.FFP.NEE$WUE.u50.qc, na.rm=TRUE)
sd(Combined.FFP.NEE$WUE.u50.qc, na.rm=TRUE)
#
#
# 95 U star
Combined.FFP.NEE$GPP.u95.gC.m2.hh <- Combined.FFP.NEE$GPP_U95_f*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$GPP.u95.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$GPP.u95.gC.m2.hh , na.rm = TRUE)
#set negative GPP, set as zero
Combined.FFP.NEE$GPP.u95.gC.m2.hh <-
  ifelse(Combined.FFP.NEE$GPP.u95.gC.m2.hh < 0, 0, Combined.FFP.NEE$GPP.u95.gC.m2.hh)
#check filter
min(Combined.FFP.NEE$GPP.u95.gC.m2.hh , na.rm = TRUE)
#GPP timeseries
plot(GPP.u95.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="GPP.u95.gC.m2.hh  timeseries")
#
plot(GPP.u95.gC.m2.hh ~ Rg, data=Combined.FFP.NEE, main="GPP.u95.gC.m2.hh   vs Rg")
# GPP vs ET
plot(GPP.u95.gC.m2.hh~ET.u95.gf, data=Combined.FFP.NEE, main="95 U: GPP vs ET - in gr/mm")
#fit lenear regression to GPP vs ET scatter
WUE.line <- lm(GPP.u95.gC.m2.hh~ET.u95.gf, data=Combined.FFP.NEE)
#regression summary
summary(WUE.line)
#slope of above regression, so estimated overall WUE in gC/gwater
WUE.line.m <- WUE.line$coeff[[2]]
print(WUE.line.m)
#
#CALCULATED WUE in g C/g water per hh
#gram of water = mm of water
Combined.FFP.NEE$WUE.u95 <- ifelse(Combined.FFP.NEE$GPP.u95.gC.m2.hh > 0 & Combined.FFP.NEE$ET.u95.gf > 0,
                                    Combined.FFP.NEE$GPP.u95.gC.m2.hh/Combined.FFP.NEE$ET.u95.gf, NA)
#check WUE values
max(Combined.FFP.NEE$WUE.u95, na.rm = TRUE)
min(Combined.FFP.NEE$WUE.u95, na.rm = TRUE)
hist(Combined.FFP.NEE$WUE.u95)
plot(WUE.u95 ~ DoY, data=Combined.FFP.NEE, main="WUE timeseries")
#subset WUE to within range of most values as shown in plots above
Combined.FFP.NEE$WUE.u95.qc <- ifelse(Combined.FFP.NEE$WUE.u95 > 0 & Combined.FFP.NEE$WUE.u95 < 10,
                                       Combined.FFP.NEE$WUE.u95, NA)
#
max(Combined.FFP.NEE$WUE.u95.qc, na.rm = TRUE)
min(Combined.FFP.NEE$WUE.u95.qc, na.rm = TRUE)
hist(Combined.FFP.NEE$WUE.u95.qc)
plot(WUE.u95.qc ~ DoY, data=Combined.FFP.NEE, main="WUE.subset timeseries")
mean(Combined.FFP.NEE$WUE.u95.qc,na.rm=TRUE)
median(Combined.FFP.NEE$WUE.u95.qc, na.rm=TRUE)
sd(Combined.FFP.NEE$WUE.u95.qc, na.rm=TRUE)





#### COMPUTE & SAVE "ANNUAL" TOTALS for the different Ustar Thr values #########
################################################################################
#NOTE: "Annual" is not necessarily 356/366 day total if measurements were taken only during the
#growing season. So these are totals for all days of measurement (which are have no missing values)
#
# user U star Thr
#calculate NEE in g C/m2/hh
Combined.FFP.NEE$NEE.user.gC.m2.hh <- Combined.FFP.NEE$NEE_userUstar_f*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$NEE.user.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$NEE.user.gC.m2.hh , na.rm = TRUE)
plot(NEE.user.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="NEE.user.gC.m2.hh  timeseries")
#
#calculate Reco in g C/m2/hh
Combined.FFP.NEE$Reco.user.gC.m2.hh <- Combined.FFP.NEE$Reco_user*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$Reco.user.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$Reco.user.gC.m2.hh , na.rm = TRUE)
plot(Reco.user.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="Reco.user.gC.m2.hh  timeseries")
#
# ANNUAL TOTAL OF Carbon fluxes in grams of C per m2 (gC/m2/yr)
sum(Combined.FFP.NEE$GPP.user.gC.m2.hh, na.rm=TRUE)
sum(Combined.FFP.NEE$Reco.user.gC.m2.hh, na.rm=TRUE)
sum(Combined.FFP.NEE$NEE.user.gC.m2.hh, na.rm=TRUE)
#
# "ANNUAL" TOTAL OF ET in mm/m2 (mm/m2/yr)
sum(Combined.FFP.NEE$ET.user.gf, na.rm=TRUE)
#
#
# U05 U star Thr
#calculate NEE in g C/m2/hh
Combined.FFP.NEE$NEE.u05.gC.m2.hh <- Combined.FFP.NEE$NEE_U05_f*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$NEE.u05.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$NEE.u05.gC.m2.hh , na.rm = TRUE)
plot(NEE.u05.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="NEE.u05.gC.m2.hh  timeseries")
#
#calculate Reco in g C/m2/hh
Combined.FFP.NEE$Reco.u05.gC.m2.hh <- Combined.FFP.NEE$Reco_U05*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$Reco.u05.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$Reco.u05.gC.m2.hh , na.rm = TRUE)
plot(Reco.u05.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="Reco.u05.gC.m2.hh  timeseries")
#
# ANNUAL TOTAL OF Carbon fluxes in grams of C per m2 (gC/m2/yr)
sum(Combined.FFP.NEE$GPP.u05.gC.m2.hh, na.rm=TRUE)
sum(Combined.FFP.NEE$Reco.u05.gC.m2.hh, na.rm=TRUE)
sum(Combined.FFP.NEE$NEE.u05.gC.m2.hh, na.rm=TRUE)
#
# "ANNUAL" TOTAL OF ET in mm/m2 (mm/m2/yr)
sum(Combined.FFP.NEE$ET.u05.gf, na.rm=TRUE)
#
# U50 star Thr
#calculate NEE in g C/m2/hh
Combined.FFP.NEE$NEE.u50.gC.m2.hh <- Combined.FFP.NEE$NEE_U50_f*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$NEE.u50.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$NEE.u50.gC.m2.hh , na.rm = TRUE)
plot(NEE.u50.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="NEE.u50.gC.m2.hh  timeseries")
#
#calculate Reco in g C/m2/hh
Combined.FFP.NEE$Reco.u50.gC.m2.hh <- Combined.FFP.NEE$Reco_U50*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$Reco.u50.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$Reco.u50.gC.m2.hh , na.rm = TRUE)
plot(Reco.u50.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="Reco.u50.gC.m2.hh  timeseries")
#
# ANNUAL TOTAL OF Carbon fluxes in grams of C per m2 (gC/m2/yr)
sum(Combined.FFP.NEE$GPP.u50.gC.m2.hh, na.rm=TRUE)
sum(Combined.FFP.NEE$Reco.u50.gC.m2.hh, na.rm=TRUE)
sum(Combined.FFP.NEE$NEE.u50.gC.m2.hh, na.rm=TRUE)
#
# "ANNUAL" TOTAL OF ET in mm/m2 (mm/m2/yr)
sum(Combined.FFP.NEE$ET.u50.gf, na.rm=TRUE)
#
# u95 U star Thr
#calculate NEE in g C/m2/hh
Combined.FFP.NEE$NEE.u95.gC.m2.hh <- Combined.FFP.NEE$NEE_U95_f*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$NEE.u95.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$NEE.u95.gC.m2.hh , na.rm = TRUE)
plot(NEE.u95.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="NEE.u95.gC.m2.hh  timeseries")
#
#calculate Reco in g C/m2/hh
Combined.FFP.NEE$Reco.u95.gC.m2.hh <- Combined.FFP.NEE$Reco_U95*1e-6*molarMass.CO2*30*60
max(Combined.FFP.NEE$Reco.u95.gC.m2.hh , na.rm = TRUE)
min(Combined.FFP.NEE$Reco.u95.gC.m2.hh , na.rm = TRUE)
plot(Reco.u95.gC.m2.hh ~ DoY, data=Combined.FFP.NEE, main="Reco.u95.gC.m2.hh  timeseries")
#
# ANNUAL TOTAL OF Carbon fluxes in grams of C per m2 (gC/m2/yr)
sum(Combined.FFP.NEE$GPP.u95.gC.m2.hh, na.rm=TRUE)
sum(Combined.FFP.NEE$Reco.u95.gC.m2.hh, na.rm=TRUE)
sum(Combined.FFP.NEE$NEE.u95.gC.m2.hh, na.rm=TRUE)
#
# "ANNUAL" TOTAL OF ET in mm/m2 (mm/m2/yr)
sum(Combined.FFP.NEE$ET.u95.gf, na.rm=TRUE)
#
#
#save totals to a text file
final.file.name <- paste(site.name, measurement.year, "Annual.Flux.totals.txt", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
sink(File.name.path)
print("Annual Flux Totals from user-defined U-star filtering")
print("GPP.user.gC.m2 total")
sum(Combined.FFP.NEE$GPP.user.gC.m2.hh, na.rm=TRUE)
print("Reco.user.gC.m2 total")
sum(Combined.FFP.NEE$Reco.user.gC.m2.hh, na.rm=TRUE)
print("NEE.user.gC.m2 total")
sum(Combined.FFP.NEE$NEE.user.gC.m2.hh, na.rm=TRUE)
print("ET.user.gf in mm/m2 total")
sum(Combined.FFP.NEE$ET.user.gf, na.rm=TRUE)
print("Annual Flux Totals from u05-defined U-star filtering")
print("GPP.05.gC.m2 total")
sum(Combined.FFP.NEE$GPP.u05.gC.m2.hh, na.rm=TRUE)
print("Reco.u05.gC.m2 total")
sum(Combined.FFP.NEE$Reco.u05.gC.m2.hh, na.rm=TRUE)
print("NEE.u05.gC.m2 total")
sum(Combined.FFP.NEE$NEE.u05.gC.m2.hh, na.rm=TRUE)
print("ET.u05 in mm/m2 total")
sum(Combined.FFP.NEE$ET.u05.gf, na.rm=TRUE)
print("Annual Flux Totals from u50-defined U-star filtering")
print("GPP.u50.gC.m2 total")
sum(Combined.FFP.NEE$GPP.u50.gC.m2.hh, na.rm=TRUE)
print("Reco.u50.gC.m2 total")
sum(Combined.FFP.NEE$Reco.u50.gC.m2.hh, na.rm=TRUE)
print("NEE.u50.gC.m2 total")
sum(Combined.FFP.NEE$NEE.u50.gC.m2.hh, na.rm=TRUE)
print("ET.u50 in mm/m2 total")
sum(Combined.FFP.NEE$ET.u50.gf, na.rm=TRUE)
print("Annual Flux Totals from u95-defined U-star filtering")
print("GPP.u95.gC.m2 total")
sum(Combined.FFP.NEE$GPP.u95.gC.m2.hh, na.rm=TRUE)
print("Reco.u95.gC.m2 total")
sum(Combined.FFP.NEE$Reco.u95.gC.m2.hh, na.rm=TRUE)
print("NEE.u95.gC.m2 total")
sum(Combined.FFP.NEE$NEE.u95.gC.m2.hh, na.rm=TRUE)
print("ET.u95 in mm/m2 total")
sum(Combined.FFP.NEE$ET.u95.gf, na.rm=TRUE)
sink()




#### MASTER BACKUP #####################
########################################
#backup file with all of the above computations in it  - THIS IS THE MASTER FILE
#should have all of the above
#remove any duplicated columns from the Master file.
Combined.FFP.NEE2 <- Combined.FFP.NEE[, !duplicated(colnames(Combined.FFP.NEE))]
final.file.name <- paste(site.name, measurement.year, "Master.backup.csv", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
write.csv(Combined.FFP.NEE, file=File.name.path)




### PREPARE FINAL OUTPUT DATA TO SHARE FOR FURTHER ANALYSIS #######
###################################################################
#### CREATE & SAVE FINAL OUTPUT FILE ###
########################################
#identify columns that you want to save in final output _ LONG FORMAT
names(Combined.FFP.NEE)
subset.Combined.FFP.NEE.long <- select(Combined.FFP.NEE,
                                 Year, DoY, Hour,
                                 Rg, Tair, Tsoil, rH, Ustar, Pressure, Precip, WS, WD, VPD,
                                 records.filter, NEE.mean.filter, NEE.outlier.user.flag,
                                 Qh.outlier.user.flag, Qe.outlier.user.flag, footprint.filter,
                                 L, zm, zm.L, h.PBL, FFPMax, FFP_80, filtered.FFP.80,
                                 Ustar_userUstar_Thres, Ustar_U05_Thres, Ustar_U50_Thres, Ustar_U95_Thres,
                                 NEE, NEE_userUstar_f, NEE_userUstar_fqc,
                                 NEE_U05_f, NEE_U05_fqc, NEE_U50_f, NEE_U50_fqc,  NEE_U95_f, NEE_U95_fqc,
                                 NEE.user.gC.m2.hh, NEE.u05.gC.m2.hh, NEE.u50.gC.m2.hh, NEE.u95.gC.m2.hh,
                                 Reco_userUstar, Reco_U05, Reco_U50, Reco_U95,
                                 Reco.user.gC.m2.hh, Reco.u05.gC.m2.hh, Reco.u50.gC.m2.hh, Reco.u95.gC.m2.hh,
                                 GPP_userUstar_f, GPP_userUstar_fqc,
                                 GPP_U05_f, GPP_U05_fqc, GPP_U50_f, GPP_U50_fqc,  GPP_U95_f, GPP_U95_fqc,
                                 GPP.user.gC.m2.hh, GPP.u05.gC.m2.hh, GPP.u50.gC.m2.hh, GPP.u95.gC.m2.hh,
                                 Qh, Qh.user.gf, Qh.u05.gf, Qh.u50.gf, Qh.u95.gf,
                                 Qe, Qe.user.gf, Qe.u05.gf, Qe.u50.gf, Qe.u95.gf,
                                 Qnet, Qg.gf,
                                 closure.ratio.final.user, closure.ratio.final.u05,
                                 closure.ratio.final.u50, closure.ratio.final.u95,
                                 Bowen.user, Bowen.u05, Bowen.u50, Bowen.u95,
                                 PET.PT,
                                 ET.user.gf, ET.u05.gf, ET.u50.gf, ET.u95.gf,
                                 WUE.user.qc, WUE.u05.qc, WUE.u50.qc, WUE.u95.qc)

#save file
final.file.name <- paste(site.name, measurement.year, "QA.QC.GAPFILLED.EC.Fluxes.LONG.csv", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
write.csv(subset.Combined.FFP.NEE.long , file=File.name.path)
#
#identify columns that you want to save in final output _ SHORTER FILE
subset.Combined.FFP.NEE.short <- select(Combined.FFP.NEE,
                                       Year, DoY, Hour,
                                       Rg, Tair, Tsoil, rH, Ustar, Pressure, Precip, WS, WD, VPD,
                                       records.filter, NEE.mean.filter, NEE.outlier.user.flag,
                                       Qh.outlier.user.flag, Qe.outlier.user.flag, footprint.filter,
                                       L, zm, zm.L, h.PBL, FFPMax, FFP_80, filtered.FFP.80,
                                       Ustar_userUstar_Thres, Ustar_U50_Thres,
                                       NEE, NEE_userUstar_f, NEE_userUstar_fqc,
                                       NEE_U50_f, NEE_U50_fqc,
                                       NEE.user.gC.m2.hh, NEE.u50.gC.m2.hh,
                                       Reco_userUstar, Reco_U50,
                                       Reco.user.gC.m2.hh, Reco.u50.gC.m2.hh,
                                       GPP_userUstar_f, GPP_userUstar_fqc,
                                       GPP_U50_f, GPP_U50_fqc,
                                       GPP.user.gC.m2.hh, GPP.u50.gC.m2.hh,
                                       Qh, Qh.user.gf, Qh.u50.gf,
                                       Qe, Qe.user.gf, Qe.u50.gf,
                                       Qnet, Qg.gf,
                                       closure.ratio.final.user,
                                       closure.ratio.final.u50,
                                       Bowen.user, Bowen.u50,
                                       PET.PT,
                                       ET.user.gf, ET.u50.gf,
                                       WUE.user.qc, WUE.u50.qc)
#save file
final.file.name <- paste(site.name, measurement.year, "QA.QC.GAPFILLED.EC.Fluxes.SHORT.csv", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
write.csv(subset.Combined.FFP.NEE.short , file=File.name.path)




#### VISULALIZE FINAL DATA ###
##############################
#Cross check subset data
plot(ET.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.user.gf timeseries", col="blue")
plot(ET.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.u05.gf timeseries", col="blue")
plot(ET.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.u50.gf timeseries", col="blue")
plot(ET.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.u95.gf timeseries", col="blue")
plot(PET.PT ~ DoY, data=subset.Combined.FFP.NEE.long , main="PET.PT timeseries", col="darkblue")
plot(GPP_userUstar_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.user timeseries in umol", col="forestgreen")
plot(GPP.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.user.gC.m2.hh timeseries", col="forestgreen")
plot(GPP_U05_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.U05 timeseries in umol", col="forestgreen")
plot(GPP.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.u05.gC.m2.hh timeseries", col="forestgreen")
plot(GPP_U50_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.U50 timeseries in umol", col="forestgreen")
plot(GPP.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.u50.gC.m2.hh timeseries", col="forestgreen")
plot(GPP_U95_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.U95 timeseries in umol", col="forestgreen")
plot(GPP.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.u95.gC.m2.hh timeseries", col="forestgreen")
plot(Reco_userUstar ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.user timeseries in umol", col="brown")
plot(Reco.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.user.gC.mm2, hh timeseries", col="brown")
plot(Reco_U05 ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u05 timeseries in umol", col="brown")
plot(Reco.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u05.gC.mm2, hh timeseries", col="brown")
plot(Reco_U50 ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u50 timeseries in umol", col="brown")
plot(Reco.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u50.gC.mm2, hh timeseries", col="brown")
plot(Reco_U95 ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u95 timeseries in umol", col="brown")
plot(Reco.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u95.gC.mm2, hh timeseries", col="brown")
plot(NEE_userUstar_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.user timeseries in umol", col="green")
plot(NEE.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.user.gC.m2.hh timeseries", col="green")
plot(NEE_U05_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u05 timeseries in umol", col="green")
plot(NEE.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u05.gC.m2.hh timeseries", col="green")
plot(NEE_U50_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u50 timeseries in umol", col="green")
plot(NEE.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u50.gC.m2.hh timeseries", col="green")
plot(NEE_U95_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u95 timeseries in umol", col="green")
plot(NEE.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u95.gC.m2.hh timeseries", col="green")
plot(GPP.user.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="user GPP and Reco in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.user.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.user.gC.m2.hh, na.rm=TRUE)))
points(Reco.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(GPP.user.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
                          main="user GPP, Reco and NEE in gC/m2/hh, neg=sink",
                          sub="forestgreen=GPP, green=NEE, brown=Reco",
                          ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.user.gC.m2.hh, na.rm=TRUE),
                                 max(subset.Combined.FFP.NEE.long$Reco.user.gC.m2.hh, na.rm=TRUE)))
points(Reco.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
points(-1*NEE.user.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green")
plot(GPP.user.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="user GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.user.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.user.gC.m2.hh, na.rm=TRUE)), type="l")
points(Reco.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown", type="l")
points(-1*NEE.user.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green", type="l")
plot(GPP.u05.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u05 GPP and Reco in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u05.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u05.gC.m2.hh, na.rm=TRUE)))
points(Reco.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(GPP.u05.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u05 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u05.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u05.gC.m2.hh, na.rm=TRUE)))
points(Reco.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
points(-1*NEE.u05.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green")
plot(GPP.u05.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u05 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u05.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u05.gC.m2.hh, na.rm=TRUE)), type="l")
points(Reco.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown", type="l")
points(-1*NEE.u05.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green", type="l")
plot(GPP.u50.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u50 GPP and Reco in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u50.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u50.gC.m2.hh, na.rm=TRUE)))
points(Reco.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(GPP.u50.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u50 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u50.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u50.gC.m2.hh, na.rm=TRUE)))
points(Reco.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
points(-1*NEE.u50.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green")
plot(GPP.u50.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u50 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u50.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u50.gC.m2.hh, na.rm=TRUE)), type="l")
points(Reco.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown", type="l")
points(-1*NEE.u50.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green", type="l")
plot(GPP.u95.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u95 GPP and Reco in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u95.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u95.gC.m2.hh, na.rm=TRUE)))
points(Reco.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(GPP.u95.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u95 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u95.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u95.gC.m2.hh, na.rm=TRUE)))
points(Reco.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
points(-1*NEE.u95.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green")
plot(GPP.u95.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u95 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u95.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u95.gC.m2.hh, na.rm=TRUE)), type="l")
points(Reco.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown", type="l")
points(-1*NEE.u95.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green", type="l")
plot(WUE.user.qc ~ DoY, data=subset.Combined.FFP.NEE.long , main="WUE.user timeseries", col="turquoise")
plot(WUE.u05.qc ~ DoY, data=subset.Combined.FFP.NEE.long , main="WUE.u05 timeseries", col="turquoise")
plot(WUE.u50.qc ~ DoY, data=subset.Combined.FFP.NEE.long , main="WUE.u50 timeseries", col="turquoise")
plot(WUE.u95.qc ~ DoY, data=subset.Combined.FFP.NEE.long , main="WUE.u95 timeseries", col="turquoise")
plot(Qe.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qe.user timeseries", col="blue")
plot(Qe.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qe.u05 timeseries", col="blue")
plot(Qe.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qe.u50 timeseries", col="blue")
plot(Qe.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qe.u95 timeseries", col="blue")
plot(Qh.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh.user timeseries", col="orange")
plot(Qh.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh.u05 timeseries", col="orange")
plot(Qh.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh.u50 timeseries", col="orange")
plot(Qh.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh.u95 timeseries", col="orange")
plot(Qe.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh and Qe user timeseries", col="blue")
points(Qh.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
plot(Qe.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh and Qe u05 timeseries", col="blue")
points(Qh.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
plot(Qe.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh and Qe u50 timeseries", col="blue")
points(Qh.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
plot(Qe.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long, main="Qh and Qe u95 timeseries", col="blue")
points(Qh.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
plot(Qnet ~ DoY, data=subset.Combined.FFP.NEE.long , main="EN fluxes (user) timeseries",
     col="goldenrod", sub="Qnet=gold, Qe=blue, Qh=orange, Qg=brown")
points(Qe.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="blue")
points(Qh.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
points(Qg.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(Qnet ~ DoY, data=subset.Combined.FFP.NEE.long , main="EN fluxes (u05) timeseries",
     col="goldenrod", sub="Qnet=gold, Qe=blue, Qh=orange, Qg=brown")
points(Qe.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="blue")
points(Qh.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
points(Qg.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(Qnet ~ DoY, data=subset.Combined.FFP.NEE.long , main="EN fluxes (u50) timeseries",
     col="goldenrod", sub="Qnet=gold, Qe=blue, Qh=orange, Qg=brown")
points(Qe.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , col="blue")
points(Qh.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
points(Qg.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(Qnet ~ DoY, data=subset.Combined.FFP.NEE.long , main="EN fluxes (u95) timeseries",
     col="goldenrod", sub="Qnet=gold, Qe=blue, Qh=orange, Qg=brown")
points(Qe.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="blue")
points(Qh.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
points(Qg.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(ET.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.gf user timeseries", col="blue4")
plot(ET.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.gf u05 timeseries", col="blue4")
plot(ET.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.gf u50 timeseries", col="blue4")
plot(ET.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.gf u95 timeseries", col="blue4")
#
#
#
#####  save plots to pdf.file ####
final.file.name <- paste(site.name, measurement.year, "Final.Long.data.plots.pdf", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
pdf(file=File.name.path)
plot(ET.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.user.gf timeseries", col="blue")
plot(ET.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.u05.gf timeseries", col="blue")
plot(ET.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.u50.gf timeseries", col="blue")
plot(ET.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.u95.gf timeseries", col="blue")
plot(PET.PT ~ DoY, data=subset.Combined.FFP.NEE.long , main="PET.PT timeseries", col="darkblue")
plot(GPP_userUstar_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.user timeseries in umol", col="forestgreen")
plot(GPP.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.user.gC.m2.hh timeseries", col="forestgreen")
plot(GPP_U05_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.U05 timeseries in umol", col="forestgreen")
plot(GPP.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.u05.gC.m2.hh timeseries", col="forestgreen")
plot(GPP_U50_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.U50 timeseries in umol", col="forestgreen")
plot(GPP.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.u50.gC.m2.hh timeseries", col="forestgreen")
plot(GPP_U95_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.U95 timeseries in umol", col="forestgreen")
plot(GPP.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="GPP.u95.gC.m2.hh timeseries", col="forestgreen")
plot(Reco_userUstar ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.user timeseries in umol", col="brown")
plot(Reco.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.user.gC.mm2, hh timeseries", col="brown")
plot(Reco_U05 ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u05 timeseries in umol", col="brown")
plot(Reco.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u05.gC.mm2, hh timeseries", col="brown")
plot(Reco_U50 ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u50 timeseries in umol", col="brown")
plot(Reco.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u50.gC.mm2, hh timeseries", col="brown")
plot(Reco_U95 ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u95 timeseries in umol", col="brown")
plot(Reco.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="Reco.u95.gC.mm2, hh timeseries", col="brown")
plot(NEE_userUstar_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.user timeseries in umol", col="green")
plot(NEE.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.user.gC.m2.hh timeseries", col="green")
plot(NEE_U05_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u05 timeseries in umol", col="green")
plot(NEE.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u05.gC.m2.hh timeseries", col="green")
plot(NEE_U50_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u50 timeseries in umol", col="green")
plot(NEE.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u50.gC.m2.hh timeseries", col="green")
plot(NEE_U95_f ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u95 timeseries in umol", col="green")
plot(NEE.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long , main="NEE.u95.gC.m2.hh timeseries", col="green")
plot(GPP.user.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="user GPP and Reco in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.user.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.user.gC.m2.hh, na.rm=TRUE)))
points(Reco.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(GPP.user.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="user GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.user.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.user.gC.m2.hh, na.rm=TRUE)))
points(Reco.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
points(-1*NEE.user.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green")
plot(GPP.user.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="user GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.user.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.user.gC.m2.hh, na.rm=TRUE)), type="l")
points(Reco.user.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown", type="l")
points(-1*NEE.user.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green", type="l")
plot(GPP.u05.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u05 GPP and Reco in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u05.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u05.gC.m2.hh, na.rm=TRUE)))
points(Reco.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(GPP.u05.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u05 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u05.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u05.gC.m2.hh, na.rm=TRUE)))
points(Reco.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
points(-1*NEE.u05.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green")
plot(GPP.u05.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u05 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u05.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u05.gC.m2.hh, na.rm=TRUE)), type="l")
points(Reco.u05.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown", type="l")
points(-1*NEE.u05.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green", type="l")
plot(GPP.u50.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u50 GPP and Reco in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u50.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u50.gC.m2.hh, na.rm=TRUE)))
points(Reco.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(GPP.u50.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u50 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u50.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u50.gC.m2.hh, na.rm=TRUE)))
points(Reco.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
points(-1*NEE.u50.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green")
plot(GPP.u50.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u50 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u50.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u50.gC.m2.hh, na.rm=TRUE)), type="l")
points(Reco.u50.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown", type="l")
points(-1*NEE.u50.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green", type="l")
plot(GPP.u95.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u95 GPP and Reco in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u95.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u95.gC.m2.hh, na.rm=TRUE)))
points(Reco.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(GPP.u95.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u95 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u95.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u95.gC.m2.hh, na.rm=TRUE)))
points(Reco.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
points(-1*NEE.u95.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green")
plot(GPP.u95.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long,  col="forestgreen",
     main="u95 GPP, Reco and NEE in gC/m2/hh, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(subset.Combined.FFP.NEE.long$GPP.u95.gC.m2.hh, na.rm=TRUE),
            max(subset.Combined.FFP.NEE.long$Reco.u95.gC.m2.hh, na.rm=TRUE)), type="l")
points(Reco.u95.gC.m2.hh ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown", type="l")
points(-1*NEE.u95.gC.m2.hh*-1 ~ DoY, data=subset.Combined.FFP.NEE.long, col="green", type="l")
plot(WUE.user.qc ~ DoY, data=subset.Combined.FFP.NEE.long , main="WUE.user timeseries", col="turquoise")
plot(WUE.u05.qc ~ DoY, data=subset.Combined.FFP.NEE.long , main="WUE.u05 timeseries", col="turquoise")
plot(WUE.u50.qc ~ DoY, data=subset.Combined.FFP.NEE.long , main="WUE.u50 timeseries", col="turquoise")
plot(WUE.u95.qc ~ DoY, data=subset.Combined.FFP.NEE.long , main="WUE.u95 timeseries", col="turquoise")
plot(Qe.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qe.user timeseries", col="blue")
plot(Qe.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qe.u05 timeseries", col="blue")
plot(Qe.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qe.u50 timeseries", col="blue")
plot(Qe.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qe.u95 timeseries", col="blue")
plot(Qh.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh.user timeseries", col="orange")
plot(Qh.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh.u05 timeseries", col="orange")
plot(Qh.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh.u50 timeseries", col="orange")
plot(Qh.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh.u95 timeseries", col="orange")
plot(Qe.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh and Qe user timeseries", col="blue")
points(Qh.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
plot(Qe.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh and Qe u05 timeseries", col="blue")
points(Qh.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
plot(Qe.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="Qh and Qe u50 timeseries", col="blue")
points(Qh.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
plot(Qe.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long, main="Qh and Qe u95 timeseries", col="blue")
points(Qh.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
plot(Qnet ~ DoY, data=subset.Combined.FFP.NEE.long , main="EN fluxes (user) timeseries",
     col="goldenrod", sub="Qnet=gold, Qe=blue, Qh=orange, Qg=brown")
points(Qe.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="blue")
points(Qh.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
points(Qg.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(Qnet ~ DoY, data=subset.Combined.FFP.NEE.long , main="EN fluxes (u05) timeseries",
     col="goldenrod", sub="Qnet=gold, Qe=blue, Qh=orange, Qg=brown")
points(Qe.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="blue")
points(Qh.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
points(Qg.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(Qnet ~ DoY, data=subset.Combined.FFP.NEE.long , main="EN fluxes (u50) timeseries",
     col="goldenrod", sub="Qnet=gold, Qe=blue, Qh=orange, Qg=brown")
points(Qe.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , col="blue")
points(Qh.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
points(Qg.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(Qnet ~ DoY, data=subset.Combined.FFP.NEE.long , main="EN fluxes (u95) timeseries",
     col="goldenrod", sub="Qnet=gold, Qe=blue, Qh=orange, Qg=brown")
points(Qe.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="blue")
points(Qh.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="orange")
points(Qg.gf ~ DoY, data=subset.Combined.FFP.NEE.long, col="brown")
plot(ET.user.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.gf user timeseries", col="blue4")
plot(ET.u05.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.gf u05 timeseries", col="blue4")
plot(ET.u50.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.gf u50 timeseries", col="blue4")
plot(ET.u95.gf ~ DoY, data=subset.Combined.FFP.NEE.long , main="ET.gf u95 timeseries", col="blue4")
dev.off()




#### DAILY SUMS & MEANS OF FLUXES per u-star Thr filters ###############
################################################################
#select columns from Combined.FFP.NEE that will be in the Daily.sums data table
Daily.sums.variables <- c("DoY", "Hour",
                          "NEE.user.gC.m2.hh", "GPP.user.gC.m2.hh", "Reco.user.gC.m2.hh", "ET.user.gf",
                          "NEE.u05.gC.m2.hh", "GPP.05.gC.m2.hh", "Reco.u05.gC.m2.hh", "ET.u05.gf",
                          "NEE.u50.gC.m2.hh", "GPP.50.gC.m2.hh", "Reco.u50.gC.m2.hh", "ET.u50.gf",
                          "NEE.u95.gC.m2.hh", "GPP.95.gC.m2.hh", "Reco.u95.gC.m2.hh", "ET.u95.gf")
#create subset of Combined.FFP.NEE columns which will sum up into a new Data table
Variables.to.sum <- Combined.FFP.NEE[Daily.sums.variables]
# new data table
Daily.sums <- Variables.to.sum %>% group_by(DoY) %>% summarise_all(funs(sum))
#rename columns
old=c("Hour",
      "NEE.user.gC.m2.hh", "GPP.user.gC.m2.hh", "Reco.user.gC.m2.hh", "ET.user.gf",
      "NEE.u05.gC.m2.hh", "GPP.05.gC.m2.hh", "Reco.u05.gC.m2.hh", "ET.u05.gf",
      "NEE.u50.gC.m2.hh", "GPP.50.gC.m2.hh", "Reco.u50.gC.m2.hh", "ET.u50.gf",
      "NEE.u95.gC.m2.hh", "GPP.95.gC.m2.hh", "Reco.u95.gC.m2.hh", "ET.u95.gf")
new=c("HH.sum.day",
      "NEE.user.gC.m2", "GPP.user.gC.m2", "Reco.user.gC.m2", "ET.user.mm.m2",
      "NEE.u05.gC.m2", "GPP.u05.gC.m2", "Reco.u05.gC.m2", "ET.u05.mm.m2",
      "NEE.u50.gC.m2", "GPP.u50.gC.m2", "Reco.u50.gC.m2", "ET.u50.mm.m2",
      "NEE.u95.gC.m2", "GPP.u95.gC.m2", "Reco.u95.gC.m2", "ET.u95.mm.m2")
setnames(Daily.sums, old, new)
#drop the hours sum column (i.e. second col) that has a duplicate of observation ID number
Daily.sums <- Daily.sums[ -c(2) ]
#add the year column to the front
Daily.sums$Year <- measurement.year
Col.num.Year <- which( colnames(Daily.sums)=="Year" )
Daily.sums <- Daily.sums[colnames(Daily.sums)[c(Col.num.Year,1:17)]]
#
#
#select columns from Combined.FFP.NEE that will be in the Daily.means data table
Daily.means.variables <- c("DoY", "Hour",
                           "Tair", "Tsoil", "rH","VPD", "Qnet", "Qg.gf",
                           "Qe.user.gf", "Qe.u05.gf", "Qe.u50.gf", "Qe.u95.gf",
                           "Qh.user.gf", "Qh.u05.gf", "Qh.u50.gf", "Qh.u95.gf")
#create subset of Combined.FFP.NEE columns which will sum up into a new Data table
Variables.to.average <- Combined.FFP.NEE[Daily.means.variables]
# new data table
Daily.means <- Variables.to.average %>% group_by(DoY) %>% summarise_all(funs(mean))
#drop the hours sum column (i.e. second col) that has a duplicate of observation ID number
Daily.means <- Daily.means[ -c(2) ]
#
#Combine the two data sets (sums and means) into a single dataset, and clean up columns
Daily.Fluxes <-merge(x=Daily.sums,y=Daily.means, all.x = TRUE)
#
#
#sum up for the season, cross-check with above sums
sum(Daily.Fluxes$GPP.user.gC.m2, na.rm=TRUE)
sum(Daily.Fluxes$GPP.u05.gC.m2, na.rm=TRUE)
sum(Daily.Fluxes$GPP.u50.gC.m2, na.rm=TRUE)
sum(Daily.Fluxes$GPP.u95.gC.m2, na.rm=TRUE)
#
sum(Daily.Fluxes$Reco.user.gC.m2, na.rm=TRUE)
sum(Daily.Fluxes$Reco.u05.gC.m2, na.rm=TRUE)
sum(Daily.Fluxes$Reco.u50.gC.m2, na.rm=TRUE)
sum(Daily.Fluxes$Reco.u95.gC.m2, na.rm=TRUE)
#
sum(Daily.Fluxes$NEE.user.gC.m2, na.rm=TRUE)
sum(Daily.Fluxes$NEE.u05.gC.m2, na.rm=TRUE)
sum(Daily.Fluxes$NEE.u50.gC.m2, na.rm=TRUE)
sum(Daily.Fluxes$NEE.u95.gC.m2, na.rm=TRUE)
#
#plot daily sums of C fluxes
#User U star
plot(GPP.user.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes (u user) in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.user.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.user.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2), type="l")
points(Reco.user.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown", type="l")
points(-1*NEE.user.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green" , type="l")
abline(h=0, col="gray")
plot(GPP.user.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes u-user in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.user.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.user.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2))
points(Reco.user.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown")
points(-1*NEE.user.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green")
abline(h=0, col="gray")
#
#plot daily ET sums
plot(ET.user.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET fluxes - user U in mm/m2/day", mar=c(5,4,4,2))
plot(ET.user.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET fluxes userU in mm/m2/day", mar=c(5,4,4,2), type="l")
#WUE plot
plot(GPP.user.gC.m2~ET.user.mm.m2, data=Daily.Fluxes,  col="cornflowerblue",
     main="user u:GPP vs ET ~ WUE", mar=c(5,4,4,2))
#
#U05 star
plot(GPP.u05.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes (u05) in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u05.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u05.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2), type="l")
points(Reco.u05.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown", type="l")
points(-1*NEE.u05.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green" , type="l")
abline(h=0, col="gray")
plot(GPP.u05.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes u-u05 in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u05.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u05.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2))
points(Reco.u05.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown")
points(-1*NEE.u05.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green")
abline(h=0, col="gray")
#
#plot daily ET sums
plot(ET.u05.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET fluxes - u05 in mm/m2/day", mar=c(5,4,4,2))
plot(ET.u05.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET - u05 fluxes in mm/m2/day", mar=c(5,4,4,2), type="l")
#WUE plot
plot(GPP.u05.gC.m2~ET.u05.mm.m2, data=Daily.Fluxes,  col="cornflowerblue",
     main="u05 u:GPP vs ET ~ WUE", mar=c(5,4,4,2))
#
#
#u50 star
plot(GPP.u50.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes (u50) in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u50.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u50.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2), type="l")
points(Reco.u50.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown", type="l")
points(-1*NEE.u50.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green" , type="l")
abline(h=0, col="gray")
plot(GPP.u50.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes u-u50 in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u50.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u50.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2))
points(Reco.u50.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown")
points(-1*NEE.u50.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green")
abline(h=0, col="gray")
#
#plot daily ET sums
plot(ET.u50.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET fluxes - u50 in mm/m2/day", mar=c(5,4,4,2))
plot(ET.u50.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET -u50 fluxes in mm/m2/day", mar=c(5,4,4,2), type="l")
#WUE plot
plot(GPP.u50.gC.m2~ET.u50.mm.m2, data=Daily.Fluxes,  col="cornflowerblue",
     main="u50 u:GPP vs ET ~ WUE", mar=c(5,4,4,2))
#
#u95 star
plot(GPP.u95.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes (u95) in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u95.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u95.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2), type="l")
points(Reco.u95.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown", type="l")
points(-1*NEE.u95.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green" , type="l")
abline(h=0, col="gray")
plot(GPP.u95.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes u-u95 in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u95.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u95.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2))
points(Reco.u95.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown")
points(-1*NEE.u95.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green")
abline(h=0, col="gray")
#
#plot daily ET sums
plot(ET.u95.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET fluxes - u95 in mm/m2/day", mar=c(5,4,4,2))
plot(ET.u95.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET - u95 fluxes in mm/m2/day", mar=c(5,4,4,2), type="l")
#WUE plot
plot(GPP.u95.gC.m2~ET.u95.mm.m2, data=Daily.Fluxes,  col="cornflowerblue",
     main="u95 u:GPP vs ET ~ WUE", mar=c(5,4,4,2))
#
#Energy gluxes
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16,
     main="Mean daily EN fluxes (u user) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16)
points(Qe.user.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16)
points(Qh.user.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16)
#
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16,
     main="Mean daily EN fluxes (u05) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16)
points(Qe.u05.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16)
points(Qh.u05.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16)
#
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16,
     main="Mean daily EN fluxes (u50) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16)
points(Qe.u50.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16)
points(Qh.u50.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16)
#
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16,
     main="Mean daily EN fluxes (u95) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16)
points(Qe.u95.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16)
points(Qh.u95.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16)
#
#
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16, type="l",
     main="Mean daily EN fluxes (u user) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16, type="l")
points(Qe.user.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16, type="l")
points(Qh.user.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16, type="l")
#
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16, type="l",
     main="Mean daily EN fluxes (u05) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16, type="l")
points(Qe.u05.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16, type="l")
points(Qh.u05.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16, type="l")
#
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16, type="l",
     main="Mean daily EN fluxes (u50) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16, type="l")
points(Qe.u50.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16, type="l")
points(Qh.u50.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16, type="l")
#
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16, type="l",
     main="Mean daily EN fluxes (u95) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16, type="l")
points(Qe.u95.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16, type="l")
points(Qh.u95.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16, type="l")

#
#####  save plots to pdf.file ####
final.file.name <- paste(site.name, measurement.year, "Daily.Flux.plots.pdf", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
pdf(file=File.name.path)
plot(GPP.user.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes (u user) in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.user.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.user.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2), type="l")
points(Reco.user.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown", type="l")
points(-1*NEE.user.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green" , type="l")
abline(h=0, col="gray")
plot(GPP.user.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes u-user in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.user.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.user.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2))
points(Reco.user.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown")
points(-1*NEE.user.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green")
abline(h=0, col="gray")
plot(ET.user.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET fluxes - user U in mm/m2/day", mar=c(5,4,4,2))
plot(ET.user.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET fluxes userU in mm/m2/day", mar=c(5,4,4,2), type="l")
plot(GPP.user.gC.m2~ET.user.mm.m2, data=Daily.Fluxes,  col="cornflowerblue",
     main="user u:GPP vs ET ~ WUE", mar=c(5,4,4,2))
plot(GPP.u05.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes (u05) in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u05.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u05.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2), type="l")
points(Reco.u05.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown", type="l")
points(-1*NEE.u05.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green" , type="l")
abline(h=0, col="gray")
plot(GPP.u05.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes u-u05 in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u05.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u05.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2))
points(Reco.u05.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown")
points(-1*NEE.u05.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green")
abline(h=0, col="gray")
plot(ET.u05.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET fluxes - u05 in mm/m2/day", mar=c(5,4,4,2))
plot(ET.u05.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET - u05 fluxes in mm/m2/day", mar=c(5,4,4,2), type="l")
plot(GPP.u05.gC.m2~ET.u05.mm.m2, data=Daily.Fluxes,  col="cornflowerblue",
     main="u05 u:GPP vs ET ~ WUE", mar=c(5,4,4,2))
plot(GPP.u50.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes (u50) in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u50.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u50.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2), type="l")
points(Reco.u50.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown", type="l")
points(-1*NEE.u50.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green" , type="l")
abline(h=0, col="gray")
plot(GPP.u50.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes u-u50 in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u50.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u50.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2))
points(Reco.u50.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown")
points(-1*NEE.u50.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green")
abline(h=0, col="gray")
plot(ET.u50.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET fluxes - u50 in mm/m2/day", mar=c(5,4,4,2))
plot(ET.u50.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET -u50 fluxes in mm/m2/day", mar=c(5,4,4,2), type="l")
plot(GPP.u50.gC.m2~ET.u50.mm.m2, data=Daily.Fluxes,  col="cornflowerblue",
     main="u50 u:GPP vs ET ~ WUE", mar=c(5,4,4,2))
plot(GPP.u95.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes (u95) in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u95.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u95.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2), type="l")
points(Reco.u95.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown", type="l")
points(-1*NEE.u95.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green" , type="l")
abline(h=0, col="gray")
plot(GPP.u95.gC.m2*-1 ~ DoY, data=Daily.Fluxes,  col="forestgreen",
     main="Daily summed C fluxes u-u95 in gC/m2/day, neg=sink",
     sub="forestgreen=GPP, green=NEE, brown=Reco",
     ylim=c(-1*max(Daily.Fluxes$GPP.u95.gC.m2, na.rm=TRUE),
            max(Daily.Fluxes$Reco.u95.gC.m2, na.rm=TRUE)),
     mar=c(5,4,4,2))
points(Reco.u95.gC.m2 ~ DoY, data=Daily.Fluxes, col="brown")
points(-1*NEE.u95.gC.m2*-1 ~ DoY, data=Daily.Fluxes, col="green")
abline(h=0, col="gray")
plot(ET.u95.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET fluxes - u95 in mm/m2/day", mar=c(5,4,4,2))
plot(ET.u95.mm.m2 ~ DoY, data=Daily.Fluxes,  col="blue",
     main="Daily summed ET - u95 fluxes in mm/m2/day", mar=c(5,4,4,2), type="l")
plot(GPP.u95.gC.m2~ET.u95.mm.m2, data=Daily.Fluxes,  col="cornflowerblue",
     main="u95 u:GPP vs ET ~ WUE", mar=c(5,4,4,2))
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16,
     main="Mean daily EN fluxes (u user) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16)
points(Qe.user.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16)
points(Qh.user.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16)
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16,
     main="Mean daily EN fluxes (u05) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16)
points(Qe.u05.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16)
points(Qh.u05.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16)
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16,
     main="Mean daily EN fluxes (u50) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16)
points(Qe.u50.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16)
points(Qh.u50.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16)
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16,
     main="Mean daily EN fluxes (u95) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16)
points(Qe.u95.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16)
points(Qh.u95.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16)
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16, type="l",
     main="Mean daily EN fluxes (u user) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16, type="l")
points(Qe.user.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16, type="l")
points(Qh.user.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16, type="l")
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16, type="l",
     main="Mean daily EN fluxes (u05) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16, type="l")
points(Qe.u05.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16, type="l")
points(Qh.u05.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16, type="l")
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16, type="l",
     main="Mean daily EN fluxes (u50) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16, type="l")
points(Qe.u50.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16, type="l")
points(Qh.u50.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16, type="l")
plot(Qnet ~ DoY, data=Daily.Fluxes,  col="goldenrod",  pch=16, type="l",
     main="Mean daily EN fluxes (u95) in W/m2",
     sub="Qnet=yellow, Qg=brown, Qe=blue, Qh=orange",
     mar=c(5,4,4,2))
points(Qg.gf ~ DoY, data=Daily.Fluxes, col="brown", pch=16, type="l")
points(Qe.u95.gf ~ DoY, data=Daily.Fluxes, col="blue", pch=16, type="l")
points(Qh.u95.gf ~ DoY, data=Daily.Fluxes, col="darkorange2", pch=16, type="l")
dev.off()
#
#save dataframe to file called Daily.Fluxes.csv
final.file.name <- paste(site.name, measurement.year, "Daily.Fluxes.csv", sep=".")
File.name.path <- paste(Output.directory, final.file.name, sep="/")
write.csv(Daily.Fluxes , file=File.name.path)





##########################################################################################
##################################### END OF SCRIPT ######################################
##########################################################################################


#########################################################################################
###########################   REFERENCE LIST  ###########################################
#########################################################################################
# 1. Aubinet et al (2012) Eddy Covariance: A Practical Guide to Measurement and Data
#        Analysis.  Springer Atmospheric Sciences. book
# 2. [2018]:
#        http://bioma.jrc.ec.europa.eu/components/componentstools/evapotranspiration/help/Priestley-Taylor.html
# 3. Kljun et al (2004) A simple parameterisation for flux footprint predictions.
#        Boundary-Layer Meteorology, 112(3), pp.503-523.
# 4. Kljun et al (2015) A simple two-dimensional parameterisation for Flux Footprint
#        Prediction (FFP). Geoscientific Model Development, 8(11), pp.3695-3713.
#        NOTE: code adapted from "http://footprint.kljun.net/download.php" [November 2018]
# 5. Wutzler et al (2018) Basic and extensible post-processing of eddy covariance flux data
#        with REddyProc. Biogeosciences Discussions. 15, 5015-5030
# 6. Abbasi et al (2017). Effects of atmospheric stability conditions on heat fluxes from
#         small water surfaces in (semi-) arid regions. Hydrological sciences journal, 62(9),
#         pp.1422-1439.
#
#
#
#
#
#
#
#
########################################################################################


#########################################################################################
########################    SCRIPT MODIFICATION  HISTORY     ############################
#########################################################################################
#Please enter the date the script was modified, by whom (include contact info if possible)
#and a brief outline of what was modified in script above on that date.
#
#2018.11.29 - cross checked script and reworked Energy filter and closure (Adam/Myroslava)
#
#2018.11.30 - added PET calculation by Priestly-Taylor method (Adam/Myroslava)
#
#2018.12.02 - cleaned-up script, added final plots, added output into a different directory
#             from working directory (Myroslava)
#
#2019.01.14 - issues with merging files before ReddyProc runs. Reran script line by line and
#             fixed the following bugs: 1) lines 343, added "zm" column to table, so that
#             can filter by zm in lines 350, also commented out line 352, now redundant. 2)
#             Line 507 "merge" function, the x and y input tables were reversed, now fixed.
#             Line 720, directory link in read.csv was missing, now updated and fixed. 4) STILL
#             Outstanding issues with column names for the "select" function on line 1080. Q.net.Qf,
#             Qe.Qf, Qh.Qf are missing, fixed Qnet and Qe by replacing .Qf ending with .QaQc as in
#             original Input file from Adam, but Qh still missing; seems Input file has two Qe columns
#             which may be mislabelled, so one of those is likely for Qh. Adam to check.
#
#2019.01.16 - Added in a save function for the footprint.
#           - Added in a filtering step where fluxes are remved if the are outside of 1 daily standard
#             deviation away from the daily mean.
#
#2019.01.20 - Added new lines (188, 204, and 261-326). This bit of code filters out NEE for outliers
#             based on mean and 3.5* sd of neighbouring 5 half-hours (either to right or left).
#             Should this be also applied to H and LE? Right now code does that. So check then if
#             lines 916+ and 941+ filters are no longer needed.
#
#2019.01.21 - Added lines 334 to 396 - the user-defined-limits filter section, for filtering out
#             outliers in NEE, Qe and Qh flux values. Note this requires user interaction with the
#             script. Updated some instructions in the intro section, which still needs edits.
#             updated lines 940 and 1057 with new variable names (".ratio.initial" and ".ratio.final")
#             added lines 951-959 and 1072-1078 to have Total.initial.closure calculated in Watts,
#             and Total.final.closure.Watts, as per Adam's request. Also updated lines 1288 to 1303,
#             with updated variable names and additional variables of interest which will be included
#             in the final output file: "Gapfilled.EC.Fluxes.hh.csv". To cross check with Adam. Saved
#             script as EC.data.processing_v20190121.R
#
#2019.01.22 - Clearned up script, improved comments/intro, added code to save some tables as txt files
#             and some figures. Added code for two additional break points, in the footpring sections
#             modified variable names in the final output file to make them more clear: use subscript
#             .gf to indicate gap-filled value, .qc - quality checked value, flag - to indicate any
#             filters used to filter data 0/1; renamed in the final output files (LONG and SHORT)
#             Ustar_U50_Thres to Ustar.threshold, NEE_U50_gf to NEE.gf, NEE_U50_fqc = NEE.qc.flag,
#             GPP_U50_gf to GPP, GPP_U50_fqc to GPP.qc.flag, and Reco_U50 to Reco; removed redundant
#             code from old lines 689-736 (mean.daily filters); updated na.string code on imported
#             Input.data to read "NA" as "-9999" and removed the "-1000" filter in the footpring
#             filtering section (redundant and also did not work); added "zm" as a variable in the
#             FFP.variables (old line 499); Upddated Qe/Qh.fu to Qe.Qh.ufiltered; also updated
#             energy balance calculations to use .footfilter values instead of .filter only values, also
#             in input to ReddyProc (now takes in NEE.footfiltered vs NEE.filtered); in estimates of
#             annual GPP total, the mean was multiplied through for 365.25 days, which is not valid
#             for shorter timeseries like ours, so added an additional User-defined variable called
#             "measurement.days" where user can specify how many days the data was measured. Added
#             Lit.reference section at end of script.
#
#2019.01.23 - Finalized Intro section at the start of the script, which describes what this script
#             does, needs, outputs, etc. Added two more output files (annual totals txt and Daily.sums)
#             Added section to calculate "annual" totals for NEE, Reco and ET. Also a section to
#             calculate daily sums of C adn water fluxes - these are saved now and also plots from
#             this data. Found error in GPP.gC.m2.hh calculation, GPP_U05 was used instead of U50, now
#             corrected. Added section on gapfilling Qh adn Qe, so two new variables Qh.gf, Qe.gf
#             also added.Added an extra filter where NEE is filtered out for nightime uptake .Rg10.f
#             to the output file, so that can be traced back too, if need be. Updated .SHORT and .LONG
#             output files to have the newly added variables (i.e. .gf Qe adn Qh adn NEE and Reco in
#             gC.m2.hh, Reran updated file - seems to run without issues.
#
#2019.02.04 - Script updated and cleaned-up further: user-defined u*-threshold has been added and
#             calculation, plots and output modified to include user-U derived values; updated package
#             list; added REddyProc estimated U-star outputs for 5% and 95%, so now have all 3 (u05, u50
#             and u95), in addition to user-defined U* option and all associated calculated values and
#             plots and outputs. Updated filter names, to avoid confusion, esp mean and user-defined
#             QA/QC filters at the start; updated file-names that are being saved to be consistent;
#             added extra R-file to save and recall for plotting footprint plots; added daily means
#             of energy plots to daily sums file and associated plot files; updated LONG and SHORT
#             data subsets with newer variables, to include different u*-fitler-derived values; updated
#             instructions/info section; added new file plots and text plots (these save some of the
#             PET/ET regression results and also annual totals of C and ET fluxes for different u*-filter
#             options); changed Qe filter from zero to -100.
#
