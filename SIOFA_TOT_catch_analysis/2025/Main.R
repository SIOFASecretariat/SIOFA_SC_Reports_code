

#-------------------------------------------------------------------------------
## Main R script, which runs the analysis 
## Jules Selles + Marco Milardi
## Ascripts adapted from CCAMLR trend analysis https://github.com/CCAMLR-Science/Trend_Analysis
## Update : November 2024
#-------------------------------------------------------------------------------

setwd("D:/SIOFA/Data/SIOFA_2025_Toothfish_catch_esitmate")   #### !!!check that this points to the parent folder!!!

# Remove SIOFA .xlsx catch/effort and biological data from ./Data/ folder
rm(list=ls())

# Load required package, and install it if not ---------------------------------
list.of.packages <- c('ggplot2','MAPPOEPA', 'CCAMLRGIS','terra','geosphere','rnaturalearth','ggtext','oceanmap',# plot and spatial object
                      'dplyr','rotations','plyr', # data manipulation
                      'readxl', #read xlsx
                      'lubridate', # manipulate date 
                      'BERT', # ccamlr trend analysis
                      'lm.beta',#regression
                      'DiagrammeR' #diagram 
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, type = 'binary')
lapply(list.of.packages, require, character.only = TRUE)


#theme ggplot 
source('Functions/ggplot2/SimpleTheme_ggplot2.R')
source("Functions/PlotVar.R")

# Install BERT
# devtools::install_github("ccamlr/BERT", build_vignettes = TRUE)
## if errors show up try reinstalling BERT, but only ONCE

#BERT modified functions 
source('Functions/CPUE_seabed_rev.R')
source("Functions/est_fish_weight_rev.R")
source("Functions/assign_areas_rev.R")
source("Functions/extract_catch_data_tag_est_rev.R")
source("Functions/SIOFA_Season.R")

#Other functions
source('Functions/Utils/cut_borders.R')


#Timestamp to be added to file names
Time=Sys.time() 
Time=format(Time,"%d-%b-%Y")

#Get fishable areas ------------------------------------------------------------
update.fishableArea=F

if(update.fishableArea==T){
  source('Fishable_area/RefArea_Shapefiles.R')
  source('Fishable_area/FishableArea.R')
}
fishable_area = read.csv('Data/FishableArea2023.csv')

#Set main parameters -----------------------------------------------------------

#Season of estimation
Est_Season = 2023 #need to change this every year, point to last year of data
Min_Season = Est_Season-4 #this doesn't need to be changed

#List ref area
RefArea=c("HIMI") #"CI"
RefArea.selected=c('HIMI')

#Set biomass and CV for Reference Areas
HIMI_biomass_est=23485 # to update! Current WG FSA IMAF 2024 value/CCAMLR BUlletin 2024
HIMI_CV_biomass_est=0.0435 # to update! Current WG FSA IMAF 2024 value/CCAMLR BUlletin 2024

# CI_biomass_est=11000 #to update ! 
# CI_CV_biomass_est=0.0581 #to update!

#set cpue calculation - if 'dist' kg/km line 
# if 'hooks' then kg/1000hooks
CPUE_mod = 'dist' #'dist' #'hooks'
if (CPUE_mod=='hook'){ # do not change the CPUE_mod here!
  HIMI_cpue_est= 0.214 # to update! Value calculated through the CCAMLR_CPUE routine in \Scripts  
  HIMI_CV_cpue_est=0.1
  #CI_cpue_est=0.245
  #CI_CV_cpue_est=0 #0.1
} else{
  HIMI_cpue_est= 149.86 # to update! Current WG-SAM-2024/12 value
  HIMI_CV_cpue_est=0.1
  #CI_cpue_est=175
  #CI_CV_cpue_est=0#0.1
}

# Threshold recapture for  chapman estimator 
n_min_chapman=3

#Harvest rate used for the trend decision 
HarvestRateTrend = 0.04

#List MU in the proper order
Mu=c("DC", "SIR")

#Set number of bootstrap iterations
n_boot=10000

#Chose whether to output data extracts ("Y") or not ("N")
Output="Y"

#1. Load data ------------------------------------------------------------------
source("01_LoadData.R")

#2. Estimate Biomass -----------------------------------------------------------
source("02_EstimateBiomass.R")

#3. Trend analysis -------------------------------------------------------------
source("03_AnalyseTrend.R")

#4. Inspect tagging data -------------------------------------------------------
source("04_TaggingData.R")
