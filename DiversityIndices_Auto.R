##### BMI Diversity Indices ######
## Author: Weston Slaughter
## 7/24/2020
## Prepared for Chloe Tremper, WQ Lab Manager
## Sierra Streams Institute

# Hello!

# Below is an automated function to compute four different 
# diversity indices for our BMI data

# This script is made to run with the absolute minimum user input
# or effort. Areas of the script YOU must make any changes to 
# have this symbol:

#*****#

# To the right of the line of code. There where be a description of
# anything YOU need to do above that line, always. I will comment
# throughout this script also to describe what's happening line by
# line, most of which is just FYI stuff-

# I will explicitly say if YOU need to do something

# Otherwise, press Ctrl + Enter on each line to run
# that line of the script. If you don't see the #*****# symbol
# to the right of a line, Ctl+Enter on!

##### Start & Setup #####

# Here, you need to rewrite the line below (line 30) 
# to the filepath on YOUR machine where your Excel (.csv) 
# files are. Make sure your excel files are in .csv format.

setwd("C:/Users/Wes/Documents/SSI/Climate Data Project") #*****#

# Install dplyr and magrittr
if(!require(plyr)){install.packages("plyr")}
if(!require(magrittr)){install.packages("magrittr")}
if(!require(vegan)){install.packages("vegan")}

### WAIT as these packages load onto your computer ###

# Load the Packages into your library

      # basic use packages
library(plyr)
library(magrittr)
      # this is to bring in shannon diversity index f(X)
library(vegan)

# read in BMI data- change the part in green to the name of whatever your file is
bmiData <- read.csv("BMI_DC.csv") #*****#


# ---- conditional -----
# It is important to note- the 'Count' variable must be named 
# exactly that, 'Count', and the site variable as well, must be named 'Site' exactly
# If this is not the case, either change names in Excel or use the following f(x):

# below, where it says YOURVARIABLE, input your count name inside 
# of the quotes. Do the same with YOURSITE.

# to activate the functions, highlight them and then
# press Ctrl+Shift+C   which will change their color to white and green,
# once you have made the changes to YOURVARIABLE or YOURSITE, CTL+ENTER to run!


        # bmiData <- rename(bmiData,
        #         c('YOURVARIABLE' = 'Count')
        #       )
        # bmiData <- rename(bmiData,
        #                   c('YOURSITE' = 'Site')
        # )


##### MAIN FUNCTION: Diveristy and Abundance Indices #####
#  Automatic Abundance & Diversity Statistics (aaDS)

# Create Diversity Stats functions
# Math for raw abundance
abund <- function(x) {
  sum(x>0)
}
# Menhinick Diversity metric
menhinick <- function(x) {
  sum(x>0)/sqrt(sum(x))
}
# Margalef Diversity Metric
Marge <- function(x) {
  (sum(x>0)-1)/log(sum(x))
}
# Shannon index we source from the 'vegan' package (pre-built)


# Below is the function to get automated Abundance 
# & Diversity Statistics (aaDS) from an inputted dataframe


aaDS <- function(z){
  # first, make a dataframe of just Site and Count from whatever 
  # dataframe you've input
  
  x <- z[,c(match("Site",colnames(z)),
       match("Count",colnames(z)))]
  
  # Then, this big chunk just makes a new dataframe, runningg the Site/Count
  # data through each of our diversity metrics
  
  df <-
    
  data.frame(
    # Abundance
    Abundance = ddply(x,~Site,function(y) {
      data.frame(Stat = abund(y[,-1]))
    }),
    # Menhinick
    Menhinick = ddply(x,~Site,function(y) {
      data.frame(Stat = menhinick(y[,-1]))
    }),
    # Margalef
    Margalef =  ddply(x,~Site,function(y) {
      data.frame(Stat=Marge(y[,-1]))
    }),
    # Shannon 
    Shannon =  ddply(x,~Site,function(y) {
      data.frame(Stat=diversity(y[,-1], index="shannon"))
    })
  )
  
# Take only the columns we want, the first column and all the
# even columns
  
   cbind("Site" = df[,'Margalef.Site'],df[,!c(TRUE,FALSE)]) %>%
      
# rename generic 'fn' colnames to stat function names
     
     dplyr::rename(
       Abundance =  Abundance.Stat,
       Menhinick = Menhinick.Stat,
       Margalef = Margalef.Stat,
       Shannon = Shannon.Stat
     )
}

# Run all that code over your data, save it to a new dataframe
BMI <- aaDS(bmiData)

# write it to a CSV, which should now be in your directory,
# same as you set at the beginning of this script :)
write.csv(BMI,"diversityIndices_BMI.csv")
