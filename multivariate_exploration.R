##  ------------------------------------------------------------------------------------------------------  ##
                              #  Multivariate Exploration
##  ------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas J Lyon
  ## Script modified from Manly & Navarro's "Multivariate Statistical Methods: A Primer" 4th Ed

# Purpose ###
  ## Explore and practice various multivariate analysis and visualization techniques

# Set Working Directory
setwd("~/Documents/School/_Presentations/Iowa State University/2018_11_R Seminar Guest Lecture/Multivariate.Guest.Lecture.WD")

# Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------  ##
                  # Data Prep ####
##  ----------------------------------------------------------  ##
# Read in example data
flr <- read.csv("./flr-wide.csv")

# Check what is in the dataframe
colnames(flr)
  ## Composite.Variable = year + management pasted together
  ## Adaptive.Mgmt = essentially 'treatment' of the site where the flowers were counted
  ## Year = SE
  ## Site = 3-letter abbreviation for each site (site is a replicate for this study)
 ## ...
  ## all-lowercase columns = species of flowering plant with number of flowers at each site in the cells
 ## ...
  ## Abundance = number individuals at that site
  ## Species.Density = number of species at that site
  ## Diversity = Shannon-Weiner diversity index for that row

# You may want a community matrix for some of the stuff we'll do as we go
flr.rsp <- flr[,-c(1:4, (ncol(flr)-2):ncol(flr))]

# Check to see if you were successful (no column names with capitalized letters)
colnames(flr.rsp)

# And some functions only play nice with things that count as matrices, so let's get one of those
flr.mat <- as.matrix(flr.rsp)

# To review, we have:
  ## A traditional community dataframe
class(flr)

  ## Another without non-data columns
ncol(flr) - ncol(flr.rsp)

  ## And a matrix of community information
class(flr.mat)

##  ----------------------------------------------------------  ##
       # Principal Components Analysis ####
##  ----------------------------------------------------------  ##










