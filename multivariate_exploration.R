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
#install.packages("vegan")
library(vegan)

# Get example lichen data from the vegan package
data(varespec); data(varechem)

# Check out the source if you're interested
?varespec

##  ----------------------------------------------------------  ##
       # Principal Components Analysis ####
##  ----------------------------------------------------------  ##










