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

# I do not find these dataframes to have valuable names though so let's change them a bit
lichen.spp <- varespec
lichen.chem <- varechem

##  ----------------------------------------------------------  ##
       # Principal Components Analysis ####
##  ----------------------------------------------------------  ##
# PCA works best when the measured variables are highly correlated
  # (It is easier to summarize the data by combining it when they obviously vary together)
  # so check the correlation of our lichen species' cover with one another

# Get the correlation matrix
lichen.cor <- cor(lichen.spp)
  ## Pretty unwieldy, right?
  ## Fortunately, the correlation of A with B is equal to the correlation of B with A, so we can simplify

# Ditch one triangle of the matrix (and the diagonal because A is 100% correlated with itself)
lichen.cor[upper.tri(lichen.cor, diag = T)] <- NA
lichen.cor

# Better, but still a lot of NAs to scroll through, so let's go one step further:
# Get a histogram of the correlations
hist(lichen.cor, main = " ", xlab = "Correlation")
  ## Looks like most of the lichen spp are not very correlated with one another

# Still, better to take a worst-case and advance with it for when your data are not perfect, right?
# Time to actually do PCA

# Compute the principal components
lichen.pca <- prcomp(varespec, scale = T)
  ## This involves eigenvectors and eigenvalues
  ## I suggest taking Dean Adams' spring course in Biostatistics if you really want to get into the math

# Check 'em out
summary(lichen.pca)
  ## Keep an eye on the "Proportion of Variance" and "Cumulative Proportion" rows
  ## Allows you to assess how good your principal components (PCs) are at summarizing your data
  ## In this case: not very good

# Once you've computed them though, you can use PCs in a frequentist statistical framework
lichen.pca.data <- cbind(lichen.chem, lichen.pca$x)

# F Test on a pretend variable (just to demo how you can test it)
var.test(PC1 ~ c(rep("A", nrow(lichen.pca.data)/2), rep("B", nrow(lichen.pca.data)/2)),
         data = lichen.pca.data)

# Classic PCA plots might look something like:
par(mfrow = c(1, 3))
plot(x = lichen.pca$x[,1], y = lichen.pca$x[,2], pch = 20, xlab = "PC1", ylab = "PC2")
plot(x = lichen.pca$x[,1], y = lichen.pca$x[,3], pch = 20, xlab = "PC1", ylab = "PC3")
plot(x = lichen.pca$x[,2], y = lichen.pca$x[,3], pch = 20, xlab = "PC2", ylab = "PC3")
par(mfrow = c(1, 1))










