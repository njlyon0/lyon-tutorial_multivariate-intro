##  ------------------------------------------------------------------------------------------------------  ##
                              #  Multivariate Exploration
##  ------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas J Lyon
  ## Script modified from Manly & Navarro's "Multivariate Statistical Methods: A Primer" 4th Ed

# Purpose ####
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

# I do not find these dataframes to have intuitive names though so let's change them a bit
lichen.spp <- varespec
lichen.chem <- varechem

# Paste them together in case any function requires all the stuff to be in one dataframe
lichen.data <- cbind(lichen.chem, lichen.spp)

# Make some dummy group variables in case you want to practice analysis
two.groups <- c(rep("A", nrow(lichen.data)/2), rep("B", nrow(lichen.data)/2))
four.groups <- c(rep("A", nrow(lichen.data)/4), rep("B", nrow(lichen.data)/4),
                 rep("C", nrow(lichen.data)/4), rep("D", nrow(lichen.data)/4))

# Paste those groups into the larger dataframe
lichen.data$Group.Var.1 <- two.groups
lichen.data$Group.Var.2 <- four.groups

##  ----------------------------------------------------------  ##
    # Resampling/Permutation Procedures ####
##  ----------------------------------------------------------  ##
#install.packages("RRPP")
library(RRPP)

# Model fitting works the same way it does in univariate frequentist statistics
# This test is usually called a permutational multivariate ANOVA (perMANOVA)
mod1 <- lm.rrpp(lichen.spp ~ Group.Var.2, data = lichen.data, iter = 9999)
  ## In this case we are testing how the whole community responds to our pretend grouping variable
  ## "iter" means the number of permutations (aka iterations) used to generate the distributions

# Check significance using the anova function
anova(mod1, effect.type = "F")
  ## looks like the communities do happen to be different among the levels of our fake variable

# The test can also handle univariate analysis (perANOVA in that case)
mod2 <- lm.rrpp(Callvulg ~ Group.Var.2, data = lichen.data, iter = 9999)
anova(mod2, effect.type = "F")

# You likely want to also have pairwise comparisons to figure out *which* group is different from the others
summary(pairwise(mod1, fit.null = NULL, groups = lichen.data$Group.Var.2))
summary(pairwise(mod2, fit.null = NULL, groups = lichen.data$Group.Var.2))

# Once you've got that information you could go on to pick your favorite multiple comparison method
  ## Or do some visualization/whatever else you'd want

# A full permutation procedure can be found in vegan
?vegan::adonis

##  ----------------------------------------------------------  ##
      # Multivariate Data Visualization ####
##  ----------------------------------------------------------  ##
# Let's say we want to visualize our data in three dimensions
#install.packages("scatterplot3d")
library(scatterplot3d)

# Make the scatterplot
s3d <- scatterplot3d(lichen.data$Callvulg, lichen.data$Empenigr, lichen.data$Rhodtome,
                     xlab = "Callvulg", ylab = "Empenigr", zlab = "Rhodtome")
  ## I don't think this is terribly useful here, but you can do it and it might make sense for other datasets

# Scary face visualization method ("Chernoff Faces")
# install.packages("TeachingDemos")
library(TeachingDemos)
lichen.spp.mat <- data.matrix(lichen.spp)
faces2(lichen.spp.mat, labels = lichen.data$Group.Var.2, scale ="center")

# The features are: 1 Width of center 2 Top vs. Bottom width (height of split) 3 Height of Face 4 Width of top half of face
#                   5 Width of bottom half of face 6 Length of Nose 7 Height of Mouth 8 Curvature of Mouth (abs < 9)
#                   9 Width of Mouth 10 Height of Eyes 11 Distance between Eyes (.5-.9) 12 Angle of Eyes/Eyebrows
#                  13 Circle/Ellipse of Eyes 14 Size of Eyes 15 Position Left/Right of Eyeballs/Eyebrows
#                  16 Height of Eyebrows 17 Angle of Eyebrows 18 Width of Eyebrows

# I find these *super* creepy, so let's move on

# I prefer starplots
#install.packages("graphics")
library(graphics)

stars(lichen.spp, labels = lichen.data$Group.Var.2, key.loc = c(12, 0))

# Let's take a look at a subset of the community (so that the legend is more interpretable)
stars(lichen.spp[,-c(10:ncol(lichen.spp))], labels = lichen.data$Group.Var.2, key.loc = c(12, 2))
  ## Better right?

# Still works best with fewer variables though (more variables are tough to parse out of the legend)

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
  ## Still, better to take a worst-case and advance with it for when your data are not perfect, right?

# Time to actually do PCA
  ## Compute the principal components
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
var.test(PC1 ~ two.groups, data = lichen.pca.data)

# Classic PCA plots might look something like:
par(mfrow = c(1, 3))
plot(x = lichen.pca$x[,1], y = lichen.pca$x[,2], pch = 20, xlab = "PC1", ylab = "PC2")
plot(x = lichen.pca$x[,1], y = lichen.pca$x[,3], pch = 20, xlab = "PC1", ylab = "PC3")
plot(x = lichen.pca$x[,2], y = lichen.pca$x[,3], pch = 20, xlab = "PC2", ylab = "PC3")
par(mfrow = c(1, 1))

# Save a quick file of this out in case you want it later
jpeg(file = "./Graphs/practice_pca.jpg")
par(mfrow = c(1, 3))
plot(x = lichen.pca$x[,1], y = lichen.pca$x[,2], pch = 20, xlab = "PC1", ylab = "PC2")
plot(x = lichen.pca$x[,1], y = lichen.pca$x[,3], pch = 20, xlab = "PC1", ylab = "PC3")
plot(x = lichen.pca$x[,2], y = lichen.pca$x[,3], pch = 20, xlab = "PC2", ylab = "PC3")
par(mfrow = c(1, 1))
dev.off()

##  ----------------------------------------------------------  ##
     # Nonmetric Multidimensional Scaling ####
##  ----------------------------------------------------------  ##
# First you want to make your data-only dataframe into a matrix
lichen.mat <- as.matrix(lichen.spp)
str(lichen.mat)

# Then compute distances/dissimilarities among observations (i.e. community differences)
  ## Check your options
?vegan::vegdist

# Actually compute them
lichen.dst <- vegan::vegdist(lichen.mat, method = "kulczynski")

# You can see we have a huge matrix of pairwise dissimilarities now
lichen.dst

# Now you can actually do the multidimensional scaling part of NMS
lichen.mds <- metaMDS(lichen.dst, distance = "kulczynski", engine = "monoMDS",
                    autotransform = F, expand = F, k = 2, try = 100)
  ## Many of these are defaults that I left in place to be explicit about what is being done

# Check the stress that it reaches at
lichen.mds$stress
  ## ranges from 0 to 1 when engine = "monoMDS" (is a % with engine = "isoMDS")
  ## Not terrible, but also not great

# I wrote a function that conveniently (I think) does the whole aesthetic bit of creating NMS ordinations
nms.4.ord <- function(mod, groupcol, g1, g2, g3, g4, lntp1 = 1, lntp2 = 1,
                      lntp3 = 1, lntp4 = 1, legcont, legpos = "topright") {
  ## mod = object returned by metaMDS
  ## groupcol = group column in the dataframe that contains those (not the community matrix)
  ## g1 - g4 = how each group appears in your dataframe (in quotes)
  ## lntp1 - 4 = what sort of line each ellipse will be made of (accepts integers between 1 and 6 for diff lines)
  ## legcont = single object for what you want the content of the legend to be
  ## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
  
  # Create plot
  plot(mod, display = 'sites', choice = c(1, 2), type = 'none', xlab = "", ylab = "")
  
  # Set colors (easier for you to modify if we set this now and call these objects later)
  col1 <- "#253494" # shades of bluish-green
  col2 <- "#1d91c0" 
  col3 <- "#41b6c4" 
  col4 <- "#c7e9b4"
  
  # Add points for each group with a different color per group
  points(mod$points[groupcol == g1, 1], mod$points[groupcol == g1, 2], pch = 21, bg = col1)
  points(mod$points[groupcol == g2, 1], mod$points[groupcol == g2, 2], pch = 22, bg = col2)
  points(mod$points[groupcol == g3, 1], mod$points[groupcol == g3, 2], pch = 23, bg = col3)
  points(mod$points[groupcol == g4, 1], mod$points[groupcol == g4, 2], pch = 24, bg = col4)
  ## As of right now the colors are colorblind safe and each group is also given its own shape
  
  # Get a single vector of your manually set line types for the ellipses
  lntps <- c(lntp1, lntp2, lntp3, lntp4)
  
  # Ordinate SD ellipses around the centroid
  library(vegan) # need this package for the following function
  ordiellipse(mod, groupcol, col = c(g1 = col1, g2 = col2, g3 = col3, g4 = col4),
              display = "sites", kind = "sd", lwd = 2, lty = lntps, label = F)
  
  # Add legend
  legend(legpos, legend = legcont, bty = "n", 
         pch = c(21, 22, 23, 24), cex = 1.15, 
         pt.bg = c(col1, col2, col3, col4))
  
}

# As far as I know, there's not another easier way of doing that (sorry)
nms.4.ord(mod = lichen.mds, groupcol = lichen.data$Group.Var.2,
          g1 = "A", g2 = "B", g3 = "C", g4 = "D",
          legcont = c("A", "B", "C", "D"))

# Let's re-run those pairwise comparisons from way long ago to see how well signficance lines up with ocular test
summary(pairwise(mod1, fit.null = NULL, groups = lichen.data$Group.Var.2))
  ## D is different from the other three groups and no other group is different from one another
  ## (C is almost different from B and A depending on your critical point)

# Save it!
jpeg(file = "./Graphs/practice_nms.jpg")
nms.4.ord(mod = lichen.mds, groupcol = lichen.data$Group.Var.2,
          g1 = "A", g2 = "B", g3 = "C", g4 = "D",
          legcont = c("A", "B", "C", "D"))
dev.off()


# END ####


