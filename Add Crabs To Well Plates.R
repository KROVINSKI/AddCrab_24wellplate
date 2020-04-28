# This code creates a file assigning new crabs to well plates as the start of an experiment.
# It also generates image files of new well plate assignments

#load libraries
library("ggplot2", lib.loc="~/Library/R/3.3/library")
library("RColorBrewer", lib.loc="~/Library/R/3.3/library")

#this set the path where the output files will be created
# Set working directory and outfile name
setwd("C:/Users/Danielle.Perez/Documents/2020 Crabs/Data management/Add crabs/HC5")
outFileName <- "addCrab_CH10.2020.03.11.csv"

#date crabs added to wells & Chamber location
dateAdded <- "2020_03_11"
chamber <- "CH10"

# Well plate  ID numbers
# this is a vector for well numbers, can be create any legit means, (e.g. comma seperated list)
(wellIDnum <- c(300:304))
# Number of wells per plate. Option are 6, 12, or 24
nWellPerPlate <- 24
# Wellplate ID. This creates a vector of well plates that will be used
(plateID <- paste("W", nWellPerPlate, "_", wellIDnum, sep = ""))
# total number of wells. the length() function returns the number of elements in a vector
(nWells <- nWellPerPlate * length(plateID))
#Plate vector for dataframe. This code uses the rep() function to create vectors associated with each well
plate <- rep(plateID, each = nWellPerPlate)
wellNumber <- NA
if(nWellPerPlate == 24){
  wellLetter <- rep(c("A","B","C","D"), each = 6)
  wellRow <- rep(c(4:1), each = 6)
  wellNumber <- rep(c(1:6), 4)
  well <- paste(wellLetter, wellNumber, sep = "")
  crabDF <- data.frame(plate, wellLetter, wellRow, wellNumber, well)
}
if(nWellPerPlate == 6){
  wellNumber <- c(1:6)
  crabDF <- data.frame(plate, wellNumber)
}
#View(crabDF)

# plate and well related vectors are added to the data frame that will be output
# ID of the crab groups populating the wells (usually mom)
(crabGroupID <- c("HC5"))
# number of crab groups
(nGroups <- length(crabGroupID))

#TOTAL number of tracking chips, might set this as average of one per well plate, but could be more
nTrackChip <- 0
if (nWellPerPlate==24){
    (nTrackChip <- 12)
  # tracking chip IDs in format "TC_#"
  (trackChipID <- paste("TC_", c(1:nTrackChip), sep = ""))
}

# Number of crabs of each group to place in wells 
# Use nEqualGroupsize if dividing all the wells equally amoung groups, or use custome sizes
(nWellsForCrab <- nWells - nTrackChip)
(nEqualGroupsize <- floor(nWellsForCrab / nGroups))
(nCrabPerGroup <- rep(nEqualGroupsize, nGroups))
# if using user specified number of crabs per group. These correspond to the groups in crabGroupID vector
nCrabPerGroup <- c(108) 

#Create empty crab vector that will ultimately list all crabs, tracking chips and blanks
crab <- character()
#create a vector of crabs of the form groupID_#, with unique number (#) for each crab in the group
for(i in 1:length(crabGroupID)){
  crab <- c(crab, paste(crabGroupID[i], c(1:nCrabPerGroup[i]), sep="_"))
}
# add the tracking chips and blanks to the crab vector
crab <- c(crab, trackChipID)
  
#random order of crab vector. The sample() function create an random order of the vector
(crab <- sample(crab))

#Add date for complete crab and tracking chip ID. Format is groupID_#_date or TC_#_date
crabID <- paste(chamber, crab, dateAdded, sep = "_")
#crabSimple is used to set the colors for the well plate images
# essentially, this removes everything after the first "_" so the output is just groupID, "TC" or "blank"
crabSimple <- gsub("_.*", "", crab)
#crabLabel is the value printed inside the wells on the well plate images
crabLabel <- crab
crabLabel[!grepl("TC", crabLabel)] <- gsub("_.*", "", crabLabel[!grepl("TC", crabLabel)])

#create dataframe by adding the crab vectors to the crabDF vector that already contains the wellplate data
crabDF <- cbind(crabDF, crab, crabID, crabSimple, crabLabel)
#convert crabSimple from character vector to factor vector for graphing well plate images
crabDF$crabSimple <- factor(crabDF$crabSimple)
# add a colum with the chamber ID 
crabDF$chamber <- chamber
#create a location ID of the format Chamber_Plate_Well
crabDF$Location <- paste(crabDF$chamber, crabDF$plate, crabDF$well, sep = "_")
# add a column with the date crabs are added
crabDF$date <- dateAdded
#look at the dataframe to make sure is OK
#View(crabDF)

# Write the output csv file in location specied above
write.csv(crabDF, file = outFileName)

########
#Plotting well plate images
#assign plot colors to crabs, tracking chips and blanks from the brewer palette Set1
# the lock the colors so they are constent in all images in this file
# the setNames() function assigns the each of the colours the name of a level of the crabSimple vector 
lev <-  levels(crabDF$crabSimple)
colours <- brewer.pal(length(lev), "Set1")
(nameColours <- setNames(colours, lev))

#function to plot a single plate
# the rings are made by plotting a white point on top of a colored point
# can change the sizes of points to adust appearance or delete white point for solid
platePlot <- function(plateID){
  d <- subset(crabDF, plate == plateID)
  levels(d$crabSimple) <- lev
  p <- ggplot(d, aes(wellNumber, wellRow)) +
    geom_point(aes(colour = crabSimple), size = 12) +
    geom_point(colour = "white", size = 8) +
    scale_color_manual(values = nameColours )+
    labs(title = plateID) +
    theme_bw() + theme(legend.position="none", 
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank()) +
    geom_text(aes(label=crabLabel), size = 3)
  return(p)
}
# Create new folder for well plate images and reset working directory
imageFolder <- paste(gsub(".csv", "", outFileName), "_Plate_Img", sep = "")
dir.create(imageFolder)
setwd(paste(getwd(), imageFolder, sep = "/"))
#loops through all the plates calling the platePlot() function
# print(p) will graph the plot in R studio window
# ggsave will write the plot to file
# the image size is specified, but does not hold when importing to ppt
# could maybe mess with that to get it better
for(i in 1:length(plateID)){
  p <- platePlot(plateID[i])
  print(p)
  ggsave(paste(outFileName, "_plate", plateID[i], ".png", sep = ""), 
         plot = platePlot(plateID[i]), width = 5.2, height = 3.6, units = "in")
}




















