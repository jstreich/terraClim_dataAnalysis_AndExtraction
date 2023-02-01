###############################################################################
# Get Terra Climate averages across 1958-2018 for each mothn
# Author: Jared Streich
# Created on March 31st 2022
# Version 0.1.0
# email: ju0@ornl.gov if not at ORNL streich.jared@gmail.com
################################################################################

################################################################################
############# List of MOnthly Variables to extract data from ###################
################################################################################
# Get Hari his GWATS data
# aet
# def
# PDSI
# q
# soil
# swe
# vpd

################################################################################
################################# Load Rasters #################################
################################################################################
library(raster)

################################################################################
############################## Set Color Palette ###############################
################################################################################
cols <- colorRampPalette(c("grey30", "dodgerblue3", "goldenrod2"))(100)

################################################################################
############################ Get Species Coordinates ###########################
################################################################################

##### Set dir to file locartions
setwd("~/Desktop/Projects/CBI_General/")

##### Get Poplar Coordinates
poplar.coord <- read.delim("Poplar_1568_coordinates.txt", header = T)

##### Get Switchgrass coordinates
swchgs.coord <- read.delim("switchgrass_geo-coords.txt", header = T)


################################################################################
################# Set Delim to where TerraClim Data is stored ##################
################################################################################

##### Set dir to files
setwd("/Volumes/Lisa/terraClim/tmp_terraclim_years/")

##### List all terraclim files in temporary directory
fls <- list.files(pattern = "*.nc")

##### Check for seven variables and 61 files, should be seven
length(fls)/61

################################################################################
################################ Start Script ##################################
################################################################################

##### Loop through file names and get unique climate label tag
for(i in 1:length(fls)){
  fl.nm <- matrix(unlist(strsplit(fls[i], split = "_")))[2]
  if(i == 1){
    fl.nm.p <- fl.nm
  }
  else{
    fl.nm.p <- c(fl.nm.p, fl.nm)
  }
}

##### Keep non-duplicate names
fl.nm.p <- fl.nm.p[!duplicated(fl.nm.p)]

i <- 2
j <- 1
l <- 1

##### Loop through Climate Variable, Month, then average data across years
for(i in 1:length(fl.nm.p)){ ### Start Loop A; Each Clim Variable
  fl.sublist <- list.files(pattern = fl.nm.p[i]) ### Get each year of specific clim var
  for(j in 1:12){ ### Start Loop B; Loop through each month across years
    for(l in 1:length(fl.sublist)){ ### Start Loop C; read specific month data
      fl.l <- raster(fl.sublist[l], band = j) # Read Raster
      fl.l <- fl.l/61 # Divide raster by parts
      if(l == 1){ ## Start If
        fl.l.p <- fl.l # Create seed layer for raster addition
      } ## End If
      else{ ## Start Else
        fl.l.p <- fl.l.p + fl.l # Add consecutive raster layers
      } ## End Else
    } ### End Loop C
    setwd("/Volumes/Lisa/terraClim/tmp_terraclim_years/write_dir/")
    fl.name <- paste(fl.nm.p[i], j, sep = "_") # Get name of averaged data
    writeRaster(fl.l.p, filename = paste(fl.name, ".tif", sep = ""), format = "GTiff", overwrite = T) # Write averaged data to file
    png(paste(fl.name, "_1958-2018Average.png", sep = ""), width = 2400, height = 1200)
      plot(fl.l.p, col = cols, main = fl.name, xlab = "Longitude", ylab = "Latitude")
    dev.off()
    setwd("/Volumes/Lisa/terraClim/tmp_terraclim_years/")
  } ### End Loop B
} ### End Loop A


################################################################################
############## Extract Climate Data for Poplar and Switchgrass #################
################################################################################

###################################
########### Poplar Trees ##########
###################################

##### Loop through averaged climate variables for Poplar
setwd("/Volumes/Lisa/terraClim/tmp_terraclim_years/write_dir/")
fls.lst <- list.files(pattern = ".tif")
for(i in 1:length(fls.lst)){
  rast.i <- raster(fls.lst[i])
  env.i <- extract(rast.i, cbind(poplar.coord[,3], poplar.coord[,2]))
  if(i == 1){
    env.i.p <- env.i
  }
  else{
    env.i.p <- cbind(env.i.p, env.i)
  }
}

##### Write climate data for Poplar
colnames(env.i.p) <- fls.lst
env.i.p <- cbind(poplar.coord, env.i.p)
rownames(env.i.p) <- poplar.coord[,1]
env.i.p <- env.i.p[,-1]
write.table(env.i.p, file = "poplar_terraclim_AveragedData_2022-04-20.txt", quote = F, col.names = T, row.names = T, sep = " ")


env.i.p <- read.delim("poplar_terraclim_AveragedData_2022-04-20.txt", header = T)


##### Loop through month-year climate variables for Poplar
setwd("/Volumes/Lisa/terraClim/")
fls.lst <- list.files(pattern = ".nc")
i <- 1
j <- 1
for(i in 1:length(fls.lst)){
  for(j in 1:12){
    rast.i <- raster(fls.lst[i], band = j)
    env.i <- extract(rast.i, cbind(poplar.coord[,3], poplar.coord[,2]))
    fl.nm <- paste("TrClm", substr(fls.lst[i], 13, nchar(fls.lst[i])-3), "_", j, sep = "")
    if(j == 1){
      env.i.p <- env.i
      fl.nm.p <- fl.nm
    }
    else{
      env.i.p <- cbind(env.i.p, env.i)
      fl.nm.p <- c(fl.nm.p, fl.nm)
    }
  }
  if(i == 1){
    env.i.p.p <- env.i.p
    fl.nm.p.p <- fl.nm.p
  }
  else{
    env.i.p.p <- cbind(env.i.p.p, env.i.p)
    fl.nm.p.p <- c(fl.nm.p.p, fl.nm.p)
  }
}

dim(env.i.p.p)/732

##### Write climate data for Poplar
colnames(env.i.p.p) <- fl.nm.p.p
env.i.p.p <- cbind(poplar.coord, env.i.p.p)
rownames(env.i.p.p) <- poplar.coord[,1]
env.i.p.p <- env.i.p.p[,-1]
env.i.p.p[1:10,1:10]
write.table(env.i.p.p, "poplar_terraclim_MonthYearData_2022-04-21.txt", quote = F, col.names = T, row.names = T, sep = " ")




#########################################
########## Switchgrass Grasses ##########
#########################################

##### Loop through averaged climate variables for Switchgrass
setwd("/Volumes/Lisa/terraClim/tmp_terraclim_years/write_dir/")
fls.lst <- list.files(pattern = ".tif")
for(i in 1:length(fls.lst)){
  rast.i <- raster(fls.lst[i])
  env.i <- extract(rast.i, cbind(swchgs.coord[,3], swchgs.coord[,2]))
  if(i == 1){
    env.i.p <- env.i
  }
  else{
    env.i.p <- cbind(env.i.p, env.i)
  }
}

env.i.p.check <- env.i.p[is.na(env.i.p[6]), ]
rownames(env.i.p.check)


##### Write climate data for Switchgrass
colnames(env.i.p) <- fls.lst
env.i.p <- cbind(swchgs.coord, env.i.p)
rownames(env.i.p) <- swchgs.coord[,1]
env.i.p <- env.i.p[,-1]
env.i.p[1:100,1:10]
write.table(env.i.p, file = "switchgrass_terraclim_AveragedData_2022-03-31.txt", quote = F, col.names = T, row.names = T, sep = " ")


##### Loop through month-year climate variables for Switchgrass
setwd("/Volumes/Lisa/terraClim/")
fls.lst <- list.files(pattern = ".nc")
i <- 1
j <- 1
for(i in 1:length(fls.lst)){
  for(j in 1:12){
    rast.i <- raster(fls.lst[i], band = j)
    env.i <- extract(rast.i, cbind(swchgs.coord[,3], swchgs.coord[,2]))
    fl.nm <- paste("TrClm", substr(fls.lst[i], 13, nchar(fls.lst[i])-3), "_", j, sep = "")
    if(j == 1){
      env.i.p <- env.i
      fl.nm.p <- fl.nm
    }
    else{
      env.i.p <- cbind(env.i.p, env.i)
      fl.nm.p <- c(fl.nm.p, fl.nm)
    }
  }
  if(i == 1){
    env.i.p.p <- env.i.p
    fl.nm.p.p <- fl.nm.p
  }
  else{
    env.i.p.p <- cbind(env.i.p.p, env.i.p)
    fl.nm.p.p <- c(fl.nm.p.p, fl.nm.p)
  }
}

dim(env.i.p.p)/732

##### Write climate data for Poplar
colnames(env.i.p.p) <- fl.nm.p.p
env.i.p.p <- cbind(swchgs.coord, env.i.p.p)
rownames(env.i.p.p) <- swchgs.coord[,1]
env.i.p.p <- env.i.p.p[,-1]
env.i.p.p[1:10,1:10]
write.table(env.i.p.p, "swichgrass_terraclim_MonthYearData_2022-04-21.txt", quote = F, col.names = T, row.names = T, sep = " ")
env.data <- read.delim("")



################################################################################
########################### Interpolate to 365 Days ############################
################################################################################

# Increments between middle of months
inc <- c(31, 29, 30, 30, 31, 30, 31, 31, 30, 31, 30, 31)


##### Set dir to raster list of monthly data
setwd("/Volumes/Smithers/Rasters/Useful_temporal_Layers/")


##### Get list of files
fls <- list.files(pattern = "*.tif")

##### Loop through file names and get unique climate label tag
##### Keep non-duplicate names
i <- 1
for(i in 1:length(fls)){
  fl.nm <- matrix(unlist(strsplit(fls[i], split = "_")))[1]
  if(i == 1){
    fl.nm.p <- fl.nm
  }
  else{
    fl.nm.p <- c(fl.nm.p, fl.nm)
  }
}

zer_adj <- function(x){
  if(min(x) <= 0){
    correction <- 0 - min(x)
    correction <- correction + 1
    x <- x + correction
    print("zero correction made")
    print(correction)
  }
  else{
    correction <- 0
    print("All Values above zero")
  }
  return(x)
}


##### Loop through and extract 12 months, interpolate to 365
fl.nm.p <- fl.nm.p[!duplicated(fl.nm.p)]
fl.nm.p
j <- 1
i <- 1

for(i in 1:length(fl.nm.p)){
  setwd("/Volumes/Smithers/Rasters/Useful_temporal_Layers/")
  fl.ptrn <- list.files(pattern = fl.nm.p[i])
  fl.ptrn <- list.files(pattern = fl.nm.p[i])  
  out.name <- fl.nm.p[i]
  for(j in 1:length(fl.ptrn)){
    rst.j <- raster(fl.ptrn[j])
    extrd <- extract(rst.j, cbind(swchgs.coord[,3], swchgs.coord[,2]))
    if(j == 1){
      extrd.p <- extrd
    }
    else{
      extrd.p <- cbind(extrd.p, extrd)
    }
  }
  layer <- cbind(extrd.p, extrd.p[,1])
  rownames(layer) <- swchgs.coord[,1]
  dim(layer)
  layer <- layer[complete.cases(layer[,1]), ]
  layer <- layer[complete.cases(layer[,2]), ]
  layer <- layer[complete.cases(layer[,3]), ]
  layer <- layer[complete.cases(layer[,4]), ]
  layer <- layer[complete.cases(layer[,5]), ]
  layer <- layer[complete.cases(layer[,6]), ]
  layer <- layer[complete.cases(layer[,7]), ]
  layer <- layer[complete.cases(layer[,8]), ]
  layer <- layer[complete.cases(layer[,9]), ]
  layer <- layer[complete.cases(layer[,10]), ]
  layer <- layer[complete.cases(layer[,11]), ]
  layer <- layer[complete.cases(layer[,12]), ]
  #layer.ad <- zer_adj(layer)
  ##### Plus Minus days for scan
  plsmin <- 10
  ##### Create Interpolation Loop
  for(l in 1:nrow(layer)){
    x <- as.matrix(layer[l,])
    for(o in 2:13){
      em <- x[o-1] # Earlier Month
      lm <- x[o]   # Later month
      if(o == 2){ # If Jan, get Dec Data
        em.0 <- x[13]
        lm.0 <- x[2]
        mms <- seq(em.0, lm.0, length.out = inc[o-1])
      }
      else{
        mmn <- seq(em, lm, length.out = inc[o-1])
        mms <- c(mms, mmn)
      }
    }
    mms <- c(mms, mms)
    for(m in 1:2){
      for(n in 1:(length(mms)/2)){
        ri <- mean(c(mms[n], mms[n+5], mms[n+10], mms[n+15]))
        if(n == 1){
          ri.p <- ri
        }
        else{
          ri.p <- c(ri.p, ri)
        }
      }
      mms <- c(ri.p, ri.p)
    }	
    layer.n <- mms[16:(380+plsmin)]
    # plot(layer.n, cex = 0.5, pch = 16, col = "Green1")
    if(l == 1){
      layer.p <- layer.n
    }
    else{
      layer.p <- cbind(layer.p, layer.n)
    }
  }
  
  out.file <- t(layer.p)
  out.file <- out.file[,1:365]
  dim(out.file)

  plot(out.file[1,], type = "l", col = "dodgerblue2", lwd = 2)
  colnames(out.file) <- paste("Day_", c(1:365), sep = "")
  row.names(out.file) <- rownames(layer)
  
  out.file[1:5,1:10]
  setwd("/Volumes/Smithers/Rasters/")
  write.table(out.file, file = paste("swchgrs_365days_", out.name, "2022-05-03.txt", sep = ""), sep = " ", quote = F, row.names = T, col.names = T)
}


dim(extrd.p)

##### Prep data to 13 months to properly interpolate December back to January
# layer <- cbind([data 1-12], [data 1])


####################################################################################################
############# Create raw list of monthly phenotype data for day imputation ########################
####################################################################################################
input.points <- input.points[complete.cases(input.points[1,]), ]
ClimData <- input.points
dim(ClimData)

# Start Loop throug set and extract monthly values
for(i in 1:length(txtfiles)){
  # print name of current text file
  print(txtfiles[i])
  
  # Read in individual raster layers
  biolayer <- raster(txtfiles[i])
  
  # Extract BioClim Values per point	
  BioCol <- extract(biolayer, input.points[,2:1])
  
  # Paste New Column on Climate Data to ClimData Variable
  ClimData <- cbind(ClimData, BioCol)
  j <- i+2
  colnames(ClimData)[j] <- txtfiles[i]
  print(dim(ClimData))
  print(i)
  print(head(ClimData))
}# End i loop for all layers

head(ClimData)
min(ClimData[,1])
dim(ClimData)
####################################################################################################
####### Check to see the order of column extraction for human or computer numeric order ############
####################################################################################################


# Check adjustment
layer.ad <- zer_adj(layer)
min(layer.ad)
dim(layer.ad)
##### Plus Minus days for scan
plsmin <- 10

##### Create Interpolation Loop
for(l in 1:nrow(layer)){
  x <- as.matrix(layer[l,])
  for(i in 2:13){
    em <- x[i-1] # Earlier Month
    lm <- x[i]   # Later month
    if(i == 2){ # If Jan, get Dec Data
      em.0 <- x[13]
      lm.0 <- x[2]
      mms <- seq(em.0, lm.0, length.out = inc[i-1])
    }
    else{
      mmn <- seq(em, lm, length.out = inc[i-1])
      mms <- c(mms, mmn)
    }
  }
  mms <- c(mms, mms)
  for(j in 1:2){
    for(i in 1:(length(mms)/2)){
      ri <- mean(c(mms[i], mms[i+5], mms[i+10], mms[i+15]))
      if(i == 1){
        ri.p <- ri
      }
      else{
        ri.p <- c(ri.p, ri)
      }	
    }
    mms <- c(ri.p, ri.p)
  }	
  layer.n <- mms[16:(380+plsmin)]
  # plot(layer.n, cex = 0.5, pch = 16, col = "Green1")
  if(l == 1){
    layer.p <- layer.n
  }
  else{
    layer.p <- cbind(layer.p, layer.n)
  }
}

dim(layer.p)
head(layer.p)
layer.p <- layer.p - min(layer.ad)

# Observe image sorted by middle of year
layer.t <- t(layer.p)
dim(layer.t)
dim(coord)
dim(input.points)
rownames(layer.t) <- rownames(layer.ad)
layer.t <- layer.t[order(layer.t[,183]), ]
# cols <- palette(rainbow(200))
# cols <- cols[(length(cols)-40):1]

# Image of year sorted at middle date
# image(t(layer.t), col = cols)
# abline(v = 0.5, lwd = 2)
# rownames(layer.t) <- rownames(coord)
layer.t <- layer.t[,1:365]

# Create Phenotype data
setwd("~/Desktop/projects/gwats/Brachypodium/")
outfile
write.table(layer.t, file = outfile, sep = "\t", quote = F, row.names = F, col.names = F)

# write keep file
write.table(rownames(layer), file = keepfile, quote = F, col.names = F, row.names = F)

##### Create Plink based pheno files
plink.file <- cbind(rownames(layer.t), rownames(layer.t), rep(0, times = nrow(layer.t)), 
                    rep(0, times = nrow(layer.t)), rep(0, times = nrow(layer.t)), layer.t)

plink.file <- plink.file[,1:370]
dim(plink.file)
colnames(plink.file) <- c("FID", "IID", "MATID", "PATID", "SEX", paste("Day_", c(1:365), sep = "") )
plink.file[1:10,1:10]
write.table(plink.file, plinkfile, quote = F, col.names = T, row.names = F)

