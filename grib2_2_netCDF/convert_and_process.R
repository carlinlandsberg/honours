# 1.convert_and_process.R

library(ncdf4)
library(data.table)
library(tidyverse)
library(plyr)
library(lubridate)
library(stringr)
library(doMC); doMC::registerDoMC(cores = 4)


# First things first ------------------------------------------------------

# First, I get the data using wget:
# "wget -np -nd -nc -r -l3 -A=multi_1.glo*201[6-8]??.grb2 ftp://polar.ncep.noaa.gov/pub/history/waves/multi_1/"

# I used bash and NCL's ncl_convert2nc to convert all the grib2 to netCDF
# first, before continuing in R, below. Here's the bash commend that applies
# ncl_convert2nc to each file in the listing:
# "find ~/spatial/WAVEWATCH_III/*.grb2 -maxdepth 1 -exec ncl_convert2nc {} -o /Users/ajsmit/spatial/WAVEWATCH_III/netCDF \;"


# Set-up ------------------------------------------------------------------

ncDir <- "/Users/ajsmit/spatial/WAVEWATCH_III/netCDF"
outDir <- "/Users/ajsmit/spatial/WAVEWATCH_III/csv"
pixel <- c(-34.5, -34.5, 18.5, 18.5)
region <- c(-45.00, -20.00, 6.25, 45.00)


# # Function for some swell components ------------------------------------

# function to extract the dims and data from NCEP netCDFs
# works on:
# 1. Primary wave mean period (tp)
# 2. Significant height of combined wind waves and swell (hs)
# 3. Primary wave direction (dp)
read_nc <- function(ncFile, varID = varID, outDir = outDir) {
  coords <- pixel
  nc <- nc_open(ncFile)
  fileLen <- nchar(basename(ncFile))
  fNameStem <-
    substr(basename(ncFile), 1, fileLen - 9)
  fDate <- substr(basename(ncFile), fileLen -8, fileLen - 3)
  latIdx <- which(nc$dim$lat_0$vals == coords[1])
  lonIdx <- which(nc$dim$lon_0$vals == coords[3])
  timeStep <- nc$dim$forecast_time0$vals
  timeIdx <- which(nc$dim$forecast_time0$vals >= 0)
  timeVec <- as.Date(paste0(fDate, "01"), format = "%Y%m%d")
  timeSeries <-  timeVec + hours(timeStep)
  dat <- ncvar_get(nc,
                   varid = varID,
                   start = c(lonIdx[1], latIdx[1], timeIdx[1]),
                   count = c(length(lonIdx), length(latIdx), length(timeIdx))) %>%
    round(2)
  dat.df <- data.frame(value = dat,
                       time = timeSeries)
  nc_close(nc)
  fwrite(dat.df,
         file = paste0(outDir, "/", fNameStem, strtDate, "-", endDate, ".csv"),
         append = TRUE, col.names = FALSE)
  rm(dat); rm(dat.df)
}

# ncFile <- ncList[1] # for testing


# Extract!! ---------------------------------------------------------------

# first these three:
# 1. Primary wave mean period (tp)
# 2. Significant height of combined wind waves and swell (hs)
# 3. Primary wave direction (dp)

# ideally finding the start and end dates of the time series need to be added
# to the function above, but I'll do it later

# PERPW_P0_L1_GLL0[lon_0,lat_0,forecast_time0] # Primary wave mean period
ncList <- list.files(path = ncDir, pattern = "multi_1.glo_30m.tp.*.nc", full.names = TRUE, include.dirs = TRUE)
fileLen <- nchar(basename(ncList[1]))
strtDate <- str_sub(basename(ncList[1]), fileLen -8, fileLen - 3)
endDate <- str_sub(basename(ncList[length(ncList)]), fileLen -8, fileLen - 3)
system.time(llply(ncList, read_nc, varID = "PERPW_P0_L1_GLL0", outDir = outDir, .parallel = TRUE))

# HTSGW_P0_L1_GLL0[lon_0,lat_0,forecast_time0] # Significant height of combined wind waves and swell
ncList <- list.files(path = ncDir, pattern = "multi_1.glo_30m.hs.*.nc", full.names = TRUE, include.dirs = TRUE)
fileLen <- nchar(basename(ncList[1]))
strtDate <- str_sub(basename(ncList[1]), fileLen -8, fileLen - 3)
endDate <- str_sub(basename(ncList[length(ncList)]), fileLen -8, fileLen - 3)
system.time(llply(ncList, read_nc, varID = "HTSGW_P0_L1_GLL0", outDir = outDir, .parallel = TRUE))

# DIRPW_P0_L1_GLL0[lon_0,lat_0,forecast_time0] # Primary wave direction
ncList <- list.files(path = ncDir, pattern = "multi_1.glo_30m.dp.*.nc", full.names = TRUE, include.dirs = TRUE)
fileLen <- nchar(basename(ncList[1]))
strtDate <- str_sub(basename(ncList[1]), fileLen -8, fileLen - 3)
endDate <- str_sub(basename(ncList[length(ncList)]), fileLen -8, fileLen - 3)
system.time(llply(ncList, read_nc, varID = "DIRPW_P0_L1_GLL0", outDir = outDir, .parallel = TRUE))


# Wind extraction function ------------------------------------------------

# function to extract the dims and data from NCEP netCDFs
# works on:
# 1. Primary wave mean period (tp)
# 2. Significant height of combined wind waves and swell (hs)
# 3. Primary wave direction (dp)
read_nc <- function(ncFile, outDir = outDir) {
  coords <- pixel
  nc <- nc_open(ncFile)
  fileLen <- nchar(basename(ncFile))
  fNameStem <-
    substr(basename(ncFile), 1, fileLen - 9)
  fDate <- substr(basename(ncFile), fileLen -8, fileLen - 3)
  latIdx <- which(nc$dim$lat_0$vals == coords[1])
  lonIdx <- which(nc$dim$lon_0$vals == coords[3])
  timeStep <- nc$dim$forecast_time0$vals
  timeIdx <- which(nc$dim$forecast_time0$vals >= 0)
  timeVec <- as.Date(paste0(fDate, "01"), format = "%Y%m%d")
  timeSeries <-  timeVec + hours(timeStep)
  u_dat <- ncvar_get(nc,
                     varid = "UGRD_P0_L1_GLL0",
                     start = c(lonIdx[1], latIdx[1], timeIdx[1]),
                     count = c(length(lonIdx), length(latIdx), length(timeIdx))) %>%
    round(2)
  v_dat <- ncvar_get(nc,
                     varid = "VGRD_P0_L1_GLL0",
                     start = c(lonIdx[1], latIdx[1], timeIdx[1]),
                     count = c(length(lonIdx), length(latIdx), length(timeIdx))) %>%
    round(2)
  dat.df <- data.frame(u_value = u_dat,
                       v_value = v_dat,
                       time = timeSeries)
  nc_close(nc)
  fwrite(dat.df,
         file = paste0(outDir, "/", fNameStem, strtDate, "-", endDate, ".csv"),
         append = TRUE, col.names = FALSE)
  rm(dat); rm(dat.df)
}


# Extract!! ---------------------------------------------------------------

# UGRD_P0_L1_GLL0[lon_0,lat_0,forecast_time0] # U-component of wind
# VGRD_P0_L1_GLL0[lon_0,lat_0,forecast_time0] # V-component of wind
ncList <- list.files(path = ncDir, pattern = "multi_1.glo_30m.wind.*.nc", full.names = TRUE, include.dirs = TRUE)
fileLen <- nchar(basename(ncList[1]))
strtDate <- str_sub(basename(ncList[1]), fileLen -8, fileLen - 3)
endDate <- str_sub(basename(ncList[length(ncList)]), fileLen -8, fileLen - 3)
system.time(llply(ncList, read_nc, outDir = outDir, .parallel = TRUE))

# VAR_10_0_7_P0_L241_GLL0[lon_0,lat_0,lv_OSEQD0,forecast_time0]
# has 4 dimension, unlike the previous data sets...
# data only from 2017...
# use updated function
ncList <- list.files(path = ncDir, pattern = "multi_1.glo_30m.pdir.*.nc", full.names = TRUE, include.dirs = TRUE)
fileLen <- nchar(basename(ncList[1]))
strtDate <- str_sub(basename(ncList[1]), fileLen -8, fileLen - 3)
endDate <- str_sub(basename(ncList[length(ncList)]), fileLen -8, fileLen - 3)
system.time(llply(ncList, read_nc, varID = "DIRPW_P0_L1_GLL0", outDir = outDir, .parallel = TRUE))


# Unsused... --------------------------------------------------------------

# SWELL_P0_L241_GLL0[lon_0,lat_0,lv_OSEQD0,forecast_time0] # Significant height of swell waves
# has 4 dimension, unlike the previous data sets...
# data only from 2017...
# use updated function
ncList <- list.files(path = ncDir, pattern = "multi_1.glo_30m.phs.*.nc", full.names = TRUE, include.dirs = TRUE)

# SWPER_P0_L241_GLL0[lon_0,lat_0,lv_OSEQD0,forecast_time0] # Mean period of swell waves
# has 4 dimension, unlike the previous data sets...
# data only from 2017...
# use updated function
ncList <- list.files(path = ncDir, pattern = "multi_1.glo_30m.ptp.*.nc", full.names = TRUE, include.dirs = TRUE)


# Merge them... -----------------------------------------------------------

hs <- fread(paste0(outDir, "/multi_1.glo_30m.hs.201201-201809.csv"),
            colClasses = list("numeric" = 1, "Date" = 2),
            col.names = c("hs", "t"))
tp <- fread(paste0(outDir, "/multi_1.glo_30m.tp.201201-201809.csv"),
            colClasses = list("numeric" = 1, "Date" = 2),
            col.names = c("tp", "t"))
dp <- fread(paste0(outDir, "/multi_1.glo_30m.dp.201201-201809.csv"),
            colClasses = list("numeric" = 1, "Date" = 2),
            col.names = c("dp", "t"))
wi <- fread(paste0(outDir, "/multi_1.glo_30m.wind.201201-201809.csv"),
            colClasses = list("numeric" = 1:2, "Date" = 3),
            col.names = c("u", "v", "t"))
ww <- tibble(time = ymd_hms(hs$t),
             hs = hs$hs,
             tp = tp$tp,
             dp = dp$dp,
             u = wi$u,
             v = wi$v)

fwrite(ww, file = "NCEP/NCEP_dat.csv")


# Read the file -----------------------------------------------------------

dat <- read.csv("NCEP/NCEP_dat.csv")
