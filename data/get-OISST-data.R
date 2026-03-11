

#install.packages(c("ncdf4", "raster"))
library(ncdf4)
library(raster)
library(here)

data_dir <- here()

#Time period to get data 
years <- 2001:2012


#Pull full files for each year.  These are huge files, so I will extract the necessary 
#components from them and then remove the full files from the folder

for (yr in years) {
  
  url <- sprintf(
    "https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.%d.nc",
    yr
  )
  
  download.file(url,
                destfile = paste0("sst_",yr,".nc"),
                mode="wb")
  }



library(ncdf4)
library(terra)


#Pull data from specific areas
#Set bounds, here is GOA 

lon_min <- -150+360
lon_max <- -140+360
lat_min <- 50
lat_max <- 58
# -----------------------


for (yr in years) {
  
  file <- file.path(data_dir,
                    paste0("sst_", yr, ".nc"))
  
  cat("Processing:", yr, "\n")
  
  # Load SST variable
  sst <- rast(file, subds="sst")
  
  # Crop to Gulf of Alaska
  region <- ext(lon_min, lon_max, lat_min, lat_max)
  r_crop <- crop(sst, region)
  
  writeCDF(r_crop,
           filename = file.path(data_dir, paste0("/data/SST data/SST_GOA/sstGOA_", yr, ".nc")),
           varname = "sst",
           overwrite = TRUE)

}



#Set bounds for Bristol Bay now


lon_min <- -165+360
lon_max <- -160+360
lat_min <- 56.6
lat_max <- 58.5



for (yr in years) {
  
  file <- file.path(data_dir,
                    paste0("sst_", yr, ".nc"))
  
  cat("Processing:", yr, "\n")
  
  # Load SST variable
  sst <- rast(file, subds="sst")
  
  # Crop 
  region <- ext(lon_min, lon_max, lat_min, lat_max)
  r_crop <- crop(sst, region)
  
  writeCDF(r_crop,
           filename = file.path(data_dir, paste0("/data/SST data/SST_BristolBay/sstBB_", yr, ".nc")),
           varname = "sst",
           overwrite = TRUE)
  
}


#Ugashik


lon_min <- -157.775+360
lon_max <- -157.725+360
lat_min <- 57.58
lat_max <- 57.6



for (yr in years) {
  
  file <- file.path(data_dir,
                    paste0("sst_", yr, ".nc"))
  
  cat("Processing:", yr, "\n")
  
  # Load SST variable
  sst <- rast(file, subds="sst")
  
  # Crop
  region <- ext(lon_min, lon_max, lat_min, lat_max)
  r_crop <- crop(sst, region)
  
  writeCDF(r_crop,
           filename = file.path(data_dir, paste0("/data/SST data/SST_Ugashik/sstUgashik_", yr, ".nc")),
           varname = "sst",
           overwrite = TRUE)
  
}


#Egegik


lon_min <- -157.6+360
lon_max <- -157.55+360
lat_min <- 58.22
lat_max <- 58.24



for (yr in years) {
  
  file <- file.path(data_dir,
                    paste0("sst_", yr, ".nc"))
  
  cat("Processing:", yr, "\n")
  
  # Load SST variable
  sst <- rast(file, subds="sst")
  
  # Crop
  region <- ext(lon_min, lon_max, lat_min, lat_max)
  r_crop <- crop(sst, region)
  
  writeCDF(r_crop,
           filename = file.path(data_dir, paste0("/data/SST data/SST_Egegik/sstEgegik_", yr, ".nc")),
           varname = "sst",
           overwrite = TRUE)
  
}


#Kvichak


lon_min <- -157.35+360
lon_max <- -157.25+360
lat_min <- 58.7
lat_max <- 58.75



for (yr in years) {
  
  file <- file.path(data_dir,
                    paste0("sst_", yr, ".nc"))
  
  cat("Processing:", yr, "\n")
  
  # Load SST variable
  sst <- rast(file, subds="sst")
  
  # Crop
  region <- ext(lon_min, lon_max, lat_min, lat_max)
  r_crop <- crop(sst, region)
  
  writeCDF(r_crop,
           filename = file.path(data_dir, paste0("/data/SST data/SST_Kvichak/sstKvichak_", yr, ".nc")),
           varname = "sst",
           overwrite = TRUE)
  
}


#Nushagak


lon_min <- -158.7+360
lon_max <- -158.6+360
lat_min <- 58.7
lat_max <- 58.75



for (yr in years) {
  
  file <- file.path(data_dir,
                    paste0("sst_", yr, ".nc"))
  
  cat("Processing:", yr, "\n")
  
  # Load SST variable
  sst <- rast(file, subds="sst")
  
  # Crop 
  region <- ext(lon_min, lon_max, lat_min, lat_max)
  r_crop <- crop(sst, region)
  
  writeCDF(r_crop,
           filename = file.path(data_dir, paste0("/data/SST data/SST_Nushagak/sstNushagak_", yr, ".nc")),
           varname = "sst",
           overwrite = TRUE)
  
}


#Togiak


lon_min <- -160.425+360
lon_max <- -160.35+360
lat_min <- 58.97
lat_max <- 59.01



for (yr in years) {
  
  file <- file.path(data_dir,
                    paste0("sst_", yr, ".nc"))
  
  cat("Processing:", yr, "\n")
  
  # Load SST variable
  sst <- rast(file, subds="sst")
  
  # Crop 
  region <- ext(lon_min, lon_max, lat_min, lat_max)
  r_crop <- terra::crop(sst, region)
  
  writeCDF(r_crop,
           filename = file.path(data_dir, paste0("/data/SST data/SST_Togiak/sstTogiak_", yr, ".nc")),
           varname = "sst",
           overwrite = TRUE)
  
}
