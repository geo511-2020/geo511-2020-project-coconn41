# Libraries required
library(readr)
library(MODIS)
library(raster)
# Data gathering
## Tick data
adulturl <- "https://health.data.ny.gov/api/views/vzbp-i2d4/rows.csv?accessType=DOWNLOAD&bom=true&query=select+*"
nymphurl <- "https://health.data.ny.gov/api/views/kibp-u2ip/rows.csv?accessType=DOWNLOAD&bom=true&query=select+*"
download.file(adulturl, destfile = "./Adultdata.csv",cacheOK = TRUE) 
download.file(nymphurl,destfile = "./Nymphdata.csv",cacheOK = TRUE)
Nymphdata <- read_csv("Nymphdata.csv")
Adultdata <- read_csv("Adultdata.csv")

## Land use data
LU <- getProduct("MCD12Q1",collection="006",
              tileH=12,
              tileV=4)
#end = "2019.12.31")

LU <- as(LU,Class = "Spatial")
Productsavail <- getProduct()  


