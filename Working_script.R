# Libraries required
library(readr)
library(tidyverse)
library(FedData)
library(raster)
library(sf)
library(MODIS)
library(MODISTools)
library(curl)
# Data gathering
tdir=tempdir()
## Tick data
adulturl <- "https://health.data.ny.gov/api/views/vzbp-i2d4/rows.csv?accessType=DOWNLOAD&bom=true&query=select+*"
nymphurl <- "https://health.data.ny.gov/api/views/kibp-u2ip/rows.csv?accessType=DOWNLOAD&bom=true&query=select+*"
if(file.exists(paste(tdir,"/Adultdata.csv",sep=""))==FALSE){
download.file(adulturl, destfile = file.path(tdir,"/Adultdata.csv"))}
if(file.exists(paste(tdir,"/Nymphdata.csv",sep=""))==FALSE){
download.file(nymphurl,destfile = file.path(tdir,"/Nymphdata.csv"))}
Nymphdata <- read_csv(paste(tdir,"/Nymphdata.csv",sep=""))
Adultdata <- read_csv(paste(tdir,"/Adultdata.csv",sep=""))

#NYS Shapefiles
shpurl="http://gis.ny.gov/gisdata/fileserver/?DSID=927&file=NYS_Civil_Boundaries.shp.zip"
if(file.exists(paste(tdir,"/NYS_Civil_Boundaries_SHP/Counties_Shoreline.shp",sep=""))==FALSE){
download.file(shpurl, destfile = file.path(tdir,"Boundaries.zip"))
unzip(file.path(tdir,"Boundaries.zip"),exdir = tdir)
Cnt=read_sf(paste(tdir,"/NYS_Civil_Boundaries_SHP/Counties_Shoreline.shp",sep=""))}
#list.files(paste(tdir,"/NYS_Civil_Boundaries_SHP",sep=""))
County_Boundaries = Cnt[,c(1,18)]
NYS = st_union(County_Boundaries)
simpSpNYS=st_simplify(NYS,
                      dTolerance=100)
SpNYS=as(NYS,Class="Spatial")




## Land use data

download.file(dataurl,destfile=file.path(tdir,"temp.zip"))
unzip(file.path(tdir,"temp.zip"),exdir = tdir)
#list.files(tdir)
storm_data = read_sf(list.files(tdir,pattern='.shp',full.names = T))

NYSbb=data.frame(lat = c(40,45.2),
                 lon = c(-71,-80))
NYSbb_df <- st_as_sf(NYSbb, 
                    coords = c("lon","lat"), 
                    crs = 4326)
NLCD = get_nlcd(template = NYSbb_df,
        label="NYS",
         dataset = "Land_Cover",
         year = 2016,
         force.redo = T)


if(file.exists("NLCD/NLCD_2016_Land_Cover_L48_20190424.img")==FALSE){
NLCDurl="https://s3-us-west-2.amazonaws.com/mrlc/NLCD_2016_Land_Cover_L48_20190424.zip"
download.file(NLCDurl,destfile = "NLCD.zip",cacheOK=TRUE)}
a=raster("/Users/collinoconnor/Desktop/Rprojects/geo511-2020-project-coconn41/NLCD/NLCD_2016_Land_Cover_L48_20190424.img")
NYS_NLCD=raster::mask(x = a,mask = SpNYS)



NLCD=get_nlcd(template = s,
        label = "NYS",
        year = 2016,
        dataset='Land_Cover',
        force.redo = T)

install.packages("devtools")
devtools::install_github("ropensci/FedData")

a=mt_products()
b=mt_bands('MCD12Q1')

test=runGdal(product = "MCD12Q1",
             extent = County_Boundaries,
             begin = "2013-06-01",
             end = "2013-06-02")


test=mt_subset(df = County_Boundaries,
             product = "MCD12Q1",
             band = "LC_Prop1",
             out_dir=getwd())


LU <- getProduct("MCD12Q1",collection="006",
              tileH=12,
              tileV=4)
#end = "2019.12.31")

LU <- as(LU,Class = "Spatial")
Productsavail <- getProduct()  

getCollection("MCD12Q1")

tfs <- runGdal(product = "MCD12Q1", collection = "006",
               tileH = 12, tileV = 4, 
               begin = "2016001", end = "2017002", 
               SDSstring = "101100")

