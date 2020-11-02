# Libraries required
#install.packages("devtools")
#devtools::install_github("ropensci/FedData")
library(readr)
library(tidyverse)
library(FedData)
library(raster)
library(sf)
library(spData)
library(leaflet)
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

data(us_states)
NY = us_states[17,]
NLCD=get_nlcd(template = st_as_sf(NY),
              label = "NYS",
              year = 2016,
              dataset='Land_Cover',
              landmass="L48")

# Start plotting
names(Adultdata)[6]="Total_Tested"
Adult_BBdat=Adultdata[,1:7]
Adult_BBdat$Pathogen="Borrelia burgdorferi"
names(Adult_BBdat)[7]="Percent_Positive"
Adult_Apdat=Adultdata[,c(1:6,8)]
Adult_Apdat$Pathogen="Anaplasma phagocytophilum"
names(Adult_Apdat)[7]="Percent_Positive"
Adult_Bmiydat=Adultdata[,c(1:6,10)]
Adult_Bmiydat$Pathogen="Borrelia miyamotoi"
names(Adult_Bmiydat)[7]="Percent_Positive"
Adult_Bmicdat=Adultdata[,c(1:6,9)]
Adult_Bmicdat$Pathogen="Babesia microti"
names(Adult_Bmicdat)[7]="Percent_Positive"
Adultnewdat = rbind(Adult_BBdat,Adult_Apdat)
Adultnewdat = rbind(Adultnewdat,Adult_Bmiydat)
Adultnewdat = rbind(Adultnewdat,Adult_Bmicdat)
Adultnewdat$`Percent Positiveby100`=Adultnewdat$`Percent_Positive`/100
Adultnewdat$Totpos=round(Adultnewdat$`Total_Tested`*Adultnewdat$`Percent Positiveby100`)
Adultnewdat = subset(Adultnewdat,is.na(Adultnewdat$Percent_Positive)==FALSE)

Adulttotpos = Adultnewdat %>%
        group_by(Year,Pathogen) %>%
        summarize(sum(Totpos))
Adulttotpos$`sum(Totpos)` = ifelse(is.na(Adulttotpos$`sum(Totpos)`)==TRUE,0,Adulttotpos$`sum(Totpos)`)
Adulttottested = Adultnewdat %>%
        group_by(Year,Pathogen) %>%
        summarize(sum(Total_Tested))
Adultgroupings=cbind(Adulttottested,Adulttotpos)
Adultgroupings=Adultgroupings[,c(1:3,6)]
Adultgroupings$percpos=(Adultgroupings$`sum(Totpos)`/Adultgroupings$`sum(Total_Tested)`)*100
names(Adultgroupings)[1]="Year"
names(Adultgroupings)[2]="Pathogen"
Adultgroupings$`Life Stage`="Adult"

names(Nymphdata)[6]="Total_Tested"
Nymph_BBdat=Nymphdata[,1:7]
Nymph_BBdat$Pathogen="Borrelia burgdorferi"
names(Nymph_BBdat)[7]="Percent_Positive"
Nymph_Apdat=Nymphdata[,c(1:6,8)]
Nymph_Apdat$Pathogen="Anaplasma phagocytophilum"
names(Nymph_Apdat)[7]="Percent_Positive"
Nymph_Bmiydat=Nymphdata[,c(1:6,10)]
Nymph_Bmiydat$Pathogen="Borrelia miyamotoi"
names(Nymph_Bmiydat)[7]="Percent_Positive"
Nymph_Bmicdat=Nymphdata[,c(1:6,9)]
Nymph_Bmicdat$Pathogen="Babesia microti"
names(Nymph_Bmicdat)[7]="Percent_Positive"
Nymphnewdat = rbind(Nymph_BBdat,Nymph_Apdat)
Nymphnewdat = rbind(Nymphnewdat,Nymph_Bmiydat)
Nymphnewdat = rbind(Nymphnewdat,Nymph_Bmicdat)
Nymphnewdat$`Percent Positiveby100`=Nymphnewdat$`Percent_Positive`/100
Nymphnewdat$Totpos=round(Nymphnewdat$`Total_Tested`*Nymphnewdat$`Percent Positiveby100`)
Nymphnewdat = subset(Nymphnewdat,is.na(Nymphnewdat$Percent_Positive)==FALSE)

Nymphtotpos = Nymphnewdat %>%
        group_by(Year,Pathogen) %>%
        summarize(sum(Totpos))
Nymphtotpos$`sum(Totpos)` = ifelse(is.na(Nymphtotpos$`sum(Totpos)`)==TRUE,0,Nymphtotpos$`sum(Totpos)`)
Nymphtottested = Nymphnewdat %>%
        group_by(Year,Pathogen) %>%
        summarize(sum(Total_Tested))
Nymphgroupings=cbind(Nymphtottested,Nymphtotpos)
Nymphgroupings=Nymphgroupings[,c(1:3,6)]
Nymphgroupings$percpos=(Nymphgroupings$`sum(Totpos)`/Nymphgroupings$`sum(Total_Tested)`)*100
names(Nymphgroupings)[1]="Year"
names(Nymphgroupings)[2]="Pathogen"
Nymphgroupings$`Life Stage`="Nymph"

All_groupings=rbind(Adultgroupings,Nymphgroupings)

ggplot(data = All_groupings,aes(x=Year,y=percpos,color=Pathogen)) +
        geom_line() +
        ggtitle(expression(paste("Time Series Graph of ", italic("Ixodes scapularis")," Infectivity Rates (NYS)",sep=""))) +
        theme_classic() +
        geom_hline(yintercept = 0,linetype='solid',color='black',size=.5,alpha=.7) +
        geom_vline(xintercept = 2008,linetype='solid',color='black',size=.5,alpha=.7) +
        facet_wrap(~`Life Stage`) +
        scale_x_continuous(breaks=c(2008:2019))

#Create tables/line graphs of pathogen prevalence by year by county compared to state average
# That table will then be inserted into leaflet map


# Leaflet of NYS
County_Boundaries=st_transform(County_Boundaries,CRS("+proj=longlat +datum=WGS84"))
leaflet() %>%
        addTiles() %>%
        addPolylines(data=County_Boundaries,
                     color = "black",
                     stroke = TRUE,
                     weight=3) # %>%
        #addCircleMarkers() this is where tables will pop up when clicked, see: https://stackoverflow.com/questions/57446415/table-on-click-in-r-shiny-leaflet-is-not-shown

# Do statistical testing using spearman rank corrs for each county as an observation