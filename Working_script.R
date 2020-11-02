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
Adultnewdat1 = rbind(Adult_BBdat,Adult_Apdat)
Adultnewdat1 = rbind(Adultnewdat1,Adult_Bmiydat)
Adultnewdat1 = rbind(Adultnewdat1,Adult_Bmicdat)
Adultnewdat1$`Percent Positiveby100`=Adultnewdat1$`Percent_Positive`/100
Adultnewdat1$Totpos=round(Adultnewdat1$`Total_Tested`*Adultnewdat1$`Percent Positiveby100`)
Adultnewdat2 = subset(Adultnewdat1,is.na(Adultnewdat1$Percent_Positive)==FALSE)

Adulttotpos = Adultnewdat2 %>%
        group_by(Year,Pathogen) %>%
        summarize(sum(Totpos))
Adulttotpos$`sum(Totpos)` = ifelse(is.na(Adulttotpos$`sum(Totpos)`)==TRUE,0,Adulttotpos$`sum(Totpos)`)
Adulttottested = Adultnewdat2 %>%
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
Nymphnewdat1 = rbind(Nymph_BBdat,Nymph_Apdat)
Nymphnewdat1 = rbind(Nymphnewdat1,Nymph_Bmiydat)
Nymphnewdat1 = rbind(Nymphnewdat1,Nymph_Bmicdat)
Nymphnewdat1$`Percent Positiveby100`=Nymphnewdat1$`Percent_Positive`/100
Nymphnewdat1$Totpos=round(Nymphnewdat1$`Total_Tested`*Nymphnewdat1$`Percent Positiveby100`)
Nymphnewdat2 = subset(Nymphnewdat1,is.na(Nymphnewdat1$Percent_Positive)==FALSE)

Nymphtotpos = Nymphnewdat2 %>%
        group_by(Year,Pathogen) %>%
        summarize(sum(Totpos))
Nymphtotpos$`sum(Totpos)` = ifelse(is.na(Nymphtotpos$`sum(Totpos)`)==TRUE,0,Nymphtotpos$`sum(Totpos)`)
Nymphtottested = Nymphnewdat2 %>%
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

Ad_county_totpos = Adultnewdat1 %>%
        group_by(Year,Pathogen,County) %>%
        summarize(sum(Totpos))
Ad_county_tottested = Adultnewdat1 %>%
        group_by(Year,Pathogen,County) %>%
        summarize(sum(Total_Tested))
Ad_county_groupings=cbind(Ad_county_tottested,Ad_county_totpos)
Ad_county_groupings=Ad_county_groupings[,c(1:4,8)]
Ad_county_groupings$percpos=Ad_county_groupings$`sum(Totpos)`/(Ad_county_groupings$`sum(Total_Tested)`)*100
names(Ad_county_groupings)=c("Year","Pathogen","County","sum(Total_Tested)","sum(Totpos)","percpos")
Ad_county_groupings$`Life Stage`="Adult"

Ny_county_totpos = Nymphnewdat1 %>%
        group_by(Year,Pathogen,County) %>%
        summarize(sum(Totpos))
Ny_county_tottested = Nymphnewdat1 %>%
        group_by(Year,Pathogen,County) %>%
        summarize(sum(Total_Tested))
Ny_county_groupings=cbind(Ny_county_tottested,Ny_county_totpos)
Ny_county_groupings=Ny_county_groupings[,c(1:4,8)]
Ny_county_groupings$percpos=Ny_county_groupings$`sum(Totpos)`/(Ny_county_groupings$`sum(Total_Tested)`)*100
names(Ny_county_groupings)=c("Year","Pathogen","County","sum(Total_Tested)","sum(Totpos)","percpos")
Ny_county_groupings$`Life Stage`="Nymph"

All_county_groupings=rbind(Ad_county_groupings,Ny_county_groupings)
All_county_groupings = subset(All_county_groupings,All_county_groupings$`sum(Total_Tested)`>0)
All_county_groupings = subset(All_county_groupings,is.na(All_county_groupings$`sum(Totpos)`)==FALSE)
Allcnts=as.data.frame(list(unique(All_county_groupings$County)))
names(Allcnts)[1]="County"
Allcnts$ID=c(1:length(unique(All_county_groupings$County)))
All_county_groupings=left_join(All_county_groupings,Allcnts)
for(i in 1:length(unique(All_county_groupings$County))){
        a=subset(All_county_groupings,All_county_groupings$ID==7)
        ggplot(data=a,aes(x=Year,y=percpos,color=Pathogen)) +
                geom_line() +
                ggtitle(expression(paste(italic("Ixodes scapularis")," Infectivity Rates in",i,sep=""))) +
                theme_classic() +
                geom_hline(yintercept = 0,linetype='solid',color='black',size=.5,alpha=.7) +
                geom_vline(xintercept = 2008,linetype='solid',color='black',size=.5,alpha=.7) +
                facet_wrap(~`Life Stage`) +
                scale_x_continuous(breaks=c(2008:2019))
        ggsave(paste(i," County Line Graph.pdf",sep = ""))
}

## ^ need to figure out conditional county name paste for each i


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