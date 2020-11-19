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
library(purrr)
library(MODIS)
library(MODISTools)
library(gdalUtilities)
library(gdalUtils)
library(rgdal)
library(rgeos)
library(tmap)
library(glue)
library(terra)
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
unzip(file.path(tdir,"Boundaries.zip"),exdir = tdir)}
Cnt=read_sf(paste(tdir,"/NYS_Civil_Boundaries_SHP/Counties_Shoreline.shp",sep=""))
#list.files(paste(tdir,"/NYS_Civil_Boundaries_SHP",sep=""))
County_Boundaries = Cnt[,c(1,18)]
NYS = st_union(County_Boundaries)
simpSpNYS=st_simplify(NYS,
                      dTolerance=100)
SpNYS=as(NYS,Class="Spatial")

#MODIStsp::MODIStsp_get_prodlayers("LandCover_Type_Yearly_500m (MCD12Q1)")
#NYSbb=data.frame(lat = c(40,45.2),
#                 lon = c(-71,-80))
#NYSbb_df <- st_as_sf(NYSbb, 
#                     coords = c("lon","lat"), 
#                     crs = 4326)
#username=source("MODISusername.R")
#password=source("MODISpassword.R")
#MODIStsp::MODIStsp(gui=FALSE,
#                   selprod = "LandCover_Type_Yearly_500m (MCD12Q1)",
#                   bandsel = "Land Cover Type 1 (IGBP)*",
#                   start_date = "2008.01.01",
#                   end_date = "2020.12.31",
#                   spafile = simpSpNYS,
#                   user = username$value,
#                   password = password$value,
#                   reprocess = TRUE)

#hdf4 <- paste(tdir,"/MODIStsp/HDFs/MCD12Q1.A2014001.h18v04.006.2018146020544.hdf",sep="")
#a=rast(hdf4)
#a=gdalUtils::get_subdatasets(paste(tdir,
 #       "/MODIStsp/HDFs/MCD12Q1.A2014001.h18v04.006.2018146020544.hdf",sep=""))

#gdal_translate(a[6,1],dst_dataset = "name.tif")
#gdal_translate(src_dataset = paste(tdir,
 #       "/MODIStsp/HDFs/MCD12Q1.A2014001.h18v04.006.2018146020544.hdf",sep=""),
 #       dst_dataset = 'test.tif')
#b=raster('test.tif')
#crs(b)="+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs" 
#plot(b)
#plot(SpNYS,add=T)

#tm_shape(b)+tm_raster()+tm_graticules()+
#        tm_shape(SpNYS)+tm_borders()
#        tmap_mode("view")











#rgdal::readGDAL(paste(tdir,"/MODIStsp/HDFs/MCD12Q1.A2008001.h18v04.006.2018145230343.hdf",sep=""))

#a=runGdal(product = "MCD12Q1", 
 #         collection = "006",forceDownload = T,extent = NY,
 #       begin = "2016001", end = "2017002", 
 #       SDSstring = "101100")


        
#gdalinfo(datasetname = paste(tdir,"/MODIS_ARC/MODIS/MCD12Q1.006/2016.01.01/MCD12Q1.A2016001.h12v04.006.2018149125204.hdf",sep=""))
#MODISTools::MODISG
#gdal_translate(a,dst_dataset = "test.tif")


## Land use data



################################################################################
#when using website, get data from the below calls
#data(us_states)
#NY = us_states[17,]
#NLCD=get_nlcd(template = st_as_sf(NY),
#              label = "NYS",
#               year = 2016,
#             dataset='Land_Cover',
#             landmass="L48",
#             extraction.dir = tdir)


################################################################################
#####
#For now
#####
a=raster("NYS_NLCD_2016_Land_Cover_L48_nlcd copy.tif")
crs(a)=crs(County_Boundaries)
crs(a)
# example: highval=raster::extract(tmax_annual,worldwoAnt,na.rm=TRUE,small=TRUE,sp=TRUE,fun=max)
simpCounties=st_simplify(County_Boundaries,dTolerance=100)
filter_image <- a %in% c(41,43)
filter_image2 <- ifelse(a==41,1,ifelse(a==43,1,0))

tm_shape(NLCD)+tm_raster()+tm_shape(newcounties[1,])+tm_borders(col='red')+tm_shape(newcounties[35,])+tm_borders(col='red')
NLCD2=projectRaster(from=NLCD,crs="+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs")
masknew2 = masknew %in% c(41,43)

newcounties=County_Boundaries %>% filter(NAME != "New York",
                                         NAME != "Queens",
                                         NAME != "Kings",
                                         NAME != "Richmond",
                                         NAME != "Bronx")
for(i in 1:length(newcounties)){
        NLCD=get_nlcd(template = st_as_sf(newcounties[i,]),
                      label = paste(newcounties[[i,1]]),
                      year = 2016,
                      dataset='Land_Cover',
                      landmass="L48",
                      extraction.dir = tdir)
        NLCD2=projectRaster(from=NLCD,crs="+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs")
        cropnew=crop(NLCD2,extent(newcounties[i,]))
        masknew=mask(cropnew,newcounties[i,])
        masknew2= masknew %in% c(41,43)
        #need to find total number of cells in each raster
        #or determine if below line finds if cells are adding up
        cellStats(masknew2,stat=sum)
        
}






















NLCD=get_nlcd(template = st_as_sf(simpCounties[1,]),
              label = "Alb",
               year = 2016,
             dataset='Land_Cover',
             landmass="L48",
             extraction.dir = tdir)


tm_shape(NLCD)+tm_raster()+tm_shape(simpCounties[1,])+tm_borders(col='red')+tm_graticules()

a2=simpCounties[1,]
a3=crop(a,albany)
plot(a)
plot(a2,add=T)

albany=simpCounties[as.character(simpCounties$NAME)%in% "Albany",]

tm_shape(a)+tm_raster()+tm_graticules()+
tm_shape(a2)+tm_borders(col='red')+tm_graticules()

highval=raster::extract(tmax_annual,worldwoAnt,na.rm=TRUE,small=TRUE,sp=TRUE,fun=max)
numCores=detectCores()
registerDoParallel(cores=numCores)
totals=foreach(i = 1:length(simpCounties)) %do% 
        raster::extract(filter_image2,simpCounties,na.rm=T,small=T,sp=T,fun=sum)
numCores=detectCores()
registerDoParallel(cores=numCores)
d=NULL
for(i in 1:length(simpCounties$NAME)){
        a1=simpCounties[i,]
        a2=mask(filter_image,a1)
        c=cellStats(a2,stat=sum)
        if(is.null(d)!=TRUE){d=c}
        else{d=rbind(d,c)}
}

cwer2=mask(filter_image,Albany)
cwer=cellStats(cwer2,stat=sum)

allmyvals <- raster::extract(x =filter_image, simpCounties)
myploys$sum_in_poly <- unlist(lapply(allmyvals , function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA ))

totals=foreach(i = 1:length(simpCounties)) %do% 
        raster::extract(filter_image,simpCounties,na.rm=T,small=T,sp=T,fun=sum)


for(i in 1:length(simpCounties)){PortionClassInPoly(MySingleShape=simpCounties[i,], MainRaster=a)}
rgeos::A

area(simpCounties)

Albany=simpCounties[1,]
Albany2=rasterize(Albany,a)
Albany3=extract(a,Albany,fun=sf::st_area(a@data))
atest=extract(a,Albany,na.rm=T,sm=T,)
plot(Albany3)

ex.mat.r = extract(a, as(Albany, 'Spatial'), buffer=1000, df=T) # raster extract

asdf=mask(a,Albany)
plot(asdf)
b=raster::extract(a,simpCounties,na.rm=TRUE,small=T,sp=T,fun=mean)
zonal(a,)
projectra
plot(a)
tm_shape(a) +
        tm_raster() + 
tm_shape(County_Boundaries) +
        tm_borders()
b=crop(a,extent(Albany))


ab=as.factor(a)



filter_image2 <- a 


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
        theme(axis.text.x = element_text(angle=65,vjust=.65)) +
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

All_county_groupings=All_county_groupings[c(-8)]
test_copy=All_groupings
test_copy=test_copy[-c(3,4)]
names(test_copy)[3]="Avg_pos"
test_copy$Pathogen=ifelse(test_copy$Pathogen=="Anaplasma phagocytophilum","A. phagocytophilum",
                          ifelse(test_copy$Pathogen=="Babesia microti","B. microti",
                                 ifelse(test_copy$Pathogen=="Borrelia burgdorferi","B. burgdorferi",
                                        ifelse(test_copy$Pathogen=="Borrelia miyamotoi","B. miyamotoi",NA))))
test_copy$Pathogen1=ifelse(test_copy$Pathogen=="A. phagocytophilum","NYS Average A. phagocytophilum",
                           ifelse(test_copy$Pathogen=="B. microti","NYS Average B. microti",
                                  ifelse(test_copy$Pathogen=="B. burgdorferi","NYS Average B. burgdorferi",
                                         ifelse(test_copy$Pathogen=="B. miyamotoi","NYS Average B. miyamotoi",NA))))
All_county_groupings$Pathogen=ifelse(All_county_groupings$Pathogen=="Anaplasma phagocytophilum","A. phagocytophilum",
                          ifelse(All_county_groupings$Pathogen=="Babesia microti","B. microti",
                                 ifelse(All_county_groupings$Pathogen=="Borrelia burgdorferi","B. burgdorferi",
                                        ifelse(All_county_groupings$Pathogen=="Borrelia miyamotoi","B. miyamotoi",NA))))

#create data to bind that has 6 not listed counties, 4 pathogens each, and 12 years each. then expand, then join

asdf=as.data.frame(c(2008:2019))
names(asdf)[1]="Year"
names(County_Boundaries)[1]="County"
asdf$County= c(subset(County_Boundaries$County, !(County_Boundaries$County %in%  All_county_groupings$County)),
               subset(County_Boundaries$County, !(County_Boundaries$County %in%  All_county_groupings$County)))
asdf$Pathogen=c("A. phagocytophilum","B. microti","B. miyamotoi","B. burgdorferi","A. phagocytophilum",
                "B. microti","B. miyamotoi","B. burgdorferi","A. phagocytophilum","B. microti","B. miyamotoi","B. burgdorferi")
asdf$Life_Stage=c("Nymph","Nymph","Nymph","Nymph","Nymph","Nymph",
                  "Adult","Adult","Adult","Adult","Adult","Adult")

asdf2=expand(asdf,Year,County,Pathogen,Life_Stage)
asdf2$Total_tested=0
asdf2$Tot_pos=0
asdf2$percpos=0
names(All_county_groupings)[4]="Total_tested"
names(All_county_groupings)[5]="Tot_pos"
names(All_county_groupings)[7]="Life_Stage"
All_county_groupings2=rbind(All_county_groupings,asdf2)
All_county_groupings3=expand(All_county_groupings2,Year,Pathogen,County,Life_Stage)

All_county_groupings4=left_join(All_county_groupings3,All_county_groupings,by=c("Year","Pathogen","Life_Stage","County"))
names(test_copy)[4]="Life_Stage"


test1 = All_county_groupings4 %>%
        nest(-County)

test2 = test1 %>% mutate(plot = map2(data, County, 
                             ~ggplot()+ 
                                     geom_hline(yintercept = 0,linetype='solid',color='black',size=.5,alpha=.7)+
                                     geom_vline(xintercept = 2008,linetype='solid',color='black',size=.5,alpha=.7)+
                                     geom_line(data=.x,aes(x=Year,y=percpos,color=Pathogen))+
                                     geom_point(data=.x,aes(x=Year,y=percpos,color=Pathogen))+
                                     scale_color_manual(name="Pathogen",values=c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#E41A1C","#377EB8","#4DAF4A","#984EA3"))+
                                     geom_line(data=test_copy,aes(x=Year,y=Avg_pos,color=Pathogen,linetype="NYS Average Pathogen Prevalence"))+
                                     scale_linetype_manual("",values = c(2,2,2,2,1,1,1,1))+
                                     theme(axis.text.x = element_text(angle=65,vjust=.65))+
                                     facet_wrap(~Life_Stage)+
                                     scale_x_continuous(breaks=c(2008:2019))+
                                     ylab("Pathogen Prevalence (%)")+
                                     labs(caption="Solid lines indicate pathogen prevalence from sample specimens in County")+
                                     ggtitle(glue("{.y}"))))


# Leaflet of NYS
County_Boundaries=st_transform(County_Boundaries,CRS("+proj=longlat +datum=WGS84"))
names(County_Boundaries)[1]="County"
test_new=left_join(County_Boundaries,test2,by='County')

leaflet() %>%
        addTiles() %>%
        addPolygons(data=test_new,
                    group='County',
                    weight=1,
                    highlight=highlightOptions(
                        weight=5,
                        color='red',
                        bringToFront=TRUE)) %>%
        leafpop::addPopupGraphs(graph=test_new$plot,
                                group='County',
                                width=600,height=300)




# Do statistical testing using spearman rank corrs for each county as an observation