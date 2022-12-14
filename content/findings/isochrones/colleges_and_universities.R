setwd("isochrone")
library(RColorBrewer)
library(traveltime)
library(tidyverse)
library(tidycensus)
library(sf)
library(osmdata)
library(leaflet)
library(sp)
library(purrr)
library(mapview)
library(osrm)
library(rmapzen)
library(rgdal)
library(ggplot2)
library(scales)
#webshot::install_phantomjs()


#getting population and maps of surrounding of page from acs




myACSkey <- "2580514e97d888fe585f59b4e328fce92342fe8f"

#show available variables for ACS survey
acs5 <- load_variables(2018, "acs5", cache=T)

# B19013_001 - MedianIncome
# B01003_001 - Total Population


#FUNCTIONS:

# 1. "acs_tables" calls "get_acs" (from tidycensus) on a vector of table names. It returns a dataframe of
# all the tables bound together.  The function requires a vector of table names,
# a census API key, and a geographical unit.  The user can add other parameters as well.

acs_tables<-function(tables,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(tables)){
    data<-get_acs(geography = geography,
                  table = tables[i],
                  key = key,
                  show_call = T,
                  cache_table=T,
                  ...
    )
    acs_data<-rbind(acs_data,data.frame(data))
  }
  return(acs_data)
}

# 2. "acs_wide" cleans the data returned from a census API call.  More specifically,
# it separates the variable column into separate variables, and it separates "NAME" into
# different columns with pre-defined column names (NAME_col_names). The function also
# drops the "margin of error" column.

acs_wide<-function(data,NAME_col_names){
  data%>%
    select (-moe)%>%
    pivot_wider(names_from = variable,values_from=estimate)%>%
    separate(NAME, into=NAME_col_names, sep = ", ")
}


#3. acs_years retrieves individual variables (or a list of variables) across a series of years.
acs_years<-function(years,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(years)){
    acs<-get_acs(geography = geography,
                 #variables = vars,
                 key = key,
                 year=years[i],
                 output = "wide",
                 show_call = T,
                 geometry = F,
                 ...)
    acs_data<-(rbind(acs_data,data.frame(acs)))
  }
  acs_data<-cbind(acs_data,year=rep((years),each=length(unique(acs_data$GEOID))))
  return(acs_data)
}


#4. "acs_years_tables" uses two previously defined functions (acs_tables and acs_wide) to return multiple
# variable tables across multiple years in one single tibble.  A couple of notes: the way that
# get_acs handles variables before 2013 varies, so this function only works for 2013 and after.
# For variable tables before 2013, use acs_tables to pull individual sets of tables.  Also, I have
# not included "geometry" in the function.  If the user includes geometry, he/she may need
# to modify the call to acs_wide.


acs_years_tables<-function(tables,years,key,geography,NAME_col_names,...){
  acs_data<-NULL
  for (j in 1:length(years)){
    acs<-acs_tables(tables=tables,year=years[j],key=key,geography = geography,...)
    year<-rep(years[j],times=length(acs$GEOID))
    acs_years2<-cbind(year,data.frame(acs))
    acs_data<-(rbind(acs_data,acs_years2))
  }
  acs_data<-acs_wide(acs_data,NAME_col_names = NAME_col_names)
  return(acs_data)
}


#NATIONAL AND Page DATA

# Variables
# B19013_001 - Median Income
# B01003_001 - Total Population

tables<-c("B01003","B19013")
years<-c(2018)
colnames=c("Census_tract","County","State")

# Pull ACS data for Page only
acs_Page<-acs_years_tables(tables=tables,
                           years=years,
                           key=myACSkey,
                           geography="tract",
                           state="VA",
                           county="Page",
                           NAME_col_names = colnames)
# Rename Variable Columns
acs_Page <- acs_Page %>%
  rename(
    Median_Income = B19013_001,
    Total_Population = B01003_001
  )

# Surrounding Counties
# Page, Rockingham, Greene, Madison, Rappahannock, Warren, Shenandoah, Harrisonbrug
acs_Page_area<-acs_years_tables(tables=tables,
                                years=years,
                                key= myACSkey,
                                geography="tract",
                                state="VA",
                                county=c("Page county",
                                         "Rockingham county",
                                         "Greene county",
                                         "Madison county",
                                         "Rappahannock county",
                                         "Warren county",
                                         "Shenandoah county",
                                         "HARRISONBURG"),
                                NAME_col_names = colnames)

# Rename Variable Columns
acs_Page_area <- acs_Page_area %>%
  rename(
    Median_Income
    = B19013_001,
    Total_Population = B01003_001
  )



# Get VA County Outlines
va_sf<-get_acs(geography = "county",
               state="VA",
               county=c("Page county",
                        "Rockingham county",
                        "Greene county",
                        "Madison county",
                        "Rappahannock county",
                        "Warren county",
                        "Shenandoah county",
                        "HARRISONBURG"),
               variables = "B19058_002",
               survey = "acs5",
               key = myACSkey,
               year=2018,
               output = "wide",
               show_call = T,
               geometry = T,
               keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)

# Get Page County outline
page_outline<-get_acs(geography = "county",
                      state="VA",
                      county=c("Page county"),
                      variables = "B19058_002",
                      survey = "acs5",
                      key = myACSkey,
                      year=2018,
                      output = "wide",
                      show_call = T,
                      geometry = T,
                      keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)


# Mapping

# Read in Virginia shape file
# mapVA  <- st_read("~data/VirginiaShapeFiles/tl_2019_51_tract.shp", stringsAsFactors = FALSE)
mapVA  <- st_read("VirginiaShapeFiles/tl_2019_51_tract.shp",
                  stringsAsFactors = FALSE)

map_and_data <- inner_join(mapVA, acs_Page_area, by = "GEOID")

mapview_for_page = mapview(map_and_data,zcol = "Total_Population", layer.name = "census tract total population",alpha.regions= 1)
all_outline <- mapview(va_sf, color="black", size=.5,legend=FALSE,alpha.regions= 0)
map_page_outline <- mapview(page_outline, zcol= NULL, color="red", size=2, legend= FALSE,alpha.regions= 0)
(map_page <- all_outline+mapview_for_page +map_page_outline)


#getting all health_service points
Colleges_and_Universities <- read_csv("Colleges_and_Universities.csv")
#Workforce_Development_Centers <- read_csv("Workforce_Development_Centers.csv")
#colnames(Workforce_Development_Centers)[1] <- "X"
#workforce_dev_center_data <- Workforce_Development_Centers %>%
#  filter(Zip %in% c("22851", "22650", "22827", "22849", "22835", # Page
#                    "22801", "22802", "22807", # Harrisonburg
#                    "22626", "22842", "22810", "22845", "22824",
#                    "22660", "22641", "22657", "22644", "22847",
#                    "22652", "22664", # Shenandoah
#                    "22968", "22935", "22965", "22968", "22935",
#                    "22973", # Greene
#                    "22727" # Part of Madison
#  ))



filtered_college_university <- Colleges_and_Universities %>% filter( STATE == "VA") %>%
                    filter(COUNTY %in% c("PAGE",
                                         "ROCKINGHAM",
                                         "GREENE",
                                         "MADISON",
                                         "RAPPAHANNOCK",
                                         "WARREN",
                                         "SHENANDOAH",
                                         "HARRISONBURG"))


#college_uni_plot <- leaflet(data = filtered_college_university) %>% # create leaflet object
#  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
#  addMarkers()
# #call plot
#college_uni_plot



cor_filtered_college_university <- filtered_college_university
coordinates(cor_filtered_college_university) <-c("LONGITUDE", "LATITUDE")
proj4string(cor_filtered_college_university) <-CRS("+proj=longlat +datum=WGS84")
map_for_points <- mapview(cor_filtered_college_university, cex =2,color="orange", col.regions= "orange", layer.name= "Universities and Colleges")
#map_for_points

(map_with_all_points <- map_page+map_for_points)

#traveltime_api <- "316e5ee78722869b9d6a2fbe87400219"
#traveltime_id <- "e6318bfa"


#clg_uni_iso_20_index_1 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                        location=c(filtered_college_university$LATITUDE[1],filtered_college_university$LONGITUDE[1]),
#                                        traveltime=1200,
#                                        type="driving",
#                                        departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(clg_uni_iso_20_index_1, file = paste0('clg_uni_iso_20_index_1','.RDS'))
# this particular isochrone is for a 10 minute travel window

#clg_uni_iso_20_index_2 <- traveltime_map(appId= traveltime_id,
#                                         apiKey = traveltime_api,
#                                         location=c(filtered_college_university$LATITUDE[2],filtered_college_university$LONGITUDE[2]),
#                                         traveltime=1200,
#                                         type="driving",
#                                         departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(clg_uni_iso_20_index_2, file = paste0('clg_uni_iso_20_index_2','.RDS'))
# this particular isochrone is for a 10 minute travel window


#clg_uni_iso_20_index_3 <- traveltime_map(appId= traveltime_id,
#                                         apiKey = traveltime_api,
#                                         location=c(cor_filtered_college_university$LATITUDE[3],cor_filtered_college_university$LONGITUDE[3]),
#                                         traveltime=1200,
#                                         type="driving",
#                                         departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(clg_uni_iso_20_index_3, file = paste0('clg_uni_iso_20_index_3','.RDS'))
# this particular isochrone is for a 10 minute travel window



clg_uni_iso_20_index_1 <- readRDS("clg_uni_iso_20_index_1.RDS")
clg_uni_iso_20_index_2 <- readRDS("clg_uni_iso_20_index_2.RDS")
clg_uni_iso_20_index_3 <- readRDS("clg_uni_iso_20_index_3.RDS")

st_crs(clg_uni_iso_20_index_1) = 4326
st_crs(clg_uni_iso_20_index_2) = 4326
st_crs(clg_uni_iso_20_index_3) = 4326


colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

m_1 = mapview(clg_uni_iso_20_index_1, layer.name = "20 minute isochrone 1", col.regions = colors[10])
m_1
m_2 = mapview(clg_uni_iso_20_index_2, layer.name = "20 minute isochrone 2", col.regions = colors[10])
m_2
m_3 = mapview(clg_uni_iso_20_index_3, layer.name = "20 minute isochrone 3", col.regions = colors[10])
m_3




(m_last<- map_with_all_points+m_1+m_2+m_3)

#m_last@map= m_last@map%>%
#  setView(clg_uni_iso_20_index_3$lat,clg_uni_iso_20_index_3$lng, zoom = 9)


m_last
#getting isochrones for page 5,60
