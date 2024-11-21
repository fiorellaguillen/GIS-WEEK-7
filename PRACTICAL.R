library(here)
library(janitor)
library(sf)
library(tidyverse)
library(tmap)
library(spdep)

LondonWards <-  st_read(here::here("data", "London_Ward.shp"))

LondonWardsMerged <- st_read(here::here("data","London_Ward_CityMerged.shp"))%>%
  st_transform(.,27700)

WardData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                     locale=locale(encoding = "latin1"),
                     na=c("NA", "n/a"))%>%
  clean_names()

LondonWardsMerged <- LondonWardsMerged %>%
  left_join(WardData,
            by=c("GSS_CODE"="new_code"))%>%
  dplyr::distinct(GSS_CODE,.keep_all = T)%>%
  dplyr::select(GSS_CODE,ward_name,average_gcse_capped_point_scores_2014)


st_crs(LondonWardsMerged)

BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")%>%
  st_transform(.,27700)

tmap_mode("plot")
tm_shape(LondonWardsMerged)+
  tm_polygons(fill=NA, fill_alpha = 0.5)+
tm_shape(BluePlaques)+
  tm_dots(fill="blue")

summary(BluePlaques)

#get only blue plaques inside London
BluePlaquesSub <- BluePlaques[LondonWardsMerged,]
tm_shape(LondonWardsMerged)+
  tm_polygons(fill=NA, fill_alpha = 0.5)+
  tm_shape(BluePlaquesSub)+
  tm_dots(fill="blue")

example <- st_intersects(LondonWardsMerged, BluePlaquesSub)
example

check_example <- LondonWardsMerged %>%
  st_join(BluePlaquesSub)%>%
  filter(ward_name=="Kingston upon Thames - Coombe Hill")

points_sd_joined <- LondonWardsMerged%>%
  mutate(n=lengths(st_intersects(.,BluePlaquesSub)))%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(density=n/area)%>%
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)

points_sd_joined <- points_sd_joined %>%
  group_by(gss_code)%>%
  summarise(density=first(density),
            wardname=first(ward_name),
            plaquecount=first(n))

tm_shape(points_sd_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")

coordsW <- points_sd_joined%>%
   st_centroid()%>%
   st_geometry()
 
plot(coordsW,axes=TRUE)

LWard_nb <- points_sd_joined%>%
  poly2nb(.,queen=T)

summary(LWard_nb)

plot(LWard_nb, st_geometry(coordsW), col="red")
plot(points_sd_joined$geometry, add=T)