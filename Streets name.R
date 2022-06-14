library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(rmarkdown)
library(ggplot2)


Philly_Info <-
  read.csv("Speed Reduction Options.csv")

Above_30_geo <-
  st_read("SpeedLimit_30.shp")%>%
  distinct(STNAME,.keep_all = TRUE)

Above_30_distinct <-
  st_read("SpeedLimit_30.shp")%>%
  st_drop_geometry()%>%
  distinct(STNAME, .keep_all = TRUE)

Above_30_aggreated_length <-
  st_read("SpeedLimit_30.shp") %>%
  st_drop_geometry()%>%
  select(STNAME,Shape_Leng)%>%
  group_by(STNAME)%>%
  summarize(length = sum(Shape_Leng))
  
Combine <-
  left_join(Above_30, Philly_Info)

Missing <-
  Combine%>%
  filter(is.na(Proposed.Phase.1))

write.csv(Missing,"Missing Street.csv")

# Finding overlap between FHWA classification and Missing streets

FHWA_class <-
  st_read("FHWA Class.shp")%>%
  select(geometry,FHWA_FUNC_)
  
Missing_Street_Clean <-
  read.csv("Missing Street 2.csv")%>%
  left_join(Above_30_aggreated_length)%>%
  left_join(Above_30_geo)%>%
  st_as_sf()%>%
  st_join(FHWA_class)%>%
  select(STNAME,FHWA_FUNC_)%>%
  mutate(Class = case_when(FHWA_FUNC_ == 1 ~ "Interstate",
                           FHWA_FUNC_ == 2 ~ "Other Fwy or Exp",
                           FHWA_FUNC_ == 3 ~ "Principal Arterial",
                           FHWA_FUNC_ == 4 ~ "Minor Arterial",
                           FHWA_FUNC_ == 5 ~ "Major Collector",
                           FHWA_FUNC_ == 7 ~ "Local"))

# Finding overlap between High Injury Network and Missing streets

HIN <-
  st_read("HIN.shp") 

Missing_Street_HIN <-
  read.csv("Missing Street 2.csv")%>%
  left_join(Above_30_aggreated_length)%>%
  left_join(Above_30_geo)%>%
  st_as_sf()%>%
  st_join(HIN)%>%
  filter(!is.na(BUFFER))

Residential_buffer <-
  st_read("land_use_buffer_2.shp") %>%
  st_transform(crs = st_crs(Above_30_geo))

Missing_Street_res <-
  read.csv("Missing Street 2.csv")%>%
  left_join(Above_30_aggreated_length)%>%
  left_join(Above_30_geo)%>%
  st_as_sf()%>%
  st_join(Residential_buffer)

# Finding overlap between DVRPC classification and Missing streets

DVRPC <-
  st_read("DVRPC_class.shp")%>%
  st_transform(crs = st_crs(Above_30_geo))

Missing_Street_DVRPC <-
  read.csv("Missing Street 2.csv")%>%
  left_join(Above_30_aggreated_length)%>%
  left_join(Above_30_geo)%>%
  st_as_sf()%>%
  st_join(DVRPC)%>%
  select(STNAME, TYPOLOGY__)

Missing_Street_DVRPC_lane <-
  read.csv("Missing Street 2.csv")%>%
  left_join(Above_30_aggreated_length)%>%
  left_join(Above_30_geo)%>%
  st_as_sf()%>%
  st_join(DVRPC)

Complete_St <-
  st_read("https://opendata.arcgis.com/datasets/ed90e9016aab4c429cb7dd8aef2a87a3_0.geojson")

Missing_Street_Complete <-
  read.csv("Missing Street 2.csv")%>%
  left_join(Complete_St) %>%
  filter(SURFAWIDTH > 0) %>%
  select(STNAME,SURFAWIDTH,PARKING)
