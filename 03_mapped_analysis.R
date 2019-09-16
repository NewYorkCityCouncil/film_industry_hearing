library(tidyverse)
library(sf)
library(leaflet)

geopermits <- read_csv('output_files/geopermits.csv') 

geopermits <- geopermits %>% drop_na(c('long_from', 'lat_from', 'long_to', 'lat_to'))

geopermits$clong <- (geopermits$long_from + geopermits$long_to)/2
geopermits$clat <- (geopermits$lat_from + geopermits$lat_to)/2

geopermits <- st_as_sf(geopermits, coords = c("clong", "clat"), coords = '+proj=longlat +datum=WGS84')


geopermits_2018 <- filter(geopermits, startdatetime < as.POSIXct('2019-01-01 00:00:00') & startdatetime >= as.POSIXct('2018-01-01 00:00:00')) %>% 
  st_set_crs('+proj=longlat +datum=WGS84')




# Map at CD Level ---------------------------------------------------------



cds <- read_sf('community_districts/geo_export_3b2cd0ff-eff3-46ff-adc6-59ddc6430073.shp') %>% 
  select(boro_cd, geometry) %>% 
  st_transform(st_crs(geopermits_2018))


cds$num_permits <- lengths(st_intersects(cds,geopermits_2018))


permit_pal <- colorBin(
  palette = 'YlGnBu',
  domain = cds$num_permits)

permit_pop  <- paste0("Community District: ", cds$boro_cd, '<br>',
                      "Number of Permits: ", cds$num_permits)


leaflet(cds) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(fillColor = ~permit_pal(cds$num_permits),
              fillOpacity = .9,
              weight = 3,
              popup = permit_pop) %>% 
  addLegend(position = "topleft",
            pal = permit_pal,
            values = cds$num_permits,
            title = "Number of Permits by Community Disctrict, 2018")


# Map at Zipcode Level ----------------------------------------------------

zips <- read_sf('zip_code/ZIP_CODE_040114.shp') %>% 
  janitor::clean_names() %>% 
  select(zipcode, geometry) %>% 
  st_transform('+proj=longlat +datum=WGS84')




zips$num_permits <- lengths(st_intersects(zips,geopermits_2018))


zpermit_pal <- colorBin(
  palette = 'YlGnBu',
  domain = zips$num_permits)

zpermit_pop  <- paste0("Community District: ", zips$zipcode, '<br>',
                      "Number of Permits: ", zips$num_permits)


leaflet(zips) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(fillColor = ~zpermit_pal(zips$num_permits),
              fillOpacity = .9,
              weight = 3,
              popup = zpermit_pop) %>% 
  addLegend(position = "topleft",
            pal = zpermit_pal,
            values = zips$num_permits,
            title = "Number of Permits by Zipcode, 2018")


# geopermits_from <- geopermits %>% select(-one_of('lat_to', 'long_to')) %>% 
#   rename(lat = lat_from, long = long_from) %>% 
#   add_column(where = 'from')
# 
# geopermits_to <- geopermits %>% select(-one_of('lat_from', 'long_from')) %>% 
#   rename(lat = lat_to, long = long_to) %>% 
#   add_column(where = 'to')
# 
# geopermits_line <- rbind(geopermits_from, geopermits_to) %>% 
#   arrange(eventid, main, cross_st_1, cross_st_2, where)
# 
# 
# 
# counter_func <- function(df, eventid, main, cross_st_1, cross_st_2) {
#   combo <- c(df$eventid, df$main, df$cross_st_1, df$cross_st_2)
#   return(combo)
# }
# 
# 
# out <- pmap(geopermits[17:20], ~
#               c(...) %>%
#               matrix(., , ncol=2, byrow = TRUE) %>% 
#               st_linestring) %>%
#   reduce(st_sfc) %>%
#   mutate(geopermits, geometry = .)
# 
# 
# test$unistrs <- paste(test$eventid, test$main, test$cross_st_1, test$cross_st_2)
#     
# 
# 
# 
# test <- geopermits_line[1:5,]
# 
# test$ls <- st_linestring(c(rbind(c(test$long_from, test$lat_from),c(test$long_to, test$lat_to))))
# 
# multipoints <- st_multipoint(matrix(c(test$long_from, test$lat_from, test$long_to, test$lat_to), nrow = 2, byrow = TRUE), dim = "XY")
# points <- st_cast(st_geometry(multipoints), 'POINT')
# 
# 
# # Number of total linestrings to be created
# n <- length(points) - 1
# 
# # Build linestrings
# linestrings <- lapply(X = 1:n, FUN = function(x) {
#   
#   pair <- st_combine(c(points[x], points[x + 1]))
#   line <- st_cast(pair, "LINESTRING")
#   return(line)
#   
# })
# 
# # One MULTILINESTRING object with all the LINESTRINGS
# multilinetring <- st_multilinestring(do.call("rbind", linestrings))


