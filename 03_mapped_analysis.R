library(tidyverse)
library(sf)
library(leaflet)

geopermits <- read_csv('output_files/geopermits.csv') 

geopermits <- geopermits %>% drop_na(c('long_from', 'lat_from', 'long_to', 'lat_to'))

geopermits$clong <- (geopermits$long_from + geopermits$long_to)/2
geopermits$clat <- (geopermits$lat_from + geopermits$lat_to)/2

geopermits <- st_as_sf(geopermits, coords = c("clong", "clat"))


geopermits_2018 <- filter(permits, startdatetime < as.POSIXct('2019-01-01 00:00:00') & startdatetime >= as.POSIXct('2018-01-01 00:00:00'))

geopermits_2018 <- st_as_sf(geopermits, coords = c("clong", "clat"))



cds <- read_sf('community_districts/geo_export_3b2cd0ff-eff3-46ff-adc6-59ddc6430073.shp') %>% 
  select(boro_cd, geometry) %>% 
  st_set_crs(st_crs(geopermits_2018))


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


