# Kabacoff Ch.7: Maps


# geocoding ----

# translate physical address into longitude and latitude

library(tidygeocoder)

location <- c("lunch", "view")
addr <- c("10 Main Street, Middletown, CT",
          "20 W 34th St., New York, NY, 10001")
df <- data.frame(location, addr)

df <- tidygeocoder::geocode(df, address = addr,
                            method = "osm")
# osm = open street map
# supports US census, ArcGIS, Google, MapQuest
# ?geocode


# dot density maps ----

library(ggmap)
data(crime, package = "ggmap")
library(dplyr)
homicide <- filter(crime, offense == "murder") |>
  select(date, offense, address, lon, lat)

## interactive maps ----

library(mapview)
library(sf)

mymap <- st_as_sf(homicide,
                  coords = c("lon", "lat"),
                  crs = 4326)
mapview(mymap, color = "black", col.regions = "red",
        alpha.regions = 0.5, legend = FALSE,
        homebotton = FALSE, map.types = "OpenStreetMap")
# convert df into simple features (sf) object
# sf: df containing attributes and spatial geometries
# crs = : coordinate reference system
# mapview: take sf object, generate interactive map

## leaflet ----

library(leaflet)
leaflet() |>
  addTiles() |>
  addMarkers(lng = -72.6560002,
             lat=41.5541829,
             popup="The birthplace of quantitative wisdom.</br>
             No, Waldo is not here.")
# the package allow creating both dot density
# and choropleth maps

## static maps with ggmap ----

# issue: no connection to osm and google api

# choropleth maps ----

## using choroplethr ----

data(gapminder, package = "gapminder")

data(country.map, package = "choroplethrMaps")
head(unique(country.map$region), 10)

library(dplyr)

plotdata <- gapminder |>
  filter(year == 2007) |>
  rename(region = country, value = lifeExp) |>
  # need to be renamed for country_choropleth() to work
  mutate(region = tolower(region)) |>
  mutate(region = 
           recode(region,
                  "united states"    = "united states of america",
                  "congo, dem. rep." = "democratic republic of the congo",
                  "congo, rep."      = "republic of congo",
                  "korea, dem. rep." = "south korea",
                  "korea. rep."      = "north korea",
                  "tanzania"         = "united republic of tanzania",
                  "serbia"           = "republic of serbia",
                  "slovak republic"  = "slovakia",
                  "yemen, rep."      = "yemen"))

library(choroplethr)
country_choropleth(plotdata,
                   num_colors = 5)

# choroplethr functions return ggplot2 graphs
# num_colors = controls number of color, default 7, max 9
# other arguments: state_choropleth(), county_choropleth()

## building from sf and ggplot2 ----

library(sf)

HKMap <- st_read("data_visualization/data/Hong_Kong_18_Districts/HKDistrict18.shp")
head(HKMap, 3)

fire <- read.csv("data_visualization/data/Fire_Calls_18_District_eng.csv")

# cleaning data
fire_selected <- fire |>
  filter(Year.Month == "2022-1", Type == "Total") |>
  select(District, Count)

# check match
setdiff(fire_selected$District, HKMap$ENAME)
# [1] "CENTRAL AND WESTERN"

# fix difference
fire_selected$District <- sub("CENTRAL AND WESTERN", "CENTRAL & WESTERN",
    fire_selected$District)

# check match again
setdiff(fire_selected$District, HKMap$ENAME)

# join datasets
HKfire <- HKMap |>
  left_join(fire_selected, by=c("ENAME"="District"))

# map and customize
library(ggplot2)
ggplot(HKfire, aes(geometry = geometry,
                   fill = Count)) +
  geom_sf() +
  theme_void() +
  geom_sf_text(aes(label = ENAME),
               size = 2,
               color = "white") +
  labs(title = "Classification of Fire Calls by 18 District Council districts",
       subtitle = "Date: 2022-1",
       caption = "Source: https://data.gov.hk/en-data/dataset/hk-fsd-fsd1-fsdfc18")
