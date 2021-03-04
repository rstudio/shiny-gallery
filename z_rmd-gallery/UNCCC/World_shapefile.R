library(rgdal)
library(leaflet)
library(data.table)
library(htmltools)

### To download
# download.file(file.path('http://www.naturalearthdata.com/http/',
#                         'www.naturalearthdata.com/download/50m/cultural',
#                         'ne_50m_admin_0_countries.zip'), 
#               f <- tempfile())
# unzip(f, exdir=tempdir())
# 
# world <- readOGR(tempdir(), 'ne_50m_admin_0_countries', encoding='UTF-8')

load("total.rdata")
total2 <- total[Year == 2000 & GHG_gas == 'CH4']
total2 <- total2[!duplicated(total2[, iso])]

# After downloading, saving to the "Mapping" folder (by using writeOGR), use the saved file:
world <- readOGR("/Users/marwasalem/Desktop/UNCCCapp", 'ne_50m_admin_0_countries', encoding='UTF-8')


# To explore the data (the "slots" of a SpatialPolygonDataFrame are: data, polygons, plotOrder, bbox, proj4string)

head(world@data)

slotNames(world@polygons[[1]])

# To merge data into the data slot
#The ID slot is what ties the geometries and the attributes.
all(lapply(world@polygons, slot, "ID") == row.names(world))
# [1] TRUE

# Note that like row.names, polygon IDs are character not integer.
summary(as.integer(lapply(world@polygons, slot, "ID")))

# Save the data row.names into an explicit variable
world$rn <- row.names(world)

# Create temporary data tables to work on the attributes
tmp <- data.table(world@data)

# To merge data from UNCCC data into world
setkey(tmp, iso_a3)
setnames(total2, 'iso', 'iso_a3')
setkey(total2, iso_a3)

#tmp <- merge(total2, tmp, by = 'iso_a3')
tmp <- total2[tmp]

# Then let's re-attach the table to the original SpatialPolygonsDataFrame
# (preserving the original order of the row.names)
setkey(tmp, rn)
world@data <- tmp[row.names(world)]

# *** Export back to shapefile (or to any spatial format)
# writeOGR(world, "/Users/marwasalem/Desktop/UNCCCapp", "ne_50m_admin_0_countries", driver="ESRI Shapefile")


# The whole world
pal <- colorNumeric(
  palette = "Oranges",
  domain = total2$Value
)

map <- leaflet() %>% addTiles() %>% addProviderTiles('Esri.WorldStreetMap') %>% 
  setView(lng = 0, lat = 0, zoom = 2) %>%
  addPolygons(data = world,  stroke = FALSE, #fillColor = terrain.colors(10, alpha = NULL),
              color = ~pal(Value), fillOpacity = 0.6, smoothFactor = 0.2, 
              popup = ~htmlEscape(paste(world$name, "Total CH4 = ", world$Value)))
map

# Also see 'merge' in the sp package for merging spatial with non-spatial data





