# Cancesiones_TURF
Creating code to map boundaries of TURF's in Mexico and add the associated attributes about regulations and management

# Canciones_TURF, creating spatial dataframes that coincide with TURF boundaries 
#Created 12/13/2019 by Claire Atkins-Davis
#Add packages
install.packages("sp")
install.packages("tidyverse")
install.packages("ggplot")
install.packages("dplyr")
install.packages("sf")
#Load Packages
library(sp)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(sf)

setwd('C:Users\ATKIN\OneDrive\Desktop\MPA\Data')
turf <-read.csv("C:/Users/ATKIN/OneDrive/Desktop/MPA/Data/Turf_polygons.csv")
head(turf)
dir(turf)
dim(turf)

#Create dataframe and combining all of the latitude and longitude from TURFS and grouping them by sub_id
turf.poly <- data.frame(
  lon = length(turf$longitude),
  lat = length(turf$latitude),
  grouping("sub_id"))

polygon <- turf.poly %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
polygon

mx_c_1.1 <- filter("subid")

#Loop to create multiple polygons 
# Example data
turf <- t(replicate(50, {
  o <- runif(2)
  c(o, o + c(0, 0.1), o + 0.1, o + c(0.1, 0), o)
}))
ID <- paste0('poly', seq_len(nrow(square)))

# Create SP
polys <- SpatialPolygons(mapply(function(poly, sub_id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(poly.turf, row(poly.turf)), ID))

# Create SPDF
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

plot(polys.df, col=rainbow(50, alpha=0.5))


#Add spatial attibutes to polygons
extent(turf)

## class       : Extent 
## xmin        : 731405.3 
## xmax        : 732275.3 
## ymin        : 4712845 
## ymax        : 4713846

# add extra space to right of plot area; 
# par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(extent(plot.locationsSp_HARV),
     col="purple", 
     xlab="latitude",
     ylab="longitude", lwd=8,
     main="Extent Boundary of Plot Locations \nCompared to the AOI Spatial Object",
     ylim=c(4712400,4714000)) # extent the y axis to make room for the legend

plot(extent(turf), 
     add=TRUE,
     lwd=6,
     col="springgreen")

legend("bottomright",
       #inset=c(-0.5,0),
       legend=c("Layer One Extent", "Layer Two Extent"),
       bty="n", 
       col=c("purple","springgreen"),
       cex=.8,
       lty=c(1,1),
       lwd=6)
