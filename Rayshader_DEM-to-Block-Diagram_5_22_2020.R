# 02/28/2020
# @author: andrew paolucci 
#          based on demo by dylan beaudette and andrew brown

# Load packages
library(rayshader)
library(rgl)
library(raster)
library(rgdal)
library(viridis)
library(sf)
library(fasterize)
library(rasterVis)
library(RColorBrewer)


# Read SOILS shapefile
thematic_shp <- readOGR('C:/geodata/ProjectData/WI111BD1Soils2.shp')
# Insepct to see what column has MUSYM symbol
head(thematic_shp)
# Assign column
mu.col <- "MUSYM"

# Load DEM in TIFF Format
elev <- raster('C:/geodata/ProjectData/WI111BD1LIDAR.tif')

# Plot
plot(elev)
lines(as(extent(elev), 'SpatialPolygons'))

#Crop raster using shapefile
#Load extent shapefile
extent <- readOGR(dsn='C:/geodata/ProjectData/Extent2.shp')

## crop and mask
elev.crop <- crop(elev, extent(extent))
elev <- mask(elev.crop, extent)

## Check that it worked
plot(elev)
plot(extent, add=TRUE, lwd=2)



#If necessary resample raster to different resolution. 
## copy elevation raster
#elev_template <- elev

## change raster resolution in template (leaving all else the same)
#res(elev_template) <- c(1, 1) #1 m by 1 m

## resample elevation raster to desired template 
#elev <- resample(elev, elev_template)

## OPTIONAL Save to file
#writeRaster(elev, filename='D:/GIS/CA794/elevation/1meterDEM.tif')

# convert elevation raster to a matrix
elmat <- rayshader::raster_to_matrix(elev)

# calculate (rectangular) boundary of DEM, use that to cut the overlay shapefile
extent.poly <- as(extent(elev), 'SpatialPolygons')
proj4string(extent.poly) <-  proj4string(elev)
thematic_shp <- spTransform(thematic_shp, CRS(proj4string(elev)))
extent.poly <- spTransform(extent.poly, CRS(proj4string(elev)))
thematic_shp <- crop(thematic_shp, y = extent.poly)

# assign numeric value that is 1:1 with mukey
thematic_shp$munum <- match(thematic_shp[[mu.col]], unique(thematic_shp[[mu.col]]))

#Fix island polygons
thematic_shp <- spTransform(thematic_shp, CRS(proj4string(elev)))
area <- sapply(1:nrow(thematic_shp), function(x) {thematic_shp@polygons[[x]]@Polygons[[1]]@area}) 
shape1 <- thematic_shp[order(area), ]

#Convert soils shapefile to raster
theme <- fasterize::fasterize(sf::st_as_sf(thematic_shp), elev, field = 'munum')

# slow, but does not require sf or fasterize
#  theme <- rasterize(shape1, elev, "munum", fun = "first")

#CHECK: Plot rasterized soil layer
plot(theme)

# Change color scheme
n <-length(unique(theme$layer))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colors <- sample(col_vector, n)

#CHECK: Plot new color scheme
pie(rep(1,n), col=colors)
pie(rep(1,n), col=colors, labels = colors)
plot(theme, col=colors)

#Replace individual colors (Optional) RGB Method colors[4] <- rgb(0,0,132/255)
colors[1] <- "#FFFFB3" # yellow summit
colors[2] <- "#a97939" # orangish brown terrace
colors[3] <- "#1B9E77" # blueish river
colors[4] <- "#415c17" # dark green sideslope
# color #5 is not used
colors[6] <- "#B15928" # dark orange valley
colors[7] <- "#B3B3B3" # light gray floodplain 
#colors[1] <- "#FFFFB3" # keep for now. could be # 12 too
#colors[8] <- "#FDBF6F" # keep for now


#CHECK: Individual color
barplot(c(1), col=colors[1])
#CHECK: Plot rasterized soil layer with new color scheme
plot(theme, col=colors)

# create RGB array from rasterized theme
tf <- tempfile()
old.par = par(no.readonly = TRUE)
on.exit(par(old.par))
png(tf, width = nrow(elmat), height = ncol(elmat))

fliplr = function(x) { x[,ncol(x):1] }

par(mar = c(0,0,0,0))
raster::image(fliplr(raster_to_matrix(theme)), 
              axes = FALSE, 
              col = colors)
dev.off()
my.array <- png::readPNG(tf)

# compute shadows
raymat <- ray_shade(elmat)
ambmat <- ambient_shade(elmat)

# Opintional: with big files save the intermediates in case rgl crashes R or something
#save(elmat, raymat, ambmat, file = "C:/Paolucci/Projects/RIntermediates/intermediates.Rda")

#### INTERACTIVE 3D PLOT WITH RGL ####
# set perspective with right-mouse + drag
# zoom with mouse wheel
# rotate with left-mouse + drag
#Clear rgl plot
rgl::rgl.clear()

# Create 3D plot
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(my.array, alphalayer=0.9) %>%
  #add_water(detect_water(elmat), color="desert") %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.4) %>%
  plot_3d(elmat, zscale=3, fov=0.8, theta=10, zoom=0.5, phi=45, windowsize = c(1000,800), lineantialias = TRUE, water=FALSE, waterdepth=0, wateralpha=0.75, watercolor = "lightblue")

# take a static picture of the rgl window
render_snapshot()

#Add legend
#Convert symbols to characters
musyms <- as.character(unique(thematic_shp$General_Na))
#plot legend
legend3d("topright", legend = c(musyms), pch = 16, col = colors, cex=1, inset=c(0.02))

# Take snapshot
render_snapshot()

# important to clear the previous rgl window if any settings are adjusted
rgl::rgl.clear()
