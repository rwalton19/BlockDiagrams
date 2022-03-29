library(rayshader)
library(sp)
library(raster)
library(scales)
library(rgdal)

elev = raster::raster("C:/paolucci/Projects/BlockDiagrams/Jason_Block/JB_LIDAR.tif")


height_shade(raster_to_matrix(elev)) %>%
  plot_map()

r <- raster('C:/paolucci/Projects/BlockDiagrams/Jason_Block/Band1.tif')
g <- raster('C:/paolucci/Projects/BlockDiagrams/Jason_Block/Band2.tif')
b <- raster('C:/paolucci/Projects/BlockDiagrams/Jason_Block/Band_3.tif')

#CHECK:
imagery <- stack(r, g, b)
plotRGB(imagery)

# Nor sure if this is neccessary. In tutorial it was used to brighten image
#imagery_corrected = sqrt(stack(r, g, b))
#plotRGB(imagery_corrected)

#CHECK:  make sure coordinate reference systems are the same 
crs(r)
crs(elev)

# Load extent shapefile
extent <- readOGR(dsn='C:/paolucci/Projects/BlockDiagrams/Jason_Block/JB_Extent.shp')

## crop and mask DEM
elev.crop <- crop(elev, extent(extent))
elev <- mask(elev.crop, extent)

# Get rectangular extent of DEM and use that to crop imagery
extent.poly <- as(extent(elev), 'SpatialPolygons')
imagery_cropped <- crop(imagery, y = extent.poly)

# Convert DEM to matrix
elev_matrix <- raster_to_matrix(elev)

# CHECK: inspect imagery to see column names 
head(imagery_cropped)
#define columnes 
names(imagery_cropped) = c("r","g","b")

# Convert imagery bands to matrix
imagery_r_cropped = rayshader::raster_to_matrix(imagery_cropped$r)
imagery_g_cropped = rayshader::raster_to_matrix(imagery_cropped$g)
imagery_b_cropped = rayshader::raster_to_matrix(imagery_cropped$b)

imagery_array = array(0,dim=c(nrow(imagery_r_cropped),ncol(imagery_r_cropped),3))

imagery_array[,,1] = imagery_r_cropped/255 #Red layer
imagery_array[,,2] = imagery_g_cropped/255 #Blue layer
imagery_array[,,3] = imagery_b_cropped/255 #Green layer

imagery_array = aperm(imagery_array, c(2,1,3))

# Add contrast
imagery_contrast = scales::rescale(imagery_array,to=c(0,1))

plot_map(imagery_contrast)

# Clear RGL PLOT WINDOW
rgl::rgl.clear()

# Static Plot # Default settings from rayshader tutorial 
plot_3d(imagery_contrast, elev_matrix, windowsize = c(1100,900), zscale = 2, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_snapshot(title_text = "Vernon County, Wisconsin | Imagery: NAIP17 | DEM: 3m LIDAR",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)

# In Motion # Default settings from rayshader tutorial  
angles= seq(0,360,length.out = 1441)[-1]
for(i in 1:1440) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("vernon%i.png", i), 
                  title_text = "Vernon, Wisconsin | Imagery: NAIP17 | DEM: 3m LIDAR",
                  title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
}
rgl::rgl.close()

av::av_encode_video(sprintf("vernon%d.png",seq(1,1440,by=1)), framerate = 60, output = "vernon2.mp4")

rgl::rgl.close()





