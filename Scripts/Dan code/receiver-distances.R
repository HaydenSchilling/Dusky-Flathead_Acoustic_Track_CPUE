# 1. Housekeeping ------------------------------------------------------------ #
# Load packages
library(tidyverse)         # General data-wrangling
#library(readxl)            # Opening .xls and .xlsx extensions
library(sf)                # Simple features (spatial data)
library(raster)            # More spatial data handling
library(fasterize)         # For working with rasters
library(janitor)           # General tidying functions
#library(lunar)             # For calculating lunar phase
library(ggspatial)

# # List the files
# files <- list.files(path = "data/", full.names = TRUE)
# files

# Open the array files
shoalhaven <- read_csv("Dusky_Env_Analysis/Scripts/Dan code/shoalhaven-distances.csv") %>% clean_names()
clyde <- read_csv("Dusky_Env_Analysis/Scripts/Dan code/clyde-distances.csv") %>% clean_names()

# Open the coastline shapefile
coast <- st_read("Dusky_Env_Analysis/Scripts/Dan code/Coastaline_WGS84_MGA56_NSWOEH.shp")
# ---------------------------------------------------------------------------- #

# 2. Exploratory plotting ---------------------------------------------------- #
# Shoalhaven array
ggplot() +
  geom_sf(data = coast) +
  geom_point(data = shoalhaven, aes(x = longitude, y = latitude)) +
  coord_sf(xlim = c(min(shoalhaven$longitude), max(shoalhaven$longitude)),
           ylim = c(min(shoalhaven$latitude), max(shoalhaven$latitude))) +
  annotation_scale()

# For Shoalhaven max(longitude) will return the receiver approximately at the 
# mouth.

# Clyde
ggplot() +
  geom_sf(data = coast) +
  geom_point(data = clyde, aes(x = longitude, y = latitude)) +
  coord_sf(xlim = c(min(clyde$longitude), max(clyde$longitude + 0.5)),
           ylim = c(min(clyde$latitude), max(clyde$latitude)))

# For Clyde the most upstream recevier is ever so slightly wider than the one at
# the mouth. The mouth receiver is clyde$standard_name == St.1.
# ---------------------------------------------------------------------------- #

# 3. Caclulate distances ----------------------------------------------------- #
# Make shapefile 'valid'
coast <- st_make_valid(coast)

# Get rid of the water polygon
coast <- coast %>% filter(ID_NAME != "Water")

for(i in 1:2){
  
  # Assign array dataframe to generic `data` object for looping
  if(i == 1){
    # Create object
    data <- shoalhaven
    
    # Set a 'reference' cell (i.e., the one we will calculate distances from)
    # This is chosen by looking at the `fas_points` object created below, then 
    # setting this value as the row index that matches the coordinates of the 
    # receiver/location you want to use.
    reference <- 1 
    
  } else {
    
    # Create object
    data <- clyde
    
    # Set a 'reference' cell (i.e., the one we will calculate distances from)
    # Turns out they're the same, but this isn't guaranteed so I've left it
    # inside this conditional.
    reference <- 1
  
  }
  
  # Define boxes around the arrays
  bbox <- st_bbox(c(xmin = min(data$longitude) - 0.5, 
                              ymin = min(data$latitude) - 0.5, 
                              xmax = max(data$longitude) + 0.1, 
                              ymax = max(data$latitude)) + 0.1, 
                            crs = st_crs(coast))
  
  # Convert the bounding box to a spatial object
  bbox_polygon <- st_as_sfc(bbox)
  
  # Clip the coastfile
  clip_coast <- coast %>% st_intersection(bbox_polygon)
  
  # Plot it again to see it worked
  ggplot() + geom_sf(data = clip_coast)
  
  # Create a raster
  ras <- raster(extent(clip_coast), nrows = 10000, ncols = 10000)
  
  # Convert it to a faster (whatever that is...)
  fas <- fasterize(summarize(clip_coast), ras)
  
  # Add the CRS to the raster
  crs(fas) <- crs(coast)
  
  # Convert the receiver locations to an sf object
  points <- st_as_sf(data, coords = c("longitude", "latitude"), crs = crs(coast))
  
  # Duplicate the raster for calculations
  fas_points <- fas
  
  # Get the coordinates out of the spatial data frame
  xy <- st_coordinates(points)
  
  # Identify the cells that the points fall in
  cells <- cellFromXY(fas, xy)
  
  # Set an 'origin' value
  origin <- 2
  
  # Assign the value to the reference cell
  fas_points[cells[reference]] <- origin
  
  # Omit any cells with a value = 1 (i.e., land)
  omit <- 1
  
  # Now calculate the distances (in metres, this takes a long while...)
  distances <- fas_points %>% raster::gridDistance(origin = origin, omit = omit)
  
  # Now get out the distances at our receivers
  distances <- distances[cells]
  
  # Append these to the array dataframe
  if(i == 1){
    shoalhaven <- data %>% bind_cols(distance_to_mouth = distances)
  } else {
    clyde <- data %>% bind_cols(distance_to_mouth = distances) 
  }
}

# Save the files
write_csv(x = shoalhaven, file = "Dusky_Env_Analysis/Scripts/Dan code/shoalhaven-distances2.csv")
write_csv(x = clyde, file = "Dusky_Env_Analysis/Scripts/Dan code/clyde-distances2.csv")
# ---------------------------------------------------------------------------- #