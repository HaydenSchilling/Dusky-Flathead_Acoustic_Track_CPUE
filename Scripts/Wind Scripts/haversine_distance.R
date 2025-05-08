# Function to calculate distance between two points given their latitude and longitude
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  earth_radius <- 6371 # Earth radius in kilometers
  
  # Convert latitude and longitude from degrees to radians
  lat1 <- deg2rad(lat1)
  lon1 <- deg2rad(lon1)
  lat2 <- deg2rad(lat2)
  lon2 <- deg2rad(lon2)
  
  # Calculate differences in latitude and longitude
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  # Haversine formula
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  distance <- earth_radius * c
  
  return(distance)
}