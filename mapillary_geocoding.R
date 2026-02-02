

#############################################################################################################
#############################################################################################################

# Libraries

library(readr)
library(dplyr)
library(tidygeocoder)

#############################################################################################################
#############################################################################################################

# Read in Data

mapillary_coords <- read_csv("C:/Users/daria/Desktop/mapillary_all_43k_coords.csv")

View(mapillary_coords)

# format ID, get rid of scientific numbers

mapillary_coords$id <- format(mapillary_coords$id, scientific = FALSE, trim = TRUE)

#############################################################################################################
#############################################################################################################

# Create batches for geocoding

mapillary_coords <- mapillary_coords %>%
  mutate(batch_id = ntile(row_number(), 5))

#############################################################################################################
#############################################################################################################

# Geocode Batch 1

library(tidygeocoder)

batch1 <- mapillary_coords %>% filter(batch_id == 1)

geo1 <- reverse_geocode(
  batch1,
  lat = lat,
  long = lon,
  method = "osm",
  full_results = TRUE
)

saveRDS(geo1, "mapillary_geo_batch1.rds")


# Geocode Batch 2 

batch2 <- mapillary_coords %>%
  filter(batch_id == 2)

geo2 <- reverse_geocode(
  batch2,
  lat = lat,
  long = lon,
  method = "osm",
  full_results = TRUE
)

saveRDS(geo2, "mapillary_geo_batch2.rds")


# Geocode Batch 3

batch3 <- mapillary_coords %>%
  filter(batch_id == 3)

geo3 <- reverse_geocode(
  batch3,
  lat = lat,
  long = lon,
  method = "osm",
  full_results = TRUE
)

# saveRDS(geo3, "mapillary_geo_batch3.rds")

# Geocode Batch 4

batch4 <- mapillary_coords %>%
  filter(batch_id == 4)

geo4 <- reverse_geocode(
  batch4,
  lat = lat,
  long = lon,
  method = "osm",
  full_results = TRUE
)

# saveRDS(geo4, "mapillary_geo_batch4.rds")

# Geocode Batch 5

batch5 <- mapillary_coords %>%
  filter(batch_id == 5)

geo5 <- reverse_geocode(
  batch5,
  lat = lat,
  long = lon,
  method = "osm",
  full_results = TRUE
)

# saveRDS(geo5, "mapillary_geo_batch5.rds")


