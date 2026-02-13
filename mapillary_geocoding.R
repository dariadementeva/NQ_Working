

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
gare_coords <- read_csv("C:/Users/daria/Desktop/mapillary_gare_coords.csv")
belair_coords <- read_csv("C:/Users/daria/Desktop/mapillary_belair_coords.csv")
vh_coords <- read_csv("C:/Users/daria/Desktop/mapillary_vh_coords.csv")

View(mapillary_coords)

# format ID, get rid of scientific numbers

mapillary_coords$id <- format(mapillary_coords$id, scientific = FALSE, trim = TRUE)
gare_coords$id <- format(gare_coords$id, scientific = FALSE, trim = TRUE)
belair_coords$id <- format(belair_coords$id, scientific = FALSE, trim = TRUE)
vh_coords$id <- format(vh_coords$id, scientific = FALSE, trim = TRUE)


#############################################################################################################
#############################################################################################################

# Create batches for geocoding

mapillary_coords <- mapillary_coords %>%
  mutate(batch_id = ntile(row_number(), 5))

#############################################################################################################
#############################################################################################################

# FYI-1: Takes long to run, each batch will take approx. 3h 
# FYI-2: OSM Nominatim geocoder may block you if you geocode multiple batches per day: avoid doing so.
# FYI-3: To play safe, do a batch per day

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

saveRDS(geo3, "mapillary_geo_batch3.rds")

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

saveRDS(geo4, "mapillary_geo_batch4.rds")

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

saveRDS(geo5, "mapillary_geo_batch5.rds")

# Geocode The Gare Batch 


gare_batch <- reverse_geocode(
  gare_coords,
  lat = lat,
  long = lon,
  method = "osm",
  full_results = TRUE
)

saveRDS(gare_batch, "mapillary_gare_batch.rds")

# Geocode The Belair Batch 


belair_batch <- reverse_geocode(
  belair_coords,
  lat = lat,
  long = lon,
  method = "osm",
  full_results = TRUE
)

saveRDS(belair_batch, "mapillary_belair_batch.rds")


# Geocode The VH Batch 

vh_batch <- reverse_geocode(
  vh_coords,
  lat = lat,
  long = lon,
  method = "osm",
  full_results = TRUE
)

saveRDS(vh_batch, "mapillary_vh_batch.rds")


#############################################################################################################
#############################################################################################################

# Filter for NAs in the street entries

#############################################################################################################
#############################################################################################################

# helper, identify NAs in the street entries

get_na_rows <- function(df, column) {

  if (!column %in% names(df)) {
    stop(paste("Column", column, "not found in data frame"))
  }
  
  na_indices <- which(is.na(df[[column]]))
  
  na_df <- df[na_indices, ]
  
  return(list(
    na_count = length(na_indices),
    na_indices = na_indices,
    na_rows = na_df
  ))
}

# helper, subset by NAs 

get_na_subset <- function(df, column) {
  df[is.na(df[[column]]), ]
}

geo1_na <- get_na_rows(geo1, "road") # n = 28 

geo1_na_df <- get_na_subset(geo1, "road")

table(geo1_na_df$county) # Luxembourg, 3 NAs


geo2_na <- get_na_rows(geo2, "road") # n = 267 

geo2_na_df <- get_na_subset(geo2, "road")

table(geo2_na_df$county) # Luxembourg, 18 NAs


geo3_na <- get_na_rows(geo3, "road") # n = 31 

geo3_na_df <- get_na_subset(geo3, "road")

table(geo3_na_df$county) # Luxembourg, 31 NAs


geo4_na <- get_na_rows(mapillary_geo_batch4, "road") # n = 28

geo4_na_df <- get_na_subset(mapillary_geo_batch4, "road")

table(geo4_na_df$county) # Luxembourg, 28 NAs


geo5_na <- get_na_rows(mapillary_geo_batch5, "road") # n = 23 

geo5_na_df <- get_na_subset(mapillary_geo_batch5, "road")

table(geo5_na_df$county) # Luxembourg, 23 NAs

#############################################################################################################
#############################################################################################################

# Create names for pictures

#############################################################################################################
#############################################################################################################

# Filter only for Canton Luxembourg

geo1_lux <- geo1[geo1$county=="Canton Luxembourg", ]
geo2_lux <- geo2[geo2$county=="Canton Luxembourg", ]
geo3_lux <- geo3[geo3$county=="Canton Luxembourg", ]
geo4_lux <- mapillary_geo_batch4[mapillary_geo_batch4$county=="Canton Luxembourg", ]
geo5_lux <- mapillary_geo_batch5[mapillary_geo_batch5$county=="Canton Luxembourg", ]


# Create image_name column

geo1_lux$image_name <- rep(NA, nrow(geo1_lux))
geo2_lux$image_name <- rep(NA, nrow(geo2_lux))
geo3_lux$image_name <- rep(NA, nrow(geo3_lux))
geo4_lux$image_name <- rep(NA, nrow(geo4_lux))
geo5_lux$image_name <- rep(NA, nrow(geo5_lux))


# Create helper function 

make_image_names <- function(df, road_col = "road", address_col = "address") {
  
  # helper, sanitize the  names
  sanitize_to_underscores <- function(x) {
    x <- trimws(x)
    x <- gsub("\\s+", "_", x)
    x <- gsub("[^[:alnum:]_]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    x
  }
  
  # helper, create base name: road if present, otherwise first address token
  base_name <- ifelse(
    !is.na(df[[road_col]]) & trimws(df[[road_col]]) != "",
    df[[road_col]],
    sub(",.*$", "", df[[address_col]])
  )
  
  base_name <- sanitize_to_underscores(base_name)
  
  # make unique with suffixes _1, _2, _3
  image_name <- ave(base_name, base_name, FUN = function(v) {
    if (length(v) == 1) return(v)
    paste0(v, c("", paste0("_", seq_len(length(v) - 1))))
  })
  
  return(image_name)
  
}


geo1_lux$image_name <- make_image_names(geo1_lux)
geo2_lux$image_name <- make_image_names(geo2_lux)
geo3_lux$image_name <- make_image_names(geo3_lux)
geo4_lux$image_name <- make_image_names(geo4_lux)
geo5_lux$image_name <- make_image_names(geo5_lux)

#############################################################################################################
#############################################################################################################

# Sanity checks and save all

#############################################################################################################
#############################################################################################################


get_na_rows(geo1_lux, "image_name") # 0
get_na_rows(geo2_lux, "image_name") # 0
get_na_rows(geo3_lux, "image_name") # 0
get_na_rows(geo4_lux, "image_name") # 0
get_na_rows(geo5_lux, "image_name") # 0

# stack the datasets

geo_all_lux <- bind_rows(
  geo1_lux,
  geo2_lux,
  geo3_lux,
  geo4_lux,
  geo5_lux
)

# check for non-unique image IDs

nrow(geo_all_lux) # n = 36118
sum(length(unique(geo_all_lux$id))) # n = 36118

# Save 

saveRDS(geo_all_lux, "mapillary_luxembourg_geocoded_renamed.rds")

write_csv(geo_all_lux, "geo_all_lux.csv")

install.packages(c("leaflet", "leaflet.extras"))
library(leaflet)
library(leaflet.extras)

leaflet(geo_all_lux) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addHeatmap(
    lng = ~lon, lat = ~lat,
    blur = 20,
    radius = 15,
    max = 1
  ) 

df <- data.frame(
  lon = c(6.05, 6.20),
  lat = c(49.55, 49.67))


leaflet(df) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addHeatmap(
    lng = ~lon, lat = ~lat,
    blur = 20,
    radius = 7,
    max = 1
  ) 


lux_neighborhoods <- st_read(
  "C:/Users/daria/Desktop/Luxembourg_Ville/Quartier_Luxembourg_Ville/24_quartiers_LuxVille_Generalises.shp"
)


belair_boundary <- lux_neighborhoods[lux_neighborhoods$POPO=="Belair",]
ville_haut_boundary <- lux_neighborhoods[lux_neighborhoods$POPO=="Ville Haute", ]
gare_boundary <- lux_neighborhoods[lux_neighborhoods$POPO=="Gare",]


st_write(belair_boundary, "belair_boundary.shp")
st_write(ville_haut_boundary, "ville_haut_boundary.shp")
st_write(gare_boundary, "gare_boundary.shp")




leaflet(gare_coords) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addHeatmap(
    lng = ~lon, lat = ~lat,
    blur = 20,
    radius = 15,
    max = 1
  ) 

leaflet(belair_coords) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addHeatmap(
    lng = ~lon, lat = ~lat,
    blur = 20,
    radius = 15,
    max = 1
  ) 

leaflet(vh_coords) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addHeatmap(
    lng = ~lon, lat = ~lat,
    blur = 20,
    radius = 15,
    max = 1
  ) 


leaflet(geo_all_lux) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addHeatmap(
    lng = ~lon, lat = ~lat,
    blur = 20,
    radius = 15,
    max = 1
  ) 

