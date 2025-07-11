# ---------------------------
# 📦 Load Required Libraries
# ---------------------------
library(terra)
library(sf)
library(dplyr)
library(exactextractr)
library(tidyr)
library(ggplot2)
library(viridis)  # For clean color scale
library(purrr)

# ---------------------------
# 📂 Load Data
# ---------------------------
points <- st_read("ProblemSites.shp")        # Site points (WGS84)
nlcd   <- rast("NLCD.tif")                   # Full NLCD raster
wayne  <- st_read("WayneCounty.shp")         # Wayne County boundary

# ---------------------------
# 🗺️ Project Spatial Data to Match NLCD
# ---------------------------
points_proj <- st_transform(points, crs(nlcd))
wayne_proj  <- st_transform(wayne, crs(nlcd))
wayne_vect  <- vect(wayne_proj)

# ---------------------------
# ✂️ Clip NLCD Raster to Wayne County
# ---------------------------
nlcd_clipped <- mask(crop(nlcd, wayne_vect), wayne_vect)

# ---------------------------
# 🔵 Create 1.26 km Buffers Around ProblemSites
# ---------------------------
buffers <- st_buffer(points_proj, dist = 1260)
buffers$site_id <- 1:nrow(buffers)

# ---------------------------
# 📦 Expand Clipping Extent Slightly Around Buffers
# ---------------------------
buffer_bbox <- st_bbox(buffers)
margin <- 500  # meters

expanded_ext <- ext(
  buffer_bbox$xmin - margin,
  buffer_bbox$xmax + margin,
  buffer_bbox$ymin - margin,
  buffer_bbox$ymax + margin
)

# Clip NLCD again using the extended extent (overrides earlier Wayne mask)
nlcd_crop <- crop(nlcd, expanded_ext)

# ---------------------------
# 🔁 Reclassify Raster into 4 Categories
# ---------------------------
# 1 = Water     (11, 12, 90, 95)
# 2 = Open      (21-22, 31, 51, 52, 71–74, 81–82)
# 3 = Developed (23–24)
# 4 = Forest    (41–43)
rcl <- matrix(c(
  11, 1, 12, 1, 90, 1, 95, 1,
  21, 2, 22, 2, 31, 2, 51, 2, 52, 2, 71, 2, 72, 2, 73, 2, 74, 2, 81, 2, 82, 2,
  22, 3, 23, 3, 24, 3,
  41, 4, 42, 4, 43, 4
), ncol = 2, byrow = TRUE)

# Classify NLCD into new categories
nlcd_reclass <- classify(nlcd_crop, rcl)

# Set unmatched values to NA (fixing the previous mask error)
valid_classes <- rcl[,1]
nlcd_vals <- unique(values(nlcd_crop))
nlcd_reclass[!(nlcd_crop %in% valid_classes)] <- NA

# ---------------------------
# 🗺️ Map to Verify Reclassification and Overlay
# ---------------------------
# Convert raster to data frame for ggplot
nlcd_df <- as.data.frame(nlcd_reclass, xy = TRUE, na.rm = TRUE)
colnames(nlcd_df)[3] <- "landcover_class"


# Create named factor labels
landcover_labels <- c(
  "1" = "Water",
  "2" = "Open",
  "3" = "Developed",
  "4" = "Forest"
)

# Convert raster to data frame for ggplot
nlcd_df <- as.data.frame(nlcd_reclass, xy = TRUE, na.rm = TRUE)
colnames(nlcd_df)[3] <- "landcover_class"

# Convert to factor with labels
nlcd_df$landcover_class <- factor(
  nlcd_df$landcover_class,
  levels = names(landcover_labels),
  labels = landcover_labels
)

# Plot
ggplot() +
  geom_raster(data = nlcd_df, aes(x = x, y = y, fill = landcover_class)) +
  scale_fill_manual(
    name = "Land Cover Type",
    values = c(
      "Water"     = "#5DADE2",
      "Open"      = "#82E0AA",
      "Developed" = "#E74C3C",
      "Forest"    = "#196F3D"
    )
  ) +
  geom_sf(data = buffers, fill = NA, color = "yellow", size = 0.8) +
  geom_sf(data = points_proj, shape = 21, fill = "white", color = "black", size = 2) +
  coord_sf(crs = st_crs(nlcd)) +
  theme_minimal() +
  labs(
    title = "Problem Sites, Buffers, and Reclassified Land Cover",
    x = "Easting (m)", y = "Northing (m)"
  )

# ---------------------------
# 🗺️ Map Natural Habitat
# ---------------------------

# NLCD class codes to binary categories:
# 1 = Natural (Water, Open, Forest)
# 0 = Developed

rcl_bin <- matrix(c(
  11, 1, 12, 1, 90, 1, 95, 1,      # Water
  21, 1, 22, 1, 31, 1, 51, 1, 52, 1,      # Open
  71, 1, 72, 1, 73, 1, 74, 1,
  81, 1, 82, 1,
  41, 1, 42, 1, 43, 1,             # Forest
  22, 0, 23, 0, 24, 0              # Developed
), ncol = 2, byrow = TRUE)


# Classify NLCD into new categories
nlcd_reclass <- classify(nlcd_crop, rcl_bin)

# Convert raster to data frame for ggplot
nlcd_df <- as.data.frame(nlcd_reclass, xy = TRUE, na.rm = TRUE)
colnames(nlcd_df)[3] <- "landcover_class"

# Convert to factor with binary labels (Natural = 1, Developed = 0)
nlcd_df$landcover_class <- factor(
  nlcd_df$landcover_class,
  levels = c(1, 0),
  labels = c("Natural", "Developed")
)

# Plot
ggplot() +
  geom_raster(data = nlcd_df, aes(x = x, y = y, fill = landcover_class)) +
  scale_fill_manual(
    name = "Land Cover Type",
    values = c(
      "Natural"    = "palegreen4",
      "Developed"  = "#d9d9d9"
    )
  ) +
  geom_sf(data = buffers, fill = NA, color = "orange", size = 0.8) +
  geom_sf(data = points_proj, shape = 21, fill = "orange", color = "black", size = 2) +
  coord_sf(crs = st_crs(nlcd)) +
  theme_minimal() +
  labs(
    title = "Problem Sites and Coyote Home-Range Buffers",
    x = "Easting (m)", y = "Northing (m)"
  )

# ---------------------------
# 🧮 Calculate % Land Cover per Buffer
# ---------------------------
# Assuming:
# - nlcd_reclass: SpatRaster with values 0 (Developed) and 1 (Natural)
# - buffers: sf polygons of buffer zones around points

# Convert buffers to Spatial for exactextractr
buffers_sp <- as(buffers, "Spatial")

# Calculate pixel area (in map units squared)
pixel_area <- res(nlcd_reclass)[1] * res(nlcd_reclass)[2]  # e.g. 30 * 30 = 900 m²

# Use exact_extract to get fractional coverages, then calculate % natural by area
natural_area_list <- exact_extract(nlcd_reclass, buffers_sp, function(values, coverage_fractions) {
  # total pixel area inside polygon = sum of fractional coverages * pixel_area
  total_area <- sum(coverage_fractions, na.rm = TRUE) * pixel_area
  
  # natural pixel area = sum fractional coverage where raster value == 1 * pixel_area
  natural_area <- sum(coverage_fractions[values == 1], na.rm = TRUE) * pixel_area
  
  # % natural cover = natural_area / total_area * 100
  pct_natural <- (natural_area / total_area) * 100
  
  return(pct_natural)
})

# Prepare output data.frame with site ids (assuming buffers rows correspond to sites)
natural_cover_df <- data.frame(
  site_id = seq_along(natural_area_list),
  pct_natural = unlist(natural_area_list)
)

# Join with buffer attributes (remove geometry for join)
buffers_df <- buffers %>% 
  st_drop_geometry() %>%
  mutate(site_id = row_number())

final_output <- buffers_df %>%
  left_join(natural_cover_df, by = "site_id") %>%
  arrange(desc(pct_natural))

# View or save results
print(final_output)

# Optionally save CSV
write.csv(final_output, "ProblemSites_NaturalCover_Percentages.csv", row.names = FALSE)
