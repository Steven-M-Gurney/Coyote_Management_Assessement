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
#points <- st_read("ProblemSites.shp")        # Site points (WGS84)
points_df <- read.csv("Coyote_PerimeterMeasurements_05Jul2025.csv")
# Convert to sf object (assuming your CSV has lon/lat columns)
points <- st_as_sf(points_df, coords = c("Longitude", "Latitude"), crs = 4326)

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
p <- ggplot() +
  geom_raster(data = nlcd_df, aes(x = x, y = y, fill = landcover_class)) +
  scale_fill_manual(
    name = "Land cover type",
    values = c(
      "Water"     = "#5DADE2",
      "Open"      = "#82E0AA",
      "Developed" = "#E74C3C",
      "Forest"    = "#196F3D"
    )
  ) +
  coord_sf(crs = st_crs(nlcd)) +
  scale_x_continuous(labels = function(x) format(x, trim = TRUE)) +
  scale_y_continuous(labels = function(y) format(y, trim = TRUE)) +
  theme_minimal(base_size = 16) +  # Sets a larger base font size
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 14)
  ) +
  labs(
    #title = "Reclassified Land Cover",
    x = NULL, y = NULL
  )

# Take a look
p

# Save the plot
ggsave("Plot_reclassified_land_cover_map.png", plot = p, width = 8, height = 6, dpi = 300)

# Calculate pixel area
pixel_area <- res(nlcd_reclass)[1] * res(nlcd_reclass)[2]  # e.g., 30m x 30m = 900 m²

# Get pixel values
vals <- values(nlcd_reclass, na.rm = TRUE)

# Create data frame of counts
landcover_counts <- as.data.frame(table(vals))
colnames(landcover_counts) <- c("Class", "Count")

# Match class names
landcover_labels <- c("1" = "Water", "2" = "Open", "3" = "Developed", "4" = "Forest")
landcover_counts$Class <- as.character(landcover_counts$Class)
landcover_counts$Category <- landcover_labels[landcover_counts$Class]

# Add area and percent
total_area_m2 <- sum(landcover_counts$Count) * pixel_area
landcover_counts <- landcover_counts %>%
  mutate(
    Area_m2 = Count * pixel_area,
    Area_km2 = Area_m2 / 1e6,
    Percent = (Area_m2 / total_area_m2) * 100
  )

# Print result
print(landcover_counts)
cat("Total Area (km²):", total_area_m2 / 1e6, "\n")


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
p3 <- ggplot() +
  geom_raster(data = nlcd_df, aes(x = x, y = y, fill = landcover_class)) +
  scale_fill_manual(
    name = "Land cover type",
    values = c(
      "Natural"    = "palegreen4",
      "Developed"  = "#d9d9d9"
    )
  ) +
  geom_sf(data = buffers, fill = NA, color = "#FFD700", size = 0.8) +
  geom_sf(data = points_proj, shape = 21, fill = "#FFD700", color = "black", size = 2) +
  coord_sf(crs = st_crs(nlcd)) +
  scale_x_continuous(labels = function(x) format(x, trim = TRUE)) +
  scale_y_continuous(labels = function(y) format(y, trim = TRUE)) +
  theme_minimal(base_size = 16) +  # Sets a larger base font size
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 14)
  ) +
  labs(x = NULL, y = NULL)

# Take a look
p3

# Save the plot
ggsave("Plot_buffers.png", plot = p3, width = 8, height = 6, dpi = 300)

# Recalculate pixel area (should be the same)
pixel_area_bin <- res(nlcd_reclass)[1] * res(nlcd_reclass)[2]

# Get pixel values
vals_bin <- values(nlcd_reclass, na.rm = TRUE)

# Count values
bin_counts <- as.data.frame(table(vals_bin))
colnames(bin_counts) <- c("Class", "Count")
bin_counts$Class <- as.character(bin_counts$Class)
bin_counts$Category <- c("Developed", "Natural")[as.integer(bin_counts$Class) + 1]

# Compute area and percent
total_area_bin <- sum(bin_counts$Count) * pixel_area_bin
bin_counts <- bin_counts %>%
  mutate(
    Area_m2 = Count * pixel_area_bin,
    Area_km2 = Area_m2 / 1e6,
    Percent = (Area_m2 / total_area_bin) * 100
  )

# Print result
print(bin_counts)
cat("Total Area (km²):", total_area_bin / 1e6, "\n")

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


# ---------------------------
# 📂 Load Perimeter-Gap Data
# ---------------------------
df <- read.csv("Coyote_PerimeterMeasurements_05Jul2025.csv")

# ---------------------------
# 🔢 Quick Summaries
# ---------------------------

# Total surveyed sites
nrow(df)  # e.g., 93

# Count non-compliant sites under SP20
sum(df$SP20.compliant == "N", na.rm = TRUE)  # e.g., 67

# Count non-compliant under 2025 recommendations
sum(df$Meets.reccomendations == "N", na.rm = TRUE)  # e.g., 74

# Summarize non-compliance by site type
df %>%
  group_by(Site.type) %>%
  summarise(
    Total   = n(),
    SP20_N  = sum(SP20.compliant == "N", na.rm = TRUE),
    Rec_N   = sum(Meets.reccomendations == "N", na.rm = TRUE)
  )

# Summarize SP20 non-compliance counts by Site.type
summary_df <- df %>%
  group_by(Site.type) %>%
  summarise(SP20_N = sum(SP20.compliant == "N", na.rm = TRUE)) %>%
  filter(SP20_N > 0)  # Optional: remove types with 0

# Match your custom color palette
custom_colors <- c(
  "Hydrologic" = "#3288bd",
  "Gate"       = "gray",
  "Fence"      = "#fee08b"
)

# Make sure Site.type is a factor and in desired order (optional)
summary_df$Site.type <- factor(summary_df$Site.type, levels = names(custom_colors))

# Plot
ggplot(summary_df, aes(x = Site.type, y = SP20_N, fill = Site.type)) +
  geom_col(width = 0.7) +
  scale_fill_manual(
    name = "Site Type",
    values = custom_colors
  ) +
  labs(
    x = "Site Type",
    y = "# of SP20 Non-Compliant Sites"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"  # remove if bars are self-explanatory
  )


# ---------------------------
# 🧹 Filter Problematic Sites
# ---------------------------
df <- df %>%
  filter(Meets.reccomendations == "N")

# Save filtered set
write.csv(df, "Coyote_ProblemSites_05Jul2025.csv", row.names = FALSE)

# ---------------------------
# 🔄 Reshape to Long Format
# ---------------------------
df_long <- df %>%
  pivot_longer(
    cols = c(Max.gap.height, Max.gap.width),
    names_to = "Gap.type",
    values_to = "Gap.value"
  )

# ---------------------------
# 📊 Gap Summary by Site Type
# ---------------------------
df_long %>%
  group_by(Site.type) %>%
  summarise(
    Gap.min = min(Gap.value, na.rm = TRUE),
    Gap.max = max(Gap.value, na.rm = TRUE),
    Gap.avg = mean(Gap.value, na.rm = TRUE),
    Gap.sd  = sd(Gap.value, na.rm = TRUE)
  )

# ---------------------------
# 🔁 Standardize Column Names
# ---------------------------
# Replace '.' with '_' to match final_output
names(df) <- gsub("\\.", "_", names(df))
names(final_output) <- gsub("\\.", "_", names(final_output))

# ---------------------------
# 🔁 Ensure Matching Types for Unique_ID
# ---------------------------
# Make sure Unique_ID is the same type in both dataframes
df <- df %>% mutate(Unique_ID = as.character(Unique_ID))
final_output <- final_output %>% mutate(Unique_ID = as.character(Unique_ID))

# ---------------------------
# 🔗 Merge df with final_output
# ---------------------------

# Select only the columns you need from final_output
natural_cover <- final_output %>% select(Unique_ID, pct_natural)

# Join only pct_natural to df
df <- df %>%
  left_join(natural_cover, by = "Unique_ID")

# ---------------------------
# 💾 Export or Inspect Merged Output
# ---------------------------
print(df)

# Optionally save to CSV
write.csv(df, "Merged_SiteData_With_NaturalCover.csv", row.names = FALSE)


########################################
############# SITE RANKING ##############
#########################################

# 🧮 Prepare data for ranking priority.
# Calculate average gap size using max height and width.
df <- df %>%
  mutate(Gap.mean = rowMeans(across(c(Max_gap_height, Max_gap_width)), na.rm = TRUE)) %>%
  
  # 🧹 Remove sites that have been fixed already (601 and 1401).
  filter(!Unique_ID %in% c(601, 1401))

# Address NAs that should be true Ns
df <- df %>%
  mutate(
    Confirmed_access = replace_na(Confirmed_access, "N"),
    Nearby_sign      = replace_na(Nearby_sign, "N")
  )

# 🎯 Define weights for different ranking criteria.
#w_access <- 0.5  # Known coyote access
w_gap    <- 0.5  # Gap size
w_cover  <- 0.5  # % Natural cover
#w_sign   <- 0.5  # Nearby sign
w_access <- 0.25 # Bonus on top of 1.0 scale above, forces known access to top.

# 🧮 Apply scores and calculate final composite rank.
df_ranked <- df %>%
  mutate(

    
    # Normalize natural cover (0–1 scale).
    Nat.norm = (pct_natural - min(pct_natural, na.rm = TRUE)) / 
      (max(pct_natural, na.rm = TRUE) - min(pct_natural, na.rm = TRUE)),
    
    # Normalize average gap size.
    Gap.norm = (Gap.mean - min(Gap.mean, na.rm = TRUE)) / 
      (max(Gap.mean, na.rm = TRUE) - min(Gap.mean, na.rm = TRUE)),
    
    # Binary score for confirmed access
    Confirmed_access_score = if_else(Confirmed_access == "Y", 1, 0),
    
    # Composite score with weights.
    Composite.score = #Confirmed.access.score * w_access +
      Gap.norm * w_gap +
      Nat.norm * w_cover +
      #Nearby.sign.score * w_sign
      Confirmed_access_score * w_access
    
  ) %>%
  arrange(desc(Composite.score)) %>%
  mutate(Fix.rank = row_number()) %>%
  relocate(Fix.rank, Composite.score, .before = everything())

# Assign priority levels by dividing up data set.
library(classInt)

# Compute 5 natural breaks based on data clusters.
jenks <- classIntervals(df_ranked$Composite.score, n = 5, style = "jenks")

# Assign groups
df_ranked <- df_ranked %>%
  mutate(
    Priority_level = cut(
      Composite.score,
      breaks = jenks$brks,
      labels = c("Very Low", "Low", "Medium", "High", "Very High"),
      include.lowest = TRUE
    )
  )

# Save output
write.csv(df_ranked, "Coyote_SitesRanked_05Jul2025.csv", row.names = FALSE)


########################################
########### SUMMARIZE DATA #############
########################################

# Summarize by priority level
df_ranked %>%
  group_by(Priority_level) %>%
  summarise(
    Total   = n(),
  )

# Now by Site_type and Priority_level
summary_table <- df_ranked %>%
  group_by(Priority_level, Site_type) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  group_by(Priority_level) %>%
  mutate(
    Percent = round(Count / sum(Count) * 100, 1)
  )

print(summary_table)

#count number of Access occurrences 
length(which(df_ranked$Confirmed_access=="Y"))

# Reorder factor levels
df_ranked$Priority_level <- factor(
  df_ranked$Priority_level,
  levels = c("Very High", "High", "Medium", "Low", "Very Low")
)

# Assign custom colors to each Site_type
custom_colors <- c(
  "Hydrologic" = "#3288bd",  
  "Gate"       = "gray",  # green
  "Fence"      = "#fee08b"   # red
)

# Plot
ggplot(df_ranked, aes(x = Priority_level, fill = Site_type)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    name = "Site Type",
    values = custom_colors
  ) +
  labs(
    #title = "Site Type Composition by Priority Level",
    x = "Priority level",
    y = "Proportion of sites"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "right"
  )
