####################################
############# PREFACE ##############
####################################

# The purpose of this code is to first summarize perimeter-gap data and then rank sites by priority level. 

# Load R packages for data manipulation.
library(dplyr)
library(tidyr)


################################################
############# GAP-SPACE SUMMARIES ##############
################################################

# Read in the raw site data (manually created from field survey data).
df <- read.csv("Coyote_PerimeterMeasurements_19Jun2025.csv")

# Count the number of rows to see how many sites were surveyed. 
nrow(df) # 93 sites surveyed.

# Of surveyed sites, how many did not meet the original SP20 requirements?
# Count "N" in SP20.compliant column.
sum(df$SP20.compliant == "N", na.rm = TRUE) # 67

# How many sites did not meet 2025 updated SP20 requirements (i.e., our study recommendations)? 
# Count "N" in Meets.recommendations column.
sum(df$Meets.reccomendations == "N", na.rm = TRUE) # 74


# Now do the same as above, but summarize by site type (fence, gate, hydrologic).
# Count the number of sites that did not meet Security's original and updated SP20 requirements; and take a look.
df %>%
  group_by(Site.type) %>%
  summarise(
    Total = n(),
    SP20_N = sum(SP20.compliant == "N", na.rm = TRUE),
    Rec_N  = sum(Meets.reccomendations == "N", na.rm = TRUE)
  )

# Filter out non-problematic sites for further calculations.
df <- df %>%
  filter(Meets.reccomendations == "N")

# Save this filtered data set for Gmapping purposes.
write.csv(df, "Coyote_ProblemSites_24Jun2025.csv", row.names = FALSE)
  
# Reshape data to long format for calculations.
df_long <- df %>%
  pivot_longer(
    cols = c(Max.gap.height, Max.gap.width),
    names_to = "Gap.type",
    values_to = "Gap.value"
  )

# Summarize gap spacing, including range, mean, and standard deviation by site type; and take a look.
df_long %>%
  group_by(Site.type) %>%
  summarise(
    Gap.min = min(Gap.value, na.rm = TRUE),
    Gap.max = max(Gap.value, na.rm = TRUE),
    Gap.avg = mean(Gap.value, na.rm = TRUE),
    Gap.sd  = sd(Gap.value, na.rm = TRUE)
  )


#########################################
############# SITE RANKING ##############
#########################################

# prepare data for ranking priority.
# Create new columns based on row-wise calculation..
# take gap height and gap width and get an average gap size.
df$Gap.mean <- rowMeans(df[, c("Max.gap.height", "Max.gap.width")], na.rm = TRUE)

# Filter out sites that have been fixed (i.e., exclusion problem addressed; sites 601 and 1401).
df <- df %>%
  filter(!Unique.ID %in% c(601, 1401))



# OLD TEST HERE

# Rank the highest mean gap as #1 priority, and so on.
#df <- df %>%
#  mutate(Gap.rank = rank(-Gap.mean, ties.method = "min")) %>%
#  relocate(Gap.rank, .before = everything()) %>%
#  arrange(Gap.rank)

# Working code here, build on it 

# Create a system for weighting different criteria important to priority ranking.
# First, define the weights of criteria.
w_access <- 3 # Known coyote access gets highest weight.
w_gap    <- 2 # Gap size gets the second most weight.
w_sign   <- 1 # Evidence of nearby coyote sign carries some weight.

# Apply scores and rank.
df_ranked <- df %>%
  mutate(
    # Convert Confirmed.access column to score.
    Confirmed.access.score = case_when(
      Confirmed.access == "Y" ~ 3, # If confirmed access is yes, then 3 points.
      Confirmed.access == "N" ~ 2, # If no, 2 points.
      is.na(Confirmed.access) ~ 1, # If NA, 1 point.
      TRUE ~ 0
    ),
    
    # Convert Nearby.sign to score.
    Nearby.sign.score = case_when(
      Nearby.sign == "Y" ~ 3, # If nearby coyote sign confirmed, then 3 points.
      Nearby.sign == "N" ~ 2, # If no, then 2 points.
      is.na(Nearby.sign) ~ 1, # If NA, then 1 point.
      TRUE ~ 0
    ),
    
    # Normalize Gap.mean to a 0–1 scale.
    Gap.norm = (Gap.mean - min(Gap.mean, na.rm = TRUE)) / 
      (max(Gap.mean, na.rm = TRUE) - min(Gap.mean, na.rm = TRUE)),
    
    # Compute weighted composite score.
    Composite.score = Confirmed.access.score * w_access +
      Gap.norm * w_gap +
      Nearby.sign.score * w_sign
  ) %>%
  arrange(desc(Composite.score)) %>%
  mutate(
    Fix.rank = row_number()
  ) %>%
  relocate(Fix.rank, Composite.score, .before = everything())





# NEW STUFF WITH COVER

# Merge % Natural data with above data.
#df <- read.csv("ProblemSites_NaturalCover_Percentages.csv")

# Create a system for weighting different criteria important to priority ranking.
# First, define the weights of criteria.
w_access <- 4 # Known coyote access gets highest weight.
w_gap    <- 3 # Gap size gets the second most weight.
w_cover <- 2 # % Naural cover in the area is also important.
w_sign   <- 1 # Evidence of nearby coyote sign carries some weight.

# Apply scores and rank.
df_ranked <- df %>%
  mutate(
    # Convert Confirmed.access column to score.
    Confirmed.access.score = case_when(
      Confirmed_access == "Y" ~ 3, # If confirmed access is yes, then 3 points.
      Confirmed_access == "N" ~ 2, # If no, 2 points.
      is.na(Confirmed_access) ~ 1, # If NA, 1 point.
      TRUE ~ 0
    ),
    
    # Convert Nearby.sign to score.
    Nearby.sign.score = case_when(
      Nearby_sign == "Y" ~ 3, # If nearby coyote sign confirmed, then 3 points.
      Nearby_sign == "N" ~ 2, # If no, then 2 points.
      is.na(Nearby_sign) ~ 1, # If NA, then 1 point.
      TRUE ~ 0
    ),
    
    # Normalize pct_natural to a 0–1 scale.
    Nat.norm = (pct_natural - min(pct_natural, na.rm = TRUE)) / 
      (max(pct_natural, na.rm = TRUE) - min(pct_natural, na.rm = TRUE)),
    
    # Normalize Gap.mean to a 0–1 scale.
    Gap.norm = (Gap.mean - min(Gap.mean, na.rm = TRUE)) / 
      (max(Gap.mean, na.rm = TRUE) - min(Gap.mean, na.rm = TRUE)),
    
    # Compute weighted composite score.
    Composite.score = Confirmed.access.score * w_access +
      Nat.norm * w_cover +
      Gap.norm * w_gap +
      Nearby.sign.score * w_sign
  ) %>%
  arrange(desc(Composite.score)) %>%
  mutate(
    Fix.rank = row_number()
  ) %>%
  relocate(Fix.rank, Composite.score, .before = everything())

