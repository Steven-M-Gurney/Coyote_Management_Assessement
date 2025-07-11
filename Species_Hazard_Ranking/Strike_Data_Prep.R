#####################################
###### Prepare FAA Strike Data ######
#####################################

# Load packages.
library(tidyverse)

# Read in recent data exported from FAA database.
# Note these data may be pending info and may be later updated in the FAA database.
faa <- read.csv("FAA_Wildlife_Export_21Feb2025.csv")

# Look at column names to see what we're working with.
print(colnames(faa))

# Filter rows where INCIDENT_YEAR is between 2016 and 2024.
# This aligns data with inception of WCAA wildlife program; and makes within most recent decade.
faa <- faa %>%
 filter(INCIDENT_YEAR >= 2016 & INCIDENT_YEAR <= 2024)

# Select columns of interest (very similar to WCAA subset, but some differences).
faa <- faa %>% select(INDX_NR, # Note no unique identifier to tie to WCAA data.
                      INCIDENT_YEAR, INCIDENT_MONTH,
                      SPECIES, NUM_STRUCK, # No guild option here.
                      RUNWAY,
                      HEIGHT, # Use to filter to "airfield environment"
                      REG, # Helps identify pilot-reported strikes.
                      REMAINS_SENT, # TRUE means it was sent to Smithsonian.
                      INDICATED_DAMAGE, DAMAGE_LEVEL, # Damage level is based on an FAA ranking system.
                      EFFECT,
                      EFFECT_OTHER,
                      COST_REPAIRS,
                      AOS, # Time aircraft was out of service (blank = unknown).
                      COST_OTHER,
                      COST_REPAIRS_INFL_ADJ, # Costs adjusted to the most recent year based on Consumer Price Index, U.S. Department of Labor. Inflation-adjusted costs are updated annually
                      COST_OTHER_INFL_ADJ, # Costs adjusted to the most recent year based on Consumer Price Index, U.S. Department of Labor. Inflation-adjusted costs are updated annually
                      REMARKS, # Helpful reference!
                      COMMENTS # Helpful resource!
) 

# Look at what species column contains.
print(unique(faa$SPECIES))

# Need to combine some species.
# Aggregate unknown bird variants.
# Rename rows that include both words "Unknown" and "bird" to "UNKNOWN BIRD".
faa <- faa %>%
  mutate(SPECIES = if_else(
    str_detect((SPECIES), "Unknown") & str_detect((SPECIES), "bird"),
    "Unknown bird",
    SPECIES
  ))

# Look at species.
species <- (unique(faa$SPECIES))

# Save list of all species struck for making mass table later.
write.csv(species, "All_Species_Struck.csv", row.names = FALSE)

# Aggregate gull variants.
faa <- faa %>%
  mutate(SPECIES = if_else(
    str_detect((SPECIES), "Gull") | str_detect((SPECIES), "gull"),
    "Gulls",
    SPECIES
  ))

# Aggregate bat variants.
faa <- faa %>%
  mutate(SPECIES = if_else(
    str_detect((SPECIES), "Bats") | str_detect((SPECIES), "bat"),
    "Bats",
    SPECIES
  ))

# Look at species.
print(unique(faa$SPECIES))

# Take a look at AGL
print(unique(faa$HEIGHT))

# Filter < or = 500 like DeVault et al. (2011)
faa <- faa %>%
  filter(HEIGHT <= 500 | is.na(HEIGHT)) # Important to include NAs here!

# Double check AGL
print(unique(faa$HEIGHT))
