############################################
###### Strikes by Species 2016 - 2024 ######
############################################

# Count the number of occurrences of each unique value in the species column
species_counts <- as.data.frame(table(faa$SPECIES)) 
colnames(species_counts) <- c("Species", "Strike.Count") # Rename the columns for clarity

# List sums in descending order.
species_counts <- species_counts %>% 
  arrange(desc(Strike.Count))

# Take a look.
print(species_counts) # Check for areas to simplify


#####################################################
###### Damaging Strikes by Species 2016 - 2024 ######
#####################################################

# Filter rows where Damaging.Strike is "Yes"
damage_counts <- faa %>%
  filter(INDICATED_DAMAGE == "TRUE")

# Count the number of occurrences of each unique value in the year column (to sum strikes).
damage_counts <- as.data.frame(table(damage_counts$SPECIES))
colnames(damage_counts) <- c("Species", "Damage.Count") # Rename the columns for clarity

# List sums in descending order.
damage_counts <- damage_counts %>% 
  arrange(desc(Damage.Count))

# take a look.
print(damage_counts)


################################################################
###### % of Strikes with Damage by Species 2016 - 2024 ##########
################################################################

# Merge dataframes
df <- species_counts %>%
  left_join(damage_counts, by = "Species") %>%
  mutate(Damage.Count = replace_na(Damage.Count, 0)) %>% # Replace NA damage with zero
  mutate(Percent.Damaging = ((Damage.Count/Strike.Count) *100)) # Calculate % damaging


###########################################################
###### Substantial Damage by Species 2016 - 2024 ##########
###########################################################

# Need to adjust 2024's turkey vulture strike to substantial (inappropriately marked in FAA)
faa <- faa %>% 
  mutate(DAMAGE_LEVEL = ifelse(INCIDENT_YEAR == 2024 & SPECIES == "Turkey vulture", "S", DAMAGE_LEVEL))

# Filter rows where Damage.Level is Substantial or Destroyed
substatial_damage_counts <- faa %>%
  filter(DAMAGE_LEVEL %in% c("S", "D"))

# Count the number of occurrences of each unique value in the year column (to sum strikes).
substatial_damage_counts <- as.data.frame(table(substatial_damage_counts$SPECIES))
colnames(substatial_damage_counts) <- c("Species", "Substantial.Damage.Count") # Rename the columns for clarity

# List sums in descending order.
substatial_damage_counts <- substatial_damage_counts %>% 
  arrange(desc(Substantial.Damage.Count))

# take a look.
print(substatial_damage_counts)


#############################################################
###### % Substantial Damage by Species 2016 - 2024 ##########
#############################################################

# Merge dataframes
df <- df %>%
  left_join(substatial_damage_counts, by = "Species") %>%
  mutate(Substantial.Damage.Count = replace_na(Substantial.Damage.Count, 0)) %>% # Replace NA substantial damage with zero
  mutate(Percent.Substantial.Damage = ((Substantial.Damage.Count/Strike.Count) *100)) # Calculate % substantial damage


######################################################
###### Effect on Flight by Species 2016 - 2024 ######
######################################################

print(unique(faa$EFFECT)) # Includes "None" and blanks
print(unique(faa$EFFECT_OTHER)) # Includes blanks

# Prep EFFECT column so "None" is blank (that way it will get a FALSE later)
effect_counts <- faa %>%
  mutate(EFFECT = ifelse(EFFECT == " None", "", EFFECT))

# Assign FALSE to all blanks and all NAs, otherwise TRUE (i.e., it contained text).
effect_counts <- effect_counts %>%
  mutate(across(c(EFFECT, EFFECT_OTHER), 
                ~ ifelse(is.na(.) | . == "", FALSE, TRUE)))

# Convert TRUE to 1 and FALSE to 0 for summation.
# If any column in the row has TRUE (i.e., sum > 0), then the new column, disruptive, is set to TRUE.
effect_counts <- effect_counts %>%
  mutate(Effect.Count = rowSums(across(c(EFFECT, EFFECT_OTHER), as.numeric)) > 0
  ) 

# Filter rows where Effect.Count is "Yes"
effect_counts <- effect_counts %>%
  filter(Effect.Count == "TRUE")

# Count the number of occurrences of each unique value in the year column (to sum strikes).
effect_counts <- as.data.frame(table(effect_counts$SPECIES))
colnames(effect_counts) <- c("Species", "Effect.Count") # Rename the columns for clarity

# List sums in descending order.
effect_counts <- effect_counts %>% 
  arrange(desc(Effect.Count))

# take a look.
print(effect_counts)

# Merge dataframes
df <- df %>%
  left_join(effect_counts, by = "Species") %>%
  mutate(Effect.Count = replace_na(Effect.Count, 0)) %>% # Replace NA substantial damage with zero
  mutate(Percent.Effect = ((Effect.Count/Strike.Count) *100)) # Calculate % substantial damage


###########################
###### More cleaning ######
###########################

# Remove killdeer and barn swallow because there was not sufficient info to confirm damage was caused.
# Note that this removes them from the list completely. Remove unknowns too becasue it is uninformative.
# And remove Species struck less than 2 times (this cleans things up a bit).
df <- df %>%
  filter(!Species %in% c("Barn swallow", "Killdeer", "Unknown bird") & Strike.Count > 1)

# Look at number species-groups used for ranking.
print(length(unique(df$Species)))

# Look at the number of species-groups that contain damage or effect.
# Count rows where the sum of the three columns is >= 1
count_rows <- sum(rowSums(df[, c("Damage.Count", "Substantial.Damage.Count", "Effect.Count")], na.rm = TRUE) >= 1)

# Print the result
print(count_rows)


#####################
###### Ranking ######
#####################

# Rank and do not apply any tie-breaking rules.
df <- df %>%
  mutate(
    Damage.Rank = rank(-Percent.Damaging, ties.method = "min"),
    Substantial.Damage.Rank = rank(-Percent.Substantial.Damage, ties.method = "min"),
    Effect.Rank = rank(-Percent.Effect, ties.method = "min"),
    Composite.Score = Damage.Rank + Substantial.Damage.Rank + Effect.Rank, # Sum the 3 metrics
    Composite.Rank = rank(Composite.Score, ties.method = "min") # Rank the sums
  ) %>% 
  arrange(Composite.Rank)

# Slice the top 10 and then round to nearest whole number
df_top10 <- head(df, 10) %>%
  mutate_if(is.numeric, round) # Note that there are questionable horned lark and eastern meadowlark strikes. 


###################################
###### Relative Hazard Score ######
###################################

# Create the Relative.Hazard column by summing the values of Percent.Damaging, Percent.Substantial.Damage, and Percent.Effect.
# Then scale them so that the maximum value is 100.
df_top10 <- df_top10 %>%
  mutate(
    TotalPercent = Percent.Damaging + Percent.Substantial.Damage + Percent.Effect,
    Relative.Hazard = (TotalPercent / max(TotalPercent, na.rm = TRUE)) * 100
  ) %>%
  select(-TotalPercent)  # Remove intermediate column if not needed


###########################
###### Final Results ######
###########################

# Save as csv
write.csv(df_top10, "Hazard_Ranks_18Feb2025.csv", row.names = FALSE)


####################################################
###### Look at Damaging Small Birds ################
###################################################$

# This is just for general or future reference.

# Filter by damaging strikes or effect or other effect (removing NAs, blanks, and None and ignoring spaces)
# These may or may not need to be addressed based on damage and effect by year.
df2 <- faa %>%
  filter(SPECIES %in% c("Hermit thrush", "Common goldeneye", "Peregrine falcon", "Eastern meadowlark", "European starling", "Killdeer", "Horned lark", "Barn swallow")) %>%
  filter(INDICATED_DAMAGE == TRUE | 
           (!is.na(EFFECT) & trimws(EFFECT) != "" & trimws(EFFECT) != "None") | 
           (!is.na(EFFECT_OTHER) & trimws(EFFECT_OTHER) != "" & trimws(EFFECT_OTHER) != "None"))

# Take a look 
View(df2)
# There are at least 1 killdeer and 1 barn swallow record that inappropriately report damage.
# Horned lark and eastern meadowlark records here are questionable too.