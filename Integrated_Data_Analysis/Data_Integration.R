####################################
############# PREFACE ##############
####################################

# The purpose of this code is to aggregate all sources of coyote data to strengthen inference.
# Note that data sets (i.e., CSV files uploaded here) can be updated with new CSV files later to produce new results.

# Load R packages for data manipulation.
library(tidyverse)
library(date)


####################################
### WCAA Strikes and Close Calls ###
####################################

# Read in coyote strike data and close-call data (close call = pilot suspected coyote strike but no biological evidence found).
# Close call data were gathered by reviewing Maximo Wildlife Report descriptions prior to this step.
# Strike data came from the a WCAA Wildlife Command Center export.
# These 2 data sources were combined manually before this step.
scc <- read.csv("Coyote_Strikes_CloseCalls_27Nov2024.csv")

# Rename date column to "Date".
scc <- scc %>% rename("Date" = "Service.Log.Date")

# Remove irrelevant description column.
scc <- scc %>% select(-c(Description..Short.))

# Format date so it is yyyy-mm-dd format.
scc <- scc %>% mutate(scc, Date = as.Date(Date, format = "%m/%d/%Y"))

# Break up date into month and year columns.
scc <- scc %>% mutate(scc,
                    Year = as.numeric(format(Date, format = "%Y")),
                    Month = as.numeric(format(Date, format = "%m")))

# Remove old date column.
scc <- scc %>% select(-c(Date))

# Split the Coordinates column into Latitude and Longitude columns.
scc <- scc %>%
  separate(LatLong, into = c("Lat", "Long"), sep = ", ")

# Save cleaned up CSV file for future reference.
write.csv(scc, "Coyote_R_Strikes_27Nov2024.csv")


###################################
### Historical FAA Strikes Data ###
###################################

# Here we use DTW coyote strike data exported from the FAA strike database to capture really old records.
# Prior to reading in the data, coordinates were approximated using notes in the descriptions.
# Also, records from 2016 and later were removed because we already captured that in the code chunk above.
# WARNING: Runways changed names over time, so double check things as you go!

# Read in exported file.
faa <- read.csv("FAA_Wildlife_Export_12312024.csv")

# Rename Month, Year, and ID columns so formatting is consistent with above code chunk..
faa <- faa %>% rename("Month" = "INCIDENT_MONTH",
                      "Year" = "INCIDENT_YEAR",
                      "ID" = "INDX_NR")

# Select only the columns of interest.
faa <- faa %>% select(ID, Month, Year, LatLong)

# Split the coordinates column into Latitude and Longitude columns.
faa <- faa %>%
  separate(LatLong, into = c("Lat", "Long"), sep = ", ")

# Add some extra columns so WCAA and FAA strike data can be merged. 
# Specifically, a strike column where all values are "Y" and a report-type column with an "FAA" identifier.
faa <- faa %>% add_column(ReportType = "FAA",
                          Strike = "Y")


######################################
### Merge WCAA and FAA Strike Data ###
######################################

# Merge the WCAA and FAA data sets prepped above.
scc <- rbind(scc, faa)

# Save file for future reference.
write.csv(scc, "Coyote_R_Strikes_27Nov2024.csv", row.names = FALSE)


##########################################
### Recent Work-Order (Dig-Under) Data ###
##########################################

# Read in work order data. Note minor revisions were made to the original file prior to this step, 
# including removing the first row (title) and combining 2024 and 2023 spreadsheets. Also, some
# problematic white spaces were removed.
wo <- read.csv("DTW Coyote Digunder Workorders 2023-2024_15Nov2024_smg.csv")

# Remove irrelevant columns.
wo <- wo %>% select(-c(Site, Notes))

# Remove rows with a blank or NA coordinates.
wo <- wo %>% 
  filter(!is.na(Coordinates) & Coordinates != "") # 1 record with missing coordinates.

# Add column for number of dig unders or total count.
wo <- wo %>% add_column(Count = 1)

# Add Work Order report-type identifier.
wo <- wo %>% add_column(ReportType = "Work Order")

# Rename ID column.
wo <- wo %>% rename("ID" = "W.O..")

# Rename date column.
wo <- wo %>% rename("Date" = "Reported.Date")

# Reformat date column to yyyy-mm-dd format.
wo <- wo %>% mutate(wo, Date = as.Date(Date, format = "%m/%d/%Y"))

# Break up date into month and year columns.
wo <- wo %>% mutate(wo,
                    Year = as.numeric(format(Date, format = "%Y")),
                    Month = as.numeric(format(Date, format = "%m")))

# Remove old date column.
wo <- wo %>% select(-c(Date))

# Split the Coordinates column into Latitude and Longitude.
wo <- wo %>%
  separate(Coordinates, into = c("Lat", "Long"), sep = ", ")

# Remove any more problematic white spaces from Lat and Long columns.
# Note this can easily happen when manually copying and pasting coordinates prior to coding. 
wo <- wo %>%
  mutate(Lat = trimws(Lat)) %>%
  mutate(Long = trimws(Long))


##############################################
### Historical Work-Order (Dig-Under) Data ###
##############################################

# Read in the ancient dig-under data (data before 2023). 
# Note minor revisions were made to the original file prior to this step, including
# searching reports for coordinates (e.g., reviewing map attachments) and removing
# some white spaces.
wo2 <- read.csv("DTW Coyote Digunder Workorders 2016-2022_17Dec2024_smg.csv")

# Remove irrelevant columns.
wo2 <- wo2 %>% select(-c(Description))

# Add column for number of dig unders or total count.
wo2 <- wo2 %>% add_column(Count = 1)

# Add Work Order report-type identifier.
wo2 <- wo2 %>% add_column(ReportType = "Work Order")

# Reformat date column to yyyy-mm-dd format.
wo2 <- wo2 %>% mutate(wo2, Date = as.Date(Date, format = "%m/%d/%Y"))

# Break up date into month and year columns.
wo2 <- wo2 %>% mutate(wo2,
                    Year = as.numeric(format(Date, format = "%Y")),
                    Month = as.numeric(format(Date, format = "%m")))

# Remove old date column.
wo2 <- wo2 %>% select(-c(Date))

# Split the Coordinates column into Latitude and Longitude.
wo2 <- wo2 %>%
  separate(Coordinates, into = c("Lat", "Long"), sep = ", ")


#########################################
### Merge Work-Order (Dig-Under) Data ###
#########################################

# Merge the recent and historical dig-under data prepped above.
wo <- rbind(wo, wo2)

# Save as a CSV copy of the merged data.
write.csv(wo, "Coyote_R_WorkOrders_17Dec2024.csv")

# Summarize counts by year.
wo_sums <- wo %>%
  group_by(Year) %>%
  summarise(Total = sum(Count))

# Take a look.
print(wo_sums) # Note that data is very limited prior to 2023.

# Compute summary stats across 2023 and 2024 (when there were standardized surveys).
# Filter for 2023 and 2024.
wo_filtered <- wo %>%
  filter(Year %in% c(2023, 2024))

# Summarize total number of work orders per year.
yearly_totals <- wo_filtered %>%
  group_by(Year) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop")

# Summarize mean, SD, min, and max across 2023 and 2024. 
yearly_summary <- yearly_totals %>%
  summarise(
    Mean = mean(Total),
    SD = sd(Total),
    Min = min(Total),
    Max = max(Total)
  )


#################################
### WCAA Incident Report Data ###
#################################

# Note that coyotes are no longer reported in Maximo Incident Reports, so this just captures some old stuff.
# Note that some irrelevant rows of data were manually removed prior to this step.
# Location data were approximated manually (from long descriptions) and input prior to this step.
# Number of coyotes (i.e., count) were derived from long description.

# Load Maximo incident reports.
incident <- read.csv("Coyote_MaximoIncidentReports_13Nov2024_cleaned.csv")

# Remove irrelevant columns.
incident <- incident %>% select(-c(Summary))

# Add Incident report-type identifier.
incident <- incident %>% add_column(ReportType = "Incident")

# Rename ID column.
incident <- incident %>% rename("ID" = "Incident")

# Add column for the number of individuals reported.
# This step is hard coded because all reports had a count of 1 coyote.
# Note that the hard coding here will unlikely be an issue down the road because coyotes are no longer being reported in Incident Reports.
incident <- incident %>% add_column(Count = 1) 

# Reformat date column to yyyy-mm-dd format.
incident <- incident %>% mutate(incident, Date = as.Date(Date, format = "%m/%d/%Y"))

# Break up date into month and year columns.
incident <- incident %>% mutate(incident,
                                Year = as.numeric(format(Date, format = "%Y")),
                                Month = as.numeric(format(Date, format = "%m")))

# Remove old date column
incident <- incident %>% select(-c(Date))

# Save as a CSV copy for records.
write.csv(incident, "Coyote_R_IncidentReports_13Nov2024.csv")


####################################
### WCAA Service-log Report Data ###
####################################

# Note that some irrelevant rows of data were manually removed prior to this step.
# Location data were approximated manually (from long descriptions) and input prior to this step.
# Number of coyotes (i.e., count) were derived from long description.
# Long descriptions that used plural language were assumed to be 2 coyotes unless stated otherwise.

# Read in the Maximo Service-Log data.
sl <- read.csv("Coyote_MaximoServiceLogReports_13Nov2024_cleaned.csv")

# Remove irrelevant columns.
sl <- sl %>% select(-c(Description..Short., Service.Log.Type,  Edits))

# Add Service Log report-type identifier.
sl <- sl %>% add_column(ReportType = "Service Log")

# Rename ID column.
sl <- sl %>% rename("ID" = "Service.Log.ID")

# Rename date column.
sl <- sl %>% rename("Date" = "Service.Log.Date")

# Reformat date column to yyyy-mm-dd format.
sl <- sl %>% mutate(sl, Date = as.Date(Date, format = "%m/%d/%Y"))

# Break up date into month and year columns.
sl<- sl %>% mutate( sl,
                    Year = as.numeric(format(Date, format = "%Y")),
                    Month = as.numeric(format(Date, format = "%m")))

# Remove old date column.
sl<- sl%>% select(-c(Date))

# Split the Coordinates column into Latitude and Longitude
sl<- sl%>%
  separate(LatLong, into = c("Lat", "Long"), sep = ", ") 

# Save as a CSV copy.
write.csv(sl, "Coyote_R_ServiceLog_15Nov2024.csv")


####################################
#### WCAA Wildlife Report Data #####
####################################

# Note that some irrelevant rows of data were manually removed prior to this step.
# Location data were approximated manually (from long descriptions) and input prior to this step.
# Number of coyotes (i.e., count) were derived from long description.
# Long descriptions that used plural language were assumed to be 2 coyotes unless stated otherwise.

# Read in the Maximo Wildlife Report data. 
wr <- read.csv("Coyote_MaximoWildlifeReports_13Nov2024_cleaned_smg.csv")

# Remove irrelevant columns.
wr <- wr %>% select(-c(Report.Type, Summary, Site, Status))

# Add Wildlife Report report-type identifier.
wr <- wr %>% add_column(ReportType = "Wildlife Report")

# Rename ID column.
wr <- wr %>% rename("ID" = "Report.ID")

# Rename date column.
wr <- wr %>% rename("Date" = "Reported.Date")

# Reformat date column to yyyy-mm-dd format.
wr <- wr %>% mutate(wr, Date = as.Date(Date, format = "%m/%d/%Y"))

# Break up date into month and year columns.
wr<- wr %>% mutate( wr,
                    Year = as.numeric(format(Date, format = "%Y")),
                    Month = as.numeric(format(Date, format = "%m")))

# Remove old date column.
wr<- wr%>% select(-c(Date))

# Split the Coordinates column into Latitude and Longitude.
wr<- wr%>%
  separate(LatLong, into = c("Lat", "Long"), sep = ", ")

# Save as a CSV copy.
write.csv(wr, "Coyote_R_WildlifeReports_15Nov2024.csv")


############################
### Activity Report Data ###
############################

# Load all wildlife Activity data (Survey123 data).
activity <- read.csv("Wildlife_ActivityData_14Nov2024.csv")

# Select only the coyote records.
activity <- filter(activity, species_name == "COYOTE")

# Select columns of interest.
activity <- activity %>% select(c(objectid, activity_year, activity_month, x, y, num_observed))

# Rename columns so formatting is similar to above.
activity <- activity %>% rename("Lat" = "y",
                                "Long" = "x",
                                "Year" = "activity_year",
                                "Count" = "num_observed",
                                "Month" = "activity_month")
# Make month numeric.
activity$Month <- ifelse(activity$Month == "Jan", 1, 
                         ifelse(activity$Month == "Feb", 2,
                                ifelse(activity$Month == "Mar", 3,
                                       ifelse(activity$Month == "Apr", 4,
                                              ifelse(activity$Month == "May", 5,
                                                     ifelse(activity$Month == "Jun", 6,
                                                            ifelse(activity$Month == "Jul", 7,
                                                                   ifelse(activity$Month == "Aug", 8,
                                                                          ifelse(activity$Month == "Sep", 9,
                                                                                 ifelse(activity$Month == "Oct", 10,
                                                                                        ifelse(activity$Month == "Nov", 11,
                                                                                               ifelse(activity$Month == "Dec", 12,
                                                                                                      activity$Month))))))))))))

# Add Activity report-type identifier.
activity <- activity %>% add_column(ReportType = "Activity")

# Rename ID column.
activity <- activity %>% rename("ID" = "objectid")

# Save as a CSV copy.
write.csv(activity, "Coyote_R_ActivityReports_14Nov2024.csv")


################################
#### Combine All Data Sets #####
################################

# First, make a Maximo-only dataset.
# Merge Maximo data sets one at a time.
maximo <- rbind(incident, sl) # Incidents and service logs.
maximo <- rbind(wr, maximo) # Now add wildlife reports.

# Save as a CSV copy of combined Maximo reports.
write.csv(maximo, "Coyote_R_MaximoCombined_15Nov2024.csv")

# Now merge Maximo, Work Order, and Activity data into a single dataset.
all <- rbind(maximo, activity) # Maximo + Activity data.
all <- rbind(all, wo) # Now add Work orders to the above.

# Save as a CSV copy of everything combined.
write.csv(all, "Coyote_R_AllData_15Nov2024.csv", row.names = FALSE)
