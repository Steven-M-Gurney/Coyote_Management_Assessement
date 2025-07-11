####################################
############# PREFACE ##############
####################################

# The purpose of this code is visualize all coyote data together to help inform recommendations.

# Load packages for wrangling and plotting
library(ggplot2)
library(dplyr)


################################
########## DIG UNDERS ##########
################################

# Data = 2016 - 2024 (note very limited data in the early years).
# Standardize and plot data as percentage of total.

# Aggregate counts by Month (across all years).
du <- wo %>%
  group_by(Month) %>%
  summarize(Total_Count = sum(Count))

du2 <- wo %>%
  group_by(Year) %>%
  summarize(Year_Total_Count = sum(Count))

# Look at the total counts for fun.
print(sum(du$Total_Count)) # Number of dig-under work orders since 2016.

# Put in month in MMM format.
du <- du %>%
  mutate(Month = month.abb[Month])

# Convert Month column to a factor with levels in the correct order.
du$Month <- factor(du$Month, levels = month.abb)

# Calculate percentage of total counts.
du <- du %>%
  mutate(
    Percent = (Total_Count / sum(Total_Count)) * 100
  )

# Plot percentages by month.
du.month.percent <- ggplot(du, aes(x = Month, y = Percent)) +
  geom_col(fill = "palegreen4") + # Bar chart.
  labs(
    x = "Month",
    y = "Coyote dig unders (%)"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black")
  ) +
  scale_y_continuous(
    breaks = seq(0, 25, by = 5), # Set y-axis breaks by 5.
    limits = c(0, 28)           # Set y-axis limits from 0 to 25.
  ) # Note hard coding for breaks and limits above, may need to adjust in future if ceiling changes.

# Take a look.
du.month.percent

# Save output.
ggsave("plot_monthly_digging_percent.png", plot = du.month.percent, width = 6.5, height = 4.5, dpi = 300)


####################################
########## Maximo Reports ##########
####################################

# Data = 2016 - 2024
# Standardize and plot data as percentage of total.

# Aggregate counts by Month (across all years).
mx <- maximo %>%
  group_by(Month) %>%
  summarize(Total_Count = sum(Count))

# What is the count sum? 
print(sum(mx$Total_Count)) # Number of reports since 2016.

# Put in month in MMM format.
mx <- mx %>%
  mutate(Month = month.abb[Month])

# Convert Month column to a factor with levels in the correct order.
mx$Month <- factor(mx$Month, levels = month.abb)

# Calculate percentage of total counts.
mx <- mx %>%
  mutate(
    Percent = (Total_Count / sum(Total_Count)) * 100
  )

# Plot percentages by month.
mx.month.percent <- ggplot(mx, aes(x = Month, y = Percent)) +
  geom_col(fill = "palevioletred4") + # Bar chart.
  labs(
    x = "Month",
    y = "Coyote reports (%)"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black")
  ) +
  scale_y_continuous(
    breaks = seq(0, 25, by = 5), # Set y-axis breaks by 5.
    limits = c(0, 28)           # Set y-axis limits from 0 to 25.
  ) # Note hard coding for breaks and limits above, may need to adjust in future if ceiling changes.

# Take a look.
mx.month.percent 

# Save output.
ggsave("plot_monthly_maximo_percent.png", plot = mx.month.percent, width = 6.5, height = 4.5, dpi = 300)


######################################
########## Activity Reports ##########
######################################

# Data = 2016 - 2024 (note very limited data in the early years).
# Standardize and plot data as percentage of total.

# Aggregate counts by Month.
monthly.activity.percent <- activity %>%
  group_by(Month) %>%
  summarize(Total_Count = sum(Count))

# Put in month in MMM format.
monthly.activity.percent <- monthly.activity.percent %>%
  mutate(Month = as.numeric(Month)) %>% # Ensure Month is numeric
  mutate(Month = month.abb[Month]) 

# Convert Month column to a factor with levels in the correct order.
monthly.activity.percent$Month <- factor(monthly.activity.percent$Month, levels = month.abb)

# Calculate percentage of total counts.
monthly.activity.percent <- monthly.activity.percent %>%
  mutate(
    Percent = (Total_Count / sum(Total_Count)) * 100
  )

# Plot percentages by month.
monthly.activity.percent <- ggplot(monthly.activity.percent, aes(x = Month, y = Percent)) +
  geom_col(fill = "skyblue4") + # Bar chart.
  labs(
    x = "Month",
    y = "Coyote activity (%)"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black")
  ) +
  scale_y_continuous(
    breaks = seq(0, 25, by = 5), # Set y-axis breaks by 5.
    limits = c(0, 28)           # Set y-axis limits from 0 to 25.
  ) # Note hard coding for breaks and limits above, may need to adjust in future if ceiling changes.

# Take a look.
monthly.activity.percent 

# Save output.
ggsave("plot_monthly_activity_percent.png", plot = monthly.activity.percent, width = 6.5, height = 4.5, dpi = 300)


##########################################
########## Maximo Control Chart ##########
##########################################

# Summarize and calculate annual rates and plot conflicts.
summary_data <- maximo %>%
  group_by(Year) %>%
  summarize(total_count = sum(Count)) %>%
  mutate(Rate = total_count / sum(total_count)) # Rate as a proportion of the total.

# Calculate the center line (mean) and control limits.
# Note 2 SDs were selected as opposed to the default 3 to detect finer change.
CL <- mean(summary_data$total_count)
UCL <- CL + 2 * sd(summary_data$total_count)  # Upper Control Limit, 95.45%.
LCL <- CL - 2 * sd(summary_data$total_count)  # Lower Control Limit, 95.45%.

# Populate dataframe with summary results (calculated above).
summary_data$UCL <- UCL
summary_data$LCL <- LCL

# count per year--control chart.
cp.conflict <- ggplot(summary_data, aes(x = Year, y = total_count)) +
  geom_hline(aes(yintercept = CL), linetype = "solid", color = "gray", linewidth = 1.5) +
  geom_hline(aes(yintercept = UCL), linetype = "dashed", color = "gray", linewidth = 1.5) +
  geom_hline(aes(yintercept = LCL), linetype = "dashed", color = "gray", linewidth = 1.5) +
  geom_line(color = "firebrick4", size = 2) +
  geom_point(color = "firebrick4", size = 5) +
  labs(
    x = "Year",
    y = "Coyote conflicts per year"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.text.y =element_text(size = 12, color = "black"),
    axis.text.x =element_text(size = 12, color = "black"))

# Take a look.
cp.conflict 

# Save output.
ggsave("plot_yearly_conflict_control_chart.png", plot = cp.conflict, width = 6.5, height = 5, dpi = 300)
