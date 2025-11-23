# R Visualisation 1: Temporal Risk Heatmap
# --- Libraries ---
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)
library(plotly)

library(scales)
library(zoo)
library(purrr)

# Load data
fatality <- read_excel("cleaned_data.xlsx", sheet = "Processed_Fatality")

# Filter data to ONLY include years 2010-2024
fatality <- fatality %>%
  filter(Year >= 2010 & Year <= 2024)

# Enhanced data preparation
fatality <- fatality %>%
  mutate(
    Hour = hour(hms(Time)),
    Weekday = factor(Dayweek, 
                     levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                "Friday", "Saturday", "Sunday"),
                     labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    Month = factor(Month, levels = 1:12, 
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
    Year = as.factor(Year),
    # Create high-risk time period flag
    HighRiskPeriod = case_when(
      Hour >= 12 & Hour <= 17 ~ "High Risk (12pm-17pm)",
      TRUE ~ "Standard Risk"
    ),
    # Create weekend flag
    Weekend = ifelse(Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  )

# Calculate overall statistics for annotations (2010-2024 only)
total_deaths <- nrow(fatality)
peakhour_deaths <- fatality %>% 
  filter(Hour >= 12 & Hour <= 17) %>% 
  nrow()
peakhour_percentage <- round(peakhour_deaths / total_deaths * 100, 1)

# Calculate the actual number of years in the filtered data
years_in_data <- length(unique(fatality$Year))

# Aggregate counts by Hour x Weekday with additional metrics
heat_data <- fatality %>%
  group_by(Hour, Weekday) %>%
  summarise(
    Deaths = n(),
    # Calculate rate per year using actual years in data
    AnnualRate = Deaths / years_in_data,
    .groups = "drop"
  ) %>%
  # Add risk level categorization
  mutate(
    RiskLevel = case_when(
      Deaths >= quantile(Deaths, 0.8) ~ "Very High",
      Deaths >= quantile(Deaths, 0.6) ~ "High", 
      Deaths >= quantile(Deaths, 0.4) ~ "Moderate",
      Deaths >= quantile(Deaths, 0.2) ~ "Low",
      TRUE ~ "Very Low"
    ),
    # Highlight the dangerous period
    DangerPeriod = ifelse(Hour >= 12 & Hour <= 17, "Danger Zone", "Normal")
  )

# Create the enhanced heatmap
p1 <- ggplot(heat_data, aes(x = Hour, y = Weekday, fill = Deaths)) +
  geom_tile(color = "white", linewidth = 0.3) +
  # Add border around high-risk hours
  geom_rect(data = filter(heat_data, Hour >= 12 & Hour <= 17),
            aes(xmin = Hour - 0.5, xmax = Hour + 0.5, 
                ymin = as.numeric(Weekday) - 0.5, ymax = as.numeric(Weekday) + 0.5),
            fill = NA, color = "red", linewidth = 1, inherit.aes = FALSE) +
  # Use a more sophisticated color scale
  scale_fill_viridis_c(option = "plasma", name = "Deaths\n(2010-2024)",
                       trans = "sqrt", # Square root transformation for better color distribution
                       labels = scales::comma_format()) +
  # Enhanced scales and labels
  scale_x_continuous(breaks = seq(0, 23, 2), 
                     labels = paste0(seq(0, 23, 2), ":00")) +
  scale_y_discrete() +
  
  # Professional styling
  labs(
    title = "Australian Road Deaths: Temporal Risk Analysis (2010-2024)",
    subtitle = paste0("High-risk period (12pm-17pm) accounts for ", peakhour_percentage, "% of all fatalities | Years analyzed: ", years_in_data),
    x = "Hour of Day",
    y = "Day of Week",
    caption = "Source: Australian Road Deaths Database | Red borders highlight Danger Zone (2010-2024)"
  ) +
  
  # Clean theme
  theme_minimal() +
  theme(
    axis.title.y = element_text(
      size = 11, face = "bold",
      margin = margin(r = 15)   # increase this value to push title away from tick labels
    ),
    axis.text.y = element_text(
      margin = margin(r = 5)    # keep small margin between labels and tick marks
    ),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "darkred"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.caption = element_text(size = 9, color = "gray50")
  )

# Additional Analysis: Create a summary statistics table
risk_summary <- fatality %>%
  group_by(HighRiskPeriod) %>%
  summarise(
    Deaths = n(),
    Percentage = round(n() / nrow(fatality) * 100, 1),
    DeathsPerYear = round(n() / years_in_data, 1)
  )

# Also create hourly summary to identify actual peak hours
hourly_summary <- fatality %>%
  count(Hour, name = "Deaths") %>%
  arrange(desc(Deaths)) %>%
  mutate(
    Percentage = round(Deaths / sum(Deaths) * 100, 1),
    RankRisk = rank(-Deaths)
  )

# Print data summary
print(paste("Data filtered for years 2010-2024"))
print(paste("Total years in dataset:", years_in_data))
print(paste("Total deaths in filtered period:", total_deaths))
print("")
print("Risk Period Analysis:")
print(risk_summary)
print("\nTop 6 Deadliest Hours:")
print(head(hourly_summary, 6))

# Show the plot
print(p1)

