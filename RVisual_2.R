# R Visualisation 2: Bubble Plot with Known Baseline
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

# 1. Find most common known demographic group (exclude unknowns)
fatality <- fatality %>%
  mutate(
    Year = if (inherits(Year, "Date") || inherits(Year, "POSIXt")) year(Year) else as.integer(as.character(Year))
  )

# --- pipeline ---
most_common_known <- fatality %>%
  filter(
    Gender != "Unknown",
    `National Remoteness Areas 2021` != "Unknown",
    !is.na(`Age group`)
  ) %>%
  count(`Age group`, Gender, `National Remoteness Areas 2021`,
        sort = TRUE, name = "Deaths") %>%
  slice(1)

baseline_age       <- most_common_known$`Age group`
baseline_gender    <- most_common_known$Gender
baseline_remoteness<- most_common_known$`National Remoteness Areas 2021`

# 2. Calculate risk ratio based on pre-defined baseline (2010-2024)
demo_stats <- fatality %>%
  filter(Year >= 2010, Year <= 2024) %>%     
  group_by(`Age group`, Gender, `National Remoteness Areas 2021`) %>%
  summarise(Deaths = n(), .groups = "drop") %>%
  mutate(
    baseline_deaths = most_common_known$Deaths,
    Risk_Ratio = Deaths / baseline_deaths,
    Risk_category = case_when(
      Risk_Ratio >= 2.0  ~ "Very High Risk (2x+)",
      Risk_Ratio >= 1.5  ~ "High Risk (1.5-2x)",
      Risk_Ratio >= 0.75 ~ "Moderate Risk",
      TRUE               ~ "Lower Risk"
    ),
    Has_Unknown = ifelse(Gender == "Unknown" |
                           `National Remoteness Areas 2021` == "Unknown",
                         "Unknown Data", "Known Data")
  )

# Separate known vs unknown
known_data <- demo_stats %>% filter(Has_Unknown == "Known Data")
unknown_data <- demo_stats %>% filter(Has_Unknown == "Unknown Data")

# 3. Enhanced visualisation with pre-defined baseline
p2 <- plot_ly()

# Unknown data as diamonds (bottom layer)
if(nrow(unknown_data) > 0) {
  p2 <- p2 %>% add_trace(
    data = unknown_data,
    x = ~`Age group`,
    y = ~Gender,
    size = ~Deaths,
    color = ~Risk_Ratio,
    colors = c("#0D0887", "wheat", "#F98C0A"),
    text = ~paste("<b>", `Age group`, Gender, "</b>",
                  "<br><b>Location:</b>", `National Remoteness Areas 2021`,
                  "<br><b>Deaths:</b>", Deaths,
                  "<br><b>Risk Ratio:</b>", round(Risk_Ratio, 2),
                  "<br><b>Risk Level:</b>", Risk_category,
                  "<br><b>Data Quality:</b>", Has_Unknown),
    hovertemplate = "%{text}<extra></extra>",
    type = "scatter",
    mode = "markers",
    marker = list(
      symbol = "diamond",
      sizemode = "diameter",
      sizemin = 5,
      sizemax = 50,
      line = list(width = 2, color = "darkgray"),
      opacity = 0.7,
      showscale = FALSE
    ),
    name = "Unknown Data",
    showlegend = TRUE,
    hoverinfo = "text"
  )
}

# Known data as circles (top layer)
p2 <- p2 %>% add_trace(
  data = known_data,
  x = ~`Age group`,
  y = ~Gender,
  size = ~Deaths,
  color = ~Risk_Ratio,
  colors = c("#0D0887", "wheat", "#F98C0A"),
  text = ~paste("<b>", `Age group`, Gender, "</b>",
                "<br><b>Location:</b>", `National Remoteness Areas 2021`,
                "<br><b>Deaths:</b>", Deaths,
                "<br><b>Risk Ratio:</b>", round(Risk_Ratio, 2),
                "<br><b>Risk Level:</b>", Risk_category,
                "<br><b>Data Quality:</b>", Has_Unknown),
  hovertemplate = "%{text}<extra></extra>",
  type = "scatter",
  mode = "markers",
  marker = list(
    symbol = "circle",
    sizemode = "diameter", 
    sizemin = 5,
    sizemax = 50,
    line = list(width = 0.5, color = "black"),
    colorbar = list(
      title = ""
    )
  ),
  name = "Known Data",
  showlegend = TRUE
) %>%
  layout(
    title = list(
      text = paste0("<b>Australian Road Fatalities: Demographic Risk Analysis</b><br>",
                    "<sub>Source: Australian Road Deaths Database 2010-2024 | Risk Ratios vs Pre-defined Baseline</sub>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "<b>Age Group</b>",
      titlefont = list(size = 14),
      categoryorder = "array",
      categoryarray = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")
    ),
    yaxis = list(
      title = "<b>Gender</b>",
      titlefont = list(size = 14)
    ),
    annotations = list(
      list(
        text = "<i>Bubble size = Total Deaths | Color = Risk Ratio vs Pre-defined Baseline<br> </i>",
        x = 0.5, y = -0.15, 
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 11, color = "gray")
      ),
      list(
        text = paste0("<b>Pre-defined Baseline:</b> ", baseline_gender, ", ", baseline_remoteness, 
                      ", ", baseline_age, " (Risk Ratio = 1.0)"),
        x = 1, y = 1.02, 
        xref = "paper", yref = "paper",
        showarrow = FALSE, 
        font = list(size = 10, color = "#0D0887"),
        bgcolor = "wheat",
        bordercolor = "#0D0887",
        borderwidth = 1
      )
    ),
    margin = list(t = 80, b = 100),
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

p2

