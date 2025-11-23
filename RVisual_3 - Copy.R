# R Visualisation 3: Policy Impact Analysis - Monthly Deaths
# --- Libraries ---
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(zoo)

# Load data
fatality <- read_excel("cleaned_data.xlsx", sheet = "Processed_Fatality")
# 1) Clean + Constrain to 2010–2024

fatality <- fatality %>%
  mutate(
    Year_chr   = as.character(Year),
    Month_chr  = as.character(Month),
    Month_lower   = tolower(trimws(Month_chr)),
    Month_numeric = dplyr::case_when(
      grepl("^[0-9]{1,2}$", Month_lower) ~ as.integer(Month_lower),
      Month_lower %in% tolower(month.abb)  ~ match(Month_lower, tolower(month.abb)),
      Month_lower %in% tolower(month.name) ~ match(Month_lower, tolower(month.name)),
      TRUE ~ NA_integer_
    ),
    Year_numeric = suppressWarnings(as.integer(Year_chr))
  ) %>%
  filter(!is.na(Year_numeric), !is.na(Month_numeric),
         Year_numeric >= 2010, Year_numeric <= 2024) %>%
  mutate(Date = make_date(year = Year_numeric, month = Month_numeric, day = 1))

# 2) Policy events: x from Date, only manual y_position + per-line colors
policy_events <- data.frame(
  Date = as.Date(c("2011-01-01","2018-01-01","2018-07-01","2021-01-01")),
  Policy = c("National Strategy\n2011-20",
             "Vehicle Safety\nStandards",
             "Mobile Phone\nEnforcement",
             "National Strategy\n2021-30"),
  Policy_Short = c(" Strategy 2011-20\n commerces",
                   " Enhanced Vehicle\n Safety Standards",
                   " Mobile Phone\n Penalties\n Enforcement",
                   " Strategy 2021-30\n commerces"),
  Type = c("National Strategy","Vehicle Safety","Driver Behaviour","National Strategy"),
  Impact_Expected = "Reduce",
  y_position = c(125, 80, 60, 125),
  # New: color each dashed vertical line (2011–20 and 2021–30 get custom colors)
  LineColor = c("#0D0887", "#415A77", "#415A77", "#89226A"),
  stringsAsFactors = FALSE
)

# 3) Monthly series
monthly_data <- fatality %>%
  group_by(Date, Year_numeric, Month_numeric) %>%
  summarise(Deaths = n(), .groups = "drop") %>%
  arrange(Date) %>%
  mutate(
    Deaths_MA12 = zoo::rollmean(Deaths, k = 12, fill = NA, align = "right"),
    YoY_Change  = (Deaths - dplyr::lag(Deaths, 12)) / dplyr::lag(Deaths, 12) * 100
  )

# 4) Policy impact (optional analytics)
policy_impact <- policy_events %>%
  mutate(
    Pre_Period_Deaths = purrr::map_dbl(Date, function(d0) {
      pre <- monthly_data %>% filter(Date >= (d0 %m-% months(12)), Date < d0, !is.na(Deaths))
      if (nrow(pre) > 0) mean(pre$Deaths) else NA_real_
    }),
    Post_Period_Deaths = purrr::map_dbl(Date, function(d0) {
      post <- monthly_data %>% filter(Date >= d0, Date < (d0 %m+% months(12)), !is.na(Deaths))
      if (nrow(post) > 0) mean(post$Deaths) else NA_real_
    }),
    Impact_Percent = dplyr::case_when(
      is.na(Pre_Period_Deaths) | is.na(Post_Period_Deaths) ~ NA_real_,
      Pre_Period_Deaths == 0 ~ NA_real_,
      TRUE ~ round((Post_Period_Deaths - Pre_Period_Deaths) / Pre_Period_Deaths * 100, 1)
    )
  )

# 5) Targets
baseline_2018_2020 <- 1142
target_2030 <- 571
target_2030_monthly <- target_2030 / 12  # ~47.6

# 6) Plot
p3 <- ggplot(monthly_data, aes(x = Date)) +
  # Strategy backgrounds
  annotate("rect", xmin = as.Date("2011-01-01"), xmax = as.Date("2020-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#0D0887") +
  annotate("rect", xmin = as.Date("2021-01-01"), xmax = as.Date("2024-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#89226A") +
  # COVID window
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-09-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#F98C0A") +
  # Series
  geom_line(aes(y = Deaths), color = "#E8E8E8", alpha = 0.7, size = 0.3) +
  geom_line(aes(y = Deaths_MA12), color = "#0D1B2A", size = 1.2) +
  # Policy lines at actual dates, now with per-line colors
  geom_vline(data = policy_events,
             aes(xintercept = Date, color = LineColor),
             linetype = "dashed", size = 0.8, alpha = 0.8) +
  # Labels: x = Date, y = manual y_position (text color kept uniform)
  geom_text(data = policy_events,
            aes(x = Date, y = y_position, label = Policy_Short),
            color = "#0D1B2A", size = 2.8, fontface = "bold",
            hjust = 0, vjust = 0.5) +
  # 2030 target
  geom_hline(yintercept = target_2030_monthly, color = "#D23105",
             linetype = "dashed", size = 1, alpha = 0.8) +
  annotate("text", x = as.Date("2023-01-01"), y = target_2030_monthly + 5,
           label = paste0(" 2030 Target\n (", round(target_2030_monthly, 1), " deaths/mth)"),
           color = "#D23105", size = 3, fontface = "bold", hjust = 0) +
  # Titles on backgrounds
  annotate("text", x = as.Date("2015-11-15"), y = 138,
           label = "National Strategy 2011-2020", color = "#0D0887",
           size = 3.5, fontface = "bold", alpha = 0.8) +
  annotate("text", x = as.Date("2022-11-15"), y = 138,
           label = "National Strategy 2021-2030", color = "#89226A",
           size = 3.5, fontface = "bold", alpha = 0.8) +
  # COVID label
  annotate("text", x = as.Date("2020-09-01"), y = 110,
           label = "  COVID-19\n  PERIOD", color = "#F98C0A",
           size = 3, fontface = "bold", hjust = 0.5) +
  # Trend note
  annotate("curve", x = as.Date("2019-06-01"), xend = as.Date("2019-01-01"),
           y = 120, yend = 102, color = "#D23105", size = 1,
           arrow = arrow(length = unit(0.2, "cm")), curvature = 0.2) +
  annotate("text", x = as.Date("2019-12-01"), y = 125,
           label = "Trend Reversal\n in 2019", color = "#D23105",
           size = 3, fontface = "bold", hjust = 0.5) +
  # Scales
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               limits = c(as.Date("2010-01-01"), as.Date("2025-01-01")),
               expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(breaks = seq(40, 140, 10),
                     limits = c(40, 145),
                     expand = expansion(mult = c(0.02, 0.02))) +
  # Use the literal hex colors from LineColor and suppress legend
  scale_color_identity(guide = "none") +
  # Labels + theme
  labs(
    title = "Australian Road Deaths: Policy Impact Analysis (2010-2024)",
    subtitle = "Monthly deaths with 12-month moving average and Policy Intervention Timeline",
    x = NULL, y = "Monthly Deaths",
    caption = "Source: Australian Road Deaths Database \n2030 Target: 571 annual fatalities (50% reduction from 2018-2020 baseline of 1,142)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0, color = "#415A77", margin = margin(b = 20)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 10, color = "#0D1B2A"),
    axis.text.y = element_text(size = 10, color = "#0D1B2A"),
    panel.grid.major.x = element_line(color = "#E8E8E8", size = 0.3),
    panel.grid.major.y = element_line(color = "#E8E8E8", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.caption = element_text(size = 9, color = "#778DA9", hjust = 0, margin = margin(t = 15)),
    plot.margin = margin(20, 30, 20, 20)
  )

print(p3)


