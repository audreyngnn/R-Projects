# Australian Road Death Analysis (2010-2024)

<img width="919" height="529" alt="image" src="https://github.com/user-attachments/assets/2f9fae77-d42c-4070-ae9f-7bf26a5d818c" />


## 📊 Project Summary

Analyzed Australian road fatalities (2010-2024) using three R visualizations to identify temporal patterns, high-risk demographics, and policy effectiveness toward the 2030 target.

**[📈 View Full Interactive Report on RPubs](http://rpubs.com/audreyyy_ng/au-road-deaths)**

<br>

## 🛠️ Tech Stack

| Category | Tools |
|----------|-------|
| **Language** | R|
| **Key Libraries** | `dplyr`, `ggplot2`, `plotly`, `lubridate`, `viridis` |
| **Data Source** | [Australian Road Safety Data Hub](https://datahub.roadsafety.gov.au/progress-reporting/monthly-road-deaths#anchor-explore-the-data) |

<br>

## 🎯 Key Visualisations
### Temporal Risk Analysis
<img width="669" height="481" alt="image" src="https://github.com/user-attachments/assets/f242c33c-5beb-4e1a-a9da-222519c4d49e" />

<br>

### Demographic Risk Analysis
<img width="665" height="442" alt="image" src="https://github.com/user-attachments/assets/282ce0d0-df0a-486a-a8f1-2f94e075684d" />

<br>

### Policy Impact Analysis
<img width="690" height="462" alt="image" src="https://github.com/user-attachments/assets/36431f5e-3a0d-4eb4-b716-723d735d8b76" />


## 🎯 Key Findings

| Visualization | Chart Type | Key Finding |
|--------------|-----------|-------------|
| **Temporal Risk Analysis** | Heatmap | 12pm-5pm accounts for ~33% of deaths; weekends are highest risk |
| **Demographic Risk Analysis** | Bubble Plot | Men aged 18-34 face highest risk; females significantly below baseline |
| **Policy Impact Timeline** | Line Chart | Post-2011 decline reversed in 2019; not on track for 2030 target (50% reduction) |


<br>

## 💡 Recommendations

| Priority | Action |
|----------|--------|
| 🚨 High | Target enforcement during 12pm-5pm, especially weekends |
| 👥 High | Focus safety campaigns on men aged 18-34 |
| 📍 Medium | Increase resources for remote areas |
| 📊 Critical | Accelerate policy interventions to meet 2030 target |

<br>

## 📁 Repository Files

| File | Description |
|------|-------------|
|'cleaned_data.xlsx' | Cleaned Dataset |
| `RVisual_1.R` | Temporal Risk Heatmap |
| `RVisual_2.R` | Demographic Risk Bubble Plot |
| `RVisual_3.R` | Policy Impact Timeline |
| `Australian_Road_Death_Analysis.Rmd` | R Markdown source |

<br>

**Author**: Audrey Nguyen
