## -----------------------------------------------------------------------------
#| label: library
#| message: false
#| warning: false

# Data Manipulation
library(tidyverse)
library(readxl)
library(knitr)
library(gt)
library(dplyr)

# Treemap
library(treemapify)


## -----------------------------------------------------------------------------
#| label: fig-oldvs
#| echo: false
#| fig.cap: "Visualization of The Top 10 GHG Emitters Contribute Over Two-Thirds of Global Emissions."

include_graphics("images/old_visualization.jpg")


## -----------------------------------------------------------------------------
#| label: explore-raw
#| message: false

ghg_raw <- read_csv("data/historical_emissions_orig.csv")
gt_preview(ghg_raw)


## -----------------------------------------------------------------------------
#| label: explore-unique
#| message: false

ghg_raw |>
  summarize(
    n_distinct(Country),
  )
ghg_raw |>
  summarize(
    n_distinct(Sector),
    n_distinct(Gas),
    n_distinct(Unit)
  )


## -----------------------------------------------------------------------------
ghg_raw <-
  ghg_raw |>
  select(-Unit, -"Data source")

gt_preview(ghg_raw)

# Print size of the Data set
dim(ghg_raw)


## -----------------------------------------------------------------------------
#| label: emissions_across_country_and_sector
#| warning: false

# Function to process each gas
process_gas <- function(data, gas_name) {
  data |>
    filter(Gas == gas_name) |>
    pivot_longer(cols = 5:ncol(data), names_to = "Year", values_to = "Emissions", values_transform = list(Emissions = as.numeric)) |>
    group_by(ISO, Country, Sector, Year) |>
    summarize(Emissions = sum(Emissions, na.rm = TRUE), .groups = "drop")
}

# Process each gas and combine results
ghg_gases <- c("CO2", "F-Gas", "N2O", "CH4")
ghg_combined <- map_dfr(ghg_gases, ~ process_gas(ghg_raw, .x))

# Sum up the emissions for each country and sector across all years
ghg_total_sector <- ghg_combined |>
  group_by(ISO, Country, Sector) |>
  summarize(Emissions = sum(Emissions, na.rm = TRUE), .groups = "drop") |>
  filter(ISO != "WORLD") |>
  arrange(desc(Emissions))

# Calculate total emissions for each country
country_total_emissions <- ghg_total_sector |>
  group_by(Country) |>
  summarize(Total_Emissions = sum(Emissions), .groups = "drop")

# Get the top 10 countries by total emissions
top_10_countries <- country_total_emissions |>
  top_n(10, wt = Total_Emissions) |>
  pull(Country)

# Categorize countries as "Top 10" or "Others"
ghg_total_sector <- ghg_total_sector |>
  mutate(Country = if_else(Country %in% top_10_countries, Country, "Others"))

# Sum emissions for each country and sector
ghg_total_sector <- ghg_total_sector |>
  group_by(Country, Sector) |>
  summarize(Emissions = sum(Emissions), .groups = "drop")

# Calculate total emissions for each country again after categorization
country_total_emissions <- ghg_total_sector |>
  group_by(Country) |>
  summarize(Total_Emissions = sum(Emissions), .groups = "drop")

# Sort countries by total emissions
ghg_total_sector <- ghg_total_sector |>
  left_join(country_total_emissions, by = "Country") |>
  arrange(desc(Total_Emissions), Country, desc(Emissions)) |>
  mutate(Country = factor(Country, levels = unique(Country))) |>
  select(-Total_Emissions)


## -----------------------------------------------------------------------------
#| label: emissions_across_country_and_sector_preview

# Preview the result (Top 10 countries and sector)
ghg_total_sector |>
  filter(Country != "Others") |>
  gt_preview(10)


## -----------------------------------------------------------------------------
#| label: fig-source1-treemap-data

# Remove negative Emissions (We don't want to show negative emissions)
treemap_data <-
  ghg_total_sector |>
  filter(Emissions >= 0)

# Select only the specified sectors and group the rest under "Other Sectors"
treemap_data <-
  treemap_data |>
  group_by(Country) |>
  mutate(Sector = if_else(
    Sector %in% c("Energy", "Electricity/Heat", "Agriculture", "Transportation", "Manufacturing/Construction"),
    Sector,
    "Other Sectors"
  )) |>
  ungroup()

# Combine all sectors in "Others" Country into "Other Sectors"
treemap_data <-
  treemap_data |>
  mutate(Sector = if_else(Country == "Others", "Other Sectors", Sector))

# Combine the emissions of the "Other Sector" for each country and rename sectors appropriately
treemap_data <-
  treemap_data |>
  group_by(Country, Sector) |>
  summarize(Emissions = sum(Emissions), .groups = "drop") |>
  mutate(Sector = if_else(Country == "Others" & Sector == "Other Sectors", "Other Country", Sector))

# Rename "European Union (27)" to "EU27" and "Others" to "184 Remaining Countries"
treemap_data <-
  treemap_data |>
  mutate(Country = case_when(
    Country == "European Union (27)" ~ "EU27",
    Country == "Others" ~ "184 Remaining Countries",
    TRUE ~ Country
  ))

# Re-group and re-summarize to ensure data consistency
treemap_data <-
  treemap_data |>
  group_by(Country, Sector) |>
  summarize(Emissions = sum(Emissions), .groups = "drop")

# Modify the factors to order it in this order:
# Energy, Electricity/Heat, Agriculture, Transportation, Manufacturing/Construction, Other Sectors
treemap_data <-
  treemap_data |>
  mutate(
    Sector = factor(Sector,
      levels = c(
        "Energy",
        "Electricity/Heat",
        "Agriculture",
        "Transportation",
        "Manufacturing/Construction",
        "Other Sectors",
        "Other Country"
      ),
      ordered = TRUE
    )
  )

# Preview the data
treemap_data |>
  gt_preview(10)

