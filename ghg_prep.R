## -----------------------------------------------------------------------------
#| label: library
#| message: false

library(tidyverse)
library(readxl)
library(knitr)
library(gt)
library(dplyr)

# Graphical Visualization
library(ggplot2)
library(ggrepel)
library(maps)
library(webr)

# To retrieve data from the World Bank Data API
library(wbstats)


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
# Print size of the dataset
dim(ghg_raw)


## -----------------------------------------------------------------------------
ghg_co2_year <- ghg_raw |>
  filter(Gas %in% c("CO2")) |>
  # Pivot longer to get ISO, Country, sector, year and emissions
  pivot_longer(
    cols = 5:ncol(ghg_raw), names_to = "Year", values_to = "Emissions",
    values_transform = list(Emissions = as.numeric)
  )

# head(ghg_co2_year)
#   group_by(ISO, Country, Year)
#
# ghg_co2_total_sector <- ghg_co2 |>
#   # Sum up the emissions for each country across all years
#   group_by(ISO, Country) |>
#   summarize(Emissions = sum(Emissions)) |>
#   arrange(desc(Emissions))
#
# ghg_fgas <- ghg_raw |>
#   filter(Gas %in% c("F-Gas")) |>
#   # Pivot longer to get ISO, Country, sector, year and emissions
#   pivot_longer(cols = 5:ncol(ghg_raw), names_to = "Year", values_to = "Emissions",
#     values_transform = list(Emissions = as.numeric)) |>
#   group_by(ISO, Country, Year)
#
# ghg_n2o <- ghg_raw |>
#   filter(Gas %in% c("N2O")) |>
#   # Pivot longer to get ISO, Country, sector, year and emissions
#   pivot_longer(cols = 5:ncol(ghg_raw), names_to = "Year", values_to = "Emissions",
#     values_transform = list(Emissions = as.numeric)) |>
#   group_by(ISO, Country, Year)
#
# ghg_ch4 <- ghg_raw |>
#   filter(Gas %in% c("CH4")) |>
#   # Pivot longer to get ISO, Country, sector, year and emissions
#   pivot_longer(cols = 5:ncol(ghg_raw), names_to = "Year", values_to = "Emissions",
#     values_transform = list(Emissions = as.numeric)) |>
#   group_by(ISO, Country, Year)
#
#
# ghg_co2_total_sector


## -----------------------------------------------------------------------------
#| label: process-data

suppressWarnings({
  # Conversion factors to CO2 equivalent
  conversion_factors <- tibble(
    Gas = c("CO2", "F-Gas", "N2O", "CH4"),
    # Factor = c(1, 1, 265, 28) # From source
    Factor = c(1, 1, 1, 1) # From source
    # Factor = c(1, 1300, 265, 28) # From source
    # Factor = c(1, 1430, 298, 25)
  )

  # Function to process each gas
  process_gas <- function(data, gas_name, factor) {
    data |>
      filter(Gas == gas_name) |>
      pivot_longer(cols = 5:ncol(data), names_to = "Year", values_to = "Emissions", values_transform = list(Emissions = as.numeric)) |>
      mutate(Emissions = Emissions * factor) |>
      group_by(ISO, Country, Sector, Year) |>
      summarize(Emissions = sum(Emissions, na.rm = TRUE), .groups = "drop")
  }

  # Process each gas
  ghg_co2 <- process_gas(ghg_raw, "CO2", conversion_factors |> filter(Gas == "CO2") |> pull(Factor))
  ghg_fgas <- process_gas(ghg_raw, "F-Gas", conversion_factors |> filter(Gas == "F-Gas") |> pull(Factor))
  ghg_n2o <- process_gas(ghg_raw, "N2O", conversion_factors |> filter(Gas == "N2O") |> pull(Factor))
  ghg_ch4 <- process_gas(ghg_raw, "CH4", conversion_factors |> filter(Gas == "CH4") |> pull(Factor))

  # Combine all gases
  ghg_combined <- bind_rows(ghg_co2, ghg_fgas, ghg_n2o, ghg_ch4)

  # Sum up the emissions for each country and sector across all years
  ghg_total_sector <- ghg_combined |>
    group_by(ISO, Country, Sector) |>
    summarize(Emissions = sum(Emissions, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(Emissions))

  # Drop row with the WORLD ISO
  ghg_total_sector <- ghg_total_sector |>
    filter(ISO != "WORLD")

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
    arrange(desc(Total_Emissions), Country, desc(Emissions))

  # Update Country factor levels to reflect the sorted order
  ghg_total_sector$Country <- factor(ghg_total_sector$Country, levels = unique(ghg_total_sector$Country))

  # Remove the Total_Emissions column as it's no longer needed
  ghg_total_sector <- ghg_total_sector |>
    select(-Total_Emissions)

  # Preview the result (Top 10 countries and sector)
  ghg_total_sector |>
    filter(Country != "Others") |>
    gt_preview(10)
})


## -----------------------------------------------------------------------------
#| label: explore-raw2
#| message: false

ghg_raw2 <- read_csv("data/historical_emissions_source2.csv")
gt_preview(ghg_raw2)


## -----------------------------------------------------------------------------
#| label: explore-unique2
#| message: false

ghg_raw2 |>
  summarize(
    n_distinct(Entity),
  )


## -----------------------------------------------------------------------------
#| label: explore-raw3
#| message: false

ghg_raw3 <- read_csv("data/historical_emissions_source3.csv")
gt_preview(ghg_raw3)


## -----------------------------------------------------------------------------
ghg_raw3 <-
  ghg_raw3 |>
  select(Country, Industry, Gas_Type, F1970:F2022)

gt_preview(ghg_raw3)


## -----------------------------------------------------------------------------
#| label: source1-heatmap

# Sum up the emissions for each country across all Sector in the same year
ghg_co2_year <- ghg_co2 |>
  group_by(Country, Year) |>
  summarize(Emissions = sum(Emissions), .groups = "drop")

# Select the countries largest overall 10 emitters and display their emissions over the years in a heatmap
ghg_co2_year |>
  filter(Country %in% c("China", "United States", "India", "Russia", "Japan", "Indonesia", "Iran", "Saudi Arabia", "South Korea", "Canada")) |>
  ggplot(aes(x = Year, y = Country, fill = Emissions)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  scale_x_discrete(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "bottom"
  ) +
  labs(
    title = "Top 10 GHG Emitters",
    subtitle = "Emissions Over The Years",
    x = "Year",
    y = "Country",
    fill = "Emissions (MtCO₂e)"
  )


## -----------------------------------------------------------------------------
#| label: source1-worldmap

world <- map_data("world")

ghg_data <- ghg_co2 |>
  # Filter top 10
  # filter(Country %in% c("China", "United States", "India", "Russia", "Japan", "Indonesia", "Iran", "Saudi Arabia", "South Korea", "Canada")) |>

  # Combine (sum) all the emissions for each country
  group_by(Country) |>
  summarize(Emissions = sum(Emissions)) |>
  # Ensure the region column matches the map data
  mutate(region = Country)


# Join the map data with the emissions data
world_map <- left_join(world, ghg_data, by = c("region" = "Country"))

# Plot the world map
ggplot(world_map, aes(x = long, y = lat, group = group, fill = Emissions)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "red", na.value = "gray") +
  coord_fixed(1.3) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "right"
  ) +
  labs(
    title = "Top 10 GHG Emitters",
    subtitle = "Emissions in 2020",
    fill = "Emissions (MtCO₂e)"
  )


## -----------------------------------------------------------------------------
#| label: source1-line

ghg_co2 |>
  filter(Country %in% c("China", "United States", "India", "Russia", "Japan", "Indonesia", "Iran", "Saudi Arabia", "South Korea", "Canada")) |>
  ggplot(aes(x = Year, y = Emissions, color = Country, shape = Country)) +
  geom_point() + # Scatterplot
  geom_smooth(method = "lm", se = FALSE) +
  # scale_y_log10() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "bottom"
  ) +
  labs(
    title = "GHG Emissions Over the Years",
    subtitle = "For Top 3 Emitting Countries",
    x = "Year",
    y = "Emissions (MtCO₂e)",
    color = "Country",
    shape = "Country"
  )


## -----------------------------------------------------------------------------
#| label: source1-pie

# Pie chart of the top 10 countries + Others
ghg_total_sector |>
  ggplot(aes(x = "", y = Emissions, fill = Country)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "right"
  ) +
  labs(
    title = "Top 10 GHG Emitters",
    subtitle = "Emissions by Country",
    fill = "Country"
  )

# Pie chart of the top 10 sectors
ghg_total_sector |>
  ggplot(aes(x = "", y = Emissions, fill = Sector)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "right"
  ) +
  labs(
    title = "Top 10 GHG Emitters",
    subtitle = "Emissions by Sector",
    fill = "Sector"
  )


## -----------------------------------------------------------------------------
#| label: source1-stackedbar

ghg_total_sector |>
  ggplot(aes(x = Country, y = Emissions, fill = Sector)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "bottom"
  ) +
  labs(
    title = "Top 10 GHG Emitters",
    subtitle = "Emissions by Sector",
    x = "Country",
    y = "Emissions (MtCO₂e)",
    fill = "Sector"
  )

# Assuming ghg_total_sector is your original data frame
# Summarize the emissions of the top 10 countries
top_10_countries <- ghg_total_sector %>%
  group_by(Country) %>%
  summarize(Total_Emissions = sum(Emissions)) %>%
  filter(Country != "Others") %>% # Remove the "Others" category
  top_n(10, Total_Emissions)

# Create a new data frame for the top 10 countries
top_10_data <- ghg_total_sector %>%
  filter(Country %in% top_10_countries$Country) %>%
  group_by(Sector) %>%
  summarize(Emissions = sum(Emissions)) %>%
  mutate(Country = "Top 10")

# Create a new data frame for the "Others" countries
others_data <- ghg_total_sector %>%
  filter(!Country %in% top_10_countries$Country) %>%
  group_by(Sector) %>%
  summarize(Emissions = sum(Emissions)) %>%
  mutate(Country = "Others")

# Combine the data frames
final_data <- bind_rows(top_10_data, others_data)

# Plot the final stacked bar chart
ggplot(final_data, aes(x = Country, y = Emissions, fill = Sector)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "bottom"
  ) +
  labs(
    title = "Top 10 GHG Emitters",
    subtitle = "Emissions by Sector",
    x = "Country",
    y = "Emissions (MtCO₂e)",
    fill = "Sector"
  )



## -----------------------------------------------------------------------------
#| label: source4-raw
#| message: false

expenditure <- read_csv("data/energy_transition.csv")

expenditure <-
  expenditure |>
  filter(Indicator %in% "Electricity Generation") |>
  mutate(across(F2000:F2022, ~ replace_na(., 0))) |>
  group_by(Country, ISO3) |>
  summarize(across(F2000:F2021, sum), .groups = "drop")

gt_preview(expenditure)


## -----------------------------------------------------------------------------
#| label: source4-line1
#| message: false

expenditure <-
  expenditure |>
  filter(ISO3 %in% c("CHN", "USA", "IND")) |>
  rename_with(~ str_replace(., "F", "")) |>
  pivot_longer(cols = 3:24, names_to = "Year", values_to = "Energy")

expenditure$Year <- as.numeric(as.character(expenditure$Year))
gt_preview(expenditure)


ggplot(expenditure, aes(x = Year, y = Energy, color = Country)) +
  geom_line(linewidth = 1) + # Line graph
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "bottom"
  ) +
  labs(
    title = "Energy Generated Over the Years",
    subtitle = "For Top 3 Emitting Countries",
    x = "Year",
    y = "Energy Generated (GWh)",
    color = "Country",
    shape = "Country"
  )


## -----------------------------------------------------------------------------
#| label: source1-donut

# Top 3 sectors in each country by Emissions data
ghg_total_sectors <- ghg_combined |>
  group_by(ISO, Country, Sector) |>
  summarize(Emissions = sum(Emissions, na.rm = TRUE), .groups = "drop") |>
  # filter(Sector != "Energy") |> #Filter Energy
  group_by(ISO, Country) |>
  top_n(3, Emissions) |>
  arrange(desc(Emissions)) |>
  ungroup()

# Top 10 countries by Emissions data
ghg_total_countries <- ghg_combined |>
  filter(ISO != "WORLD") |>
  group_by(ISO, Country) |>
  summarize(Emissions = sum(Emissions, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(Emissions)) |>
  slice(1:10) |>
  select(ISO) |>
  left_join(ghg_total_sectors, by = "ISO")

countries <- ghg_total_countries$ISO |>
  unique()

# Get the population data from the World Bank API
population_data <- wb_data("SP.POP.TOTL", country = countries, start_date = 1990, end_date = 2021) |>
  rename(population = SP.POP.TOTL) |>
  select(iso3c, population) |>
  group_by(iso3c) |>
  summarize(avg_population = mean(population))

# Merge the population data with the emissions data
ghg_total_countries <- ghg_total_countries |>
  left_join(population_data, by = c("ISO" = "iso3c"))

# Adding population data before filtering top 10 countries
ghg_total_countries_2 <- ghg_combined |>
  filter(ISO != "WORLD") |>
  left_join(population_data, by = c("ISO" = "iso3c")) |>
  group_by(ISO, Country, avg_population) |>
  summarize(Emissions = sum(Emissions, na.rm = TRUE), .groups = "drop") |>
  mutate(Emissions_per_capita_country = Emissions / avg_population) |>
  arrange(desc(Emissions_per_capita_country)) |>
  slice(1:10) |>
  select(ISO, avg_population, Emissions_per_capita_country) |>
  left_join(ghg_total_sectors, by = "ISO") |>
  mutate(Emissions_per_capita = Emissions / avg_population)

PieDonut(
  ghg_total_countries,
  aes(x = Country, y = Sector, count = Emissions),
  showPieName = FALSE,
  labelposition = 1,
  title = "Top 10 GHG Emitters",
  pieLabelSize = 4,
  donutLabelSize = 3,
  showRatioThreshold = 0.001,
  showRatioPie = FALSE,
  showRatioDonut = FALSE,
  explodeDonut = TRUE
  
)

PieDonut(
  ghg_total_countries_2,
  aes(x = Country, y = Sector, count = Emissions_per_capita),
  showPieName = FALSE,
  labelposition = 1,
  title = "Top 10 GHG Emitters per capita",
  pieLabelSize = 2,
  donutLabelSize = 2,
  showRatioThreshold = 0.001,
  showRatioPie = FALSE
)

gt_preview(ghg_total_countries_2, 10)


## -----------------------------------------------------------------------------
#| label: circular_bar_chart
#| message: false

# Circular bar chart of the top 10 countries
ggplot(ghg_total_countries, aes(x=reorder(Country, Emissions), y=Emissions, fill=Sector)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar(start = 0) +
  ylim(-max(ghg_total_countries$Emissions) * 1.2, max(ghg_total_countries$Emissions) * 2) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(rep(0.1,4), "cm"),
    legend.position = "right"
  ) +
  geom_text_repel(aes(label=Emissions), position = position_stack(vjust = 0.5), size=2) +
  geom_text(aes(x=Country, y=0, label=Country), hjust=0.5, size=2.5, angle=25) +
  labs(
    title = "Top 10 GHG Emitters",
    fill = "Sector"
  )


