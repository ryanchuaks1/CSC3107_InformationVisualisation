## -----------------------------------------------------------------------------
#| label: library
#| message: false

library(tidyverse)
library(readxl)
library(knitr)
library(gt)
library(dplyr)

# Map
library(sf)
library(tmap)
library(patchwork)
library(scales)

# Graphical Visualization
library(ggplot2)
library(ggrepel)
library(maps)
library(webr) # PieDonut
library(ggiraphExtra) # ggpiedonut
library(moonBook) # ggpiedonut

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

# Process each gas
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

# Save all countries total emissions for later use
all_countries_total_emissions <- country_total_emissions

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

# Preview the result (Top 10 countries and sector)
ghg_total_sector |>
  filter(Country != "Others") |>
  gt_preview(10)


## -----------------------------------------------------------------------------
#| label: all-countries-emissions

all_countries_total_emissions |>
  gt_preview(10)


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
#| label: source1-worldmap

data(World)

# Percentage of country/all countries total emissions
all_countries_total_emissions <- all_countries_total_emissions |>
  mutate(pct_total_emissions = Total_Emissions / sum(Total_Emissions) * 100)

world <-
  World |>
  left_join(all_countries_total_emissions, by = c("name" = "Country")) |>
  select(name, area, iso_a3, Total_Emissions, pct_total_emissions) |>
  mutate(
    country_label = if_else(min_rank(desc(Total_Emissions)) <= 3,
      paste0(
        rank(desc(Total_Emissions)), ". ", iso_a3, "(", round(pct_total_emissions, 2), "%)"
        ),
      ""
    ),
    label_colour = if_else(min_rank(desc(Total_Emissions)) > 2, "black", "white"),
    pct_country_label = if_else(
      min_rank(desc(pct_total_emissions)) <= 10, 
      paste0(round(pct_total_emissions, 2), "%"),
      ""
      ),
    pct_label_color = if_else(
      !is.na(pct_total_emissions) & min_rank(desc(pct_total_emissions)) > 2, 
      "black",
      "white"
      )
  )
world


## -----------------------------------------------------------------------------
#| label: source1-worldmap-earthplot

# Define the base earth polygon for the map background
earth <- st_polygon(
  x = list(
    cbind(
      c(rep(-180, 181), rep(180, 181), -180), c(-90:90, 90:-90, -90)
    )
  )
) |>
  st_sfc() |>
  st_set_crs(4326) |> # Equirectangular projection
  st_as_sf()


## -----------------------------------------------------------------------------
#| label: source1-worldmap-ggplot

# Define a function for the base map plot
gg_ghg <- function(atm_title) {
  ggplot(world) +
    geom_sf(data = earth, fill = "aliceblue") +
    geom_sf() +
    labs(title = atm_title) +
    coord_sf(crs = "ESRI:54035") +
    theme_void() +
    theme(
      legend.margin = margin(3, 0, 0, 0), # Increase margin above legend
      legend.key.width = unit(1.2, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.frame = element_rect(), # Add a frame around the colorbar
      legend.position = "top",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 10)
    )
}


## -----------------------------------------------------------------------------
#| label: fig-source1-worldmap-plot
#| fig.cap: "Choropleth map of GHG emissions by country."

gg_ghg_total_emissions <- gg_ghg("Top GHG Emitters (as % of World Total)") +
  aes(fill = Total_Emissions) +
  labs(fill = NULL, caption = "Source: World Resources Institute") +
  scale_fill_fermenter(
    palette = "OrRd",
    direction = 1,
    labels = label_number(
      scale = 1e-6,
      suffix = "M"
    )
  ) +
  geom_sf_text(
    aes(label = country_label, color = label_colour),
    data = world,
    size = 1.25
  ) +
  scale_color_identity()

gg_ghg_pct <-
  gg_ghg("Percentage of Total GHG Emissions") +
  aes(fill = pct_total_emissions) +
  labs(fill = NULL, caption = "Source: World Resources Institute") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_sf_text(
    aes(label = pct_country_label, color = pct_label_color),
    data = world,
    size = 2
  ) +
  scale_color_identity()

gg_ghg_total_emissions


## -----------------------------------------------------------------------------
#| label: fig-source1-worldmap-plot-pct
#| fig.cap: "Choropleth map of GHG emissions by country (Percentage of total emissions)."

gg_ghg_pct


## -----------------------------------------------------------------------------
#| label: pie-chart-function

# Function to create pie chart
plot_pie_chart <- function(data, fill_var, title, subtitle) {
  data <- data |>
    group_by(.data[[fill_var]]) |>
    summarize(Emissions = sum(Emissions)) |>
    mutate(
      fraction = Emissions / sum(Emissions),
      ymax = cumsum(fraction),
      ymin = c(0, head(ymax, n = -1)),
      label = as.character(.data[[fill_var]])
    )

  ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = .data[[fill_var]])) +
    geom_rect() +
    geom_text_repel(
      aes(y = (ymin + ymax) / 2, label = label, x = 3.5),
      xlim = c(5, NA),
      nudge_x = 1,
      show.legend = FALSE
    ) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none" # Remove legend
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      fill = fill_var,
      caption = "Source: World Resources Institute"
    ) +
    scale_fill_brewer(palette = "Set3")
}


## -----------------------------------------------------------------------------
#| label: source1-piecharts

# Pie chart of the top 10 countries + Others
source_1_pie_country <- plot_pie_chart(
  data = ghg_total_sector,
  fill_var = "Country",
  title = "Top 10 GHG Emitters",
  subtitle = "Emissions by Country"
)

# Pie chart of the top 10 sectors
source_1_pie_sector <- plot_pie_chart(
  data = ghg_total_sector,
  fill_var = "Sector",
  title = "Top 10 GHG Emitters",
  subtitle = "Emissions by Sector"
)


## -----------------------------------------------------------------------------
#| label: source1-pie-country-plot
#| fig.cap: "Pie Charts of the top 10 countries and sectors by emissions."
#| fig.width: 10
#| fig.height: 10

source_1_pie_country


## -----------------------------------------------------------------------------
#| label: source1-pie-sector-plot
#| fig.cap: "Pie Charts of the top 10 countries and sectors by emissions."
#| fig.width: 10
#| fig.height: 10

source_1_pie_sector


## -----------------------------------------------------------------------------
#| label: source1-stackedbar-function

# Function to create a stacked bar chart
plot_stacked_bar <- function(data, x_var, title, subtitle) {
  ggplot(data, aes(x = .data[[x_var]], y = Emissions, fill = Sector)) +
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
      title = title,
      subtitle = subtitle,
      x = x_var,
      y = "Emissions (MtCO₂e)",
      fill = "Sector"
    )
}


## -----------------------------------------------------------------------------
#| label: fig-source1-stackedbar
#| fig.cap: "Stacked bar chart of emissions by country."

# Stacked bar chart for emissions by country
source1_stackedbar <- plot_stacked_bar(
  data = ghg_total_sector,
  x_var = "Country",
  title = "Top 10 GHG Emitters",
  subtitle = "Emissions by Sector"
)

source1_stackedbar


## -----------------------------------------------------------------------------
#| label: fig-stacked-barchart-2
#| fig.cap: "Stacked bar chart of emissions Top10 Vs Remaining 184 countries."

# Summarize the emissions of the top 10 countries
top_10_countries <- ghg_total_sector |>
  group_by(Country) |>
  summarize(Total_Emissions = sum(Emissions)) |>
  filter(Country != "Others") |>
  top_n(10, Total_Emissions)

# Create a new data frame for the top 10 countries
top_10_data <- ghg_total_sector |>
  filter(Country %in% top_10_countries$Country) |>
  group_by(Sector) |>
  summarize(Emissions = sum(Emissions)) |>
  mutate(Country = "Top 10")

# Create a new data frame for the "Others" countries
others_data <- ghg_total_sector |>
  filter(!Country %in% top_10_countries$Country) |>
  group_by(Sector) |>
  summarize(Emissions = sum(Emissions)) |>
  mutate(Country = "Remaining 184 Countries")

# Combine the data frames
final_data <- bind_rows(top_10_data, others_data)

# Stacked bar chart for top 10 countries vs remaining 184 countries
stacked_barchart_2 <- plot_stacked_bar(
  data = final_data,
  x_var = "Country",
  title = "Top 10 GHG Emitters vs Remaining 184 Countries",
  subtitle = "Emissions by Sector"
)

stacked_barchart_2


## -----------------------------------------------------------------------------
#| label: fig-source1-donut
#| fig.cap: "Donut chart of the top 10 countries and sectors by emissions."
#| fig.width: 9
#| fig.height: 5

# Plot the donut chart of the top 10 countries and top 3 sectors within each country
ghg_total_countries <- ghg_total_sector |>
  mutate(Country = as.character(Country)) |>
  filter(Country != "Others") |>
  group_by(Country) |>
  top_n(3, Emissions) |>
  ungroup()

PieDonut(
  ghg_total_countries,
  aes(x = Country, y = Sector, count = Emissions),
  showPieName = FALSE,
  labelposition = 1,
  title = "Top 10 GHG Emitters",
  pieLabelSize = 2,
  donutLabelSize = 3,
  showRatioThreshold = 0.001,
  showRatioPie = FALSE,
  showRatioDonut = FALSE,
  explodeDonut = TRUE
)

# Switch Russia Energy and Electricity/Heat rows
# Replace "Land-Use Change and Forestry" in Sector column with "LUCF"
# Above are done so to fit the labels into the chart
ghg_total_countries_gg <- ghg_total_countries |>
  slice(c(1:9, 11, 10, 12:n())) |>
  mutate(Sector = ifelse(Sector == "Land-Use Change and Forestry", "LUCF", Sector))

ggPieDonut(
  ghg_total_countries_gg,
  aes(pies = Country, donuts = Sector, count = Emissions),
  showRatioPie = FALSE,
  showRatioDonut = FALSE,
  title = "Top 10 GHG Emitters",
  labelposition = 1
) +
  geom_col(aes(x = -1, y = 0))


## -----------------------------------------------------------------------------
#| label: source1-donut-with-population-data

# Retrieving the top 10 countries ISO
top_10_countries_with_iso <- top_10_countries |>
  mutate(Country = as.character(Country)) |>
  left_join(ghg_combined |>
    select(ISO, Country) |>
    distinct(), by = "Country")

countries <- top_10_countries_with_iso$ISO

# Get the population data from the World Bank API
population_data <- wb_data("SP.POP.TOTL", country = countries, start_date = 1990, end_date = 2021) |>
  rename(population = SP.POP.TOTL) |>
  select(iso3c, population) |>
  group_by(iso3c) |>
  summarize(avg_population = mean(population))

# Top 3 sectors in each country by Emissions data
ghg_total_sectors <- ghg_combined |>
  group_by(ISO, Country, Sector) |>
  summarize(Emissions = sum(Emissions, na.rm = TRUE), .groups = "drop") |>
  group_by(ISO, Country) |>
  top_n(3, Emissions) |>
  arrange(desc(Emissions)) |>
  ungroup()

# Adding population data before filtering top 10 countries per capita
ghg_total_countries_pop <- ghg_combined |>
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


## -----------------------------------------------------------------------------
#| label: fig-source1-donut-population

PieDonut(
  ghg_total_countries_pop,
  aes(x = Country, y = Sector, count = Emissions_per_capita),
  showPieName = FALSE,
  labelposition = 1,
  title = "Top 10 GHG Emitters per capita",
  pieLabelSize = 4,
  donutLabelSize = 3,
  showRatioThreshold = 0.001,
  showRatioPie = FALSE,
  showRatioDonut = FALSE,
  explodeDonut = TRUE
)


## -----------------------------------------------------------------------------
#| label: fig-circular_bar_chart
#| message: false

# Circular bar chart of the top 10 countries
ggplot(ghg_total_countries, aes(x = reorder(Country, Emissions), y = Emissions, fill = Sector)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(start = 0) +
  ylim(-max(ghg_total_countries$Emissions) * 1.2, max(ghg_total_countries$Emissions) * 2) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(rep(0.1, 4), "cm"),
    legend.position = "right"
  ) +
  geom_text_repel(aes(label = Emissions), position = position_stack(vjust = 0.5), size = 2) +
  geom_text(aes(x = Country, y = 0, label = Country), hjust = 0.5, size = 2.5, angle = 25) +
  labs(
    title = "Top 10 GHG Emitters",
    fill = "Sector"
  )


## -----------------------------------------------------------------------------
#| label: fig-source1-line-others

# Line graph of the top 10 countries vs "Others"

countries_top_10 <- top_10_countries_with_iso$ISO
ghg_top_10_vs_others <- ghg_raw |>
  filter(ISO != "WORLD") |>
  filter(!if_any(everything(), ~ grepl("N/A", .))) |>
  mutate(Country = ifelse(ISO %in% countries_top_10, "Top 10", "184 Remaining Countries")) |>
  select(-Sector, -Gas, -ISO) |>
  mutate(across(-Country, as.numeric)) |>
  group_by(Country) |>
  summarize(across(everything(), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Emissions")

ggplot(ghg_top_10_vs_others, aes(x = Year, y = Emissions, group = Country, color = Country)) +
  geom_line(linewidth = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Top 10 GHG Emitters vs Remaining 184 Countries",
    subtitle = "Emissions Over the Years",
    x = "Year",
    y = "Emissions (MtCO2e)",
    color = "Country"
  )

gt_preview(ghg_top_10_vs_others)


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

