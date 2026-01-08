# Figure 2 - map: Number of deaths averted over the lifetime of vaccinated cohort

# load packages
library(data.table)
library(sf)
library(ggplot2)
library(scales)
library(dplyr)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)

# load results file
results <- fread("results/observed_2026-01-06.csv")  # results - real world coverage scenario
# results_what_if    <- fread("results/whatif_2026-01-06.csv")

dt <- as.data.table(results)

dt[, `:=`(
  cases  = cohort_size * inc.cecx,
  deaths = cohort_size * mort.cecx,
  yll    = cohort_size * lifey,
  yld    = cohort_size * disability
)]

dt[, dalys := yll + yld]

country_lifetime <- dt[
  ,
  .(
    cases  = sum(cases,  na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    dalys  = sum(dalys,  na.rm = TRUE)
  ),
  by = .(country, scenario)
]
country_lifetime[, scenario := fifelse(
  scenario == "pre-vaccination", "pre",
  "post"
)]

country_wide <- dcast(
  country_lifetime,
  country ~ scenario,
  value.var = c("cases", "deaths", "dalys")
)

country_wide[, `:=`(
  cases_averted  = cases_pre  - cases_post,
  deaths_averted = deaths_pre - deaths_post,
  dalys_averted  = dalys_pre  - dalys_post
)]

map_long <- country_wide[
  ,
  .(
    cases_averted,
    deaths_averted,
    dalys_averted
  ),
  by = country
] |>
  melt(
    id.vars = "country",
    variable.name = "metric",
    value.name = "value"
  )

map_long[, metric := factor(
  metric,
  levels = c("cases_averted", "deaths_averted", "dalys_averted"),
  labels = c("Cases averted", "Deaths averted", "DALYs averted")
)]

map_long_clean <- melt(
  country_wide,
  id.vars = "country",
  measure.vars = c("cases_averted", "deaths_averted", "dalys_averted"),
  variable.name = "metric",
  value.name = "value"
)

world <- ne_countries(scale = "medium", returnclass = "sf")

world_fixed <- world |>
  mutate(
    iso3 = ifelse(iso_a3 == "-99", iso_a3_eh, iso_a3)
  )

map_sf <- world_fixed |>
  left_join(map_long_clean, by = c("iso3" = "country"), relationship = "many-to-many")

# plot
plot_one_map <- function(sf_data, metric_name, title_text, fill_label) {
  
  ggplot(
    sf_data[sf_data$metric == metric_name | is.na(sf_data$metric), ]
  ) +
    geom_sf(
      aes(fill = value),
      colour = "white",
      linewidth = 0.15
    ) +
    scale_fill_viridis_c(
      option = "plasma",
      labels = comma,
      na.value = "grey90",
      guide = guide_colorbar(
        barheight = unit(35, "mm"),
        barwidth  = unit(6, "mm"),
        title.position = "top"
      )
    ) +
    labs(
      title = title_text,
      fill  = fill_label
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title   = element_text(face = "bold", size = 13, hjust = 0.5),
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9),
      legend.position = "right"
    )
}


p_cases <- plot_one_map(
  map_sf,
  metric_name = "cases_averted",
  title_text  = "Cervical cancer cases averted",
  fill_label  = "Cases averted"
)

p_deaths <- plot_one_map(
  map_sf,
  metric_name = "deaths_averted",
  title_text  = "Cervical cancer deaths averted",
  fill_label  = "Deaths averted"
)

p_dalys <- plot_one_map(
  map_sf,
  metric_name = "dalys_averted",
  title_text  = "DALYs averted",
  fill_label  = "DALYs averted"
)


p_map_final <-
  p_cases /
  p_deaths /
  p_dalys 

p_map_final

# save plot
ggsave(
  filename = "Figure_Map_Cases_Deaths_DALYs_Averted.png",
  plot = p_map_final,
  width = 8,
  height = 12,
  dpi = 300
)


