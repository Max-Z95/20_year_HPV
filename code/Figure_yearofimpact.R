# Figure 1. Vaccine impact (by year of impact) - deaths averted

# load packages
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)
library(writexl)

# Read results files
results_real_world <- fread("results/observed_bycalendaryear_2026-01-08.csv")
results_what_if    <- fread("results/whatif_bycalendaryear_2026-01-08.csv")

# burden calculation and format

compute_global_deaths_averted_by_impact_year <- function(results) {
  
  burden <- as.data.table(results)
  
  burden[, deaths := cohort_size * mort.cecx]
  
  global <- burden[, .(
    deaths = sum(deaths, na.rm = TRUE)
  ), by = .(scenario, year)]
  
  global_wide <- dcast(
    global,
    year ~ scenario,
    value.var = "deaths"
  )
  
  global_wide[, deaths_averted :=
                `pre-vaccination` - `post-vaccination`]
  
  return(global_wide)
}

real_global_impact   <- compute_global_deaths_averted_by_impact_year(results_real_world)
whatif_global_impact <- compute_global_deaths_averted_by_impact_year(results_what_if)

real_global_impact[,   scenario := "Real-world"]
whatif_global_impact[, scenario := "What-if"]

combined_impact <- rbind(real_global_impact, whatif_global_impact)
INTRO_YEAR <- 2006
combined_impact <- combined_impact[year >= INTRO_YEAR]

year_min <- min(combined_impact$year, na.rm = TRUE)
year_max <- max(combined_impact$year, na.rm = TRUE)

full_years <- CJ(
  year = year_min:year_max,
  scenario = unique(combined_impact$scenario)
)

combined_impact <- full_years[
  combined_impact,
  on = .(year, scenario)
]

combined_impact[is.na(deaths_averted), deaths_averted := 0]

setorder(combined_impact, scenario, year)

combined_impact[, cum_deaths_averted :=
                  cumsum(deaths_averted),
                by = scenario]

cum_wide_impact <- dcast(
  combined_impact,
  year ~ scenario,
  value.var = "cum_deaths_averted"
)

cum_wide_impact[, additional_impact :=
                  `What-if` - `Real-world`]

# plot
plot_dt <- melt(
  cum_wide_impact,
  id.vars = "year",
  measure.vars = c("Real-world", "What-if"),
  variable.name = "Scenario",
  value.name = "cum_deaths_averted"
)

plot_dt[, Scenario := factor(
  Scenario,
  levels = c("Real-world", "What-if"),
  labels = c(
    "Routine vaccination (real-world coverage)",
    "Routine vaccination (90% coverage)"
  )
)]
y_max <- max(cum_wide_impact$`What-if`,na.rm = TRUE)
p_impact <- ggplot() +
  geom_ribbon(
    data = cum_wide_impact,
    aes(x = year,
        ymin = `Real-world`,
        ymax = `What-if`),
    fill = "#4C72B0",
    alpha = 0.18,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = plot_dt,
    aes(x = year,
        y = cum_deaths_averted,
        colour = Scenario,
        linetype = Scenario),
    linewidth = 1.2
  ) +
  scale_colour_manual(
    values = c(
      "Routine vaccination (real-world coverage)" = "#2F4B7C",
      "Routine vaccination (90% coverage)"        = "#2F4B7C"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Routine vaccination (real-world coverage)" = "solid",
      "Routine vaccination (90% coverage)"        = "22"
    )
  ) +
  scale_y_continuous(
    limits = c(0, y_max * 1.08),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(2006, 2110),
    breaks =seq(2006, 2110, by = 20),
    expand = c(0, 0)
  ) +
  labs(
    x = "Year of impact",
    y = "Cumulative deaths averted",
    colour = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    axis.line = element_line(colour = "grey40"),
    axis.ticks = element_line(colour = "grey40"),
    legend.position = "top"
  )


print(p_impact)

# save plot and results - uncomment the following
# ggsave(
  # "Figure_Cumulative_Global_Deaths_Averted_ImpactYear.png",
  # p_impact,
  # width = 8,
  # height = 5,
  # dpi = 300
# )

# write_xlsx(
  # cum_wide_impact,
  # "Cumulative_Global_Deaths_Averted_by_ImpactYear.xlsx"
# )



