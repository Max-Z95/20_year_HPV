# Figure 1. Vaccine impact (by year of vaccine introduction) - deaths averted
# Load packages

library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)
library(writexl)

# Read results files

results_real_world <- fread("results/observed_2026-01-06.csv")
results_what_if    <- fread("results/whatif_2026-01-06.csv")
vacc_age = 14 # specify vaccination age for year of introduction calculation

# burden calculation
compute_global_deaths_averted <- function(results) {
  
  burden <- as.data.table(results)
  burden[, deaths := cohort_size * mort.cecx]
  burden[, year_intro := birthcohort + vacc_age]
  
  global <- burden[, .(
    deaths = sum(deaths, na.rm = TRUE)
  ), by = .(scenario, year_intro)]
  
  global_wide <- dcast(
    global,
    year_intro ~ scenario,
    value.var = "deaths"
  )
  
  global_wide[, deaths_averted :=
                `pre-vaccination` - `post-vaccination`]
  
  return(global_wide)
}

real_global   <- compute_global_deaths_averted(results_real_world)
whatif_global <- compute_global_deaths_averted(results_what_if)

real_global[,   scenario := "Real-world"]
whatif_global[, scenario := "What-if"]

combined <- rbind(real_global, whatif_global)

setorder(combined, scenario, year_intro)

combined[, cum_deaths_averted :=
           cumsum(deaths_averted),
         by = scenario]

cum_wide <- dcast(
  combined,
  year_intro ~ scenario,
  value.var = "cum_deaths_averted"
)

cum_wide[, additional_impact :=
           `What-if` - `Real-world`]

# Plot
plot_dt_intro <- melt(
  cum_wide,
  id.vars = "year_intro",
  measure.vars = c("Real-world", "What-if"),
  variable.name = "Scenario",
  value.name = "cum_deaths_averted"
)

plot_dt_intro[, Scenario := factor(
  Scenario,
  levels = c("Real-world", "What-if"),
  labels = c(
    "Routine vaccination (real-world coverage)",
    "Routine vaccination (90% coverage)"
  )
)]

p <- ggplot() +
  geom_ribbon(
    data = cum_wide,
    aes(
      x = year_intro,
      ymin = `Real-world`,
      ymax = `What-if`
    ),
    fill = "#4C72B0",
    alpha = 0.18,
    inherit.aes = FALSE
  ) +
  
  geom_line(
    data = plot_dt_intro,
    aes(
      x = year_intro,
      y = cum_deaths_averted,
      colour = Scenario,
      linetype = Scenario
    ),
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
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_continuous(
    limits = c(2006, 2024),
    breaks =seq(2006, 2024, by = 5),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Year of vaccine introduction",
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
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "top",
    legend.text = element_text(size = 11)
  )

print(p) 

# save plot and results - uncomment the following

# ggsave(
  # "Figure_Cumulative_Global_Deaths_Averted_IntroYear.png",
  # p,
  # width = 8,
  # height = 5,
  # dpi = 300
# )

# write_xlsx(
  # cum_wide,
  # "Cumulative_Global_Deaths_Averted_by_IntroYear.xlsx"
# )

