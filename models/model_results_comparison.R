rm(list = ls())
pacman::p_load(tidyverse)
# default parameters
# RH = 80%
# MAT = 15
# d18p = -15
# Dp17p = 10

# steady state model ----
source('models/BASSmodel_forward.R')
vars = ctrl()
vars$RH = .8
vars$d18p = -14
vars$evap = 1e-10
for (i in 1:100) {
  vars$depth = i
  results = BASS(vars)
  results$depth = i
  if (i == 1) {
    sims_bass = results
  } else {
    sims_bass = rbind(sims_bass, results)
  }
}

plot(sims_bass$Dp17sw, sims_bass$depth, type = "l",
     ylim = rev(range(sims_bass$depth)))

# numerical model ----
source('models/soil_evap_numerical_model.R')
vars = ctrl()
vars$d18_p = -13
sims_num = soil_evap_model(vars)
sims_num = sims_num |>
  mutate(dp18_sw = 1e3 * log(d18_sw / 1e3 + 1))

plot(sims_num$d18_sw, sims_num$z, type = "l",
     ylim = rev(range(sims_num$z)))

# isolated water body model ----
source('models/IWB_model.R')
vars = ctrl()
vars$RH = .8
vars$d18p = -14
sims_iwb = IWB(vars)

# comparison ----
red_clay = read_xlsx("data/Dp17.xlsx")

base_text_size = 10
ggplot() +
  geom_path(data = sims_bass, aes(x = dp18sw, y = Dp17sw, color = depth)) +
  # geom_path(data = sims_num, aes(x = dp18_sw, y = Dp17_sw), linetype = "dotted") +
  geom_path(data = sims_iwb, aes(x = dp18sw, y = Dp17sw), linetype = "dashed") +
  geom_point(data = red_clay,
             aes(x = dp18sw, y = Dp17sw, fill = section),
             size = 3, shape = 21) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_viridis_c(option = "turbo") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = base_text_size * 1),
    axis.title = element_text(size = base_text_size * 1.2),
    legend.text = element_text(size = base_text_size * 1),
    plot.title = element_text(size = base_text_size * 1.5),
    plot.subtitle = element_text(size = base_text_size * 1),
    plot.caption = element_text(size = base_text_size * .8)
  ) +
  labs(title = "Model-Data Comparison",
       subtitle = "isolated water body, steady state, and numerical modeling",
       caption = expression("RH = 80%, "*delta^"18"*"O"[p]*" = -15 (\u2030), "*Delta^"'17"*"O"[p]*" = 10 (per meg)"),
       x = expression(delta^"18"*"O"[sw]*" (\u2030)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg)"), fill = "")
ggsave("figures/model_comparison.jpg", width = 4.1, height = 3.8, dpi = 500)






