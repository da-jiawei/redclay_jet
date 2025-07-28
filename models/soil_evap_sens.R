rm(list = ls())
pacman::p_load(tidyverse, readxl, ggpubr)
source('models/soil_evap_numerical_model.R')
theme = theme(axis.ticks.length=unit(0.15, "cm"),
              axis.ticks = element_line(colour = "black"),
              text = element_text(color = "black", size = 12),
              axis.title = element_text(size = 15), 
              axis.text = element_text(color = "black", size = 12),
              legend.text = element_text(size = 12),
              panel.grid = element_blank())

# load data ----
measurements = read_xlsx("data/Dp17.xlsx")
results_10days = read_csv("output/soil_evap_10days.csv")
results_20days = read_csv("output/soil_evap_20days.csv")
results_10days_40RH = read_csv("output/soil_evap_10days_40RH.csv")
results_10days_60RH = read_csv("output/soil_evap_10days_60RH.csv")

# RH = 80% ----
vars = ctrl()
# 10 days
results_10days = soil_evap_model(vars)
results_10days = results_10days |>
  mutate(dp18_sw = 1e3 * log(d18_sw / 1e3 + 1))
write.csv(results_10days, file = "output/soil_evap_10days.csv")
# 20 days
vars$rundays = 20
results_20days = soil_evap_model(vars)
results_20days = results_20days |>
  mutate(dp18_sw = 1e3 * log(d18_sw / 1e3 + 1))
write.csv(results_20days, file = "output/soil_evap_20days.csv")
# 1 day
vars$rundays = 1
results_1day = soil_evap_model(vars)
results_1day = results_1day |>
  mutate(dp18_sw = 1e3 * log(d18_sw / 1e3 + 1))
write.csv(results_1day, file = "output/soil_evap_1day.csv")
# 1 day, max porosity and tortuosity
vars = ctrl()
vars$rundays = 1
vars$pore = .99
vars$tort = .99
results_1day_free_air = soil_evap_model(vars)
results_1day_free_air = results_1day_free_air |>
  mutate(dp18_sw = 1e3 * log(d18_sw / 1e3 + 1))

# depth profile
p1 = ggplot() +
  geom_path(data = results_10days,
            aes(x = d18_sw, y = z),
            linetype = "dashed") +
  geom_path(data = results_20days,
            aes(x = d18_sw, y = z)) +
  geom_path(data = results_1day_free_air,
            aes(x = d18_sw, y = z)) +
  scale_y_reverse() +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030)"),
       y = "depth (cm)")

p2 = ggplot() +
  geom_path(data = results_10days,
            aes(x = Dp17_sw, y = z),
            linetype = "dashed") +
  geom_path(data = results_20days,
            aes(x = Dp17_sw, y = z)) +
  geom_path(data = results_1day_free_air,
            aes(x = Dp17_sw, y = z)) +
  scale_y_reverse() +
  theme_bw() + theme +
  labs(x = expression(Delta^"'17"*"O"[sw]*" (per meg)"),
       y = "depth (cm)")
ggarrange(p1, p2, nrow = 1, ncol = 2, align = "hv")

ggplot() +
  geom_path(data = results_1day_free_air,
            aes(x = dp18_sw, y = Dp17_sw),
            linewidth = .5, color = "red") +
  geom_path(data = results_10days,
            aes(x = dp18_sw, y = Dp17_sw),
            linewidth = .5, color = "blue") +
  geom_path(data = results_20days,
            aes(x = dp18_sw, y = Dp17_sw),
            linewidth = .5, color = "black") +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg)"),
       fill = "", color = "RH (%)")

# RH = 60% ----
vars = ctrl()
vars$RH_air = 60
results_10days_60RH = soil_evap_model(vars)
results_10days_60RH = results_10days_60RH |>
  mutate(dp18_sw = 1e3 * log(d18_sw / 1e3 + 1))
write.csv(results_10days_60RH, file = "output/soil_evap_10days_60RH.csv")

# RH = 40% ----
vars = ctrl()
vars$RH_air = 40
results_10days_40RH = soil_evap_model(vars)
results_10days_40RH = results_10days_40RH |>
  mutate(dp18_sw = 1e3 * log(d18_sw / 1e3 + 1))
write.csv(results_10days_40RH, file = "output/soil_evap_10days_40RH.csv")

# compare RH results in dp18O - Dp17O space ----

results_10days$RH = 80
results_10days_60RH$RH = 60
results_10days_40RH$RH = 40
results = rbind(results_10days, results_10days_60RH, results_10days_40RH)
results$RH = as.character(results$RH)
ggplot() +
  geom_point(data = measurements,
             aes(x = dp18sw, y = Dp17sw, fill = section),
             shape = 21, size = 3, color = "black") +
  geom_path(data = results,
             aes(x = dp18_sw, y = Dp17_sw, color = RH),
             linewidth = .5) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Set2") +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg)"),
       fill = "", color = "RH (%)") +
  scale_x_continuous(limits = c(-14, 0)) +
  scale_y_continuous(limits = c(-110, 50))
ggsave("figures/soil_evap_RH.jpg", width = 4.5, height = 3.8, dpi = 500)

