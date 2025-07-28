rm(list = ls())
pacman::p_load(tidyverse, readxl, ggpubr)
theme = theme(axis.ticks.length=unit(0.15, "cm"),
              axis.ticks = element_line(colour = "black"),
              text = element_text(color = "black", size = 10),
              axis.title = element_text(size = 12), 
              axis.text = element_text(color = "black", size = 10),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              panel.grid = element_blank())

dat = read_xlsx("data/Dp17.xlsx")
source("models/BASSmodel_forward.R")

#### RELATIVE HUMIDITY ---- 
vars = ctrl()
for (p in 1:100) {
  vars$depth = p
  for (i in 1:3) {
    vars$RH = .1 * i + .5
    sens = BASS(vars)
    sens$RH = vars$RH
    sens$depth = vars$depth
    if(p == 1 & i == 1) {
      sims = sens
    } else {
      sims = rbind(sims, sens)
    }
  }
}
sims$RH = as.character(sims$RH)
p1 = ggplot() +
  geom_point(data = dat, aes(x = dp18sw, y = Dp17sw, fill = section),
             shape = 21, size = 3) +
  geom_line(data = sims, aes(x = dp18sw, y = Dp17sw, color = RH, group = RH)) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg, VSMOW)"),
       color = "RH (%)", fill = "")
p1

#### TEMP - TIME OF CARBONATE FORMATION ----
vars = ctrl()
for (p in 1:100) {
  vars$depth = p
  for (i in 1:5) {
    vars$tsc = .2 * i
    sens = BASS(vars)
    sens$tsc = vars$tsc
    sens$depth = vars$depth
    if(p == 1 & i == 1) {
      sims = sens
    } else {
      sims = rbind(sims, sens)
    }
  }
}

ggplot() +
  geom_point(data = dat, aes(x = dp18sw, y = Dp17sw),
             shape = 22, size = 4) +
  geom_line(data = sims, aes(x = dp18sw, y = Dp17sw, color = depth, group = tsc)) +
  scale_color_viridis_c(option = "turbo", direction = 1) +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg, VSMOW)"))

#### d18p ----
vars = ctrl()
for (p in 1:100) {
  vars$depth = p
  for (i in 1:3) {
    vars$d18p = -8 - 2 * i
    sens = BASS(vars)
    sens$d18p = vars$d18p
    sens$depth = vars$depth
    if(p == 1 & i == 1) {
      sims = sens
    } else {
      sims = rbind(sims, sens)
    }
  }
}
sims$d18p = as.character(sims$d18p)
p2 = ggplot() +
  geom_point(data = dat, aes(x = dp18sw, y = Dp17sw, fill = section),
             shape = 21, size = 3) +
  geom_path(data = sims, aes(x = dp18sw, y = Dp17sw, color = d18p, group = d18p)) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme +
  guides(fill = "none") +
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg, VSMOW)"),
       color = expression(delta^"18"*"O"[p]*" (\u2030)"))
p2
ggarrange(p1, p2, nrow = 1, ncol = 2, align = "hv")
ggsave("figures/BASS_sens.jpg", width = 8.5, height = 3.4, dpi = 500)
#### EVAPORATION RATE ----
vars = ctrl()
for (p in 1:100) {
  vars$depth = p
  for (i in 1:3) {
    vars$evap = i*1e-10
    sens = BASS(vars)
    sens$evap = vars$evap
    sens$depth = vars$depth
    if(p == 1 & i == 1) {
      sims = sens
    } else {
      sims = rbind(sims, sens)
    }
  }
}

sims$evap = as.character(sims$evap)
ggplot() +
  geom_point(data = dat, aes(x = dp18sw, y = Dp17sw, fill = section),
             shape = 21, size = 3) +
  geom_path(data = sims, aes(x = dp18sw, y = Dp17sw, color = evap, group = evap)) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  guides(fill = "none") +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg, VSMOW)"))

ggarrange(s)