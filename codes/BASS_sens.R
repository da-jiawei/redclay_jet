library(tidyverse)
library(readxl)
library(ggpubr)
theme = theme(axis.text.x = element_text(margin = margin(t = 0.1, unit = "cm")),
               axis.text.y = element_text(margin = margin(r = 0.1, unit = "cm")),
               axis.ticks.length=unit(0.15, "cm"),
               axis.ticks = element_line(colour = "black"),
               text = element_text(color = "black", size = 10),
               axis.title = element_text(size = 12), 
               axis.text = element_text(color = "black", size = 10),
               plot.title = element_text(hjust = 0.9, vjust = -10),
               legend.text = element_text(size = 10),
               legend.title = element_text(size = 12),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_blank())

dat = read_xlsx("data/Dp17.xlsx")
source("BASSmodel_forward.R")

# RH - d18p
vars = ctrl()
vars$RH = seq(0.1, 0.9, 0.1)
for (i in 1:4) {
  vars$d18p = -11 + 2 * (i - 1)
  sens = BASS(vars)
  sens$RH = vars$RH
  sens$d18p = vars$d18p
  if(i == 1) {
    sim = sens
  } else {
    sim = rbind(sim, sens)
  }
}
p1 = ggplot(dat, aes(x = dp18sw, y = Dp17sw)) +
  geom_point(shape = 22, size = 3) +
  geom_line(data = sim, aes(x = dp18sw, y = Dp17sw, group = d18p), linetype = "dashed") +
  geom_point(data = sim, aes(x = dp18sw, y = Dp17sw, fill = RH, group = d18p), shape = 21, size = 3) +
  scale_fill_distiller(palette = "RdBu") +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg, VSMOW)"))

# temp - d18p (time of year)
vars = ctrl()
vars$t = seq(0, 1, 0.1)
for (i in 1:4) {
  vars$d18p = -11 + 2 * (i - 1)
  sens = BASS(vars)
  sens$t = vars$t
  sens$d18p = vars$d18p
  if(i == 1) {
    sim = sens
  } else {
    sim = rbind(sim, sens)
  }
}
p2 = ggplot(dat, aes(x = dp18sw, y = Dp17sw)) +
  geom_point(shape = 22, size = 3) +
  geom_line(data = sim, aes(x = dp18sw, y = Dp17sw, group = d18p), linetype = "dashed") +
  geom_point(data = sim, aes(x = dp18sw, y = Dp17sw, fill = t, group = d18p), shape = 21, size = 3) +
  scale_fill_distiller(palette = "RdBu") +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg, VSMOW)"))

# depth - d18p 
vars = ctrl()
vars$d18p = seq(-11, -5, 2)
for (i in 1:10) {
  vars$depth = 10 * i
  sens = BASS(vars)
  sens$d18p = vars$d18p
  sens$depth = vars$depth
  if(i == 1) {
    sim = sens
  } else {
    sim = rbind(sim, sens)
  }
}
p3 = ggplot(dat, aes(x = dp18sw, y = Dp17sw)) +
  geom_point(shape = 22, size = 3) +
  geom_line(data = sim, aes(x = dp18sw, y = Dp17sw, group = d18p), linetype = "dashed") +
  geom_point(data = sim, aes(x = dp18sw, y = Dp17sw, fill = depth, group = d18p), shape = 21, size = 3) +
  scale_fill_distiller(palette = "RdBu") +
  theme_bw() + theme + 
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg, VSMOW)"),
       fill = "depth (cm)")

# evaporation rate - d18p 
vars = ctrl()
vars$d18p = seq(-11, -5, 2)
for (i in 1:3) {
  vars$evap = 5 * 10 ^ -(i + 8)
  sens = BASS(vars)
  sens$d18p = vars$d18p
  sens$evap = vars$evap
  if(i == 1) {
    sim = sens
  } else {
    sim = rbind(sim, sens)
  }
}
sim$evap = as.character(sim$evap)
p4 = ggplot(dat, aes(x = dp18sw, y = Dp17sw)) +
  geom_point(shape = 22, size = 3) +
  geom_line(data = sim, aes(x = dp18sw, y = Dp17sw, group = d18p), linetype = "dashed") +
  geom_point(data = sim, aes(x = dp18sw, y = Dp17sw, fill = evap, group = d18p), shape = 21, size = 3) +
  scale_fill_brewer(palette = "RdBu") +
  theme_bw() + theme + 
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg, VSMOW)"),
       fill = "E (m/s)")

ggarrange(p1, p3, p4, nrow = 1, ncol = 3, align = "hv")
ggsave("figures/BASS_sens.pdf", width = 12, height = 3)
