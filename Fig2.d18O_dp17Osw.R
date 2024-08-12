library(tidyverse)
library(ggpubr)
library(readxl)
theme <- theme(axis.text.x = element_text(margin = margin(t = 0.1, unit = "cm")),
               axis.text.y = element_text(margin = margin(r = 0.1, unit = "cm")),
               axis.ticks.length=unit(0.15, "cm"),
               axis.ticks = element_line(colour = "black"),
               text = element_text(color = "black", size = 12),
               axis.title = element_text(size = 15), 
               axis.text = element_text(color = "black", size = 12),
               plot.title = element_text(hjust = 0.1, vjust = -10),
               legend.text = element_text(size = 12),
               legend.title = element_text(size = 12),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_blank())
source("code/IWB_model.R")

rc = read.csv("out/dp17.csv")
rc$section = factor(rc$section, levels = c("Lantian", "Shilou", "Jiaxian"))
mw = read_xlsx("data/Aron2023.xlsx")
sw = read_xlsx("data/Kelson2023.xlsx", sheet = "data")

## comparison with modern data
ggplot(rc, aes(x = dp18sw, y = Dp17sw)) +
  geom_point(data = mw, aes(x = dp18p, y = Dp17p), color = "grey") +
  geom_errorbar(aes(ymin = Dp17sw.low, ymax = Dp17sw.high), linewidth = 0.2) +
  geom_errorbar(aes(xmin = dp18sw.low, xmax = dp18sw.high), linewidth = 0.2) +
  geom_point(data = sw, aes(x = dp18sw, y = Dp17sw), color = "tomato", shape = 3, size = 2) +
  geom_point(aes(fill = section), size = 3, shape = 21) +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O (per meg, VSMOW)"),
       fill = "")
ggsave("figures/dp18_Dp17_comparison.jpg", height = 3.6, width = 4.7)


## isolated water body
# d18p
vars = ctrl()
for (i in 1:3) {
  vars$d18p = -30 + 5 * i
  if (i == 1) {
    sens.d18p = IWB(vars)
    sens.d18p$d18p = vars$d18p
  } else {
    iter = IWB(vars)
    iter$d18p = vars$d18p
    sens.d18p = rbind(sens.d18p, iter)
  }
}
sens.d18p$d18p = as.character(sens.d18p$d18p)
# point = sens.d18p %>% filter(f %in% seq(0.1, 1, 0.1))
p1  = ggplot() +
  geom_point(data = rc, aes(x = dp18sw, y = Dp17sw, shape = section, fill = temp), size = 4) +
  geom_path(data = sens.d18p, aes(x = dp18sw, y = Dp17sw, color = d18p, group = d18p)) +
  scale_shape_manual(values = c(21,22,23)) +
  scale_fill_distiller(palette = "RdBu") +
  scale_color_brewer(palette = "Set2") +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-20, 0)) +
  scale_y_continuous(limits = c(-100, 50)) +
  guides(color = FALSE) +
  labs(x = expression(delta^"'18"*"O (\u2030)"),
       y = expression(Delta^"'17"*"O (per meg)"),
       fill = expression("T"[47]*paste(" (", degree, "C)")),
       shape = "")

# humidity
vars = ctrl()
sens.rh = IWB(vars)
for (i in 1:3) {
  vars$RH = 0.1 + 0.2 * i
  if (i == 1) {
    sens.rh = IWB(vars)
    sens.rh$RH = vars$RH
  } else {
    iter = IWB(vars)
    iter$RH = vars$RH
    sens.rh = rbind(sens.rh, iter)
  }
}
sens.rh$RH <- as.character(sens.rh$RH)
# point = sens.rh %>% filter(f %in% seq(0.1, 1, 0.1))

p2 = ggplot() +
  geom_point(data = rc, aes(x = dp18sw, y = Dp17sw, shape = section, fill = temp), size = 4) +
  geom_path(data = sens.rh, aes(x = dp18sw, y = Dp17sw, color = RH, group = RH)) +
  scale_shape_manual(values = c(21,22,23)) +
  scale_fill_distiller(palette = "RdBu") +
  scale_color_brewer(palette = "Set2") +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-20, 0)) +
  scale_y_continuous(limits = c(-100, 50)) +
  guides(fill = FALSE, shape = FALSE) +
  labs(x = expression(delta^"'18"*"O"[sw]*" (\u2030)"),
       y = expression(Delta^"'17"*"O"[sw]*" (per meg)"),
       color = "RH (%)")
ggarrange(p1, p2, nrow = 1, ncol = 2, widths = c(1.09,1))
ggsave("figures/IWB.jpg", width = 9, height = 3.7)


ggplot(rc, aes(x = age, y = Dp17sw, fill = section)) +
  geom_errorbar(aes(ymin = Dp17sw.low, ymax = Dp17sw.high), size = 0.2, width = 0) +
  geom_point(size = 4, shape = 21) +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + theme +
  scale_y_reverse() +
  labs(x = "Age (Ma)", y = expression(Delta^"'17"*"O"[sw]))
