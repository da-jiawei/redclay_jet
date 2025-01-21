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
mw = read_xlsx("data/ModernWater_China_tian2019.xlsx")
sw = read_xlsx("data/Kelson2023.xlsx", sheet = "data")

## comparison with modern data ----
# all modern waters
p1 = ggplot(rc, aes(x = dp18sw, y = Dp17sw)) +
  geom_point(data = mw, aes(x = dp18O, y = Dp17O), color = "grey", size = 3, shape = 21) +
  geom_errorbar(aes(ymin = Dp17sw.low, ymax = Dp17sw.high), linewidth = 0.2) +
  geom_errorbar(aes(xmin = dp18sw.low, xmax = dp18sw.high), linewidth = 0.2) +
  geom_point(data = sw, aes(x = dp18sw, y = Dp17sw), color = "tomato", shape = 3, size = 2) +
  geom_point(aes(fill = temp, shape = section), size = 4) +
  scale_fill_distiller(palette = "RdBu") +
  scale_shape_manual(values = c(21,22,23)) +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O (per meg, VSMOW)"),
       fill = expression(paste("T (", degree, "C)")))
p1
# ggsave("figures/dp18_Dp17_comparison.jpg", height = 3.6, width = 4.7)

# soil waters
p2 = ggplot(sw, aes(x = dp18sw, y = Dp17sw, fill = AI)) +
  geom_errorbar(aes(xmin = dp18sw - dp18sw.sd, xmax = dp18sw + dp18sw.sd),
                linewidth = 0.2, width = 0, color = "ivory3") +
  geom_errorbar(aes(ymin = Dp17sw - Dp17sw.sd, ymax = Dp17sw + Dp17sw.sd), 
                linewidth = 0.2, width = 0, color = "ivory3") +
  geom_point(shape = 21, size = 4) +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  geom_point(data = rc, aes(x = dp18sw, y = Dp17sw), 
             color = "black", fill = NA,
             shape = 22, size = 4) +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O (per meg, VSMOW)"),
       fill = "AI")
p2
arid = sw %>% filter(location %in% c("New Mexico", "Tibet", "Iran", "Nevada", "Inner Mongolia", "California"))
p3 = ggplot(arid, aes(x = dp18sw, y = Dp17sw, fill = location)) +
  geom_errorbar(aes(xmin = dp18sw - dp18sw.sd, xmax = dp18sw + dp18sw.sd),
                linewidth = 0.2, width = 0, color = "ivory3") +
  geom_errorbar(aes(ymin = Dp17sw - Dp17sw.sd, ymax = Dp17sw + Dp17sw.sd), 
                linewidth = 0.2, width = 0, color = "ivory3") +
  geom_point(data = sw, aes(x = dp18sw, y = Dp17sw),
             color = "black", fill = "white",
             shape = 21, size = 4) +
  geom_point(shape = 21, size = 4) +
  # scale_fill_brewer(palette = "Set1") +
  geom_point(data = rc, aes(x = dp18sw, y = Dp17sw), 
             color = "black", fill = NA,
             shape = 22, size = 4) +
  theme_bw() + theme +
  labs(x = expression(delta^"'18"*"O (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O (per meg, VSMOW)"),
       fill = "")
p3
ggarrange(p1, p2, p3 , nrow = 2, ncol = 2, align = "hv", labels = c("a", "b", "c"))
ggsave("figures/dp18_Dp17_sw.pdf", height = 7, width = 10.5)

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
  guides(fill = FALSE, shape = FALSE) +
  labs(x = expression(delta^"'18"*"O (\u2030)"),
       y = expression(Delta^"'17"*"O (per meg)"),
       fill = expression("T"[47]*paste(" (", degree, "C)")),
       color = expression(delta^"18"*"O"[sw]*" (\u2030)"),
       shape = "")
p1
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

