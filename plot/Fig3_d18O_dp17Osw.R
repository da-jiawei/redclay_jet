rm(list = ls())
pacman::p_load(tidyverse, ggpubr, readxl)
theme = theme(axis.ticks.length=unit(0.15, "cm"),
              axis.ticks = element_line(colour = "black"),
              text = element_text(color = "black", size = 10),
              axis.title = element_text(size = 12), 
              axis.text = element_text(color = "black", size = 10),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              panel.grid = element_blank())

# load data ----
rc = read.csv("output/dp17.csv")
rc$section = factor(rc$section, levels = c("Lantian", "Shilou", "Jiaxian"))
mw = read_xlsx("data/isotope_records/ModernWater_China_tian2019.xlsx")
sw = read_xlsx("data/isotope_records/Kelson2023.xlsx", sheet = "data")
bayes_data = read_csv("output/BASS_bayes_vary_evap.csv")
cat("\014")

# all modern waters ----
p1 = ggplot(rc, aes(x = dp18sw, y = Dp17sw)) +
  geom_point(data = mw, aes(x = dp18O, y = Dp17O), color = "grey", size = 3, shape = 21) +
  geom_errorbar(aes(ymin = Dp17sw.low, ymax = Dp17sw.high), linewidth = 0.2) +
  geom_errorbar(aes(xmin = dp18sw.low, xmax = dp18sw.high), linewidth = 0.2) +
  geom_point(data = sw, aes(x = dp18sw, y = Dp17sw), color = "tomato", shape = 3, size = 2) +
  geom_point(aes(fill = temp, shape = section), size = 4) +
  scale_fill_distiller(palette = "RdBu") +
  scale_shape_manual(values = c(21,22,23)) +
  theme_bw() + theme +
  annotate("text", x = -18, y = -120, label = "f", fontface = "bold", size = 5) +
  labs(x = expression(delta^"'18"*"O (\u2030, VSMOW)"),
       y = expression(Delta^"'17"*"O (per meg, VSMOW)"),
       fill = expression(paste("T"[Delta][47]*" (", degree, "C)")), shape = "")
p1

# joint proxy inversion of steady state model ----
p2 = ggplot(bayes_data, aes(x = 1e3 * (exp(dp18sw / 1e3) - 1), y = post_d18p)) +
  geom_errorbar(aes(ymin = post_d18p - post_d18p_sd, ymax = post_d18p + post_d18p_sd), 
                linewidth = 0.2, color = "grey80") +
  geom_point(aes(fill = temp, shape = section), size = 4) +
  scale_fill_distiller(palette = "RdBu") +
  scale_shape_manual(values = c(21,22,23)) +
  theme_bw() + theme +
  annotate("text", x = -5.5, y = -17.5, label = "g", fontface = "bold", size = 5) +
  labs(x = expression(delta^"18"*"O"[sw]*" (\u2030, VSMOW)"),
       y = expression(delta^"'18"*"O"[p]*" (\u2030, VSMOW)"),
       fill = expression(paste("T"[Delta][47]*" (", degree, "C)")), shape = "")
ggarrange(p1, p2, nrow = 2, ncol = 1, align = "hv",
          common.legend = TRUE, legend = "right")
ggsave("figures/Fig3.d18_Dp17.jpg", width = 3.8, height = 5.8, dpi = 500)

p3 = ggplot(bayes_data, aes(x = 1e3 * (exp(dp18sw / 1e3) - 1), y = post_RH * 1e2)) +
  geom_errorbar(aes(ymin = (post_RH - post_RH_sd) * 1e2, ymax = (post_RH + post_RH_sd) * 1e2), 
                linewidth = 0.2, color = "grey80") +
  geom_point(aes(fill = temp, shape = section), size = 4) +
  scale_fill_distiller(palette = "RdBu") +
  scale_shape_manual(values = c(21,22,23)) +
  theme_bw() + theme +
  annotate("text", x = -5.5, y = 32, label = "c", fontface = "bold", size = 5) +
  labs(x = expression(delta^"18"*"O"[sw]*" (\u2030, VSMOW)"),
       y = "RH (%)",
       fill = expression(paste("T"[Delta][47]*" (", degree, "C)")), shape = "")
p3

# soil waters ----
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
