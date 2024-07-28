library(tidyverse)
library(ggpubr)
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
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
dat = read_xlsx("data/GNIP.xlsx")

p1 = ggplot(dat, aes(x = month, y = prep, fill = site, color = site)) +
  geom_line(linewidth = 0.2) +
  geom_point(size = 4, shape = 21, color = "black") +
  geom_vline(xintercept = 5, color = "#A6CEE3") +
  geom_vline(xintercept = 7, color = "#1F78B4") +
  geom_vline(xintercept = 8, color = "#B2DF8A") +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  theme_bw() + theme +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  labs(y = "MAP (mm)")

p2 = ggplot(dat, aes(x = month, y = d18p, fill = site, color = site)) +
  geom_line(linewidth = 0.2) +
  geom_point(size = 4, shape = 21, color = "black") +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  theme_bw() + theme +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  labs(y = expression(delta^"18"*"O (\u2030)"))

ggarrange(p1, p2, nrow = 1, ncol = 2, common.legend = TRUE)
ggsave("figures/GNIP.jpg", width = 6.8, height = 3.4)
