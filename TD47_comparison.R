library(tidyverse)
library(readxl)
library(ggpubr)

lingtai = read_xlsx("data/regional records/Lingtai_D47.xlsx") %>%
  filter(age > 2.5)
dat = read_xlsx("data/redclay_D47.xlsx")

p1 = ggplot(dat, aes(x = age, y = temp)) +
  geom_errorbar(aes(ymin = temp - temp.sd, ymax = temp + temp.sd), size = 0.2, width = 0) +
  geom_point(aes(fill = site), size = 4, shape = 21) +
  geom_smooth(span = 0.3, se = FALSE, color = "black") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() +
  scale_x_continuous(limits = c(2.5, 7.2)) +
  scale_y_continuous(limits = c(5, 35)) +
  labs(x = "age (Ma)", y = expression(paste("T"[47]*" (", degree, "C)")))
p1
p2 = ggplot(lingtai, aes(x = age, y = T47)) +
  geom_errorbar(aes(ymin = T47 - T47.se, ymax = T47 + T47.se), size = 0.2, width = 0) +
  geom_point(size = 4, shape = 21, color = "tomato", fill = "white") +
  geom_smooth(span = 0.3, se = FALSE, color = "black") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() +
  scale_x_continuous(limits = c(2.5, 7.2)) +
  scale_y_continuous(limits = c(5, 35)) +
  labs(x = "age (Ma)", y = expression(paste("T"[47]*" (", degree, "C)")))
p2

ggarrange(p1, p2, nrow = 2, ncol = 1, align = "hv")
ggsave("figures/T47.comparison.jpg", width = 5, height = 4.7)
