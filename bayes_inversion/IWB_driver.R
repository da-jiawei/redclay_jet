rm(list = ls())
library(R2jags)
library(tidyverse)
library(readxl)
library(ggpubr)
theme = theme(axis.text.x = element_text(margin = margin(t = 0.1, unit = "cm")),
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
source('bayes_inversion/helpers.R')
cat("\014")

## Read and groom data ----
clp = read_xlsx("data/Dp17.xlsx")
clp = clp[order(clp$age),]

## Parse data into series
d18c = na.exclude(clp[c("d18c", "d18c.se")])
D47c = na.exclude(clp[c("D47", "D47.se")])
Dp17c = na.exclude(clp[c("Dp17c", "Dp17c.se")])
ages = clp$age

## MCMC ----
d = list(ages = ages, 
         d18c.obs = d18c, 
         D47c.obs = D47c, 
         Dp17c.obs = Dp17c)

parms = c("RH", "d18p", "Dp17p", "f", "Tsoil")

system.time({post.clp = jags.parallel(d, NULL, parms, "bayes_inversion/IWB_bayes.R",
                                      n.iter = 3e5, n.chains = 3, n.burnin = 1e5)})

View(post.clp$BUGSoutput$summary)

# plot iterations
for (i in 1:length(parms)) {
  param = parms[i]
  plot.jpi(ages, post.clp$BUGSoutput$sims.list[[param]], n = 100, ylab = param)
}

for (i in 1:length(parms)) {
  param = parms[i]
  param.sd = paste0(param, ".sd")
  clp[[param]] = post.clp$BUGSoutput$mean[[param]]
  clp[[param.sd]] = post.clp$BUGSoutput$sd[[param]]
  # screening criterion (Rhat < 1.1)
  dat = as.data.frame(post.clp$BUGSoutput$summary[(length(ages)*(i-1)+1):(length(ages)*i), ])
  for (p in 1:length(ages)) {
    if (dat$Rhat[p] > 1.1) {
      clp[[param]][p] = NA
    }
  }
}

clp$section = factor(clp$section, levels = c("Lantian", "Shilou", "Jiaxian"))
p1 = ggplot(clp, aes(x = temp, y = d18p, fill = section)) +
  geom_errorbar(aes(xmin = temp - temp.se, xmax = temp + temp.se), size = 0.2) +
  geom_errorbar(aes(ymin = d18p - d18p.sd, ymax = d18p + d18p.sd), size = 0.2) +
  geom_point(shape = 21, size = 4) +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + theme +
  labs(x = expression(paste("T"[Delta][47]*" (", degree, "C)")),
       y = expression(delta^"18"*"O"[p]*" (\u2030)"))
p1
p2 = ggplot(clp, aes(x = temp, y = RH * 100, fill = section)) +
  geom_errorbar(aes(xmin = temp - temp.se, xmax = temp + temp.se), size = 0.2) +
  geom_errorbar(aes(ymin = (RH - RH.sd) * 100, ymax = (RH + RH.sd) * 100), size = 0.2) +
  geom_point(shape = 21, size = 4) +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + theme +
  labs(x = expression(paste("T"[Delta][47]*" (", degree, "C)")),
       y = "RH (%)")
p2
ggarrange(p1, p2, nrow = 1, ncol = 2, common.legend = TRUE)
ggsave("figures/Bayes_IWB_section_screened.jpg", width = 7, height = 4)

ggplot(clp, aes(x = RH, y = f, fill = section)) +
  geom_errorbar(aes(xmin = RH - RH.sd, xmax = RH + RH.sd), size = 0.2) +
  geom_errorbar(aes(ymin = f - f.sd, ymax = f + f.sd), size = 0.2) +
  geom_point(shape = 21, size = 4) +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + theme +
  labs(x = expression(paste("T"[Delta][47]*" (", degree, "C)")),
       y = "f")

ggplot(clp, aes(x = temp, y = d18p, shape = section, fill = age)) +
  geom_errorbar(aes(xmin = temp - temp.se, xmax = temp + temp.se), size = 0.2) +
  geom_errorbar(aes(ymin = d18p - d18p.sd, ymax = d18p + d18p.sd), size = 0.2) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(21,22,23)) +
  scale_fill_distiller(palette = "RdBu") +
  theme_bw() + theme +
  labs(x = expression(paste("T"[Delta][47]*" (", degree, "C)")),
       y = expression(delta^"18"*"O"[p]*" (\u2030)"))

# prior vs posterior ----
par(mai = c(1, 1, 0.2, 0.2))
d18p.pri = density(runif(1e5, -35, -20))
d18p.post = density(post.clp$BUGSoutput$sims.list$d18p[,5])
plot(d18p.pri, xlim = range(d18p.pri$x, d18p.post$x),
     ylim = range(d18p.pri$y, d18p.post$y), main = "", axes = FALSE,
     xlab = expression(delta^"18"*"O"[p]), lty = 1, lwd = 2, col = "red")
lines(d18p.post, lwd = 2, col = "blue")
axis(1)
axis(2)







box()
dev.off()
