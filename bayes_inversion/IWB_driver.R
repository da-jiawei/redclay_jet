rm(list = ls())
# load libraries
libs = c("R2jags", "tidyverse", "readxl", "ggpubr")
invisible(lapply(libs, library, character.only = TRUE))
# theme for ggplot
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
# load functions
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
# priors
d18p.min = -30
d18p.max = -20
Dp17p.min = -50.1 #10
Dp17p.max = -50 #30
RH.min = 0.5 #0.5
RH.max = 0.8 #0.8
f.min = 0.1
f.max = 0.5 #0.5
Tsoil.min = 5
Tsoil.max = 35
d = list(ages = ages, 
         d18c.obs = d18c, 
         D47c.obs = D47c, 
         Dp17c.obs = Dp17c,
         d18p.min = d18p.min, d18p.max = d18p.max,
         Dp17p.min = Dp17p.min, Dp17p.max = Dp17p.max,
         RH.min = RH.min, RH.max = RH.max,
         f.min = f.min, f.max = f.max,
         Tsoil.min = Tsoil.min, Tsoil.max = Tsoil.max)

parms = c("RH", "d18p", "Dp17p", "f", "Tsoil")

system.time({post.clp = jags.parallel(d, NULL, parms, "bayes_inversion/IWB_bayes.R",
                                      n.iter = 1e6, n.chains = 3, n.burnin = 5e5)})

View(post.clp$BUGSoutput$summary)
save(post.clp, file = "output/IWB_1e6_fixed_Dp17p.rda")
load("output/IWB_1e6.rda")

# plot iterations
# time series  ----
pdf("figures/time_series_posterior_fixed_Dp17p.pdf", width = 6.8, height = 6.2)
par(mfrow = c(3, 2),
    mar = c(2.5,2.5,1,1),
    mgp = c(1.5, .5, 0))
for (i in 1:length(parms)) {
  param = parms[i]
  plot.jpi(ages, post.clp$BUGSoutput$sims.list[[param]], n = 100, ylab = param)
}

dev.off()

# pdf ---- 
# pdf("figures/posterior_pdfs.pdf", width = 4.5, height = 5.8)
par(mfrow = c(3, 2),
    mar = c(2.5,3,2,1),
    mgp = c(2, .5, 0))
for (p in 1:length(parms)) {
  param = parms[p]
  iter = post.clp$BUGSoutput$sims.list[[param]]
  dens = list()
  for (i in 1:ncol(iter)) {
    dens[[i]] = density(iter[,i])
  }
  y_max = max(sapply(dens, function(d) max(d$y)))
  y_limits = c(0, ceiling(y_max * 100)) / 100
  x = runif(1e5, 
            get(paste0(param, ".min")),
            get(paste0(param, ".max")))
  plot(density(x), 
       type = "n",
       ylim = y_limits,
       main = param, xlab = "", ylab = "density")
  polygon(density(x), col = "gray90", border = "transparent")
  for (i in 1:ncol(iter)) {
    polygon((dens[[i]]), col = rgb(0.5, 0.8, 1, alpha = 0.1), border = "skyblue")
  }
}
dev.off()

# ggplot ----
for (i in 1:length(parms)) {
  param = parms[i]
  param.sd = paste0(param, ".sd")
  clp[[param]] = post.clp$BUGSoutput$mean[[param]]
  clp[[param.sd]] = post.clp$BUGSoutput$sd[[param]]
  dat = as.data.frame(post.clp$BUGSoutput$summary[(length(ages)*(i-1)+1):(length(ages)*i), ])
  for (p in 1:length(ages)) {
    if (dat$Rhat[p] > 1.03) { # Rhat < 1.03
      clp[[param]][p] = NA
    }
  }
}

clp$section = factor(clp$section, levels = c("Lantian", "Shilou", "Jiaxian"))
write.csv(clp, "output/clp_bayes.csv")
p1 = ggplot(clp, aes(x = temp, y = d18p, fill = section)) +
  geom_errorbar(aes(xmin = temp - temp.se, xmax = temp + temp.se), size = 0.2, color = "grey70", width = 0) +
  geom_errorbar(aes(ymin = d18p - d18p.sd, ymax = d18p + d18p.sd), size = 0.2, color = "grey70", width = 0) +
  geom_point(size = 4, shape = 21) +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + theme +
  labs(x = expression(paste("T"[Delta][47]*" (", degree, "C)")),
       y = expression(delta^"18"*"O"[p]*" (\u2030)"))
p1
p2 = ggplot(clp, aes(x = temp, y = RH * 100, fill = section)) +
  geom_errorbar(aes(xmin = temp - temp.se, xmax = temp + temp.se), size = 0.2, color = "grey70", width = 0) +
  geom_errorbar(aes(ymin = (RH - RH.sd) * 100, ymax = (RH + RH.sd) * 100), size = 0.2, color = "grey70", width = 0) +
  geom_point(shape = 21, size = 4) +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + theme +
  labs(x = expression(paste("T"[Delta][47]*" (", degree, "C)")),
       y = "RH (%)")
p2
p3 = ggplot(clp, aes(x = RH * 100, y = f, fill = section)) +
  geom_errorbar(aes(ymin = f - f.sd, ymax = f + f.sd), size = 0.2, color = "grey70", width = 0) +
  geom_errorbar(aes(xmin = (RH - RH.sd) * 100, xmax = (RH + RH.sd) * 100), size = 0.2, color = "grey70", width = 0) +
  geom_point(shape = 21, size = 4) +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + theme +
  labs(x = "RH (%)", y = expression(italic(f))) +
  scale_x_continuous(limits = c(58, 75))
p3

ggarrange(p1, p2, p3, nrow = 1, ncol = 3, common.legend = TRUE)
ggsave("figures/Bayes_IWB_section_screened_fixed_Dp17p.jpg", width = 10, height = 4)
ggsave("figures/Bayes_IWB_section_screened_fixed_Dp17p.jpg", width = 10, height = 4)

clp2 = clp |> drop_na(d18p, Dp17p)
ggplot(data = clp2) +
  geom_point(aes(x = 1e3*log(d18p/1e3 + 1), y = Dp17p, fill = SID),
             shape = 21, size = 4) +
  geom_point(aes(x = dp18sw, y = Dp17sw, fill = SID),
             shape = 22, size = 4) +
  scale_fill_brewer(palette = "Paired") +
  guides(fill = "none") +
  theme_bw() + theme


