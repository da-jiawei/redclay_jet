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
source("bayes_inversion/constructors.R")
source('bayes_inversion/helpers.R')
cat("\014")

## Read and groom data ----
site_name = "Lantian"
d18c = read_xlsx("data/redclay_isotope.xlsx", sheet = site_name)
d18c = d18c[, c("age", "d18O")]
D47c = read_xlsx("data/redclay_D47.xlsx")
D47c = D47c[, c("age", "D47", "D47.se")]
Dp17c = read_xlsx("data/Dp17.xlsx") %>%
  filter(section == site_name)
Dp17c = Dp17c[, c("age", "Dp17c", "Dp17c.se")]
dat.list = list(d18c, D47c, Dp17c)
dat = reduce(dat.list, full_join, by = "age")
names(dat) = c("age", "d18c", "D47c", "D47c.se", "Dp17c", "Dp17c.se")
dat$d18c.se = 0.03

## Parse data into series
dat$age = -dat$age
dat = dat %>% 
  arrange(age) %>%
  filter(age > -7)
d18c = na.exclude(dat[c("age", "d18c", "d18c.se")])
D47c = na.exclude(dat[c("age", "D47c", "D47c.se")])
Dp17c = na.exclude(dat[c("age", "Dp17c", "Dp17c.se")])

dt = 0.1
ages = seq(-7, -2.5, dt)
d18c.ai = get.ind(d18c$age, ages)
D47c.ai = get.ind(D47c$age, ages)
Dp17c.ai = get.ind(Dp17c$age, ages)

## MCMC ----
d = list(ai = ages, dt = dt,
         d18c.obs = d18c[, 2:3], d18c.ai = d18c.ai,
         D47c.obs = D47c[, 2:3], D47c.ai = D47c.ai,
         Dp17c.obs = Dp17c[, 2:3], Dp17c.ai = Dp17c.ai)

parms = c("RH", "d18p", "f", "Tsoil")

system.time({post.clp = jags.parallel(d, NULL, parms, "bayes_inversion/IWB_time_series_bayes.R",
                                      n.iter = 1e3, n.chains = 3, n.burnin = 1e2)})

View(post.clp$BUGSoutput$summary)
# plot iterations
for (i in 1:length(parms)) {
  param = parms[i]
  plot.jpi(ages, post.clp$BUGSoutput$sims.list[[param]], n = 100, ylab = param)
}

clp = data.frame(age = ages)
for (i in 1:length(parms)) {
  param = parms[i]
  param.sd = paste0(param, ".sd")
  clp[[param]] = post.clp$BUGSoutput$mean[[param]]
  clp[[param.sd]] = post.clp$BUGSoutput$sd[[param]]
}

ggplot(clp, aes(x = Tsoil, y = d18p)) +
  geom_point(size = 2, shape = 21, fill = "white") +
  theme_bw()
