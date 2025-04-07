rm(list = ls())
library(R2jags)
library(tidyverse)
library(readxl)
library(ggpubr)
source("bayes_inversion/constructors.R")
source("bayes_inversion/helpers.R")

## Read and groom data ----
clp = read_xlsx("data/Dp17.xlsx")

## Parse data into series
d18Oc = na.exclude(clp[c("age", "d18c", "d18c.se")])
D47c = na.exclude(clp[c("age", "D47", "D47.se")])
Dp17c = na.exclude(clp[c("age", "Dp17c", "Dp17c.se")])

ages = ts(d18Oc$age, D47c$age, Dp17c$age)
tsi = ages$ts_ind
ai = ages$ts

## MCMC ----
d = list(ai = ages$ts, 
         d18Oc.obs = d18Oc[, 2:3], d18Oc.ai = tsi[[1]],
         D47c.obs = D47c[, 2:3], D47c.ai = tsi[[2]],
         Dp17c.obs = Dp17c[, 2:3], Dp17c.ai = tsi[[3]]
)

parms = c("MAT", "tsc", "MAP", "PCQ_pf", "d18p", "pore", "z_m", "ha", "E_s")

system.time({post.clp = jags.parallel(d, NULL, parms, "bayes_inversion/PSM_bayes.R",
                                      n.iter = 2e4, n.chains = 3, n.burnin = 1e4)})

View(post.clp$BUGSoutput$summary)
for (i in 1:length(parms)) {
  param = parms[i]
  plot.jpi(ages$ts, post.clp$BUGSoutput$sims.list[[param]], n = 100, ylab = param)
}

clp2 = clp %>%
  group_by(section, age) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

for (i in 1:length(parms)) {
  param = parms[i]
  clp2[[param]] = post.clp$BUGSoutput$mean[[param]]
}
clp2[] <- lapply(clp2, function(x) {
  if (is.array(x)) as.vector(x) else x
})
write_csv(clp2, file = "output/clp_bayes.csv")

ggplot(clp2, aes(x = MAT, y = d18p, fill = section)) +
  geom_point(shape = 21, size = 3)

# plot.jpi(ages$ts, post.clp$BUGSoutput$sims.list$d18p, n = 400, ylab = "")
# save(post.clp, file = "out/ms_fx_2e4_450ppm.rda")
# load("out/ms_fx_2e4_MS.rda")
# traceplot(post.clp, varname = "MAP")

