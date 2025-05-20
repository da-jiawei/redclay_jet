rm(list = ls())
pacman::p_load(R2jags, tidyverse, readxl, ggpubr)
source("bayes_inversion/constructors.R")
source("bayes_inversion/helpers.R")

## Read and groom data ----
clp = read_xlsx("data/Dp17.xlsx")
clp = clp[order(clp$age),]

## Parse data into series
d18c = na.exclude(clp[c("d18c", "d18c.se")])
D47c = na.exclude(clp[c("D47", "D47.se")])
Dp17c = na.exclude(clp[c("Dp17c", "Dp17c.se")])
ages = clp$age

## MCMC ----
d18p.min = -20
d18p.max = -5
Dp17p.min = 0 #10
Dp17p.max = 40 #30
# RH.min = 0.1 #0.5
# RH.max = 0.9 #0.8
d = list(ages = ages, 
         d18Oc.obs = d18c,
         D47c.obs = D47c,
         Dp17c.obs = Dp17c,
         d18p.min = d18p.min, d18p.max = d18p.max,
         Dp17p.min = Dp17p.min, Dp17p.max = Dp17p.max
)

parms = c("d18p", "Dp17p", "z_m", "ha", "E_s")

system.time({post.clp = jags.parallel(d, NULL, parms, "bayes_inversion/PSM_bayes.R",
                                      n.iter = 1e4, n.chains = 3, n.burnin = 1e3)})

View(post.clp$BUGSoutput$summary)
for (i in 1:length(parms)) {
  param = parms[i]
  plot.jpi(ages, post.clp$BUGSoutput$sims.list[[param]], n = 100, ylab = param)
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

