rm(list = ls())
source('bayes_inversion/IWB_forward_model.R')

vars = ctrl()
vars$RH = 0.6
results = IWB(vars)
plot(results$dp18sw, results$Dp17sw)
