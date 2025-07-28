rm(list = ls())
#### BayesianTools ----
library(BayesianTools)
library(parallel)
options(mc.cores = detectCores())
source('bayes_inversion/soil_evap_functions.R')
jiaxian = read_xlsx("data/Dp17.xlsx") |>
  filter(section == "Jiaxian") |>
  select("age", "d18c", "d18c.se",
         "Dp17c", "Dp17c.se", 
         "D47", "D47.se")

# observed values ----
obs_values <- c(
  d18_carb = jiaxian$d18c[1],    # ‰
  Dp17_carb = jiaxian$Dp17c[1],   # per meg
  D47_carb = jiaxian$D47[1]    # ‰
)
obs_sd <- c(0.2, 4, 0.015) # 每个观测值的不确定度

# define likelihood function ----
likelihood_function <- function(params) {
  RH_air = params[1]
  T_air = params[2]
  d18_p = params[3]
  Dp17_p = params[4]
  Bk_depth = params[5]
  
  # 模型输出（模拟观测值）
  pred <- tryCatch({
    soil_evap_model(RH_air = RH_air, 
                    T_air = T_air, 
                    T_air_half_range = 8,
                    time_of_year = .25, 
                    d18_p = d18_p, 
                    Dp17_p = Dp17_p, 
                    Bk_depth = Bk_depth)
  }, error = function(e) return(rep(NA, 3)))
  
  # 如果模拟失败，返回极小 log-likelihood
  if (any(is.na(pred))) return(-1e6)
  
  # 正态对数似然
  log_likelihood <- sum(dnorm(obs_values, mean = pred, sd = obs_sd, log = TRUE))
  return(log_likelihood)
}

# 2. 定义 prior 分布
prior = createUniformPrior(
  lower = c(10, 10, -15, 0, .5), 
  upper = c(90, 20, -5, 50, 29.5))

# 3. 创建 BayesianSetup
bayes_setup <- createBayesianSetup(likelihood = likelihood_function,
                                   prior = prior,
                                   names = c("RH_air", "T_air", "d18_p", "Dp17_p", "Bk_depth"))
# 4. 执行 MCMC
out = runMCMC(bayes_setup, sampler = "DEzs", settings = list(iterations = 500))
summary(out)
