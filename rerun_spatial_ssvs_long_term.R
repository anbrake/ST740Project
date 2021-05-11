
library(rjags)

load("model_spatial_ssvs2.RData")

params <- c("beta0", "delta", "prob", "gamma", "rho")

model_spatial_ssvs$recompile()

# test <- coda.samples(model_spatial_ssvs, variable.names = params, n.iter = 100)



# I've run 40000 iterations so far. 

# sample 100000 posterior samples. 
samples_spatial_ssvs7 <- coda.samples(model_spatial_ssvs, variable.names = params, n.iter = 100000)

save(samples_spatial_ssvs7, file = "alvin_model_output/samples_spatial_ssvs7.RData")

ess_spatial_ssvs7 <- effectiveSize(samples_spatial_ssvs7)

# > summary(ess_spatial_ssvs7)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 22.91  6558.51  9711.00 10011.31 13089.44 24259.59 

gr_spatial_ssvs7 <- gelman.diag(samples_spatial_ssvs7)

save(ess_spatial_ssvs7, gr_spatial_ssvs7, file = "alvin_model_output/convergence_diagnostics_spatial_ssvs7.RData")

save(model_spatial_ssvs, file = "model_spatial_ssvs3.RData")

summary_spatial_ssvs7 <- summary(samples_spatial_ssvs7)

save(summary_spatial_ssvs7, file = "alvin_model_output/summary_spatial_ssvs7.RData")








# sample 1000000 posterior samples, with thinning.
samples_spatial_ssvs8 <- coda.samples(model_spatial_ssvs, variable.names = params, n.iter = 1000000, thin = 10)

save(samples_spatial_ssvs8, file = "alvin_model_output/samples_spatial_ssvs8.RData")

save(model_spatial_ssvs, file = "model_spatial_ssvs4.RData")

ess_spatial_ssvs8 <- effectiveSize(samples_spatial_ssvs8)

gr_spatial_ssvs8 <- gelman.diag(samples_spatial_ssvs8)

save(ess_spatial_ssvs8, gr_spatial_ssvs8, file = "alvin_model_output/convergence_diagnostics_spatial_ssvs8.RData")

summary_spatial_ssvs8 <- summary(samples_spatial_ssvs8)

save(summary_spatial_ssvs8, file = "alvin_model_output/summary_spatial_ssvs8.RData")

# > summary(ess_spatial_ssvs8)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 50.92   835.02  1165.05  1690.34  1969.15 29048.22 



load("model_spatial_ssvs4.RData")

params <- c("beta0", "delta", "prob", "rho")

model_spatial_ssvs$recompile()

# sample 1000000 posterior samples, with no thinning. This time, don't track gamma, so you can go for more iterations
samples_spatial_ssvs9 <- coda.samples(model_spatial_ssvs, variable.names = params, n.iter = 1000000)

save(samples_spatial_ssvs9, file = "alvin_model_output/samples_spatial_ssvs9.RData")

save(model_spatial_ssvs, file = "model_spatial_ssvs5.RData")

ess_spatial_ssvs9 <- effectiveSize(samples_spatial_ssvs9)

gr_spatial_ssvs9 <- gelman.diag(samples_spatial_ssvs9)

save(ess_spatial_ssvs9, gr_spatial_ssvs9, file = "alvin_model_output/convergence_diagnostics_spatial_ssvs9.RData")

summary_spatial_ssvs9 <- summary(samples_spatial_ssvs9)

save(summary_spatial_ssvs9, file = "alvin_model_output/summary_spatial_ssvs9.RData")



(DIC_spatial_ssvs <- dic.samples(model_spatial_ssvs, n.iter = 20000))

# Mean deviance:  795.7 
# penalty 172.8 
# Penalized deviance: 968.5 

save(DIC_spatial_ssvs, file = "alvin_model_output/DIC_spatial_ssvs.RData")


