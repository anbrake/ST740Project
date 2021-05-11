
# ran code in spatial_spike_slab_negbin.R for
# burn in 1000
# iterations 100

# I keep track of the effective sample sizes
summary(ess_spatial_ssvs)

# summary(ess_spatial_ssvs)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 3.447  196.483  200.000  209.264  210.683 1172.358 

# Consider saving the model
# save(model_spatial_ssvs, file = "model_spatial_ssvs.RData")

# 100 more iterations
samples_spatial_ssvs2 <- coda.samples(model_spatial_ssvs, variable.names = params, n.iter = n_iter)

# 1000 more iterations
samples_spatial_ssvs3 <- coda.samples(model_spatial_ssvs, variable.names = params, n.iter = 1000)

ess_spatial_ssvs3 <- effectiveSize(samples_spatial_ssvs3)

summary(ess_spatial_ssvs3)

# > summary(ess_spatial_ssvs3)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 4.151 1861.558 2000.000 1883.060 2000.000 3382.907 

# gr_spatial_ssvs3 <- gelman.diag(samples_spatial_ssvs3)

summary_spatial_ssvs3 <- summary(samples_spatial_ssvs3)



# I've run 1200 iterations so far. I'll update for 8800 more iterations as burn in,
# for a total of 10000 burn in

update(model_spatial_ssvs, 8800)

# 1000 more iterations
samples_spatial_ssvs4 <- coda.samples(model_spatial_ssvs, variable.names = params, n.iter = 1000)

ess_spatial_ssvs4 <- effectiveSize(samples_spatial_ssvs4)

# > summary(ess_spatial_ssvs4)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 4.144 1821.874 2000.000 1857.706 2000.000 2972.865 



# I've run 11000 iterations so far. I'll update for 9000 more iterations as burn in,
# for a total of 20000 burn in

update(model_spatial_ssvs, 9000)

# 1000 more iterations
samples_spatial_ssvs5 <- coda.samples(model_spatial_ssvs, variable.names = params, n.iter = 1000)

ess_spatial_ssvs5 <- effectiveSize(samples_spatial_ssvs5)

# > summary(ess_spatial_ssvs5)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 2.993 1738.257 1909.519 1804.122 2000.000 3448.258 

save(samples_spatial_ssvs5, file = "alvin_model_output/samples_spatial_ssvs5.RData")

save(model_spatial_ssvs, file = "model_spatial_ssvs1.RData")

# tested changing from pnorm to probit. No change to results.

# get the model back

load("model_spatial_ssvs1.RData")

params <- c("beta0", "delta", "prob", "gamma", "rho")

model_spatial_ssvs$recompile()

# I've run 21000 iterations so far. I'll update for 9000 more iterations as burn in,
# for a total of 30000 burn in

update(model_spatial_ssvs, 9000)

# 10000 more iterations. I guess it's time to roll with it, and use the results I do have, somehow.
samples_spatial_ssvs6 <- coda.samples(model_spatial_ssvs, variable.names = params, n.iter = 10000)

ess_spatial_ssvs6 <- effectiveSize(samples_spatial_ssvs6)

# The ess is slowly getting kind of better, at least
# > summary(ess_spatial_ssvs6)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 21.07 16115.33 18396.65 17105.12 19485.66 21114.64 

save(samples_spatial_ssvs6, file = "alvin_model_output/samples_spatial_ssvs6.RData")

save(model_spatial_ssvs, file = "model_spatial_ssvs2.RData")








