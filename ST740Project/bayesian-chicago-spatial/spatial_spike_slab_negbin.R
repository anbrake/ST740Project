
library(rjags)

# Spatial Spike and Slab Hierarchical model

load("comarea.RData")

load("com_popl.R")

load("com_adj_mat.RData")



Y = comarea$ChlLeadPois

# I'll account for the intercept as beta0

X <- scale(comarea[, !(names(comarea) %in% c("ChlLeadDec", "ChlLeadPois"))])

# to make presentation of variable selection results easier

covnames <- colnames(X)

n = nrow(X)

p = ncol(X)

# adjacency matrix, with Aij = 0 if i = j or i and j aren't neighbors. Aij = 1 otherwise.

A <- com_adj_mat

# diagonal matrix with number of neighbors down the diagonal

M <- diag(rowSums(com_adj_mat))

dat <- list(Y = Y, X = X, N = com_popl, n = n, p = p, A = A, M = M)

  
  
# making the spatial spike and slab model

cat("model {

# Likelihood
for (i in 1:n) {
  Y[i] ~ dnegbin(q[i], N[i] * m)
  q[i] <- m / (m + lambda[i])
  log(lambda[i]) <- beta0 + inprod(X[i,], B[i,])
}
# Likelihood prior
m ~ dgamma(0.1, 0.1)

# Spike and Slab
beta0 ~ dnorm(0, 0.1)
for (j in 1:p) {
  for (i in 1:n) {
    B[i, j] <- gamma[i, j] * delta[j]
    gamma[i, j] ~ dbern(prob[i])
  }
  delta[j] ~ dnorm(0, tau)
}
for (i in 1:n) {
  prob[i] <- pnorm(l[i], 0, 1)
}
# Slab prior
tau ~ dgamma(0.1, 0.1)

# Latent Spatial Variable
l[1:n] ~ dmnorm(mu[], tau2 * Sigma_inv[,]) 
Sigma_inv <- M - rho * A
# Latent Spatial prior
for (i in 1:n) {
  mu[i] <- 0
}
tau2 ~ dgamma(0.1, 0.1)
rho ~ dbeta(1, 1)
                                }", file="model_spatial_spike_slab.txt")

params <- c("beta0", "delta", "prob", "gamma", "rho")

n_burnin <- 1000000

n_iter <- 10000

inits <- list(list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1120),
              list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1121))

model_spatial_ssvs <- jags.model("model_spatial_spike_slab.txt", data = dat, n.chains = 2, n.adapt = 2000, 
                       inits = inits)

update(model_spatial_ssvs, n_burnin)

samples_spatial_ssvs <- coda.samples(model_spatial_ssvs, variable.names = params, n.iter = n_iter)

save(samples_spatial_ssvs, file = "alvin_model_output/samples_spatial_ssvs.RData")

ess_spatial_ssvs <- effectiveSize(samples_spatial_ssvs)

gr_spatial_ssvs <- gelman.diag(samples_spatial_ssvs)

save(ess_spatial_ssvs, gr_spatial_ssvs, file = "alvin_model_output/convergence_diagnostics_spatial_ssvs.RData")

summary_spatial_ssvs <- summary(samples_spatial_ssvs)

save(summary_spatial_ssvs, file = "alvin_model_output/summary_spatial_ssvs.RData")



(DIC_spatial_ssvs <- dic.samples(model_spatial_ssvs, n.iter = 20000))

# put results here

save(DIC_spatial_ssvs, file = "alvin_model_output/DIC_spatial_ssvs.RData")





