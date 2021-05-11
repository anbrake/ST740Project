


library(rjags)

library(AER)



load("comarea.RData")

load("com_popl.R")


#####

# Exploring overdispersion in the data using a hypothesis test

poisson_glm <- glm(ChlLeadPois ~ ., data = comarea[, names(comarea) != "ChlLeadDec"], family = poisson)

dispersiontest(poisson_glm)

#####


Y = comarea$ChlLeadPois

X <- cbind(1, comarea[, !(names(comarea) %in% c("ChlLeadDec", "ChlLeadPois"))])

n = nrow(X)

p = ncol(X)



# making the Poisson model

dat <- list(Y = Y, X = X,
             N = com_popl, n = n, p = p)

model_str_poi <- textConnection("model {
                               
                               # Likelihood
                               for (i in 1:n) {
                               Y[i] ~ dpois(lambda[i] * N[i])
                               log(lambda[i]) <- inprod(X[i,], beta[])
                               }
                               
                               # Priors
                               for (j in 1:p) {
                               beta[j] ~ dnorm(0, 0.01)
                               }
                               
                               }")

params <- c("beta")

n_burnin <- 100000

n_iter <- 1000000

# inits <- list(list(beta = rnorm(p, 0, 100)),
#               list(beta = rnorm(p, 0, 100)),
#               list(beta = rnorm(p, 0, 100)),
#               list(beta = rnorm(p, 0, 100)))

inits <- list(list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 819),
              list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 820),
              list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 821),
              list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 822))

model_poi <- jags.model(model_str_poi, data = dat, n.chains = 4, n.adapt = 2000, 
                        inits = inits)

update(model_poi, n_burnin)

samples_poi <- coda.samples(model_poi, variable.names = params, n.iter = n_iter)

save(samples_poi, file = "alvin_model_output/samples_poi.RData")

(ess_poi <- effectiveSize(samples_poi))

(gr_poi <- gelman.diag(samples_poi))

summary_poi <- summary(samples_poi)

round(summary_poi$quantiles, 4)

# plot(samples_poi)

(DIC_pois <- dic.samples(model_poi, n.iter = 20000))

# Mean deviance:  3875 
# penalty 45.95 
# Penalized deviance: 3920 

save(DIC_pois, file = "alvin_model_output/DIC_pois.RData")



# making the negative binomial model

dat <- list(Y = Y, X = X,
            N = com_popl, n = n, p = p)

model_str_nb <- textConnection("model {

                               # Likelihood
                               for (i in 1:n) {
                               Y[i] ~ dnegbin(q[i], N[i] * m)
                               q[i] <- m / (m + lambda[i])
                               log(lambda[i]) <- inprod(X[i,], beta[])
                               }
                               
                               # Priors
                               for (j in 1:p) {
                               beta[j] ~ dnorm(0, 0.01)
                               }
                               m ~ dgamma(0.1, 0.1)
                               
                               }")

n_burnin <- 100000

n_iter <- 1000000

inits <- list(list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 926),
              list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 927),
              list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 928),
              list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 929))

model_nb <- jags.model(model_str_nb, data = dat, n.chains = 4, n.adapt = 2000, 
                       inits = inits)

update(model_nb, n_burnin)

params <- c("beta", "m")

samples_nb <- coda.samples(model_nb, variable.names = params, n.iter = n_iter)

save(samples_nb, file = "alvin_model_output/samples_nb.RData")

(ess_nb <- effectiveSize(samples_nb))

(gr_nb <- gelman.diag(samples_nb))

summary_nb <- summary(samples_nb)

round(summary_nb$quantiles, 4)


 
(DIC_nb <- dic.samples(model_nb, n.iter = 20000))

# Mean deviance:  984.1 
# penalty 382.6 
# Penalized deviance: 1367 

save(DIC_nb, file = "alvin_model_output/DIC_nb.RData")



