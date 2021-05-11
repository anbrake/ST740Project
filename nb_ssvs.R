Y = comarea$ChlLeadPois
X = comarea[,c(1:46)]
X = cbind(1, scale(X))
n = nrow(X)
p = ncol(X)
N = comarea_full$Pop2014.N.18.0

n_burnin <- 20000
n_iter <- 100000
#Make the data 
dat <- list(Y = Y, X = X,
            N = N, n = n, p = p)

model_str = textConnection('model{

#Likelihood                         
# Likelihood
for (i in 1:n) {
Y[i] ~ dnegbin(q[i], N[i] * m)
q[i] <- m / (m + lambda[i])
log(lambda[i]) <- inprod(X[i,], beta[])
}

#Ssvs Priors
beta[1] ~ dnorm(0,0.01)

for(j in 2:p){
beta[j]  = delta[j]*gamma[j]
gamma[j] ~ dbern(0.5)
delta[j] ~ dnorm(0,tau2)
}
m ~ dgamma(0.1, 0.1)
tau2 ~ dgamma(0.1,0.1)
}')

params <- c("gamma","beta", "m", 'tau2')

model_ssvs <- jags.model(model_str,data = dat, n.chains=4)
update(model_ssvs, n_burnin)
samples_ssvs <- coda.samples(model_ssvs, variable.names=params, n.iter = n_iter)

save(samples_ssvs, file = "samples_nbSsvs2.Rdata")
save(DIC.ssvs, file = "DIC_nbssvs.Rdata")
DIC.ssvs= dic.samples(model_ssvs, n.iter = 20000)


(ess_nbSsvs <- effectiveSize(samples_ssvs))
gelman.diag(samples_ssvs)

save(samples_ssvs, file = "samples_nbSsvs2.Rdata")






full_params = rbind(samples_ssvs[[1]], samples_ssvs[[2]],samples_ssvs[[3]],samples_ssvs[[4]])
gamma = full_params[,48:93]
model <- ""
for(j in 1:46){
  temp  <- ifelse(gamma[,j]==1,colnames(X)[j],"")
  model <- paste(model," ",temp) 
}

most_common_model <- which.max(table(model))
colnames(gamma)   <-colnames(X[,-1])
marg_inc_probs    <- colMeans(gamma)


round(marg_inc_probs,2)

round(marg_inc_probs[round(marg_inc_probs,2) >= 0.5], 2)

#Get the coefs of included variables
round(coefs,2)[c(2,3,5,8,18,19,38,39),]
#get the 95% interval



