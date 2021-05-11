####Modeling####
library(rjags)

Y = comarea$ChlLeadDec
#Transform the data (Given in Smithson M, Verkuilen J (2006))
Y = (Y*(length(Y)-1) + 0.5) / length(Y)

X = comarea[,c(1:46)]
X = cbind(1, scale(X))

n_burnin <- 20000
n_iter <- 100000
####1####
#Full Beta Regression (Logit link)

dta = list(Y = Y, X = X, p = ncol(X), n = length(Y))
params = c('beta', "r")
model_str <- textConnection("model{
for(i in 1:n){
Y[i] ~ dbeta(r*mu[i],r*(1-mu[i])) 
logit(mu[i]) <- inprod(X[i,],beta[])
}
for(j in 1:p){beta[j] ~ dnorm(0,0.01)}
r ~ dgamma(0.1,0.1)
}")

model_logit <- jags.model(model_str,data = dta, n.chains=4, n.adapt = 2000)
update(model_logit, n_burnin)
samples_logit <- coda.samples(model_logit, variable.names=params, n.iter=n_iter)

DIC.logit = dic.samples(model_logit, n.iter = 20000)

#Save the data
save(samples_logit, file = 'samples_betaLogit.Rdata')
save(DIC.logit, file = "DIC_logit.Rdata")

#########################################
#Full Beta Regression (Probit link)

dta = list(Y = Y, X = X, p = ncol(X), n = length(Y))
params = c('beta', "r")
model_str <- textConnection("model{
for(i in 1:n){
Y[i] ~ dbeta(r*mu[i],r*(1-mu[i])) 
probit(mu[i]) <- inprod(X[i,],beta[])
}
for(j in 1:p){beta[j] ~ dnorm(0,0.01)}
r ~ dgamma(0.1,0.1)
}")

model_probit <- jags.model(model_str,data = dta, n.chains=4)
update(model_probit, n_burnin)
samples_probit <- coda.samples(model_probit, variable.names=params, n.iter = n_iter)

DIC.probit= dic.samples(model_probit, n.iter = 20000)

save(samples_probit, file = "samples_betaProbit.Rdata")
save(DIC.probit, file = 'DIC_probit.RData')


####2####
#Spike and Slab Regression 

##SSVS with logit link
params = c("gamma", "beta", "r", "tau2")
model_str <- textConnection("model{
for(i in 1:n){
Y[i] ~ dbeta(r*mu[i],r*(1-mu[i])) 
probit(mu[i]) <- inprod(X[i,],beta[])
}
beta[1] ~ dnorm(0,0.01)
for(j in 2:p){
beta[j]  = delta[j]*gamma[j]
gamma[j] ~ dbern(0.5)
delta[j] ~ dnorm(0,tau2)
}
r ~ dgamma(0.1,0.1)
tau2 ~ dgamma(0.1,0.1)
}")

model_ssvs <- jags.model(model_str,data = dta, n.chains=4)
update(model_ssvs, n_burnin)
samples_ssvs <- coda.samples(model_ssvs, variable.names=params, n.iter = n_iter)

DIC.ssvs= dic.samples(model_ssvs, n.iter = 20000)


save(samples_ssvs, file = "samples_bSsvsProbit.Rdata")
save(DIC.ssvs, file = 'DIC_ssvsProbit.RData')

#Determine which vars to keep
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

marg_inc_probs[round(marg_inc_probs,2) >= 0.4]


#Get the coefs of included variables
round(coefs,2)[c(2,3,5,8,18,19,38,39),]
#get the 95% interval
