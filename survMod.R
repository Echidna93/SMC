library(nimble)
library(coda)
mod <- nimbleCode ({
	beta0.r ~ dunif(0,1)
	beta0.s ~ dunif(0,1)
	prec.beta1.r ~ dgamma(0.5,0.5)
	prec.beta1.s ~ dgamma(0.5,0.5)
	beta1.r.sigma <- 1/prec.beta1.r
	beta1.s.sigma <- 1/prec.beta1.s
	for(i in 1:ndistinctmonths){
		beta1.r[i] ~ dunif(0,prec.beta1.r)# see how month affects recovery
		beta1.s[i] ~ dunif(0,prec.beta1.s)
	}
	for(t in 1:(nmonths-1)){
			s[t] <- beta0.s + beta1.s[months[t]] * months[t]
			r[t] <- beta0.r + beta1.r[months[t]] * months[t]
		}
	for(t in 1:(nmonths - 1)){
		marr[t,1:nmonths] ~ dmulti(pi[t,1:nmonths], rel[t])
		}
	for(t in 1:(nmonths-1)){
		pi[t,t] <- (1-s[t]) * r[t] # diagonal
		for(j in (t+1):(nmonths - 1)){
			pi[t,j] <- prod(s[t:(j-1)]*(1-s[j]))*r[j]
		} # j
		for(j in 1:(t-1)){
			pi[t,j] <- 0
		} # j
	} # t
	for(t in 1:(nmonths - 1)){
		pi[t,nmonths] <- 1 - sum(pi[t,1:(nmonths-1)])
	}

})

# Build NIMBLE model
mod <- nimbleModel(
	mod,
	const = list(nmonths = nmonths,
							 ninds = nrow(marr),
							 ndistinctmonths = length(seq(1,12,by=1)),
							 months = months),
	data = list(marr = marr,
							rel = rowSums(marr))) # vector containing numeric value associated w each month

comp.mod <- compileNimble(mod)
conf.mod <- configureMCMC(comp.mod, monitors = c('beta1.s','beta1.r', 'beta1.r.sigma',
																								 'beta1.s.sigma'),  enableWAIC = TRUE)
mod.mcmc <- buildMCMC(conf.mod)
c.mod <- compileNimble(mod.mcmc)
## Run MCMC
mcmc.out <- runMCMC(c.mod,
										niter=1000,
										nburnin=100,
										nchains=3,
										thin = 5,
										WAIC=TRUE)

post.samples <- mcmc.list(sapply(mcmc.out$samples,
																 as.mcmc,simplify=FALSE))
summary(post.samples)
post.samples <- mcmc.list(sapply(mcmc.out$samples,
																 as.mcmc,simplify=FALSE))
plot(post.samples, trace=TRUE, density=FALSE)
gelman.plot(post.samples)
autocorr.plot(post.samples)

## posterior summary
summary(post.samples)
## model assessment using WAIC value
mcmc.out$WAIC
