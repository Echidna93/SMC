library(nimble)
library(coda)
nimbCode <- nimbleCode({
	shape  ~ dunif(0.1, 1)
	#lambda ~ dunif(100, 1000)
	for(i in 1:ninds) {
		lambda[i] <- exp(beta0 + beta1*x[i,4] + beta2*x[i,2])
		t[i] ~ dweib(shape, lambda[i])
	}
	beta0 ~ dunif(0, 7)
	beta1 ~ dnorm(0, 1)
	beta2 ~ dnorm(0, 1)
})
# Build NIMBLE model
nimbMod <- nimbleModel(
	nimbCode,
	const = list(ninds = ninds),
	data = list(t = as.integer(y[,1]),
							x = y[,2:5]))

comp.mod <- compileNimble(nimbMod)
conf.mod <- configureMCMC(comp.mod, monitors = c('beta0', 'beta2', 'beta1', 'lambda', 'shape'),  enableWAIC = TRUE)
mod.mcmc <- buildMCMC(conf.mod)
c.mod <- compileNimble(mod.mcmc)
mcmc.out <- runMCMC(c.mod,
										niter=1000,
										nburnin=100,
										nchains=4,
										thin = 1,
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

# simulate new datasets
deps<- comp.mod$getDependencies(c("beta0", "beta1", "beta2", "shape", "lambda"), downstream = TRUE, self = FALSE)
