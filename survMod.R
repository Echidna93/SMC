library(nimble)


modelcode <- nimbleCode({
	## priors
	beta0 ~ dnorm(0.0, sd = 5)  # vague prior on intercept
	beta1~ dnorm(0.0, sd = 1)
	gamma0 ~ dnorm(0.0, sd=5)
	gamma1 ~ dnorm(0.0, sd=1)
	prec.c ~ dgamma(0.1, 0.1)
	prec.ci ~ dgamma(0.1, 0.1)
	prec.h ~ dgamma(0.1, 0.1)
	prec.hi ~ dgamma(0.1, 0.1)
	# CAR model for spatial random effects
	phi[1:n.twnshps] ~ dcar_normal(adj[1:L], weights[1:L], num[1:n.twnshps], prec.c, zero_mean=0)
	psi[1:n.twnshps] ~ dcar_normal(adj[1:L], weights[1:L], num[1:n.twnshps], prec.ci, zero_mean=0)
	# likelihood
	for (i in 1 : n.twnshps) {
		for(j in 1: n.years){
			log(mu[i,j]) <-  s[i,j]*mu.hat[j] + beta1*precip[i,j] + beta0 + phi[i] + theta[i]
			logit(p[i,j]) <- s[i,j]*mu.hat[j] +  gamma1*precip[i,j] + gamma0 + psi[i] + eta[i]
			theta[i] ~ dnorm(0.0, prec.c)
			eta[i] ~ dnorm(0.0, prec.ci)
			y[i,j] ~ dZIP(mu[i,j], p[i,j])
			epsilon[i] <- theta[i] + phi[i]
			epsilon.hat[i] <- psi[i] + eta[i]
		}
	}
	## calculate alpha
	sd.h <- sd(phi[1:n.twnshps]) # marginal SD of heterogeneity effects
	sd.c <- sd(theta[1:n.twnshps])   # marginal SD of clustering (spatial) effects
	sd.hi <- sd(psi[1:n.twnshps])
	sd.ci <- sd(eta[1:n.twnshps])
	alpha = sd.h / (sd.c + sd.h)
	alpha.i <- sd.hi/(sd.ci + sd.hi)
	sigma2 <- 1/prec.h
	tau2 <- 1/prec.c
	sigma2i <- 1/prec.hi
	tau2i <- 1/prec.ci
})
p <- matrix(0, nrow=nrow(cwdMatAnalyzed), ncol=ncol(cwdMatAnalyzed))
## Specify data and initial values
constants <- list(n.twnshps = nrow(cwdMatAnalyzed),
									mu.hat=cwd.r,
									n.years=length(yearSamp),
									L = length(W$adj),
									adj=W$adj,
									weights=W$weights,
									num=W$num)
data <- list(y = cwdMatPos, s = cwdMatAnalyzed, p = p, precip=wiPPTStudy)
inits <- list(beta0 = 0, beta1=0, gamma1=0, gamma0 = 0)
## Build/Compile model, including steps:
## (1) build model (2) compile model in C++
## (3) specify MCMC parameters to collect and create MCMC algorithm
cwdspatmodel <- nimbleModel(modelcode, constants = constants, data = data, inits = inits)
c.cwdspatmodel <- compileNimble(cwdspatmodel)
confMC <- configureMCMC(cwdspatmodel, monitors = c('beta0', 'beta1', 'gamma1','gamma0', 'alpha', 'alpha.i', 'tau2', 'sigma2', 'tau2i', 'sigma2i'),  enableWAIC = TRUE)
cwdspatmcmc <- buildMCMC(confMC)
c.cwdspatmcmc <- compileNimble(cwdspatmcmc, project = cwdspatmodel)
## Run MCMC
mcmc.out <- runMCMC(c.cwdspatmcmc,
										niter=40000,
										nburnin=30000,
										thin=5,
										nchains=3,
										WAIC=TRUE)
## convert post samples as mcmc.list object and diagnose convergence using coda functions
post.samples <- mcmc.list(sapply(mcmc.out$samples,
																 as.mcmc,simplify=FALSE))
pars <- c("beta0",'gamma0', 'beta1', 'gamma1', 'sd.h', 'sd.hi', 'epsilon', 'epsilon.hat')
plot(post.samples, trace=TRUE, density=FALSE)
gelman.plot(post.samples)
autocorr.plot(post.samples)
## posterior summary
summary(post.samples)
## model assessment using WAIC value
mcmc.out$WAIC
