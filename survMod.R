library(nimble)
library(coda)
mod <- nimbleCode ({

	for(t in 1:(nyears-1)){
		s.const[t] ~ dunif(0,1)
		r.const[t] ~ dunif(0,1)
	}
	for(t in 1:(nyears-1)){
			s[t] <- s.const[t]
			r[t] <- r.const[t]
		}
	for(t in 1:(nyears - 1)){
		marr[t,1:nyears] ~ dmulti(pi[t,1:nyears], rel[t])
		}
	for(t in 1:(nyears-1)){
		pi[t,t] <- (1-s[t]) * r[t] # diagonal
		for(j in (t+1):(nyears - 1)){
			pi[t,j] <- prod(s[t:(j-1)]*(1-s[j]))*r[j]
		} # j
		for(j in 1:(t-1)){
			pi[t,j] <- 0
		} # j
	} # t
	for(t in 1:(nyears - 1)){
		pi[t,nyears] <- 1 - sum(pi[t,1:(nyears-1)])
	}

})

# Build NIMBLE model
mod <- nimbleModel(
	mod,
	const = list(nyears = nyears,
							 ninds = ninds),
	data = list(marr = out,
							rel = rowSums(out)),
	inits = list(r.const = rep(0.5,(nyears-1)),
							 r.const = rep(0.5,(nyears-1)))
)

comp.mod <- compileNimble(mod)
conf.mod <- configureMCMC(comp.mod, monitors = c('r.const','s.const'),  enableWAIC = TRUE)
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
