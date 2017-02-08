model {
	for (i in 1:n){
		stops[i] ~ dpois (lambda [i])
		
		log(lambda[i]) <- offset[i] + mu + b.eth[eth[i]] + b.prescint[prescint[i]] + epsilon[i]

		epsilon[i] ~ dnorm(0, tau.epsilon)
	}

	mu ~ dnorm(0, .001)
	mu.adj <- mu + mean(b.eth[]) + mean(b.prescint[])
	tau.epsilon <- pow(sigma.epsilon, -2)
	sigma.epsilon ~ dunif(o, 100)

	for (j in 1:n.eth){
		b.eth[j] ~ dnorm(0, tau.eth)
		b.eth.adj[j] <- b.eth[j] - mean(b.eth[])
	}

	tau.eth <- pow(sigma.eth, -2)
	sigma.eth ~ dunif(0, 100)

	for (j in 1:n.prescint){
		b.prescint[j] ~ dnorm(0, tau.prescint)
		b.prescint.adj[j] <- b.prescint[j] - mean(b.prescint[])
	}

	tau.prescint <- pow(sigma.prescint, -2)
	sigma.prescint ~ dunif(0, 100)
}