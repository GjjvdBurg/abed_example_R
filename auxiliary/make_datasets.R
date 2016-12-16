
library(MASS)

N_DATASETS <- 10
DATADIR <- "../datasets/"

make.regression <- function(n.samples=100, n.features=100, n.informative=10,
			    n.targets=1, bias=0.0, tail.strength=0.5, 
			    noise=0.0, shuffle=T, coef=F, random.state=0)
{
	n.informative <- min(n.features, n.informative)
	set.seed(random.state)

	mu <- matrix(0, 1, n.features)
	Sigma <- diag(1, n.features)
	X <- mvrnorm(n.samples, mu, Sigma)

	ground.truth <- matrix(0, n.features, n.targets)
	ground.truth[1:n.informative, ] = 100.0 * matrix(runif(n.informative *
							       n.targets),
							 n.informative,
							 n.targets)
	y <- X %*% ground.truth + bias
	if (noise > 0.0) {
		y <- y + rnorm(n.samples, sd=noise)
	}

	if (shuffle) {
		idx <- sample(c(1:n.samples))
		nX <- X[idx, ]
		ny <- y[idx, ]
		idx <- sample(c(1:n.features))
		X <- nX[, idx]
		thecoef <- ground.truth[idx, ]
		y <- ny
	} else {
		thecoef <- ground.truth
	}
	if (coef) {
		out <- list(X=X, y=as.matrix(y), coef=thecoef)
	} else {
		out <- list(X=X, y=as.matrix(y))
	}
	return(out);
}

train.test.split <- function(X, y, test.size=NULL, random.state=0)
{
	set.seed(random.state)
	n <- dim(X)[1]

	test.n <- round(test.size * n)
	train.n <- n - test.n

	test.idx <- sample(1:n, test.n)
	train.idx <- setdiff(1:n, test.idx)

	X.train <- X[train.idx, ]
	y.train <- y[train.idx, ]

	X.test <- X[test.idx, ]
	y.test <- y[test.idx, ]

	out <- list(X.train=X.train, y.train=y.train, X.test=X.test,
		    y.test=y.test)
	return(out)
}

main <- function()
{
	dir.create(DATADIR, showWarning=F)
	for (i in 1:N_DATASETS) {
		bias = 10.0 * runif(1)
		out <- make.regression(n.samples=900, n.features=20, 
				       n.informative=10, bias=bias, noise=2.0,
				       coef=T, 
				       random.state=round(runif(1)*1e6))
		Xys <- train.test.split(out$X, out$y, test.size=1.0/3.0, 
					random.state=42)

		train <- list(X=Xys$X.train, y=Xys$y.train, true.coef=out$coef)
		test <- list(X=Xys$X.test, y=Xys$y.test)

		fname <- sprintf("%s/dataset_%i_train.RData", DATADIR, i)
		save(train, file=fname)
		fname <- sprintf("%s/dataset_%i_test.RData", DATADIR, i)
		save(test, file=fname)
	}
}

main()
