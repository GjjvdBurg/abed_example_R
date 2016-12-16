
require(methods)
require(glmnet)

printf <- function(...) invisible(cat(sprintf(...)));

print.results <- function(y.train.true, y.train.pred, y.test.true,
			  y.test.pred, coef.true, coef.pred, total.time)
{
	printf("%% y_train_true y_train_pred\n")
	n = length(y.train.true)
	for (i in 1:n) {
		printf("%.16f\t%.16f\n", y.train.true[i], y.train.pred[i])
	}

	printf("%% y_test_true y_test_pred\n")
	n = length(y.test.true)
	for (i in 1:n) {
		printf("%.16f\t%.16f\n", y.test.true[i], y.test.pred[i])
	}

	printf("%% coef_true coef_pred\n")
	n = length(coef.true)
	for (i in 1:n) {
		printf("%.16f\t%.16f\n", coef.true[i], coef.pred[i])
	}

	printf("%% time\n")
	printf("%.16f\n", total.time)
}

make.data.from.idx <- function(X, y, idxs)
{
	n <- length(idxs)
	Xi <- X[idxs, ]
	yi <- y[idxs]

	dset <- list(X=Xi, y=yi)
	return(dset)
}

k.fold <- function(n, n.folds, shuffle=TRUE, seed=0)
{
	set.seed(seed)
	idxs <- c(1:n)
	if (shuffle) {
		idxs <- sample(idxs)
	}

	# Determine fold sizes
	fsizes <- c(1:n.folds)*0 + floor(n / n.folds)
	mod <- n %% n.folds
	if (mod > 0)
		fsizes[1:mod] <- fsizes[1:mod] + 1

	out <- list(n=n, num.folds=n.folds)
	current <- 1
	for (f in 1:n.folds) {
		fs <- fsizes[f]
		startidx <- current
		stopidx <- current + fs - 1
		test.idx <- idxs[startidx:stopidx]
		train.idx <- idxs[!(idxs %in% test.idx)]
		out$testidxs[[f]] <- test.idx
		out$trainidxs[[f]] <- train.idx
		current <- stopidx
	}
	return(out)
}

make.cv <- function(X, y, seed=0)
{
	n.folds <- 10
	n <- length(y)
	folds <- k.fold(n, n.folds=n.folds, shuffle=T, seed=seed)

	out <- list(num.folds=n.folds)
	for (f in 1:n.folds) {
		out$traindata[[f]] <- make.data.from.idx(X, y, folds$trainidx[[f]])
		out$testdata[[f]] <- make.data.from.idx(X, y, folds$testidx[[f]])
		out$trainidx[[f]] <- folds$trainidx[[f]]
		out$testidx[[f]] <- folds$testidx[[f]]
	}
	return(out)
}

model.fit <- function(X, y, lambda)
{
	start.t <- proc.time()
	fit <- glmnet(X, y, family="gaussian", alpha=1, standardize=F, 
		      lambda=lambda)
	end.t <- proc.time()
	duration <- end.t - start.t

	out <- list(fit=fit, coef=as.vector(coef(fit)[2:length(coef(fit))]), 
		    duration=as.numeric(duration[3]))
	return(out)
}

model.predict <- function(fit, X)
{
	start.t <- proc.time()
	y.pred <- predict(fit, X, type="response")
	end.t <- proc.time()
	duration <- end.t - start.t
	out <- list(y.pred=y.pred, duration=as.numeric(duration[3]))
	return(out)
}

run.cv <- function(X, y, alpha, seed)
{
	cv.time <- 0.0
	cv <- make.cv(X, y, seed=seed)
	predictions <- c(1:nrow(X))*0
	for (f in 1:cv$num.folds) {
		# Get training data
		train.X <- cv$traindata[[f]]$X
		train.y <- cv$traindata[[f]]$y

		# Fit model for fold
		run.out <- model.fit(train.X, train.y, alpha)

		# Get test data
		test.X <- cv$testdata[[f]]$X
		test.y <- cv$testdata[[f]]$y

		# Predict test data
		pred.out <- model.predict(run.out$fit, test.X)

		# Record predictions
		test.idx <- cv$testidx[[f]]
		j <- 1
		for (idx in test.idx) {
			predictions[idx] <- pred.out$y.pred[j]
			j <- j + 1
		}
		cv.time <- cv.time + run.out$duration + pred.out$duration
	}

	out <- list(predictions=predictions, cv.time=cv.time)
	return(out)
}

main <- function()
{
	argv <- commandArgs(trailingOnly=T)
	train.filename <- argv[1]
	test.filename <- argv[2]
	alpha <- as.double(argv[3])
	seed <- as.integer(argv[4])

	load(train.filename)
	load(test.filename)

	cv.out <- run.cv(train$X, train$y, alpha, seed)
	y.train.pred <- cv.out$predictions
	cv.time <- cv.out$cv.time

	run.out <- model.fit(train$X, train$y, alpha)
	train.time <- run.out$duration

	pred.out <- model.predict(run.out$fit, test$X)
	y.test.pred <- pred.out$y.pred
	pred.time <- pred.out$duration

	total.time <- cv.time + train.time + pred.time

	print.results(train$y, y.train.pred,
		      test$y, y.test.pred,
		      train$true.coef, run.out$coef,
		      total.time)
}

main()
