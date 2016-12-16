

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

model.fit <- function(train)
{
	start.t <- proc.time()
	fit <- lm(y ~ X, data=train)
	end.t <- proc.time()
	duration <- end.t - start.t

	out <- list(fit=fit, coef=as.vector(coef(fit)[2:length(coef(fit))]), 
		    duration=as.numeric(duration[3]))
	return(out)
}

model.predict <- function(fit, test)
{
	start.t <- proc.time()
	y.pred <- predict(fit, newdata=test)
	end.t <- proc.time()
	duration <- end.t - start.t
	out <- list(y.pred=as.vector(y.pred), duration=as.numeric(duration[3]))
	return(out)
}

main <- function()
{
	argv <- commandArgs(trailingOnly=T)
	train.filename <- argv[1]
	test.filename <- argv[2]

	load(train.filename)
	load(test.filename)

	run.out <- model.fit(train)
	train.time <- run.out$duration
	y.train.pred <- as.vector(predict(run.out$fit))

	pred.out <- model.predict(run.out$fit, test)
	y.test.pred <- pred.out$y.pred
	pred.time <- pred.out$duration

	total.time <- train.time + pred.time

	print.results(train$y, y.train.pred,
		      test$y, y.test.pred,
		      train$true.coef, run.out$coef,
		      total.time)
}

main()
