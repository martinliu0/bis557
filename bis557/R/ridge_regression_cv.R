#' Naive implementation of ridge regression using cross validation.
#'
#' @description a naive implementation to find optimal lambdas for ridge regression using cross validation.
#'
#' @param data the data frame on which to fit the regression model.
#' @param form a formula specifying the regression model.
#' @param folds number of folds.
#' @param lambdas a lambda values from which the optimal will be returned.
#' @param contrasts contrasts.
#'
#' @return RMSEs of different lambda values, a plot and the lambda value that minimizes RMSE.
#' @importFrom stats sd model.matrix predict var qnorm
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores
#' @importFrom rsample vfold_cv testing training
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom magrittr %>%
#' @import dplyr ggplot2
#' @examples
#' data(iris)
#' ridge_regression_cv(Sepal.Length  ~ ., iris, folds = 3, lambdas=c(0, 0.1, 1))
#'
#' @export
ridge_regression_cv <- function(form, data, folds = 5, lambdas = c(0,0.1,1),contrasts = NULL){
  i <- lambda <- `.` <- lower <- upper <- NULL
  #cutting the data into k folds
  folds <- vfold_cv(data, v = folds)

  #helper function to calculate the RMSE
  rmse <- function (y, y_hat, by)
  {
    if (missing(by)) {
      ret <- sqrt(mean((y - y_hat)^2))
    }
    else {
      ret <- sqrt(tapply((y - y_hat)^2, by, mean))
    }
    ret
  }

  #set number of cores for parallelization
  registerDoParallel(detectCores(logical = FALSE))

  #browser()
  #calculate RMSE's
  rmses <- foreach(lambda = lambdas, .combine = rbind) %dopar% {
    foreach(i = seq_len(nrow(folds)), .combine = c) %do% {
      rmse(
        testing(folds$splits[[i]])[[as.character(form[2])]],
        predict(ridge_regression(form, training(folds$splits[[i]]),
                                 lambda = lambda, contrasts = contrasts),
                testing(folds$splits[[i]]))
      )
    }
  }

  #create tibble
  edf <- tibble(mean = apply(rmses, 1, mean),
                sd = apply(rmses, 1, sd),
                lambda = lambdas) %>%
    mutate(upper = mean + 2 * sd / sqrt(nrow(.)),
           lower = mean - 2 * sd / sqrt(nrow(.)))

  best_lambda <- edf$lambda[which.min(edf$mean)]

  #return the best lambda, the tibble, and a plot
  list(cv_table = edf, cv_plot = {
    ggplot2::ggplot(edf, aes(x = lambdas, y = mean, ymin = lower, ymax = upper)) +
      geom_errorbar() +
      theme_minimal() +
      geom_point(aes(color = "red")) +
      ylab("Root Mean Squared Error") +
      xlab(expression(lambda)) +
      theme(legend.position = "none")
  },
  best_lambda = best_lambda)

}
