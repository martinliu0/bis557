#' Naive implementation of ridge regression.
#'
#' @description a naive implementation of ridge regression using singular value decomposition.
#'
#' @param form a formula specifying the regression model.
#' @param data the data frame on which to fit the regression model.
#' @param lambda ridge penalty term.
#' @param contrasts contrasts.
#'
#' @importFrom stats model.matrix
#' @return A list of beta coefficients.
#'
#' @examples
#' data(iris)
#' ridge_regression(Sepal.Length  ~ ., iris, lambda=0.1)
#'
#' @export
ridge_regression <- function(form, data, lambda = 0,contrasts = NULL) {

  #model matrix, response variable
  rownames(data) <- NULL
  X <- model.matrix(form, data,contrasts.arg = contrasts)
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]

  #standardize the model matrix and response
  n <- nrow(X)
  p <- ncol(X)-1
  Xm <- colMeans(X[, -1])
  Ym <- mean(Y)
  X <- X[, -1] - rep(Xm, rep(n, p))
  Xscale <- sqrt(drop(rep(1/n, n) %*% X^2))
  X <- X/rep(Xscale, rep(n, p))
  Y <- Y- Ym

  #singular value decomposition
  svd_obj <- svd(X)
  U <- svd_obj$u
  V <- svd_obj$v
  d <- svd_obj$d
  k <- length(lambda)


  #calculate ridge coefficients
  D <- diag(d / (d^2 + lambda))
  ridge_beta <- V %*% D %*% t(U) %*% Y
  ridge_beta <- t(as.matrix(ridge_beta / Xscale))

  #putting everything together
  ridge_beta <- drop(cbind(Ym - ridge_beta %*% Xm, ridge_beta))
  names(ridge_beta) <- c("Intercept", colnames(X))
  class(ridge_beta) <-  c(class(ridge_beta), "ridge_regression")
  attributes(ridge_beta)$formula <- form
  ridge_beta
}

#' Prediction method for ridge regression
#'
#' @param object ridge_regression object
#' @param ... `(dataframe)`
#'
#' @return An estimation of coefficients based on test data.
#' @export
#'
predict.ridge_regression <- function(object, ...){
  dots <- list(...)
  x_frame <- dots[[1]]
  if (!is.data.frame(x_frame)){
    stop("The first argument should be a data.frame of values", "to predict")
  }
  X <- model.matrix(attributes(object)$formula, x_frame)
  X %*% object
}
