% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridge_regression_cv.R
\name{ridge_regression_cv}
\alias{ridge_regression_cv}
\title{Naive implementation of ridge regression using cross validation.}
\usage{
ridge_regression_cv(form, data, folds = 5, lambdas = c(0, 0.1, 1),
  contrasts = NULL)
}
\arguments{
\item{form}{a formula specifying the regression model.}

\item{data}{the data frame on which to fit the regression model.}

\item{folds}{number of folds.}

\item{lambdas}{a lambda values from which the optimal will be returned.}

\item{contrasts}{contrasts.}
}
\value{
RMSEs of different lambda values, a plot and the lambda value that minimizes RMSE.
}
\description{
a naive implementation to find optimal lambdas for ridge regression using cross validation.
}
\examples{
data(iris)
ridge_regression_cv(Sepal.Length  ~ ., iris, folds = 3, lambdas=c(0, 0.1, 1))

}
