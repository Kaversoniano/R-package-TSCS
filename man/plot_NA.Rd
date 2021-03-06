% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_NA.R
\name{plot_NA}
\alias{plot_NA}
\title{Visualize the Spatial Distribution of Missing Observations - 2D Map}
\usage{
plot_NA(newdata, xlab = NULL, ylab = NULL, title = NULL, cex = 1)
}
\arguments{
\item{newdata}{data frame; should only contain the three variables in order: X coordinate, Y coordinate and observation.
This is the cross-section data or pure spatial data of a particular time point you have selected,
with missing observations that you want to predict. (coordinates must be numeric)}

\item{xlab}{a label for the x axis, defaults to the name of X coordinate.}

\item{ylab}{a label for the y axis, defaults to the name of Y coordinate.}

\item{title}{a main title for the plot.}

\item{cex}{numeric; size of plotting point for each spatial location. (default: 1)}
}
\description{
\code{plot_NA} shows spatial locations with or without missing observation. It is plotted based on
the cross-section data of a given time point, which is also often extracted from spatio-temporal data.
}
\details{
\code{plot_NA} is exclusive to 2D rectangular grid system. Similarly, if you want to fathom how this package
  handles 3D rectangular grid system, please refer to \code{plot3D_NA}.
}
\examples{
\dontrun{

## TSCS spatial interpolation procedure:

basis <- tscsRegression(data = data, h = 1, v = 1, alpha = 0.01); # regression
basis$percentage # see the percentage of cointegrated relationships
est <- tscsEstimate(matrix = basis$coef_matrix, newdata = newdata, h = 1, v = 1); # estimation
str(est)

## comparison of estimates and true values:

plot_compare(est = est$estimate[,3], true = true) # graphic comparison
index <- appraisal_index(est = est$estimate[,3], true = true); # RMSE & std
index

## data visualization:

plot_dif(data = data[,1:2], h = 1, v = 1) # differentiate boundary and interior spatial locations
plot_NA(newdata = newdata) # show spatial locations with missing value, for a cross-section data
plot_map(newdata = newdata) # plot the 2D spatial map, for a cross-section data
}
}
\seealso{
\code{\link{plot3D_NA}}, \code{\link{plot_map}}, \code{\link{plot_dif}}
}
