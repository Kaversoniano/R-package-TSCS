#' A Package for TSCS Spatial Interpolation Method
#' 
#' This package provides functions to implement TSCS spatial interpolation and
#'  relevant data visualization. For TSCS method, the current version is only able to
#'  make use of spatio-temporal data whose spatial domain is a 2D or 3D rectangular grid system.
#' 
#' @details
#' \enumerate{
#'   \item TSCS (abbr. of Time Series Cointegrated System) method is a spatial
#'   interpolation method based on analysis of historical spatio-temporal data.
#'   It can be regarded as a desirable alternative to spatio-temporal interpolation
#'   in some cases where we merely intend to interpolate a series of cross-section data
#'   at each observed time point for a given spatial domain.
#'   \item The basic assumption of TSCS method is that, for any spatial location within
#'   the spatial domain of spatio-temporal data, its time series and the time series of
#'   its adjacent spatial locations are cointegrated (long-term equilibrium relationships).
#'   \item As to TSCS method, package of the current version is only able to make use of
#'   spatio-temporal data whose spatial domain is a 2D or 3D rectangular grid system.
#' }
#' 
#' @section Package Functions:
#' \itemize{
#'   \item \code{tscsRegression, tscsRegression3D} : obtains regression coefficient matrix, the first step of
#'   TSCS for 2D and 3D rectangular grid system respectively.
#'   \item \code{tscsEstimate, tscsEstimate3D} : estimates the missing observations within a cross-section data
#'   (pure spatial data) of a particular time point you have selected, the second step of TSCS for 2D and 3D
#'   rectangular grid system respectively.
#'   \item \code{plot_dif, plot3D_dif} : differentiates boundary and interior spatial locations in a spatial domain.
#'   \item \code{plot_NA, plot3D_NA} : shows spatial locations with or without missing observation in a spatial domain.
#'   \item \code{plot_map, plot3D_map} : draws the spatial map for a cross-section data.
#'   \item \code{plot_compare} : visualizes the comparison between estimates and true values (if you have).
#'   \item \code{appraisal_index} : computes the two appraisal indexes used for evaluating the result of
#'   interpolation/prediction - RMSE and standard deviation of error. (if you have the true values)
#' }
#'
#' @author Tianjian Yang <yangtj5@mail2.sysu.edu.cn>
#' 
#' @docType package
#' @name TSCS
NULL
