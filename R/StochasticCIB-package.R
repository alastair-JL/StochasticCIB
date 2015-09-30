#' @name StochasticCIB
#' @title StochasticCIB
#' @docType package
#' @description
#' A collection of functions to allow Cross-Impact Balance analysis.
#' 
#' @details
#' \tabular{ll}{
#' Package: \tab AnthroTools\cr
#' Type: \tab Package\cr
#' Version: \tab 0.6\cr
#' Date: \tab 2014-23-10\cr
#' License: \tab GPL 2\cr
#' }
#' @note To get started use \code{\link{InputCibBanner}} to describe the system you are studying.
#'  Next \code{\link{MakeScoreMatrix}} will find the relative scorws of each 
#'  world state from each other world state, and you'll be able to use one of
#'   the many \code{\link{TransitionCalculators}} to determine the transition probabilities of your system.
#'   Finally, \code{\link{ForecastStatistics}} will calculate the long term forecast.
#' 
#' @keywords package
#' 
#' @references 
#' Smith, J. J., Furbee, L., Maynard, K., Quick, S., & Ross, L. (1995). Salience Counts: A Domain Analysis of English Color Terms. Journal of Linguistic Anthropology, 5(2), 203-216. http://doi.org/10.1525/jlin.1995.5.2.203.
#' 
#' @author Alastair Jamieson-Lane. <aja107@@math.ubc.ca>
NULL