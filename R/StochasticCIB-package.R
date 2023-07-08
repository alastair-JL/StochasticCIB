#' @name StochasticCIB
#' @title StochasticCIB
#' @docType package
#' @description
#' StochasticCIB is an R package containing a number of function designed to facilitate Cross-Impact Balance analysis.
#' A Brief introduction to CIB analysis is given here: \link{CIBintroduction}.
#' 
#' For a detailed discussion of Cross Impact Balance please see the references below. 
#' 
#' Please be warned, Cross Impact Balance is nothing more than a mathematical framework for exploring hypothetical scenarios.
#' It is not a crystal ball, and it would be unwise to imagine that the presence of numbers correspond with either certainty or precision.
#' The numbers merely allow us to mechanise the process of considering hypothetical situation, and allow us to estimate where we should focus our attention.
#' 
#' @details
#' \tabular{ll}{
#' Package: \tab StochasticCIB\cr
#' Type: \tab Package\cr
#' Version: \tab 0.8\cr
#' Date: \tab 2019-23-06\cr
#' License: \tab GPL 2\cr
#' }
#' 
#' @note To get started use \code{\link{InputCibBanner}} to describe the system you are studying.
#'  Next \code{\link{MakeScoreMatrix}} will find the relative scores of each 
#'  world state from each other world state, and you'll be able to use one of
#'   the many \code{\link{TransitionCalculators}} to determine the transition probabilities of your system.
#'   Use, \code{\link{CIBforecast}} will calculate the long term forecast.
#'   Finally \code{\link{CIBgraph}} will visualize your results; some tweaking may be required.
#'   
#'   To see an Example CIB matrix, use \code{\link{ExampleCIBdata}}.
#' 
#' @keywords package
#' 
#' @references 
#' Markov Models of Social Change (Part 1)- Azimuth Forum https://johncarlosbaez.wordpress.com/2014/02/24/markov-models-of-social-change-part-1/
#'
#' Markov Models of Social Change (Part 2)- Azimuth Forum https://johncarlosbaez.wordpress.com/2014/03/05/markov-models-of-social-change-part-2/
#' 
#' Wolfgang Weimer-Jehle, Cross-impact balances: a system-theoretical approach to cross-impact analysis, Technological Forecasting & Social Change 73 (2006), 334–361.
#' 
#' @author Alastair Jamieson-Lane. <aja107@@math.ubc.ca>
NULL