#' TransitionCalculators
#'
#' Cross impact balance analysis runs on the assumption that a number 
#' of factors determine how the current state of the world will effect, or lead to, the future state of the world (or system in mind).
#' However, the "scores" produced by CIB analysis can be interpretted in a variety of different ways.
#' Here we list the various "interpretation" functions, and give a basic explaination of their use. For more details, follow the links to
#' the individual transition calculators. All transition calculators take the same input.
#' 
#' @param TheList a list containing the CIB matrix, and a "shape" vector. The output of \code{\link{InputCibBanner}} is an appropriate input here.
#' @param TransRelAdj a list containing a blank transition matrix, a relative score matrix, and an adjacency matrix. The output of \code{\link{MakeScoreMatrix}} is appropriate here. If this parameter is not input, the transition function will call \code{\link{MakeScoreMatrix}} itself.
#' @note The currently available transition functions are: \code{\link{TransToMax}}, \code{\link{TransToMaxAdj}}, \code{\link{TransToFastestActing}}, \code{\link{DiceTransition}}, \code{\link{LocalBoltzmann}}, \code{\link{LocalLogistic}}, \code{\link{LocalArctan}}
#' For details on each, please follow the respective links.
#' @note Be warned, all transition functions are liable to malfunction in the presence of unreasonably large score values (999999 or higher). If you find yourself with score values that large, please consider dividing all scores by 100 (or similar).
#' @keywords CIB
#' @name TransitionCalculators
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @examples
#' data(ExampleCIBdata)
#' boltzTrans<-LocalBoltzmann(ExampleCIBdata)
#' TransScoresAdj<-MakeScoreMatrix(ExampleCIBdata)
#' TransMaxAdj<-TransToMaxAdj(ExampleCIBdata,TransScoresAdj)
#' 
#' 
NULL