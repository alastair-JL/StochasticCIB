#' LocalBoltzmann
#'
#' A Local stochastic succession rule. For all adjacent states (states where only one descriptor has changed), calculate the relative change in score (compared to staying in the current location). Take the exponential of this value to get the relative probability. Once all adjacent values have been exponentiated, we divide by the sum over all such values in order to find the transition probabilities. 
#' States mathematically, if \eqn{P_{A\rightarrow B}} is the transition rate from A to B, and \eqn{S_A(B)} is the score of the state B, from the point of view of A, then
#' \deqn{P_{A\rightarrow B= \frac{exp[\beta S_A(B)]}}{\sum exp[\beta S_A(C)]}}
#' Where here we sum over all states that involve changing at most one descriptor.
#' 
#' @keywords CIB
#' @export
#' @param TheList a list containing the CIB matrix, and a "shape" vector. The output of \code{\link{InputCibBanner}} is an appropriate input here.
#' @param TransRelAdj a list containing a blank transition matrix, a relative score matrix, and an adjacency matrix. The output of \code{\link{MakeScoreMatrix}} is appropriate here.
#' @param beta Optional parameter used to scale score values. Large beta leads to a result similar to \code{\link{TransToMaxAdj}}. Low beta leads to results similar to ``change descriptors at random''.
#' @return A matrix describing the transition probability from each world state (rows) to each other world state (columns).
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @examples
#' data(antCIB)
#' LocalBoltzmann(antCIB)

LocalBoltzmann<-function(TheList, TransRelAdj=NA, beta=1){
  if (is.na(TransRelAdj)){
    TransRelAdj<- MakeScoreMatrix(antCIB)   
  }
  Transitions<-TransRelAdj[[1]]
  RelativeScores<-TransRelAdj[[2]]
  AdjacentMatrix<-TransRelAdj[[3]]    
  Delta= beta*(RelativeScores-diag(RelativeScores))
  Transitions<-exp(Delta)*(AdjacentMatrix> -0.5)  
  Transitions<-t(apply(Transitions, 1, function(x) x/sum(x)))
} 
