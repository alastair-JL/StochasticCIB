#' LocalArctan
#'
#' A Local stochastic succession rule. For all adjacent states (states where only one descriptor has changed), calculate the relative change in score (compared to staying in the current location). Take the arctan function of this value to get the relative probability. <NOTE, need better way of explaining this, or reference to paper>. Generally speaking, this transition function will give all transitions with negitive score negligable probability, and all transitions with positive score roughly equal probability.
#' @keywords CIB
#' @export
#' @param TheList a list containing the CIB matrix, and a "shape" vector. The output of \code{\link{InputCibBanner}} is an appropriate input here.
#' @param TransRelAdj a list containing a blank transition matrix, a relative score matrix, and an adjacency matrix. The output of \code{\link{MakeScoreMatrix}} is appropriate here.
#' @param beta Optional parameter used to scale score values. Large beta leads to a sharp `switch' from low to high probabilitiy
#' @param shift Optional parameter used to determine location of switch point between low and high probability. By default, this value is 0, indicating that the switch point occurs as a worldstates relative score passes the score of the current worldstate.
#' @note Behaves very similat to \code{\link{LocalLogistic}}
#' @return A matrix describing the transition probability from each world state (rows) to each other world state (columns).
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @examples
#' data(antCIB)
#' LocalArctan(antCIB)



LocalArctan<-function(TheList, TransRelAdj=NA, beta=1,shift=0){
  ##Note, I do not remember how much weight we decided to give the "stay put" option. 
  ##I feel like it was dealt with in a special manner, but can't remember how.    
    if (is.na(TransRelAdj)){
      TransRelAdj<- MakeScoreMatrix(antCIB)   
    }
  Transitions<-TransRelAdj[[1]]
  RelativeScores<-TransRelAdj[[2]]
  AdjacentMatrix<-TransRelAdj[[3]]    
  Delta= beta*(RelativeScores-diag(RelativeScores))+shift
  Transitions<-(atan(Delta)+pi/2)*(AdjacentMatrix> -0.5)  
  Transitions<-t(apply(Transitions, 1, function(x) x/sum(x)))
} 
