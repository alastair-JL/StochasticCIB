#' TransToMax
#'
#' A transition matrix function, whereby given the current worldstate, we always transition directly to the world state with the highest possible score. 
#' @keywords CIB
#' @export
#' @param TheList a list containing the CIB matrix, and a "shape" vector. The output of \code{\link{InputCibBanner}} is an appropriate input here.
#' @param TransRelAdj a list containing a blank transition matrix, a relative score matrix, and an adjacency matrix. The output of \code{\link{MakeScoreMatrix}} is appropriate here.
#' @note In the case of a tie, transition probability is split evenly between the top .
#' @note Other transition functions can be found via \code{\link{CIBTransitionCalculators}}
#' @return A matrix describing the transition probability from each world state (rows) to each other world state (columns).
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @examples
#' data(antCIB)
#' Transitions<-TransToMax(antCIB)
#' 
#' 
TransToMax<-function(TheList, TransRelAdj=NA){
  if (is.na(TransRelAdj)){
    TransRelAdj<- MakeScoreMatrix(antCIB)   
  }
  Transitions<-TransRelAdj[[1]]
  RelativeScores<-TransRelAdj[[2]]
  AdjacentMatrix<-TransRelAdj[[3]]  
  CrossImpactMatrix<-TheList[[1]]
  listName<-colnames(CrossImpactMatrix)
  CIBshape<- TheList[[2]]    
  ##For reasons I can not fathom, apply(A,1,function(x) x) appears to give A'. Hence I need to ass a transpose to keep everything nicesly in line. Okay.
  Transitions<-t(apply(RelativeScores, 1, function(x) x==max(x)))
  Transitions<-t(apply(Transitions, 1, function(x) x/sum(x)))
} 