#' TransToMaxAdj
#'
#' A transition matrix function, whereby given the current worldstate, we switch to the world state with the highest score, changing at most one descriptor in the process.
#' @keywords CIB
#' @export
#' @param TheList a list containing the CIB matrix, and a "shape" vector. The output of \code{\link{InputCibBanner}} is an appropriate input here.
#' @param TransRelAdj a list containing a blank transition matrix, a relative score matrix, and an adjacency matrix. The output of \code{\link{MakeScoreMatrix}} is appropriate here.
#' @note In the case of a tie, transition probability is split evenly between the top candidates.
#' @note Other transition functions can be found via \code{\link{TransitionCalculators}}
#' @return A matrix describing the transition probability from each world state (rows) to each other world state (columns).
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @examples
#' data(antCIB)
#' Transitions<-TransToMaxAdj(antCIB)
#' 

TransToMaxAdj<-function(TheList, TransRelAdj=NA){
  if (is.na(TransRelAdj)){
    TransRelAdj<- MakeScoreMatrix(antCIB)   
  }
  Transitions<-TransRelAdj[[1]]
  RelativeScores<-TransRelAdj[[2]]
  AdjacentMatrix<-TransRelAdj[[3]]  
  CrossImpactMatrix<-TheList[[1]]
  listName<-colnames(CrossImpactMatrix)
  CIBshape<- TheList[[2]]    
  RelativeScoresCorrect<-RelativeScores -99999999*(AdjacentMatrix< -0.5)
  Transitions<-t(apply(RelativeScoresCorrect, 1, function(x) x==max(x)))
  Transitions<-t(apply(Transitions, 1, function(x) x/sum(x)))
} 