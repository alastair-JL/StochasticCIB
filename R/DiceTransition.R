#' DiceTransition
#'
#' Whenever one has to transition, pick a single descriptor at random, and then set that descriptor to whichever state maximises score.
#' @keywords CIB
#' @export
#' @param TheList a list containing the CIB matrix, and a "shape" vector. The output of \code{\link{InputCibBanner}} is an appropriate input here.
#' @param TransRelAdj a list containing a blank transition matrix, a relative score matrix, and an adjacency matrix. The output of \code{\link{MakeScoreMatrix}} is appropriate here. If left blank, this is calculated on the fly.
#' @return A matrix describing the transition probability from each world state (rows) to each other world state (columns).
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @note In the event of a tie, probability is spread equally between tie-ing states. Thus, if there are three descriptors, one can end up with probability split (1/6,1/6), 1/3, 1/3.
#' @examples
#' data(antCIB)
#' DiceTransitions(antCIB)


DiceTransition<-function(TheList, TransRelAdj=NA){
  if (is.na(TransRelAdj)){
    TransRelAdj<- MakeScoreMatrix(antCIB)   
  }
  Transitions<-TransRelAdj[[1]]
  RelativeScores<-TransRelAdj[[2]]
  AdjacentMatrix<-TransRelAdj[[3]]    
  Transitions<- 0*Transitions
  numDescriptors=max(AdjacentMatrix)
  for(iii in 1:numDescriptors){
    GoodAdj<-t(apply(AdjacentMatrix, 1, function(x) ((x==iii)+(x==0)) ))
    RelativeScoresCorrect<-RelativeScores +99999999*GoodAdj
    partialTransitionMatrix<-t(apply(RelativeScoresCorrect, 1, function(x) x==max(x)))
    partialTransitionMatrix<-t(apply(partialTransitionMatrix, 1, function(x) x/sum(x)))    
    Transitions<-Transitions+partialTransitionMatrix/numDescriptors
  }  
  Transitions
} 
