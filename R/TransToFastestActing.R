#' TransToFastestActing
#'
#' This transition function assumes that descriptors are written in order of "speed of update" (slowest to fastest). This function looks to see if there is any way to improve score by changing the fastest updating descriptor. If not, then it checks the 2nd fastest, 3rd fastest, and so on. 
#' For example, if your models descriptors are "Tree abundance","Wolf abundance" and "Rabbit abundance", the model will first try to update rabbit abundance. If the current rabbit abundance is stable, then the transition rule will update wolf abundance. If both are stable, then the transition rule will try to update Tree Abundance.
#' @keywords CIB
#' @export
#' @param TheList a list containing the CIB matrix, and a "shape" vector. The output of \code{\link{InputCibBanner}} is an appropriate input here.
#' @param TransRelAdj a list containing a blank transition matrix, a relative score matrix, and an adjacency matrix. The output of \code{\link{MakeScoreMatrix}} is appropriate here. If left blank, can be calculated on the fly.
#' @param Deterministic If true, bias our impact matrix ever so slightly in favour of scenarios lower in our list, so as to break ties. If false (default) in the event of a tie, transitions are randomized.
#' @note In the case of a tie, transition probability is split evenly between the top candidates.
#' @note Other transition functions can be found via \code{\link{TransitionCalculators}}
#' @return A matrix describing the transition probability from each world state (rows) to each other world state (columns).
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @examples
#' data(ExampleCIBdata)
#' TransToFastestActing(ExampleCIBdata)
#' 

TransToFastestActing<-function(TheList, TransRelAdj=NA,Deterministic=F){
  if (is.na(TransRelAdj)){
    TransRelAdj<- MakeScoreMatrix(TheList)   
  }  
  Transitions<-TransRelAdj[[1]]
  RelativeScores<-TransRelAdj[[2]]
  AdjacentMatrix<-TransRelAdj[[3]]    
  RelativeScoresCorrect<-RelativeScores -99999999*(AdjacentMatrix< -0.5)
  GoodJumps <-   (AdjacentMatrix > -0.5)* (RelativeScores>=diag(RelativeScores))
  GoodAdj<- (1+AdjacentMatrix)*GoodJumps -1
  GoodAdj<-t(apply(GoodAdj, 1, function(x) x==max(x)))
  RelativeScoresCorrect<-RelativeScores -99999999*(!GoodAdj)
  if(Deterministic){
    epsilon=10^-5;
    RelativeScoresCorrect=t(t(RelativeScoresCorrect)+epsilon*1:ncol(RelativeScoresCorrect))
  }
    Transitions<-t(apply(RelativeScoresCorrect, 1, function(x) x==max(x)))
    Transitions<-t(apply(Transitions, 1, function(x) x/sum(x)))    
  
} 