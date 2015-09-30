#' TransToFastestActing
#'
#' This transition function assumes that descriptors are written in order of "speed of update" (slowest to fastest). This function looks to see if there is any way to improve score by changing the fastest updating descriptor. If not, then it checks the 2nd fastest, 3rd fastest, and so on. 
#' @keywords CIB
#' @export
#' @param TheList a list containing the CIB matrix, and a "shape" vector. The output of \code{\link{InputCibBanner}} is an appropriate input here.
#' @param TransRelAdj a list containing a blank transition matrix, a relative score matrix, and an adjacency matrix. The output of \code{\link{MakeScoreMatrix}} is appropriate here. If left blank, can be calculated on the fly.
#' @note In the case of a tie, transition probability is split evenly between the top .
#' @return A matrix describing the transition probability from each world state (rows) to each other world state (columns).
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @examples
#' data(antCIB)
#' TransToFastestActing(antCIB)
#' 

TransToFastestActing<-function(TheList, TransRelAdj=NA){
  if (is.na(TransRelAdj)){
    TransRelAdj<- MakeScoreMatrix(antCIB)   
  }  
  Transitions<-TransRelAdj[[1]]
  RelativeScores<-TransRelAdj[[2]]
  AdjacentMatrix<-TransRelAdj[[3]]    
  RelativeScoresCorrect<-RelativeScores -99999999*(AdjacentMatrix< -0.5)
  GoodJumps <-   (AdjacentMatrix > -0.5)* (RelativeScores>=diag(RelativeScores))
  GoodAdj<- (1+AdjacentMatrix)*GoodJumps -1
  GoodAdj<-t(apply(GoodAdj, 1, function(x) x==max(x)))
  RelativeScoresCorrect<-RelativeScores +99999999*GoodAdj
  Transitions<-t(apply(RelativeScoresCorrect, 1, function(x) x==max(x)))
  Transitions<-t(apply(Transitions, 1, function(x) x/sum(x)))
} 