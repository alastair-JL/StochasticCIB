#' MakeScoreMatrix
#'
#' Given a CIB matrix, calculate the consistancy scores of each world state, as viewed from each other world state. Also calculates adjacency of states, along with making a empty transition matrix to be filled in later.
#' @param TheList A list, the first element of which is a CIB matrix, and the second of which is a vector describing the "shape" of the CIB matrix (a vector listing how many states per descriptor). The output of  \code{\link{InputCibBanner}} is a suitable input for this function.
#' @keywords CIB
#' @export
#' @note The output of this function makes suitable input for various \code{\link{TransitionCalculators}}.
#' @return A list containing a blank transition matrix, a "Score" matrix, and an adjacency matrix. The score matrix gives the consistancy score of every world state from the point of view of every other (rows being the source state, columns the target state). The adjacency matrix identifies WHICH descriptor would need to be changed to move from one state to another, taking the value 0 if no descriptor is changed, or -1 if multiple descriptors must be changed.
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @examples
#' data(antCIB)
#' TransRelAdj<-MakeScoreMatrix(antCIB)
#' Scores<-TransRelAdj[[2]]
#' Adjacency<-TransRelAdj[[3]]
#' 

MakeScoreMatrix<-function(TheList){
  CrossImpactMatrix<-TheList[[1]]
  listName<-colnames(CrossImpactMatrix)
  CIBshape<- TheList[[2]]  
  NumDesc<-length(CIBshape)
  shapeOffest<-c(0,cumsum(CIBshape)[-NumDesc])  
  shapemultiplier<-c(1,cumprod(CIBshape)[-NumDesc])  
  CurrentState<-CIBshape
  targetState<-CIBshape
  numWorldState<-prod(CIBshape)
  Transitions<-mat.or.vec(numWorldState,numWorldState)  
  RelativeScores<-mat.or.vec(numWorldState,numWorldState)  
  AdjacentMatrix<-mat.or.vec(numWorldState,numWorldState)  
  WorldStateNames<- c("blank")
  Counter<-0
  TargCounter<-0
  while(Counter<numWorldState){
    Counter=Counter+1
    CurrentState[1]<- CurrentState[1]+1
    while( any(CurrentState>CIBshape) ){
      CurrentState<-CurrentState+c(0,(CurrentState>CIBshape)[-NumDesc]) -(CurrentState>CIBshape)*CIBshape      
    }    
    nextname=paste(listName[CurrentState+shapeOffest],sep=" ",collapse=" ")
    WorldStateNames<- c(WorldStateNames,nextname)
    ##Okay, so we've identified the current state, and now need to determine its transitions.
    targetState<-CIBshape
    TargCounter<-0
    while(TargCounter<numWorldState){
      TargCounter=TargCounter+1
      targetState[1]<- targetState[1]+1
      while( any(targetState>CIBshape) ){
        targetState<-targetState+c(0,(targetState>CIBshape)[-NumDesc]) -(targetState>CIBshape)*CIBshape      
      }    
      ##Okay, currently scanning through all possible target states. A particular target state has been choosen. Cool
      RelativeScores[Counter,TargCounter]<-(sum(CrossImpactMatrix[CurrentState+shapeOffest ,targetState+shapeOffest] ) )
      AdjacentMatrix[Counter,TargCounter]<-sum((sum(targetState!=CurrentState)==1)*(targetState!=CurrentState)*(1:NumDesc)) -(sum(targetState!=CurrentState)>1)
    }  
  }  
  WorldStateNames=WorldStateNames[-1]
  rownames(Transitions)<-WorldStateNames
  rownames(RelativeScores)<-WorldStateNames
  rownames(AdjacentMatrix)<-WorldStateNames
  colnames(Transitions)<-WorldStateNames
  colnames(RelativeScores)<-WorldStateNames
  colnames(AdjacentMatrix)<-WorldStateNames  
  ReturnList<- list(Transitions,RelativeScores,AdjacentMatrix)
}
