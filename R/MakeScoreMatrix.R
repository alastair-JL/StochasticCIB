#' MakeScoreMatrix
#'
#' Given a CIB matrix, MakeScoreMatrix calculates the consistancy scores of every world state, as viewed from every other world state. It also calculates an adjacency of states which states which states are only differ in one descriptor, along with making an empty transition matrix to be filled in later using one of the \code{\link{TransitionCalculators}}. If you are interested in the consistancy scores from the point of view of a particular world state, simply examine the corresponding line of you resulting score matrix.
#' @param TheList A list, the first element of which is a CIB matrix, and the second of which is a vector describing the "shape" of the CIB matrix (a vector listing how many states per descriptor). The output of  \code{\link{InputCibBanner}} is a suitable input for this function.
#' @keywords CIB
#' @export
#' @note The output of this function makes suitable input for various \code{\link{TransitionCalculators}}.
#' @return A list containing a "Score" matrix found by summing the appropriate entries in your influence matrix, a blank transition matrix,  and an adjacency matrix. The Consistance score matrix will be of direct interest to you who is using this, the later two matrices are mainly used to help R's internal calculations, and are probably of limited interest to anyone who is using this package. The score matrix gives the consistancy score of every world state from the point of view of every other (rows being the source state, columns the target state). The adjacency matrix identifies WHICH descriptor would need to be changed to move from one state to another, taking the value 0 if no descriptor is changed, or -1 if multiple descriptors must be changed. Positive values in the adjacency matrix occur when precisely one descriptor has been changed. The value of such positive entries specifies \emph{which} descriptor is changed. Hence  "A,$,X"-->"A,$,Z" will have a "3" in the adjacency matrix, while "A,$,X"-->"A,%,X"  has "2" and "A,$,X"-->"B,?,Z"  has "-1".
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @examples
#' data(antCIB)
#' TransScoresAdj<-MakeScoreMatrix(antCIB)
#' Scores<-TransScoresAdj[[2]]
#' Adjacency<-TransScoresAdj[[3]]
#' ##Output the consistance scores assuming a particular state.
#' Scores["Pop-M $$$-H Edu-M",] 
#' Scores[3,] ##this is also valid.

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
