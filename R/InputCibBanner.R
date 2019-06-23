#' InputCibBanner
#'
#' A Zero-input function that will prompt the user to list their descriptors, along with the states associated with each descriptors. Useful for getting started with CIB.
#' @keywords CIB
#' @export
#' @note \code{\link{MakeScoreMatrix}} is probably the function you will need directly after this.
#' @return A list containing first a CIB matrix, and second a vector allowing the computer to more easily interpret said matrix.
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @examples
#' CIBobject<- InputCibBanner()
#' TransRelAdj<-MakeScoreMatrix(CIBobject)
#' Transitions<-LocalBoltzmann(CIBobject,TransRelAdj)
#' CIBforecast(Transitions)
#' 
InputCibBanner <-function(){
  fin=-1
  panic=50;
  CIBshape<- 0
  DescriptorList<- c()
  DescriptorBanner<- c()  
  StateList<- c()    
  input <- readline("What is the name of the first Descriptor?  ")
  DescriptorList<-c(DescriptorList,input)    
  while(fin==-1  && panic>0){
    input<-""
    while(input=="" && panic>0){
      input <- readline( paste("What is the name of the first state of ",DescriptorList[length(DescriptorList)] ,"?  " ,sep="") )      
    }        
    while(input!="" && panic>0){
      StateList<- c(StateList,input)
      DescriptorBanner<- c(DescriptorBanner,DescriptorList[length(DescriptorList)])
      CIBshape[length(CIBshape)]<-CIBshape[length(CIBshape)]+1;
      input <- readline( paste("What is the name of the next state for ",DescriptorList[length(DescriptorList)] ,"? (leave blank when done) "  ,sep="" ) )      
      panic=panic-1
    }    
    input <- readline( paste("What is the next Descriptor you would like to add? (leave blank if no more descriptors)  ")  )
    if(input==""){
      fin=1
    }else{
      CIBshape=c(CIBshape,0)
      DescriptorList<-c(DescriptorList,input)
    }    
  }                    
  frameNames<- paste(DescriptorBanner,StateList,sep="-")
  CrossImpactMatrix<- mat.or.vec(length(StateList),length(StateList))
  dimnames(CrossImpactMatrix) = list( frameNames,frameNames)
  CrossImpactMatrix<-edit(CrossImpactMatrix)      
  CrossImpactMatrix<-CrossImpactMatrix[1:length(StateList),1:length(StateList)]
  ReturnList<- list(CrossImpactMatrix,CIBshape)
}