
#' CIBpaintGraph
#'
#' Take your CIB transition matrix for a particular transition rule, and produce the corresponding graph.
#' @keywords CIB
#' @export
#' @param Transitions A stochastic transition matrix (rows are presumed to add to 1). Can be generated by any of the function referenced in \code{\link{CIBTransitionCalculators}}
#' @note \code{\link{MakeScoreMatrix}} is probably the function you will need directly after this.
#' @return A list containing a long term forcast vector (a vector of probabilities of world states), a number representing the entropy of this vector, and a number representing the entropy production.
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @examples
#' data(ExampleCIBdata)
#' boltzTrans<-LocalBoltzmann(ExampleCIBdata)
#' forecast<- CIBforecast(boltzTrans)
#' CIBpaintGraph(boltzTrans)
#' CIBpaintGraph(boltzTrans,forecast[[1]])
#' 


CIBforecast<-function(Transitions){
  Eigs<-Eigs/colSums(Eigs) #normalise to make a probability
  Eigs<-abs(Eigs)
  rownames(Eigs)<-rownames(Transitions)
  colnames(Eigs)<-c('')
  Entropy<- -colSums(Eigs*log(Eigs + 10^-100))
  Entropy<-Entropy[[1]]
  EntropyProduction<- -colSums(Eigs*rowSums(Transitions*log(Transitions+10^-100)))
  EntropyProduction<-EntropyProduction[[1]]
  ReturnList<- list(Eigs,Entropy,EntropyProduction)
}
