% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DiceTransition.R
\name{DiceTransition}
\alias{DiceTransition}
\title{DiceTransition}
\usage{
DiceTransition(TheList, TransRelAdj = NA)
}
\arguments{
\item{TheList}{a list containing the CIB matrix, and a "shape" vector. The output of \code{\link{InputCibBanner}} is an appropriate input here.}

\item{TransRelAdj}{a list containing a blank transition matrix, a relative score matrix, and an adjacency matrix. The output of \code{\link{MakeScoreMatrix}} is appropriate here. If left blank, this is calculated on the fly.}
}
\value{
A matrix describing the transition probability from each world state (rows) to each other world state (columns).
}
\description{
Whenever one has to transition, pick a single descriptor at random, and then set that descriptor to whichever state maximises score.
}
\note{
In the event of a tie, probability is spread equally between tie-ing states. Thus, if there are three descriptors, one can end up with probability split (1/6,1/6), 1/3, 1/3.
}
\examples{
data(ExampleCIBdata)
DiceTransitions(ExampleCIBdata)
}
\author{
Alastair Jamieson Lane. <aja107@math.ubc.ca>
}
\keyword{CIB}
