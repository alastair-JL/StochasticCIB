##Run the stuff up here once to make sure you have the packages needed.
install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)
##################



setwd("StochasticCIB") ##Change this line.
document() 
setwd("..")
install("StochasticCIB")

#########################
help(InputCibBanner)
