install.packages("sentometrics")
install.packages("data.table")
install.packages("quanteda")
install.packages('Rcpp')
install.packages('GA')
install.packages('repmis')
install.packages('lubridate')
install.packages('text2vec')
install.packages('plot.matrix')
install.packages("doParallel")
install.packages('plyr')
install.packages("dplyr")
install.packages("gplots")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("guptools")
install.packages("fastDummies")

# to build the package of RCPP function 
system("R CMD INSTALL mypack")

print(mypack:::"_mypack_rcpp_hello_world")

# test to remove the package 
# remove.packages("mypack")