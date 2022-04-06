
# create Rcpp package -----------------------------------------------------
# in parallel, the Rcpp function won't work unless it is compiled in a package 
# so, this script is to create a Rcpp package 

# !!! need to install rtools 
# https://cran.r-project.org/bin/windows/Rtools/history.html

# load library 
library(Rcpp)

# 1. create skeleton 
Rcpp.package.skeleton("mypack")

# 2. copy lines in "matrix_mul.cpp" to the file "/mypack/src/rcpp_hello_world.cpp"

# 3. ADD "#include <RcppEigen.h>" to the file "/mypack/src/rcpp_hello_world.cpp"

# 4. "LinkingTo: Rcpp, RcppEigen" in the file "/mypack/DESCRIPTION" 

# 5. compile attributes 
compileAttributes("mypack")

# 6. build and install custome package 
system("R CMD build mypack")
system("R CMD INSTALL mypack")



# test package ------------------------------------------------------------

library("mypack")
library("Matrix")


# define two matrices
m<- 763542
n<- 51
p<- 3

A<-  matrix(runif(m*n),m,n)
B<-  matrix(runif(n*p),n,p)
A[abs(A)> .01] = B[abs(B)> .01] = 0
A <- as(A,'dgCMatrix')
B<- as(B,'dgCMatrix')

mypack::eigenMapMatMult_SPARSE(A,B)