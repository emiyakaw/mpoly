pkgname <- "symmoments"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('symmoments')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("callmultmoments")
### * callmultmoments

flush(stderr()); flush(stdout())

### Name: callmultmoments
### Title: Compute multivariate moment symbolically
### Aliases: ' callmultmoments '

### ** Examples

# Compute the moment for the 4-dimensional moment c(1,2,3,4):

m.1234 <- callmultmoments(c(1,2,3,4))



cleanEx()
nameEx("convert.mpoly")
### * convert.mpoly

flush(stderr()); flush(stdout())

### Name: convert.mpoly
### Title: Convert between mpoly and list representations of multivariate
###   polynomials
### Aliases: ' convert.mpoly '

### ** Examples

# create an mpoly object here

# convert from mpoly to list representation
library(mpoly)
t0 <- mpoly(list(c(coef=3,x1=2),c(coef=2,x1=1,x2=3),
                   c(coef=-4,z=2),c(coef=1,x1=1,x2=2,z=1)))  
t1 <- convert.mpoly(t0)    
# convert from list representation back to an mpoly object
t2 <- convert.mpoly(t1) 




cleanEx()
nameEx("convert.multipol")
### * convert.multipol

flush(stderr()); flush(stdout())

### Name: convert.multipol
### Title: Convert between multipol and list representations of
###   multivariate polynomials
### Aliases: ' convert.multipol '

### ** Examples

# create an mpoly object to work with
library(mpoly)
t0 <- mpoly(list(c(coef=3,x1=2),c(coef=2,x1=1,x2=3),
                   c(coef=-4,z=2),c(coef=1,x1=1,x2=2,z=1))) 

# convert from mpoly to list representation
t1 <- convert.mpoly(t0)    
# convert from list representation to a multipol object
t2 <- convert.multipol(t1) 
# convert back to a list representation
t3 <- convert.multipol(t2) 



cleanEx()
nameEx("evaluate.moment")
### * evaluate.moment

flush(stderr()); flush(stdout())

### Name:  evaluate
### Title: Evaluate a multivariate moment
### Aliases: ' evaluate.moment' ' evaluate'

### ** Examples


evaluate(callmultmoments(c(1,2,3,4)),c(4,2,1,1,3,1,1,2,1,2))
# evaluates the moment at c(1,2,3,4) at the following covariance matrix
#    4 2 1 1
#    2 3 1 1
#    1 1 2 1
#    1 1 1 2




cleanEx()
nameEx("evaluate_expected.polynomial")
### * evaluate_expected.polynomial

flush(stderr()); flush(stdout())

### Name:  evaluate_expected.polynomial
### Title: Evaluate the expected value of a multivariate polynomial
### Aliases: ' evaluate_expected.polynomial'

### ** Examples


# define a mpoly object for a multivariate polynomial and determine
# its expected value at specified mean and covariance matrix:
# note that all moments up to c(2,3,2) must exist in the symmoments
# environment.  Use make.all.moments(c(2,3,2)) if necessary.
# use library(mpoly) for first statement below.

# t0 <- mpoly(list(c(coef=3,x1=2),c(coef=2,x1=1,x2=3),c(coef=-4,z=2),c(coef=1,x1=1,x2=2,z=1)))
# evaluate_expected.polynomial(t0,c(1,2,3),c(1,0,0,1,0,1))






cleanEx()
nameEx("evaluate_noncentral")
### * evaluate_noncentral

flush(stderr()); flush(stdout())

### Name:  evaluate_noncentral
### Title: Evaluate a noncentral multivariate moment
### Aliases: ' evaluate_noncentral'

### ** Examples


# evaluate_noncentral(c(3,1,2),c(3,4,1),c(4,2,1,3,1,2))
# evaluates the expected value of X1**3 X2 X3**2 at mean c(1,2,3) 
# and at the following covariance matrix
#    4 2 1 
#    2 3 1 
#    1 1 2 

#  requires all moments up to c(3,1,2) to exist in the symmoments environment.
#  use  make.all.moments(c(3,1,2)) if necessary.

# use moments in the global environment:
# evaluate_noncentral(c(3,1,2),c(3,4,1),c(4,2,1,3,1,2),'.GlobalEnv')




cleanEx()
nameEx("integrate.polynomial")
### * integrate.polynomial

flush(stderr()); flush(stdout())

### Name: integrate.polynomial
### Title: Numerically integrate a multivariate polynomial
### Aliases: ' integrate.polynomial'

### ** Examples

# define a mpoly object for a multivariate polynomial, and 
# determine its expected value at specified mean and covariance matrix:

# t0 <- mpoly(list(c(coef=3,x1=2),c(coef=2,x1=1,x2=3),c(coef=-4,z=2),c(coef=1,x1=1,x2=2,z=1)))


# integrate.polynomial(t0,c(1,2,3),matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,byrow=TRUE))





cleanEx()
nameEx("make.all.moments")
### * make.all.moments

flush(stderr()); flush(stdout())

### Name: make.all.moments
### Title: Create all moments up to specified size in environment
###   symmoments
### Aliases: ' make.all.moments '

### ** Examples

# Compute all moments up to c(3,3)
# First create the symmoments environment if it does not exist 
# symmoments <- new.env()
# make.all.moments(c(3,3))



cleanEx()
nameEx("print.moment")
### * print.moment

flush(stderr()); flush(stdout())

### Name: print.moment
### Title: Print the representation of a multivariate moment
### Aliases: 'print.moment '

### ** Examples
print(callmultmoments(c(1,2,3)))


cleanEx()
nameEx("simulate.moment")
### * simulate.moment

flush(stderr()); flush(stdout())

### Name: simulate.moment
### Title: Method to compute a multivariate moment using Monte Carlo
###   integration
### Aliases: ' simulate.moment '

### ** Examples

# Using 10000 samples, estimate the central moment for the moment c(2,4) at the covariance matrix
#  2 1
#  1 4

# and mean (0,0)
library(mvtnorm)
simulate(callmultmoments(c(2,4)),10000,NULL,c(0,0),c(2,1,1,4))



cleanEx()
nameEx("symmoments-package")
### * symmoments-package

flush(stderr()); flush(stdout())

### Name: symmomentsv2-package
### Title: Symbolically compute and numerically evaluate multivariate
###   central moments
### Aliases: ' symmomentsv2-package' ' symmomentsv2'

### ** Examples

# Compute the moment for the 4-dimensional moment c(1,2,3,4):
callmultmoments(c(1,2,3,4)) 

# Print the representation of the 4-dimensional moment c(1,2,3,4):
print(callmultmoments(c(1,2,3,4))) 

# Compute the LaTeX representation of the central moment c(1,2,3,4):
toLatex(callmultmoments(c(1,2,3,4)))

# Write the LaTeX representation to a file using the standard R function (not run):
# writeLines(callmultmoments(c(1,2,3,4))),con="yourfilename", sep = "\n")

# evaluate the moment c(1,2,3,4) at the following variance-covariance matrix
  #  4 2 1 1
  #  2 3 1 1
  #  1 1 2 1

evaluate(callmultmoments(c(1,2,3,4)),c(4,2,1,1,3,1,1,2,1,2))

# Using 10000 samples, estimate the central moment for c(2,4) at the covariance matrix (not run)
#  2 1
#  1 4

# and mean (0,0)
library(mvtnorm)
simulate(callmultmoments(c(2,4)),10000,NULL,c(0,0),c(2,1,1,4))

# Compute Latex representation of a non-central moment
# as.matrix(toLatex_noncentral(c(1,3))) 

# Create all 2-dimensional moment objects with exponents up to 3 
# First create the symmoments environment if it does not exist
# symmoments <- new.env()
# make.all.moments(c(3,3))

# Evaluate a non-central moment at a specified mean and covariance matrix
# Note that this invocation requires moments of order up to c(1,3)
# to exist in environment symmoments.
# evaluate_noncentral(c(1,3),c(1,2),c(1,0,1))

# Create an mpoly object 
library(mpoly)
t0 <- mpoly(list(c(coef=3,x1=2),c(coef=2,x1=1,x2=3),
                   c(coef=-4,z=2),c(coef=1,x1=1,x2=2,z=1)))  
                     
# Convert an mpolyobject to a moment object
t1 <<- convert.mpoly(t0)      

# Convert a moment object to a multipol object
t2 <<- convert.multipol(t1)

# Convert from multipol back to mpoly through moment

mpoly(convert.mpoly(convert.multipol(t2)))     

# Evaluate the expected value of a multivariate polynomial
# Required moments must exist in environment symmoments.

# evaluate_expected.polynomial(t0,c(1,2,3),c(1,0,0,1,0,1))

# Create a Newick representation of a tree
exam.Newick      <- "(((a,b),c),d);"

# Convert to phylo format
library(ape)
exam.phylo       <- read.tree(text=exam.Newick)

# Convert to matching format
exam.matching    <- as.matching(exam.phylo)

# Convert to L-matrix format
exam.L.matrix    <- toMoment(exam.matching)






cleanEx()
nameEx("toLatex.moment")
### * toLatex.moment

flush(stderr()); flush(stdout())

### Name: toLatex.moment
### Title: LaTeX a multivariate moment
### Aliases: 'toLatex.moment '

### ** Examples
toLatex(callmultmoments(c(1,2,3)))


cleanEx()
nameEx("toLatex_noncentral")
### * toLatex_noncentral

flush(stderr()); flush(stdout())

### Name: toLatex_noncentral
### Title: Compute a Latex expression for a noncentral moment
### Aliases: ' toLatex_noncentral '

### ** Examples

# Compute the Latex representation of the 2-dimensional moment c(1,3) (not run).
# This requires that all moments up to c(1,3) exist in the symmoments environment.
# toLatex_noncentral(c(1,3))



cleanEx()
nameEx("toNewick")
### * toNewick

flush(stderr()); flush(stdout())

### Name: toNewick
### Title: convert representation of phylogenetic tree as a moment L-matrix
###   to Newick form
### Aliases: toNewick

### ** Examples

# create a Newick object
exam.Newick      <- "(((a,b),c),d);"
# convert to a moment L-matrix
exam.moment <- toMoment(exam.Newick)
# convert back to Newick format
backto.Newick <- toNewick(exam.moment)



cleanEx()
nameEx("tomatching")
### * tomatching

flush(stderr()); flush(stdout())

### Name: toMatching
### Title: Convert representation of a phylogenetic tree as a moment
###   L-matrix to matching form
### Aliases: toMatching

### ** Examples

# create a Newick object
exam.Newick      <- "(((a,b),c),d);"
# convert to a moment L-matrix
exam.moment <- toMoment(exam.Newick)
# convert to matching format
exam.matching <- toMatching(exam.moment)



cleanEx()
nameEx("tomoment")
### * tomoment

flush(stderr()); flush(stdout())

### Name: toMoment
### Title: Converts a tree from Newick or matching to moment format
### Aliases: toMoment

### ** Examples

# create a Newick object
exam.Newick      <- "(((a,b),c),d);"
# convert to a moment L-matrix
exam.moment <- toMoment(exam.Newick)
# convert to matching object
exam.matching <- toMatching(exam.moment)
# convert back to moment object
backto.moment <- toMoment(exam.matching)



cleanEx()
nameEx("tounsorted")
### * tounsorted

flush(stderr()); flush(stdout())

### Name: tounsorted
### Title: Compute an unsorted central moment object from a sorted object
### Aliases: ' tounsorted '

### ** Examples

# obtain moment m312 from m123
tounsorted(c(3,1,2),callmultmoments(c(1,2,3)))




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
