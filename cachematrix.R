## Put comments here that give an overall description of what your
# This function is to create a object of input matrix that can:
# store the matrix and its inverse
# retrive the data and its inverse

# Usage:
# Mobj <- makeCacheMatrix(M)
# Set and Get matrix data: Mobjc$set(), Mobjc$get()
# Set and Get inverse of matrix data: Mobjc$setInverse(), Mobjc$getInverse()

# Input:
# a matrix to create the object of 

# Output
# a list of futions shown above

makeCacheMatrix<-function(mMatrix=matrix()){
  mMatrixInverse<-NULL   #initialize the iverse of the matrix as NULL
  set<-function(mMatrix2){
    mMatrix<<-mMatrix2   # assign the existing matrix to the new matrix
    mMatrixInverse<-NULL #initialize the iverse of the matrix as NULL
  }
  get<-function() mMatrix #return the matrix
  setInverse<-function(mMatrix2Inverse) mMatrixInverse<<-mMatrix2Inverse #assign the inverse of matrix
  getInverse<-function() mMatrixInverse #return the inverse of matrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse) # return a list of functions
  
}


# This function is used to get the iverse of a matrix 
# if the inverse of the matrix is already calculated and stored, it will be retured otherwise, it will be calculated
# Input is the object of marix that created by makeCacheMatrix
# Output is the inverse of the marix

cacheSolve<-function(mMatrixObj,...){
  mInverse<-mMatrixObj$getInverse() #Get the inverse
  if(!is.null(mInverse)){
    message("Get the cached inverse matrix") #The inverse is alreay calcuated, and return inverse, 
  }
  else {
    message("Recalculate the inverse matrix")
    mMatrix<-mMatrixObj$get()     #Get the maxtix from the object of matrix
    mInverse<-solve(mMatrix,...)  #Get the inver of maxtix
    mMatrixObj$setInverse(mInverse)    
  }
  mInverse
}
