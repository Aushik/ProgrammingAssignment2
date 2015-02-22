## The combination of the functions in this script are used to calculate
## the inverse of a matrix. 
##This script assumes that the supplied matrix is always invertible

## The makeCacheMatrix creates a special matrix object that can store its inverse.
## This matrix is a list containing the follwing fourfunctions
## setmatrix : sets the value of the matrix in cache.
## getmatrix : get the  value of the matrix from cache.
## setinverse: sets the inverse of the matrix in cache.
## getinverse: gets the inverse of the matrix from cache.

makeCacheMatrix <- function(x = matrix()) {
        result<-matrix(,nrow=0,ncol=0)
        setmatrix<-function(y){
                x<<-y
                result<<-matrix(,nrow=0,ncol=0)
        }
        
        getmatrix<-function() x
        
        setinverse<-function(inverse) result<<-inverse
        
        getinverse<- function() result
        
        list(setmatrix = setmatrix, getmatrix= getmatrix,
             setinverse=setinverse, getinverse= getinverse)
}


## The cacheSolve function computes the inverse of the matrix after the  matrix is
## passed to the above makeCacheMatrix function.
## If first checks if the inverse has been calculated for the provided matrix.
## If already calculated the function fetches the inverse from cache
## If not the function caculates the inverse and stores it in cache 
## with setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        result<-x$getinverse()
        
        if(length(result)>0){
                message("getting cached data")
                return (result)
        }
        
        message("computing inverse from data")
        data<-x$getmatrix()
        result<-solve(data,...)
        x$setinverse(result)
        result
}