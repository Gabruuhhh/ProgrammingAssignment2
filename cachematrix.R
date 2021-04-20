## For this file, two functions were used. the first function used was the makeCacheMatrix which is the one that will generate and compute the matrix.
## The second function used for this file is the cacheSolve which will cache the computed matrix

## The two functions that I will be using can be related to the example shown in the instructions which is calculating for the mean of a vector

## For the first function we used the makeCacheMatrix.
## The makeCacheMatrix is the one in charge for creating a special matrix that has the capability to cache its inverse

makeCacheMatrix <- function( matx = matrix() ) {
        inverse <- NULL 
        Set <- function(y){
                matx <<- y
                inverse <<- NULL
        }
        get <- function() {matx}
        setinv <- function(inverse) {inverse <<- inverse}
        getinv <- function() {inverse}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

## The second function involves the cacheSolve function.
## The cacheSolve function to be used in this file is the one that computes the 
## inverse of the matrix provided above.
## By using the "solve" function, we can compute for the inverse of the square matrix.

cacheSolve <- function(matx, ...) {
        inverse <- matx$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- matx$get()
        inverse <- solve(data, ...)
        matx$setinverse(inverse)
        inverse 
}

     
        
