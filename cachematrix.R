## Matrix inversion can be a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## function makeCacheMatrix will create the special matrix
## function cacheSolve will solve for the inverse. It will cache the result and as long 
## 		as the matrix stays the it will return the inverse from cache
##
## I added a test function you can use to verify that makeCacheMatrix and cacheSolve work
## correctly. Just type: 
## > testme()

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	
	cachedInverse <- NULL

	set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
      }
      get <- function() x

      setCachedInverse <- function(inverse) cachedInverse <<- inverse
      getCachedInverse <- function() cachedInverse

      list(set = set, get = get,
             setCachedInverse = setCachedInverse,
             getCachedInverse = getCachedInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x) {

	  inverse <- x$getCachedInverse()

        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        matrix <- x$get()
        inverse <- solve(matrix)
	  
	  ## message("setting cached data")
        x$setCachedInverse(inverse)
        
	  inverse
		
}

## test function to verify correctnes of cacheSolve
testme <- function (){
	
	matrixA <- rbind(c(1, -1/4), c(-1/4, 1))
	specialmatrixA <- makeCacheMatrix(matrixA)

	message("result of solve(matrixA)")
	print(solve(matrixA))
	
	message("result of makeCacheMatrix(matrixA)")
      message("1st run")
	print(cacheSolve(specialmatrixA))
	message("2nd run")
	print(cacheSolve(specialmatrixA))
      message("3d run")
	print(cacheSolve(specialmatrixA))
}

