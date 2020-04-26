## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      
	inversed = NULL
	
	# Set the matrix
	set = function(y) {                
			x <<- y
			inversed <<- NULL
	}
	
	# Get the matrix
	get = function() { x }
	
	# Set the inverse of the matrix
	setinverse = function(inverse) { inversed <<- inverse }
	
	# Get the inverse of the matrix
	getinverse = function() { inversed }
	
	# Return a list of the methods
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {

	# Return a matrix that is the inverse of 'x'        
	inversed = x$getinverse()
	
	# Inverse has already been calculated
	if (!is.null(inversed)){
		# Get inverse matrix from cache
		return(inversed)
	}
	
	# Calculate the inverse of input matrix
	matr = x$get()
	inversed = solve(matr, ...)
	
	# Set the value of the inverse matrix in the cache
	x$setinverse(inversed)        
	return(inversed)
}
