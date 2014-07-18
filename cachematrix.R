## Put comments here that give an overall description of what your
## functions do

# A pair of functions that cache the inverse of a matrix.
#	Matrix inversion is a costly process otherwise.

## Write a short comment describing this function

# special "matrix" object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
	# set inverse to empty
	inv <- NULL
	
	# set the value of the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	# get the value of the matrix
	get <- function(y) x
	
	# set the value of the inverse
	setinv <- function(solve) inv <<- solve
	
	# get the value of the inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## Write a short comment describing this function

# If the inverse has been calculated
#				retrieves the inverse from cache
# else, computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		# get inverse, if it is anything other than null
		inv <- x$getinv()
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		
		# otherwise, compute inverse
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv
}
