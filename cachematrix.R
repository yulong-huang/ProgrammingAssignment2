## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
#     This function creates a special "matrix" object that can cache its inverse.
#
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
		set <- function(y) {
		        x <<- y
				inv <<- NULL
		}
		get <- function() x
		setInv <- function(y) inv <<- y
		getInv <- function()  inv
		list( set = set, get = get, setInv = setInv, getInv = getInv )	
}

## Write a short comment describing this function
#
#    it first checks to see if the inverse matrix has already been calculated. 
#    If so, it gets the inverse matrix from the cache and skips the computation. 
#    Otherwise, it calculates the inverse matrix of the data and sets the value 
#    of the inverse matrix in the cache via the setInv function.
#
cacheSolve <- function(x, ...) {
        inv <- x$getInv()
		if (!is.null(inv)) {
		        message("getting cached data")
				return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setInv(inv)

        ## Return a matrix that is the inverse of 'x'
		inv
}
