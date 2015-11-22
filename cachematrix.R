## The following functions create a special "matrix" object that allow you to cache 
## the inverse of a matrix to avoid unnecessary recalculations, by looking for a 
## cached value instead of calculating the inverse when needed.

## Special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## Initialize the inverse (i) to NULL
	i <- NULL

	## Set function to set the matrix data
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	## Get function to get the matrix data
	get <- function() x

	## Set function to set/cache the inverse of the matrix
	setinverse <- function(inv) i <<- inv

	## Get function to get the cached inverse of the matrix
	getinverse <- function() i

	## Return the list of functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrieves the cached inverse if available, otherwise computes the inverse and caches the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## Get the cached inverse
	i <- x$getinverse()

	if(is.null(i)) {
		## The inverse must be computed

		## Get the data
		mat <- x$get()

		## Calculate the inverse
		i <- solve(mat, ...)

		## Cache the inverse
		x$setinverse(i)
	}
	
	## Return the inverse
	i
}
