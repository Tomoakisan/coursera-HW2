## The functions below cache the inverse of a matrix.

##

## This function creates a matrix object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL

	set <- function(y)

		x <<- y

		m <<- NULL

	}

	get <- function() x
		
	setsolve <- function(solve) m <<- solve

	getsolve <- function() m

	list(set = set, get = get,

	     setsolve = setsolve,

	     getsolve = getsolve)

}


## This function computes the inverse of the matrix by makeCacheMatrix above. 

## If the inverse has been calculated (and the matrix has not changed), 

## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        m <- x$getsolve()

	if(!is.null(m)) {

		message("getting cached data")

		return(m)

	}

	data <- x$get()

	m <- solve(data, ...)

	x$setsolve(m)

	m

}