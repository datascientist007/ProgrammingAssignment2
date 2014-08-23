## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
	inv <- NULL
	get <- function() x
	set <- function(y) 
	{
	    x <<- y
		inv  <<- NULL
	}
	getInverse <- function() inv
	setInverse <- function(inverse) inv <<- inverse
	list(set = set,
	     get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()

    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
