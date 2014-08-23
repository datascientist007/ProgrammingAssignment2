## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
	inv <- NULL # inv will store inverse matrix cache
	get <- function() x # getter for the matrix function
	set <- function(y) # setter for matrix function (clears data and cached inverse)
	{
	    x <<- y
		inv  <<- NULL
	}
	getInverse <- function() inv # getter and setter for the inverse function (saves to cache)
	setInverse <- function(inverse) inv <<- inverse
	list(set = set,
	     get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()

    if (!is.null(inv)) { # return the inverse from the cache if it has been cached already
        message("getting cached data")
        return(inv)
    }
    data <- x$get() # calculate the inverse, cache and then return it
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
