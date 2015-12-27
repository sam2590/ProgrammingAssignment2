## The 'makeCacheMatrix' function generates the "special" matrix that can 
## cache its inverse, similar to the "special" vector described in the 
## instructions. 
## The 'cacheSolve' function calculates inverse of the matrix outputed by 
## 'makeCacheMatrix'. However, if inverse was already calculated previously
## then it will retrieve it from the cache, assuming the matrix hasn't changed


## "special" matrix generating function that will cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	
	m <- NULL 	
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## inverse calculating function, that will first search for already
## calculated solution in the cache, and if unavailable, will calculate
## the inverse for the matrix generated above


cacheSolve <- function(x,...) {
      
	m <- x$getinverse() 
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}
