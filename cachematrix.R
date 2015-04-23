## makeCacheMatrix is a function that takes a matrix as an input
## and returns a list of functions: set, get, setinverse, getinverse
## set() changes the matrix stored in makeCacheMatrix. 
## get() returns the matrix stored in makeCacheMatrix
## setinverse() sets the value of the variable "inverse" in makeCacheMatrix
## getinverse() returns the value of the variable "inverse" stored in makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y 	## We use the <<- operator to substitute the matrix x with the input matrix y in the main function
		inverse <<- NULL	## The inverse of y is set to NULL because we have now set a new matrix
	}
	get <- function() x 
	setinverse <- function(solve) inverse <<- solve	
	getinverse <- function() inverse
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## cacheSolve is a function that checks if the inverse stored in makeCacheMatrix is NULL or not
## If inverse is not NULL, cacheSolve returns the value of inverse stored in makeCacheMatrix
## If inverse is NULL, cacheSolve computes the inverse of the matrix stored in makeCacheMatrix using the solve() function and stores it in the object generated assigned with makeCacheMatrix


cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()	
	if(!is.null(inverse)) {		## if inverse is not NULL, it will return the inverse stored in makeCacheMatrix
		message("getting cached data")	
		return(inverse)		
	}
	data <- x$get()		## If inverse is NULL, this returns the matrix that is stored in makeCacheMatrix
	inverse <- solve(data,...)	## This calculates the inverse of that matrix
	x$setinverse(inverse)
	inverse	
}
