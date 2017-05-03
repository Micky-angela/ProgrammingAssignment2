## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inver <- NULL
	set <- function(z){
		x <<- z
		inver <<- NULL
	}
	get <- function() x
	setinvert <- function(solve) inver <<- solve
	getinvert <- function() inver
	list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}
cacheSolve <- function(x, ...) {
	inver <- x$getinvert()
	if(!is.null(inver)){
		message("getting cached inverse")
		return(inver)
	}
	data <- x$get()
	inver <- solve(data,...)
	x$setinvert(inver)
	inver
        ## Return a matrix that is the inverse of 'x'
}
