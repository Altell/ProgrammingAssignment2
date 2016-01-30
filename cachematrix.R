## makeCacheMatrix    ... creates an "object" to store a matrix and
##                        cache the inverse
##
## cacheSolve         ... a function to find the inverse of the "matrix"
##                        returned by 'makeCacheMatrix', using a cached
##                        value of the inverse if possible


## This function creates an 'object' to store a matrix and a cache of
## its inverse, along with helper functions to access and set this data
makeCacheMatrix <- function(matrix = matrix()) {
	## if 'cachedInverse' is NULL then the inverse isn't cached
	cachedInverse <- NULL
	
	set <- function(m){
		## set the value of 'matrix' and 'cacheInverse' in the parent
		## environment
		matrix <<- m
		
		## 'matrix' has been assigned to, therefore the cache may be
		## invalid; we set it to NULL
		cachedInverse <<- NULL
	}
	
	## returns the value of 'matrix'
	get <- function() matrix
	
	## sets the value of 'cachedInverse'. The <<- means we bind to
	## 'cachedInverse' in the function's parent environment, rather
	## than to a local copy of 'cachedInverse'
	setCachedInverse <- function(inverse) cachedInverse <<- inverse
	
	## returns the value of 'cachedInverse'
	getCachedInverse <- function(inverse) cachedInverse
	
	## we return a list which allows the caller to access the above
	## functions
	list(	set=set,
			get=get,
			setCachedInverse=setCachedInverse,
			getCachedInverse=getCachedInverse)	
}


## Returns the inverse of an "object" returned by makeCacheMatrix
## using a cached value if possible.
cacheSolve <- function(m){
	result <- m$getCachedInverse()
	if(!is.null(result)){
		## if result is not null then the cache is valid, and stored in
		## 'result' so we we can return 'result'
		message("getting inverse from cache")
		result
	} else {
		## if result is null then the cache is invalid.
		
		## store the inverse of the matrix in 'solve'
		inverse <- solve(m$get())
		
		## cache the inverse for future use
		m$setCachedInverse(inverse)
		
		## return the inverse
		inverse
	}
}

