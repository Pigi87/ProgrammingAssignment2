
## 1. Create the inverse of a matrix and cache it

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL ## initialize the value of the inverse matrix to NULL
	set <- function(y) { ## set the value of the matrix

		x <<- y
		inv <<- NULL

		}

	get <- function() x ## get the value of the matrix
	setinv <- function(inverse) inv <<- inverse ## set the value of the inverse matrix
	getinv <- function() inv ## get the value of the inverse matrix
	list (set = set, get = get, setinv = setinv, getinv = getinv)
	

}


## 2. Calculate the inverse of the matrix returned by function 1 above. If the same matrix is submitted several times, take its inverse from the cache

cacheSolve <- function(x, ...) {
        
	inv <- x$getinv()## retrieve the inverse matrix (if available) from the cache
	if(!is.null(inv)){## the inverse is available within the cache

		message('inverse already available from the cache')
		return(inv)


		}

	## no inverse available from the cache
	data <- x$get()
	inv <- solve(data,...)## calculate the inverse
	x$setinv(inv)
	inv


}
