##Matrix inversion is a time consuming computation and by caching the inverse of a matrix rather than computing it repeatedly, we can save time.
##The makeCacheMatrix and cacheSolve functions cache the inverse of a matrix.

#The makeCacheMatrix is an object that creates a special “vector”, its function is to cache its inverse

makeCacheMatrix <- function(x = matrix()) {  		#makeCacheMatrix is a function of x = matrix, where the following happens in the control structure
        i = NULL  									#creating an empty object i to be filled with matrix inverse in the future
        set  <- function(y) {						#Object set is a function(y) where matrix, x , is set to a new matrix, y, and resets the inverse, i, to NULL
                x <<- y   							#calls Left Assignment on x with new matrix y(from the parent environment)
                i <<- NULL 							#resets the inverse, i, to NULL
        }
        get <- function()x							#Object get is the return function of the x matrix
        setinverse <- function(inverse) i <<-inverse 	#Object setinverse is the return of function inverse where i is now assigned inverse(from the parent environment)
	getinverse <- function() i 						#object getinverse is a function of i where inverse is called
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)	#returns makeCacheMatrix containing all the functions just defined
        }
        
#The cacheSolve is an object that calculates the inverse of CacheMatrix. It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x,...) {				#cacheSolve is a function of x = matrix, with the control structure... (returns a matrix that is the inverse of 'x')
        i <- x$getinverse()					#i is assigned to the getinverse() function from the x matrix
        if(!is.null(i)) {					#if i has no missing/undefined values then…
                message("getting cached data") 	#return the following message and…
                return(i)					#return the inverse previously stored “cache”
        }
        data <- x$get()						#data is assigned to the get() function from the x matrix
        i <- solve(data,...)				#i is now the inverse of data (calculates the inverse) “solve(x) returns its inverse”
        x$setinverse(i)						#setinverse function sets the value of inverse in the cache
        i									#return inverse
        }
  