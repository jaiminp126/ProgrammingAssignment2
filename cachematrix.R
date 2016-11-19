## A pair of functions that cache the inverse of a matrix

## The first function, makeCacheMatrix creates a special "vector" that has the ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x<<-y 
            m<<-NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <-function() m
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## This function actually computes the inverse of matrix created above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m <- x$getinverse()
      if (!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      matrix <- x$get()
      m <-solve(matrix,...)
      x$setinverse(m)
      m
}

