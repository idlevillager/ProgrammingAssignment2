## makeCacheMatrix returns a list of utility functions to access and set a matrix and its inverse.
## cacheSolve uses makeCacheMatrix to return the inverse of a matrix.

## Stores a matrix and its inverse. 
## Provides getter and setter methods as elements of list to get/set matrix and its inverse.
makeCacheMatrix <- function( x = matrix(nrow=0, ncol=0) ) {
  invX <- matrix(nrow=0,ncol=0)
  set <- function( y ) {
    x <<- y
    invX <<- matrix(nrow=0, ncol=0)
  }
  get <- function() x
  setInverse <- function( inv ) invX <<- inv
  getInverse <- function() invX
  list( set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse )
}


## Utility function to get the inverse of a matrix.
## Internally uses makeCacheMatrix to retrive from cache if inverse already exists, else,
## calculates inverse and stores it in makeCacheMatrix.
cacheSolve <- function(x, ...) {
         inv <- x$getInverse()
         if ( length(inv) != 0 ){
           message("getting cached data")
           return( inv )
         }
         data <- x$get()
         inv <- solve( data, ... )
         x$setInverse( inv )
         inv
}

