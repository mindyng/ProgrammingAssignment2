##I want to thank Sefa Kilic @ https://github.com/sefakilic
##for help in fine-tuning my functions.

##Overall, my functions cache the inverse of a matrix.

##This function creates a special matrix object that can cache its inverse.
##First, a matrix is set. 
##Second, the matrix is retrieved/get. 
##Third, the inverse matrix(im) is set.
##Fourth, the inverse matrix(im) is retrieved/get.

makeCacheMatrix <- function(x = matrix()) {
  
  im<- NULL
  set <- function (y) {
    x <<- y
    im<<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function computes the inverse of the matrix returned by 
##makeCacheMatrix. If the inverse has already been calculated and the matrix
##has not been changed, then the cacheSolve function retrieves the inverse from 
##the cache.

##First, is the inverse of the matrix already computed?
##Second, if the inverse of the matrix already exists, there is no computation. Straight output follows.
##Third, if the inverse of the matrix does not exist, then the inverse is computed and stored in cache.
##Fourth, the newly calculated inverse matrix is returned.

cacheSolve <- function(x, ...) {
  im <- x$getinverse() 
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data)
  x$setinverse(im)
  im
}
