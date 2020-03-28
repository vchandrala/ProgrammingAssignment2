## The functions below can be used to cache the inverse of a matrix. 
## The function below assumes that the input matrix is always invertible

## The makeCacheMatrix() function 
## 1. sets and gets the value of matrix
## 2. sets and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(inv){
    inverse <<- inv
  }
  getinverse <- function(){
    inverse
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )

}


## The cacheSolve() function returns the inverse of a matrix. It first checks if the inverse is cached.
## If so it the returns the cached inverse without calculating again.
##If the inverse is not cached then inverse is first computed and then retured

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Getting cached data")
      return(inv)
  }
  mat <- x$get()
  inverse <- solve(mat)
  x$setinverse(inverse)
  return(inverse)
}
