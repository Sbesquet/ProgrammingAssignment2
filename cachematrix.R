## creates a list
#set value of matrix, get value of matrix, set value of inverse matrix
#get value of inverse matrix

makeCacheMatrix <- function(x = numeric()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Using the list created above, computes inverse matrix and stores in above 
#list. When called again, recognizes inverse matrix as stored in list and
#does not again calculate the inverse matrix.

cacheSolve <- function(x, ...) {
  I <- x$getinv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinv(I)
  I
}
