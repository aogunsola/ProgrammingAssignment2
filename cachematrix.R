## Put comments here that give an overall description of what your
## functions do

##create special matrix
makeCacheMatrix <- function (x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #create list with instructions for getting/setting inverse
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## Return a matrix that is the inverse of 'x'
##Pass the output of makeCacheMatrix to get or calculate the value of the inverse
#Be sure to pass the output of makeCacheMatrix!
cacheSolve <- function (x, ...) {
  #check if inverse exists in cashe
  inv<- x$getinv ()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if not in cache,get data
  matrix <- x$get()
  #calculate inverse
  inv <- solve(matrix)
  #store inverse in cache
  x$setinv(inv)
  inv
}

