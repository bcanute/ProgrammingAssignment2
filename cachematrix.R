## Put comments here that give an overall description of what your

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    Print("Caching matrix.")
  }
  get <- function() {
    print("Retrieving from cache.")
    x
  }
  setmatrix<- function(m) {
    m<<-matrix(x)
  }
  getmatrix <- function(){
    m
  } 
  list(set = set, 
       get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}




## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(y=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x
  m <- solve(y)
  x$setmatrix(m)
  m
}
