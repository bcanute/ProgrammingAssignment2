## The purpose of these functions is to save time by storing the inverse of a
##matrix, so that it can be used repeatedly without the need to recalculate the
##inverse of the matrix.
##Instructions:
##Load and source the two functions.
##Call x<-makeCacheMatrix
##Call new<-cacheSolve(your_matrix)
##new will then contain the inverse of your matrix.
##You must set up a separate cache call variable for each new matrix as the
##the function does not compare matrices for you.

## Function - makeCacheMatrix(x = matrix()) 
##The argument is actually the inverse of the matrix.
##When this function is called (by the second function), having first checked to see if the 
##inverse of the matrix is already cached.
##If it is there, the function finds it and returns it.
##If not, the inverse is cached and returned to the calling program.
##This also flags the inverse function as cached and available for future calls. 

makeCacheMatrix <- function(x = matrix()){
  ## m is going to act as a flag, indicating if Imatrix exists.
  ##So set m to null, when we first create our list of functions.
    m <- NULL
  ##Now create a list of 4 functions, which will be called by the cachesolve function.
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
  ##Returns 4 functions as a list, callable as
  #x$get()
  #x$set().....
  list(set = set, 
       get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}




##Function - cacheSolve(yourmatrix=matrix(),....)
##This function takes your matrix and checks to see if the inverse is already cached.
##If it is, it calls on the first function to retrieve it from memory without recalculating.
##If the inverse has not yet been cached, this function calculates it and calls on the first function to cache it for future reuse.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(y=matrix(), ...) {
  ##check if m has anything in it.
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  ## if it is already cached, retrun it to the caller. 
  ##else calreate the inverse and cache it. 
  }else
  data <- x
  m <- solve(y)
  x$setmatrix(m)
  return(m)
}

##Instructions to test the program.
##However, you are warned against loading this code into your machine.
##No liability is granted for any damage to your software.
y<-matrix(1:4,2,2)
y

x<-makeCacheMatrix()
x

new<-cacheSolve(y)
new
