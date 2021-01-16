
# The purpose of these functions is to first create a matrix
# that should be square and invertible with the function makeCacheMatrix
# and then, using the cacheSolve function, calculate the inverse
# of that matrix only if it does not already exist in the cache.


##Function to Create a Matrix ######################

makeCacheMatrix <-function(x=matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m<<-NULL 
  }
  get <-function() x
  setinverse <- function(solve) m <<-solve
  getinverse <- function()m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##Function to Calculate the Inverse of the matrix ################

cacheSolve <-function(x,...){
  m<- x$getinverse()
  if (!is.null(m)){
    message ("It's cached,no need to compute again.")
    return(m)
  }
  data <- x$get()
  m <-solve(data,...)
  x$setinverse(m)
  m
}


