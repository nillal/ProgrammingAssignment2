## [Put comments here that describe what your functions do]
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## x ist expected to be invertible.

makeCacheMatrix <- function(x = matrix()) 
  {
  invM<-NULL                        ## shall crate empty variable
  set <- function(y)                ## function to chache matrtix
          {
            x<<-y
            invM<<-NULL
          }
  get <- function() {x}
  setInv <- function(x)             ## function to chache inverse of matrix
            {
            invM<<-cacheSolve(x)
            }
  getInv <-function() {invM}
  list(set=set, get=get, setInv = setInv, getInv = getInv)
}
  cacheSolve <- function(x, ...) 
{
          ## Return a matrix that is the inverse of 'x'
          y <- solve(x)
          return(y)
}