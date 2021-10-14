# #########################################################

# The inverse of matrix  is another matrix, which on multiplication
#  the given matrix gives the multiplicative identity.
# For a matrix A, its inverse is A-1, and A.A-1 = I.
# Not all matrices can have inverse couterparts. The following code
# works only for the invertible matrices only. 
# 
# After the matrix inverse if we multiply the initial and its inverse 
# matrices we have to get an identity matrix ( Where diagonal values are 1 
#  and all other elements are 0s)
# 


# Function to make CaheMatrix
# This has set, get, setInverse, getInverse methods as members

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) {j <<- inverse}
  getInverse <- function() {j} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to inverse(Using solve function) a function
# solve(A) returns inverse of A matrix. 
# if thre is not matrix assigned, it would give out the cached data mesage
# and returns the cached matrix. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j))
    {
      message("getting cached data")
      return(j)
    }
    mat <- x$get()
    j <- solve(mat,...) 
    x$setInverse(j)
    j

}

