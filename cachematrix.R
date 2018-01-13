################################################################################
##                                                                            ##
##             F U N C T I O N   m a k e C a c h e M a t r i x                ##
##                                                                            ##
################################################################################
#  This function creates a special "matrix" object that can cache its inverse  #
#  If x is not passed, then a default empty matrix is passes to this function  #
################################################################################
makeCacheMatrix <- function(mdata = matrix()) {
      
      inv_result <- NULL
      
      ## 1) Set the matrix data (and initialize the inverse var) in the cache
      set <- function(y) {
            # x <<- val_in
            # Store the input matrix in glob. var g_matrix
            # initialize global cache var g_inv_result
            mdata      <<- y 
            inv_result <<- NULL   
      }
      
      ## 2) Get the value of the matrix from the cache
      get <- function() mdata
      
      
      ## 3) Store the inverse result of a matrix in the cache
      setinverse <- function(inverse) inv_result <<- inverse
      
      ## 4) Get the value of the matrix inverse from the cache
      getinverse <- function() inv_result
      
      ## Return a list containing the above 4 functions 
      list(set        = set,        
           get        = get, 
           setinverse = setinverse, 
           getinverse = getinverse)
      
}

################################################################################
##                                                                            ##
##             F U N C T I O N   c a c h e S o l v e                          ##
##                                                                            ##
################################################################################
# Computes the inverse of the special "matrix" returned by makeCacheMatrix     #
# If the inverse has already been calculated (and the matrix has not changed), #
# then the cachesolve should retrieve the inverse from the cache.              #
################################################################################
cacheSolve <- function(x, ...) {
      
      ## A) Check if inverse matrix is stored in global cache and return that
      inv_result <- x$getinverse()
      
      ## inverse present in cache: pass the value from the cache to inv_result
      if(!is.null(inv_result)) {
            message("getting cached inverse data.")
            return(inv_result)           
      }
      
      ## inverse not present in cache: calculate it now and store in cache
      
      mdata       <- x$get()       # get matrix from cache
      inv_result  <- solve(mdata)  # inverse matrix
      x$setinverse(inv_result)     # store inverse result in cache
      
      return(inv_result)
      
}
