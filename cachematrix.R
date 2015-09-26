## Functions to handle matrix inversion with cache feature

## Function to implement the cache matrix feature
## It stores the input matrix and inverted matrix in cache
## Input matrix might be provided when makeCacheMatrix is called or using the set function
makeCacheMatrix <- function(x = matrix()) {
 cacheinv <- NULL
 # function to save input matrix in cache and reset inverse matrix
 set <- function(y) {  
   x <<- y
   cacheinv <<- NULL
 }
 if (!is.null(x)) set(x) # set input matrix in cache if already provided
 # get input matrix from cache
 get <- function() x
 setinv <- function(inv) {    
   cacheinv <<- inv   # save inverse matrix in cache
 }
 getinv <- function() cacheinv            # get inverse matrix from cache
 list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Function to invert a matrix with cache feature
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cache data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## cache inverse matrix tests
testCacheMatrixInverse <- function() {
  # m1Cache <- NULL
  m1 <- replicate(1e3, rnorm(1e3))
  cacheMatrixFactory <- makeCacheMatrix(m1)
  # test input matrix cache
  m1 <- NULL
  m1 <- cacheMatrixFactory$get()
  if (is.null(m1)) stop("Failed to cache matrix m1")
  # compute inverse matrix and record time (no cache)
  ptm <- proc.time()
  m1inv <- cacheSolve(cacheMatrixFactory)
  m1invtest <- cacheMatrixFactory$getinv()
  if (is.null(m1invtest)) stop("Failed to cache inverted matrix m1")
  # compute inverse matrix and record time (cache mode)
  dur1 <- proc.time() - ptm
  ptm <- proc.time()
  m1inv <- cacheSolve(cacheMatrixFactory)
  durcache1 <- proc.time() - ptm
  # compute inverse matrix and record time (no cache)
  ptm <- proc.time()
  cacheMatrixFactory$set(m1)
  m1inv <- cacheSolve(cacheMatrixFactory)
  dur2 <- proc.time() - ptm
  testCacheMatrixInverse = list( elapsedtime1=dur1, elapsedtimeWithCache=durcache1, elapsedtime2=dur2)
}