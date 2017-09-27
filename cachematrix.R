}## This function is intended to create an inverse matrix off of a given
matrix
## the makeCacheMatrix will take matrix X and invert, producing an inverted
matrix upon completion 


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
+ set <- function(y) {
+    x <<- y
+    inv <<- NULL
+    }
+ get <- function() x
+ setInverse <- function(inverse) inv <<- inverse
+ getInverse <- function() inv
+ list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
+ }

}

## This function is intended to return the inverse of the inverted matrix
(i.e., it inverts makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 + inv <- x$getInverse()
 + if (!is.null(inv)) {
 +      message("getting cached data")
 +      return(inv)
 +      }
 + mat <- x$get()
 + inv <- solve(mat, ...)
 +  x$setInverse(inv)
 +  inv
 + }
