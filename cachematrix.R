# Programming Assignment 2
# Ernesto Hernández-Novich <emhn@usb.ve>
# A cute exercise on simulating objects using closures,
# including memoizing techniques :)
# Wed, 23 Apr 2014 19:15:48 -0430

# NOTE: I've written the solution to specification by mimicking the
# example given for vectors. However, I'd rather have the "special
# matrix" compute and cache the inverse "on demand", so I've written
# an alternative solution below, hopefully interesting for the
# curious reader.

# makeCacheMatrix -- creates a list holding a value (the matrix),
# a memoized inverse for said value and several closures meant to
# get/set (as one would do in OOP languages)

makeCacheMatrix <- function(thematrix = matrix()) {
    # The cached inverse
    theinverse <- NULL

    # Setter -- replaces matrix with new one and clear memoized inverse
    setMatrix <- function(newmatrix) {
        thematrix  <<- newmatrix
        theinverse <<- NULL
    }

    # Getter -- returns the current matrix
    getMatrix <- function() thematrix

    # Cache setter -- sets the memoized inverse
    setInverse <- function(inverse) theinverse <<- inverse

    # Cache getter -- returns the currently cached inverse
    getInverse <- function() theinverse

    # Return the list holding matrix, cache and closed functions
    list(set = setMatrix,
         get = getMatrix,
         set.inverse = setInverse,
         get.inverse = getInverse)
}

# cacheSolve -- find a matrix inverse given that:
# (1) The matrix was created using makeCacheMatrix above.
# (2) If the matrix hasn't had its inverse computed before,
#     do so and cache it.
# (3) Return the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$get.inverse()
  if (!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
  }
  thematrix <- x$get()
  inverse <- solve(thematrix)
  x$set.inverse(inverse)
  inverse
}

# smarterMatrix -- a matrix that compute it's inverse "on demand"
# It fuses makeCacheMatrix and cacheSolve into a single function
# that creates a list containing the matrix, the cached inverse, a
# getter and setter for the matrix, and *only* a getter for the cache.
# When the cache getter is used, if the cache is NULL, the inverse is
# computed, cached for further uses and returned. If the matrix is
# changed using the setter, the cache is set back to NULL, so further
# uses of the cache getter will be forced to recompute the inverse.

# It's case uses are as follow:
#
# > m <- smarterMatrix()
# > m$set(matrix(seq(1:4),2)
# > m$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > m$get.inverse()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > m$get.inverse()
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > m$set(matrix(...some other matrix...))
# This NULLs the cache, so the next m$get.inverse() will
# force computing the inverse and caching again.


smarterMatrix <- function(thematrix = matrix()) {
    # The cached inverse for the Matrix.
    theinverse <- NULL

    # Setter -- replaces matrix with new one and clear memoized inverse
    setMatrix <- function(newmatrix) {
        thematrix  <<- newmatrix
        theinverse <<- NULL
    }

    # Getter -- returns the current matrix
    getMatrix <- function() thematrix

    # Cache getter -- returns the currently cached inverse, computing
    # it beforehand if needed. I left the "getting cache data" there
    # just for informational purposes, but I'd rather write the simpler
    # 
    #    if (!is.null(theinverse) { theinverse <<- solve(thematrix) }
    #    return(theinverse)
    #
    getInverse <- function() {
      if (!is.null(theinverse)) {
         message("getting cached data")
      } else {
         theinverse <<- solve(thematrix)
      }
      return(theinverse)
    }

    # Return list with matrix, cache and closed (as in closure) functions
    list(set = setMatrix,
         get = getMatrix,
         get.inverse = getInverse)
}
