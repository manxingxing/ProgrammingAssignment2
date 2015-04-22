# Well, the instructors make a template function for us. They
# expected us to copy the way how `makeVector` and `cachemean` works.

# But i don't like the code of `makeVector` and `cachemean`:
#    The `setmean` function should not be exported as a public member.
# So i wrote my own version. I think it's more OOP-style. You can find it at the end of this file.

#
# Factory function that wraps a square matrix and can cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  # store cached matrix inverse in a variable
  cached_inv_mat = NULL

  # getter function that returns the matrix itself
  get = function() x

  # setter function that allows replacing the original matrix
  set = function(new_mat) {
    x <<- new_mat
    # reset cached inverse of the matrix
    cached_inv_mat <<- NULL
  }

  # getter function that returns the cached inverse of matrix
  get_inv_mat = function() cached_inv_mat

  # setter function that allow to set cached inverse of matrix explicitly
  # DANGER: you must assure that the given `inv_mat` is EFFECTIVELY the inverse of the matrix
  set_inv_mat = function(inv_mat) {
    cached_inv_mat <<- inv_mat
  }

  list(
    get = get,
    set = set,
    get_inv_mat = get_inv_mat,
    set_inv_mat = set_inv_mat
  )
}

# Get the cached inverse matrix of `makeCacheMatrix` object.
# if the inverse matrixe has not been cached yet. calculate
# and store the result into the object.
cacheSolve <- function(x) {
  cached_inv_mat = x$get_inv_mat()

  if (!is.null(cached_inv_mat)) { # fetch the cached result if the cache exists
    message('Fetching cached result')
    return(cached_inv_mat)
  } else {
    # calculate inverse of the matrix and  store it into the `makeCacheMatrix` object
    inv_mat = solve(x$get())
    x$set_inv_mat(inv_mat)
    inv_mat
  }
}

## Example
##   hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
##   h8 <- hilbert(8)
##   my_mat = makeCacheMatrix(h8)
##   cacheSolve(my_mat)
##   cacheSolve(my_mat)

#########################################################################
##   2nd implementation
#########################################################################
# Factory function that returns an enhanced matrix
# which can cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  # store cached matrix inverse in a variable
  cached_inv_mat = NULL

  # getter function that returns the matrix itself
  get = function() x

  # setter function that allows replacing the original matrix
  set = function(new_mat) {
    x <<- new_mat
    # reset cached inverse of the matrix
    cached_inv_mat <<- NULL
  }

  # get the cached inverse matrix
  # if it hasn't been cached, calculate its inverse and cache the result
  get_inv_mat = function(){
    if (!is.null(cached_inv_mat)) {
      message('Fetching cached result')
    } else {
      set_inv_mat(solve(x))
    }
    cached_inv_mat
  }

  # setter function that allow to set cached inverse of matrix
  # this function should be kept private within this scope
  set_inv_mat = function(inv_mat) {
    cached_inv_mat <<- inv_mat
  }

  list(get = get, set = set, inverse = get_inv_mat)
}

## Example
##   hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
##   h8 <- hilbert(8)
##   my_mat = makeCacheMatrix(h8)
##   my_mat$inverse()
##   my_mat$inverse()
