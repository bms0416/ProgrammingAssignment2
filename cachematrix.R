## makeCacheMatrix - constructor for the inversion cache class
## setInvertedHashValues - Puts a new key value pair into the cache
## setInputMatrix - Specifies the matrix that will be inverted next
## getInputMatrix - Retrieves the last input method set to setInputMatrix
## getFullKeysVector - Gets the entire vector of the input matrices
##    in the orer they were inserted
## getFullInvertedVector - Gets the entire vector of the inverted
##    matrices in order they were inserted
## cacheSolve - Gets the inversion of the matrix passed in
##    - from the cache if it exists or newly created and inserted for use next time
## get_whole_list_of_keys
## go() - Simple test driver

## 
## cache is a set of functions to implement
## a simple key - value pair data store
## 
## It uses the input matrix as the key and the
## inverted matrix as the value
##
makeCacheMatrix <- function(inputMatrix = NULL, ...) {
  ## The variables used to generate a key value pair
  ## Of the matrices and their inverses
  keys <- list(nrow=0)
  values <- list(nrow=0)
  currentInputMatrix <- inputMatrix
  
  ## Add a new key value pair to the cache
  ## The key is the matrix passed in
  ## The value is its inverse 
  setInvertedHashValues <- function(key, value, ...) {
    keys <<- append(keys, list(key))
    values <<- append(values, list(value))
  }
  
  setInputMatrix <- function(newMatrix, ...) {
    currentInputMatrix <<- newMatrix
  }
  
  getInputMatrix <- function() currentInputMatrix
  getFullKeysVector <- function() keys
  getFullInvertedVector <- function() values
  
  ## Return the inverse of the matrix passed in
  ## A newly created matrix if not cached yet
  ## The caced one if it exists
  cacheSolve <- function() {
    res <- NULL
    if (length(keys) > 0) {
      for (i in 1:length(keys)) {
        if (dim(currentInputMatrix) == dim(keys[[i]]) &&
            all.equal(currentInputMatrix, keys[[i]]) == TRUE) {
          res <- values[i]
          print("EXISTS")
        }
      }
    }
    if ( is.null(res)) {
      print("NEW ENTRY")
      inverted_matrix <- solve(currentInputMatrix)
      setInvertedHashValues(currentInputMatrix, inverted_matrix)
      res <- inverted_matrix
    }
    res
  }
  
  ## Print the set of unique matrices
  ## That have had their inverse cached
  get_whole_list_of_keys <- function() {
    print(keys)
  }
  
  ## add the private function to cache
  list (
    setInvertedHashValues = setInvertedHashValues,
    setInputMatrix = setInputMatrix,
    getInputMatrix = getInputMatrix,
    getFullKeysVector = getFullKeysVector,
    getFullInvertedVector = getFullInvertedVector,
    cacheSolve = cacheSolve,
    get_whole_list_of_keys = get_whole_list_of_keys
  )
}

##
## Test driver
## runs 3 different matrices
## 3 different times
## Shows they are only inserted one time each
## returning the cached entry for the 2nd and 3rd time
##
go <- function(...) {
  ss <- makeCacheMatrix()
  ar1 <- runif(16, 1, 16)
  ar2 <- runif(4, 1, 4)
  ar3 <- runif(16, 1, 16)
  a1 <- matrix(ar1, ncol=4)
  a2 <- matrix(ar2, ncol=2)
  a3 <- matrix(ar3, ncol=4)
  
  ss$setInputMatrix(a1)
  ss$cacheSolve()
  ss$setInputMatrix(a1)
  ss$cacheSolve()
  ss$setInputMatrix(a2)
  ss$cacheSolve()
  ss$setInputMatrix(a3)
  ss$cacheSolve()
  ss$setInputMatrix(a1)
  ss$cacheSolve()
  ss$setInputMatrix(a2)
  ss$cacheSolve()
  ss$setInputMatrix(a3)
  ss$cacheSolve()
  ss$setInputMatrix(a1)
  ss$cacheSolve()
  ss$setInputMatrix(a2)
  ss$cacheSolve()
  ss$setInputMatrix(a3)
  ss$cacheSolve
  ss$getFullKeysVector()
  ss$getFullInvertedVector()
  print(ss$getFullKeysVector())
  print(ss$getFullInvertedVector())
}