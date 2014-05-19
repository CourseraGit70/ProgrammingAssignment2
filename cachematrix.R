# makeCacheMatrix creates a matrix that can cache it's computed inverse.
# cacheSolve actually does the computation and stores it in the cache 

# creates the matrix, creates an empty cache, provides setters and getters for working with the values

makeCacheMatrix <- function(matrixVal = matrix()) { #initialize a blank matrix if one isn't provided
  cache <- NULL                                     #initialize blank cache
  set <- function(y) {                              #create a setter method called set
    matrixVal <<- y                                 #put the value into the matrix variable
    cache <<- NULL                                  #empty the cache since the matrix just changed
  }
  get <- function() matrixVal                       #create a getter method called get that retuns the matrix value in x
  setcache <- function(cachein) cache <<- cachein   #create a setter method for setting the cache in the parent scope
  getcache <- function() cache                      #returns the value in the cache
  list(set = set, get = get,                        #defines a list of names for easy access
       setcache = setcache,
       getcache = getcache)
}


## loads the cache, returns it if it's unchanged and not empty, otherwise calculates the inverse

cacheSolve <- function(matrixVal, ...) {   #loads the matrix and anything else neccessary
  cache <- matrixVal$getcache()            #loads the cached matrix into the local cache
  if(!is.null(cache)) {                    #checks if something got loaded
    message("getting cached data")         #informs the user
    return(cache)                          #returns the cache value
  }
  data <- matrixVal$get()                  #gets the matrix for computation
  cache <- solve(data)                     #calculates the inverse and puts it in the local cache
  matrixVal$setcache(cache)                #loads the local cache value into the parent cache
  cache                                    #returns the cached value
}
