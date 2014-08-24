##  These functions speed up the process of solving for inverse of various
## matrices, by caching the results of each solution in the parent environment.
## If called [1] to calculate the inverse of a previously solved matrix,
## the function will retrieve & return the previously calculated solution rather
## than recomputing.##  [1] cached solutions do not persist beyond R session).

# makeCacheMatrix initializes data holder, creates functions for later use.
# and lists those so they are accessible outside this function
# makes use of superassignment to enable values to be used outside this fn.
makeCacheMatrix <- function(x = matrix()) {
    # input x will be a MATRIX
    m <- list(NULL) #stores 'INV'; resets m-local to NULL on makeCacheMatrix run
    set <- function(y) {    # takes an input MATRIX
        x <<- y             # stores SET's input vector as x-outer
        m <<- list(NULL)    # resets m-outer <INV> to NULL when 'set' is called
    }
    #  next 3 functions ie object 'methods' don't run in this fn, just in
    # cachemean() to get values for x or for m (mean) and for setting the mean.
    get <- function() { x }   # this function should return x-outer (matrix)

    setINV <- function(inverted)  # called within cachemean()'s 1st iteration...
    { m <<- inverted }    # ...store <calculated INV> as m-outer using superasgt

    getINV <- function() { m } #m-outer<fetches cached INV value> if exists

    list(get = get,      # This list is returned with the newly created object.
         setINV = setINV, # Contains the "methods" as part of the object to
         getINV = getINV) # enable use outside this function.
}

## function 'cacheSolve' accepts matrix definition &  test matrix & list of functions
# created by makeCacheMatrix, tests for existence of solved matrix (inverse),
# then returns matrix that's inverse of XX (cached or newly-calculated)

cacheSolve <- function(x, ...) { # test matrix & list methods < makeCacheMatrix
    ## Return a matrix that is the inverse of 'x'
    m <- x[getINV()]   # get 'x' & place val in m-local (local = inner)
    #    m <- x  # sets m-local to value of x (input for this fn)
    if(!is.null(m)) {    # if INV was already cached (not NULL) then ...
        message("getting cached data-INV")  # ...send message to the console
        return(m)                       # and return INV; end func

    }  #   [ends here if using cached info]
    data <- x[get()]    # only runs if  INV was returned NULL
    m <- solve(data, ...)   # if m was NULL then solve the matrix, store in m-inner
    x[setINV(m)] # m to store the calculated INV in x (see setINV() in makeCacheMatrix)
    m                      # return the INV to the code that called this function
}
