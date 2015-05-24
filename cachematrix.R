## The following functions calculate the inverse of a matrix
## if it hasn't been done yet. If the inverse has already been
## computed, the inverse is taken out of the cache.
## Here, cacheSolve checks via makeCacheMatrix if the matrix
## has already been calculated. If so cacheSolve takes the matrix 
## from the cache (in makeCacheMatrix), if not it computes the
## matrix and passes it to the cache.

## makeCacheMatrix takes a square matrix by e.g. 
## example <- makeCacheMatrix(matrix(1:4, 2, 2))
## It has four subfunctions: set, get, setinv, and getinv
## from which get, setinv, and getinv are called by cacheSolve
## After passing the matrix like described in the example above,
## a new matrix can be set by e.g.
## example$set(matrix(c(10, 99, 54, 3, 87, 32, 65, 34, 18), 3, 3))
## and be printed out by example$get()
## get is also used by cacheSolve to get the original matrix
## The functions setinv and getinv are also invoked by the
## function cacheSolve to pass the inverted matrix (calculated
## by cacheSolve) to the cache (setinv) and to get the inverted
## matrix from the cache (getinv) 
## But if the inverted matrix was already computed by cacheSolve
## it can be called directly with example$getinv() as well (because 
## now it is in the cache)
## Don't call setinv directly (e.g. example$setinv(a)), since it would
## set the passed matrix a as the inverse!
## (Note: If there is only one command in the function
## the curly braces are not required)

makeCacheMatrix <- function(x = matrix()) { # Takes a matrix x
    m <- NULL                               # Sets the inverted matrix m to NULL if a new original matrix is passed
    set <- function(y) {                    # set takes matrix y if it is passed via the set-function
        x <<- y                             # Sets x to y as a global varialbe (with operator <<-)
        m <<- NULL                          # and m to NULL again
    }
    get <- function() x                     # get passes the original matrix x
    setinv <- function(inv) m <<- inv       # setinv takes the inverted matrix (from cacheSolve that invokes this
                                            # function) and assigns it to m (globally)
    getinv <- function() m                  # getinv passes the inverted matrix m
    list(set = set, get = get,              # list returns the executed function as a list
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes the result of makeCacheMatrix e.g.
## cacheSolve(example)
## and invokes the function getinv of makeCacheMatrix
## that passes the inverted matrix from the cache
## (if there is one, otherwise it returns NULL)
## If the result m of this call is not NULL it prints out
## "Getting cached data" and m (the cached inverse matrix)
## If the result m is NULL it gets the original matrix via
## x$get of makeCacheMatrix and stores it in the variable data
## Via solve(data) it calculates the inverse and passes it
## to the cache via x$setinv(m) which calls the function
## setinv of makeCacheMatrix
## It then returns the calculated inverse matrix m
## (Note: With return(m) the execution of the function is stopped, 
## and the code afterwards is not executed. Therefore, no "else" is
## needed, since the code after the if-statement is only executed if
## "if" is FALSE)

cacheSolve <- function(x) {                # Takes the result of makeCacheMatrix
        m <- x$getinv()                    # Calls getinv of makeCacheMatrix and stores the result (the 
                                           # inverted matrix if there is one in the cache, otherwise NULL) in m
        if(!is.null(m)) {                  # Checks if m is not NULL (that is if something is in the cache)
            message("Getting cached data") # If so it prints out "Getting cached data"
            return(m)                      # and returns the inverted matrix m from the cache
        }
        data <- x$get()                    # If m was NULL the function gets the original matrix
        m <- solve(data)                   # and computes the inverse
        x$setinv(m)                        # It subsequently passes the calculated inverse m to the cache
                                           # via the function setinv of makeCacheMatrix
        m                                  # and returns the inverse matrix m
}