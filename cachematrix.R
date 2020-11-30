## The following functions are designed with the purpose of taking a matrix as
## input, calculating its inverse and then caching the inverse matrix so we can
## use the inverse matrix for further calculations without calculating the inverse 
## all over again

## makeCacheMatrix makes an object which contains a matrix, its inverse and
## four functions: set, get, setinverse, getinverse. It is used to create a matrix,
## and store its inverse in memory for future use

makeCacheMatrix <- function(x = matrix()) {
    # initializes x_inv variable in which we'll store our inverse matrix
    x_inv <- NULL
    
    # set() function sets the matrix we wish to use to a new one without creating
    # a new object (overwrites the previous data stored in x in the parent environment)
    
    # x_inv value is NULL because if we set a new matrix, we need to wipe any
    # previous results from cache
    
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    
    # get() function is a getter which will be used in cacheSolve() later on
    # to call on our matrix
    
    get <- function() x
    
    # set_inverse() is used to store an inverse matrix to cache. We will use this
    # function in cacheSolve()
    
    set_inverse <- function(inverse) {
        x_inv <<- inverse
    }
    
    # get_inverse() function is a getter which will be used in cacheSolve() later on
    # to retrieve a stored inverse matrix from cache, if there is one
    
    get_inverse <- function() x_inv
    
    # naming each function to later access it by name in cacheSolve() using the
    # $ operator
    
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Checks if there is anything stored in x_inv. If not, calculates the inverse
## matrix. Uses functions from makeCacheMatrix() to set and get the values needed
## for calculations. Takes in an object of makeCacheMatrix type.

cacheSolve <- function(x, ...) {
        # Checks if there's anything cached:
    x_inv <- x$get_inverse()
        # If there is, retrieves the cached data:
    if(!is.null(x_inv)) {
        message('retrieving cached data...')
        return(x_inv)
    }
        # If not, calculates the inverse and sets it to x_inv, saving it in
        # memory
    data <- x$get()
    x_inv <- solve(data, ...)
    x$set_inverse(x_inv)
        # Prints the resulting inverse matrix in the terminal
    x_inv
}


## Matrix examples from Alan E. Berger's forum post:
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1 #Non-inverted

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
n2 #Non-inverted

my_matrix <- makeCacheMatrix(m1)

# First call on m1 returns inverted
cacheSolve(my_matrix)
# Second call on the same object retrieves from cache
cacheSolve(my_matrix)

my_matrix2 <- makeCacheMatrix(n2)

# First call on n2 returns inverted
cacheSolve(my_matrix2)
# Second call on the same object retrieves from cache
cacheSolve(my_matrix2)