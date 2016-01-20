## Funcion que guarda en cache la inversa de una funcion
## Y luego si la requiere la toma del cache sino la calcula

## Guarda en el cache la matriz

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    ## Se usa <<- para guardar el objeto en un ambiente diferente al actual
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inversa <<- inverse
  getinv <- function() inversa
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calcula la Inversa de una matriz
## Si existe en cache ya no la calcula


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversa <- x$getinv()
  ## Si existe la inversa en cache ya no la calculamos
  if(!is.null(inversa)) {
    message("getting cached data - la extraemos del cache")
    return(inversa)
  }
  ## Si no existe calculamos la inversa de 'x'
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinv(inversa)
  inversa
}