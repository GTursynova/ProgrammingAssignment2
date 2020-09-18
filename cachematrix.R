makeCacheMatrix <- function(a = matrix()) {
  mx <- NULL
  set <- function(b){
    a <<- b
    mx <<- NULL
  }
  get <- function() {a}
  setmtx <- function(mtx) {mx <<- mtx}
  getmtx <- function() {mx}
  list(set = set, get = get,
       setmtx = setmtx,
       getmtx = getmtx)
}

cacheSolve <- function(a, ...) {
  mx <- a$getmtx()
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  mat <- a$get()
  mx <- solve(mat, ...)
  a$setmtx(mx)
  mx
}
