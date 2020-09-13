

matrix = as.matrix(runif(300))
dim(matrix)=c(10,10,3)

standardizer <- function(x) {
  mews = apply(x,3,mean)
  sigmas = apply(x,3,sd)
  for (i in 1:dim(x)[3]) {
    x[,,i] = (x[,,i] - mews[i] )/sigmas[i]
  }
  return(x)
}


standardized = standardizer(matrix)
