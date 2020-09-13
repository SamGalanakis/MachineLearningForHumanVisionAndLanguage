library(here)

logsumexp <- function (x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

softmax <- function (x) {
  exp(x - logsumexp(x))
}


relu = function(x) {
  return (max(0,x))
}

reluVectorize = Vectorize(relu)




features = as.matrix(rnorm(100))
dim(features) = c(1,dim(features)[1])





fully_connected_pass <- function(input,layer_size) {
 
  input_size = max(dim(input))
  dim(input) = c(1,input_size)
  
  weight<- 0.01*matrix(rnorm(input_size*layer_size, sd=0.5),
                       nrow=input_size,
                       ncol=layer_size)
  
  bias     <- matrix(0, nrow=1, ncol = input_size)

  
  
    return (sweep(input %*% weight ,2, bias, '+') )
  
 
  
  
  
  
}



output= fully_connected_pass(input=input,layer_size=10)
activated_output = softmax_vectorize(softmax())
