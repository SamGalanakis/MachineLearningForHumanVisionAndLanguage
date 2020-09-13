


matrix = as.matrix(runif(300,-1,1))
dim(matrix) = c(10,10,3)
print(matrix)



relu = function(x) {
  return (max(0,x))
}

reluVectorize = Vectorize(relu)

print(reluVectorize(matrix))
