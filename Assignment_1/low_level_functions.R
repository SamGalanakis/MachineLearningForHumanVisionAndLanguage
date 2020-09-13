library(imager)
library(here)


img = load.image(here("Assignment_1", "cat.jpg"))
plot(img)
print(dim(img))
grayscale_img = grayscale(img)





input = as.array(img)
dim(input) = dim(input)[c(1,2,4)]






conv <- function (img, conv_filter,output_width,output_height,stride) {
  filter_size = dim(conv_filter)[1]
  feature_map = rep(0,output_width* output_height)
  dim(feature_map) = c(output_width,output_height)
  
  filter_spacing = (filter_size-1)/2
  
  row_indices = seq(filter_spacing+1,dim(img)[1]-filter_spacing-1,stride)
  column_indices = seq(filter_spacing+1,dim(img)[2]-filter_spacing-1,stride)
  
  print("feature")
  for (row_index in row_indices) {
    for (column_index in column_indices) {
      print(row_index)
      print((row_index - filter_spacing):(row_index+filter_spacing))
      print((column_index-filter_spacing):(column_index+filter_spacing))
      print(dim(img))
      cell = img[(row_index - filter_spacing):(row_index+filter_spacing),(column_index-filter_spacing):(column_index+filter_spacing),]
      print(cell)
    }
  }
  
}












convolver = function(img,filters,bias,stride=1,padding=0) {
  print("convolving")
  
  dim(img) = dim(img)[c(1,2,4)]
  print("reshaped")
  n_filters = dim(filters)[1]
  filter_width = dim(filters)[2]
  filter_height = dim(filters)[3]
  img_shape = dim(img)
  stopifnot(img_shape[3]==dim(filters)[4]) # Make sure channels match
  stopifnot(filter_width == filter_height) #Make sure filter square
  stopifnot(filter_width %% 2 != 0) #Make sure it is odd
  
  output_width = (img_shape[1] - filter_width + 2*padding)/stride +1
  output_height = (img_shape[2] - filter_width + 2*padding)/stride +1
  
  feature_maps = rep(0,output_width* output_height *n_filters )
  dim(feature_maps)  = c(output_width ,output_height,n_filters)
  
  
  for (filter_ind in 1:n_filters) {
    print(filter_ind)
    current_filter = filters[filter_ind,,,]
    print("after")
    feature_map = conv(img, current_filter,output_width,output_height,stride)
   
    
    
    
    
    
  }
  

  
}

n_filters=2
filters = runif(3*3*3*n_filters)
dim(filters) = c(n_filters,3,3,3)    


bias= 0
convolver(img,filters,bias,1,0)












