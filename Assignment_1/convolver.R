library(imager)
library(here)


image = load.image("https://i.pinimg.com/originals/34/80/8a/34808a2d1efdd8dea77b888358ccee49.jpg")
plot(image)












conv <- function (img, conv_filter,output_width,output_height,stride) {

  
  filter_size = dim(conv_filter)[1]
  feature_map = rep(0,output_width * output_height*3)
  dim(feature_map) = c(output_width,output_height,3)
  
  filter_spacing = (filter_size-1)/2
  
  row_indices = seq(filter_spacing+1,dim(img)[1]-filter_spacing-1,stride)
  column_indices = seq(filter_spacing+1,dim(img)[2]-filter_spacing-1,stride)
  black_slice = rep(0,dim(conv_filter)[1] * dim(conv_filter)[1]*3)
  dim(black_slice)= c(dim(conv_filter)[1],dim(conv_filter)[1],3)
  output_pixel_ind =0
  image_with_cell <- img
  for (column_index in column_indices) {
  
  for (row_ind in row_indices) {
    Sys.sleep(0.05)
    plot(as.cimg(image_with_cell))
    
    #Sys.sleep(0.5)
    
      output_pixel_ind = output_pixel_ind +1
      
    
      cell_rows <- seq(row_ind - filter_spacing,row_ind + filter_spacing,1)
             
      cell_columns <- seq(column_index-filter_spacing,column_index+filter_spacing,1)
      
      
      
      slice = as.matrix(img[cell_columns,cell_rows,]) 
      
      
      image_with_cell[cell_columns,cell_rows,] = black_slice
      
      
      dim(slice)= dim(conv_filter)
      
      
      feature = colSums(slice * conv_filter,dims=2)
      
     # print(as.integer(output_pixel_ind/output_height) +1)
      #print(output_pixel_ind %% output_height)
     # Sys.sleep(0.5)
      feature_map[as.integer(output_pixel_ind/output_height+1) ,output_pixel_ind %% output_height,]= feature
      
      
    }
  }
  #dim(feature_map) =  c(output_width,output_height,3)
  print(dim(feature_map))
  return( feature_map)
  
}


convolver = function(img,filters,bias,stride=1,padding=0) {
  print("convolving")
  
  
  dim(img) = dim(img)[c(1,2,4)]
 
  img = as.array(img)
  
  n_filters = dim(filters)[1]
  filter_width = dim(filters)[2]
  filter_height = dim(filters)[3]
  img_shape = dim(img)
  stopifnot(img_shape[3]==dim(filters)[4]) # Make sure channels match
  stopifnot(filter_width == filter_height) #Make sure filter square
  stopifnot(filter_width %% 2 != 0) #Make sure it is odd
  
  output_width = (img_shape[1] - filter_width + 2*padding)/stride +1
  output_height = (img_shape[2] - filter_width + 2*padding)/stride +1
  
  feature_maps = rep(0,output_width* output_height * img_shape[3] * n_filters )
  
  dim(feature_maps)  = c(output_width ,output_height,img_shape[3],n_filters)
  
  
  
  
  for (filter_ind in 1:n_filters) {

    print(sprintf('Filter index: %d', filter_ind))
    current_filter = filters[filter_ind,,,]
    
    feature_map = conv(img, current_filter,output_width,output_height,stride)
    
    feature_maps[,,,filter_ind] = feature_map
   
    
 
    plot(as.cimg(feature_maps[,,,filter_ind]))
    
    
    
  
    
    
  }
 
  
  
  
}

n_filters=1
filters = runif(3*3*3*n_filters)
dim(filters) = c(n_filters,3,3,3)    


bias= 0
gaussian_blur_kernel= c (1,2,1,2,4,2,1,2,1)
dim(gaussian_blur_kernel) = c(3,3)
gaussian_blur_kernel = as.matrix(gaussian_blur_kernel)*(1/16)
sobel_filter_horizontal = c(1,0,-1,2,0,-2,1,0,-1)


dim(sobel_filter_horizontal) = c(3,3)


sobel_filter_horizontal = as.matrix(sobel_filter_horizontal)

filters[1,,,] = sobel_filter_horizontal
#print(gaussian_blur_kernel)
convolver(image,filters,bias,1,0)












