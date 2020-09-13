library(imager)
library(Matrix)
library(ggplot2)

img = load.image("https://i.pinimg.com/originals/34/80/8a/34808a2d1efdd8dea77b888358ccee49.jpg")
plot(img)
#img <- resize(orig, 100, 100)  # make it smaller and equal dimension

pooling <- function(image, filter, stride)
{
  f <- filter
  s <- stride 
  col <- dim(image[,,1])[2]  # get image dimensions
  row <- dim(image[,,1])[1]
  c <- (col-f)/s+1             # calculate new dimension size 
  r <- (row-f)/s+1  
  
  newImage <- array(0, c(c, r, 3)) # create new image object
  for(channel in 1:3)                  # loops in RGB layers 
  {
    print(dim(image))
    m <- image[,,,channel]

    m3 <- matrix(0, ncol = c, nrow = r)
    i <- 1
      for(ii in 1:r)
      {
        j=1
        for(jj in 1:c)
        {
          m3[ii,jj]<-max(as.numeric(m[i:(i+(f-1)), j:(j+(f-1))]))
          j <- j+s
        } 
        i <- i+s
      }
    newImage[,,channel] <- m3
  }
  
  return(newImage)
}

output_image <- pooling(image = img, filter = 3, stride = 3)

correct_dim = dim(output_image)
pmax = as.matrix(output_image)
dim(output_image) = correct_dim

plot(as.cimg(output_image,x=dim(output_image)[1],y= dim(output_image)[2],cc=3))  



 