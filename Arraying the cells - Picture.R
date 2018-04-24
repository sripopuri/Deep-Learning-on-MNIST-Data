sample = sample(1:nrow(data),1)

pic = data[sample,-1]

m = matrix(0,28,28)
 
# Generating a picture out of those 784 columns
for(i in 1:28){
  m[i,] = as.matrix(pic[,(28*(i-1) + 1):(28*i)])
}
image(m,col=grey.colors(225),axes=F)

# Note that:
# For the image function the cells should be transposed 
# and Transposed in such a way that
# The transposed first rows should be last column and so on

# Inorder to know what columns are each of those cells
cell = matrix(0,28,28)

for(i in 1:28){
  for(j in 1:28){
    cell[i,j] = (i-1)*28 + j
  }
}
View(cell)
