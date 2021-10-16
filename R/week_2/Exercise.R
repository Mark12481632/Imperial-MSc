#############
# Exercise 1
#############
# a. Create a (3,4) matrix as specified in the question paper.
#    We want 12 entries starting at 17 and ending at 55 all
#.   evenly spaced.  Column major ordering.
mat_a = matrix(seq(from=17, to=55, length=12), byrow=T, nrow=3)

# b. Create a (4,3) matrix which contains the 12 integers 0..11.
#.   Row major ordering.
#.   Then right multiply this matrix to mat_a (defined above).
#.   Matrix multiplication (in R) done by "%*%".
mat_b = matrix(0:11, byrow=F, nrow=4)
result = mat_a %*% mat_b

# c. Create the "sin" of the components of the "result" matrix.
#.   Also, multiply this "sin" matrix with its transpose and find the
#.   Cholesky decomposition of the result.
mat_sin = sin(result)
mat_prod = mat_sin %*% t(mat_sin)
mat_cholesky = chol(mat_prod)


############
# Exercise 2
############
# a. Load dog image:
library(imager)
dog = load.image("./dog.jpg")
plot(dog)

# b. Create random vector - 1 entry for each pixel.
apply_fuzziness <- function(img, fuzzy_factor=0.1) {
  # Don't need to convert to a matrix:
  std_norm_vec = rnorm(dim(img)[1] * dim(img)[2]) * fuzzy_factor
  img_fuzzy <- img + std_norm_vec
  return(img_fuzzy)
}

dog_fuzzy <- apply_fuzziness(dog, 0.2)
plot(dog_fuzzy)


############
# Exercise 3
############
# Plot the given function between the ranges (x,y) in [-10,10] X [-30,30].
# Each dimension to have 50 points (total of 2500 points)
xcoords = seq(from=-10, to=10, length=50)
ycoords = seq(from=-30, to=30, length=50)

create_plot_data <- function(x, y) {
  graph <- matrix(1:(length(x)*length(y)), byrow=T, nrow=length(x))
  for (xind in seq(x)) {
    for (yind in seq(y)) {
      graph[xind, yind] <- x[xind]^2 - 0.1*y[yind]^2
    }
  }
  return(graph)
}

# Evaluate the function (i.e. graph)
graph <- create_plot_data(xcoords, ycoords)

# Display graph.
contour(xcoords, ycoords, graph)
