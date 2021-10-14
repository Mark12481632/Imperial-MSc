
############################
# Week 2 - Part III
############################

## Random Number Generator

# Generate 5000 numbers from an uniform distribution
u = runif(5000)
plot(u)

# Generate 5000 numbers from a gaussian distribuition with mean 0 and sd 1

x = rnorm(5000, mean=0, sd=1) # note that by default of rnorm function, it produces a standard normal so the arguments mean and sd can be omited
print(x) # Doesn't say much ... Perhaps another plot?

hist(x)
plot(density(x), main="Kernel Density of Normal distribuition") # Using a kernel density estimation, plot the values using the optimal bandwidth

# Let's generate  5000 numbers from three  gamma distributions and examine their skewness
y1 = rgamma(5000, shape = 1, scale=1)
y2 = rgamma(5000, shape = 4, scale=1)
y3 = rgamma(5000, shape = 7, scale=1)

hist(y1)
hist(y2)
hist(y3)

plot(density(y1), main="Kernel Density of Gamma distribuitions",xlim=c(0,20),xlab="")
lines(density(y2),col=2)
lines(density(y3),col=4)

legend("topright",  c("Gamma(1,1)", "Gamma(4,1)", "Gamma(7,1)"), lty=c(1,1,1),
       col = c(1,2,4), title= "Parameters")
# Command lty is just to specify we are using a line, while col specify the colors.







#### Sampling
s = 1:10 # create a vector with a sequence of 1 to 10

# Create a sample (without replacement, the default) of 5 elements
s1 = sample(s,5)

# Create a sample with 15 elements, note that the sample size is bigger than the length of the original vector, so we need to allow replacement by setting replace=TRUE
s2 = sample(s,15,replace=TRUE)

# Role of seed
# To  produce the same random number, fix the seed value using set.seed function
set.seed(129)
s1 = sample(s,5)
print(s1)
set.seed(129)
s1 = sample(s,5)
print(s1)

set.seed(129)
s2 = sample(s,15,replace=TRUE)
print(s2)
set.seed(129)
s2 = sample(s,15,replace=TRUE)
print(s2)


#### Multivariate normal
# First let's load the appropriate package
library("mvtnorm")

# Generate 500 observations from a bivariate normal
Sigma = matrix(c(0.5,0.2,0.2,1), ncol=2)
mu = c(0.2,0.6)
X = rmvnorm(n=500, mean=mu, sigma=Sigma)

plot(X)



# A surface plot using threejs package
library("threejs") #this library is just for the following plot. You might need to install it
u <- seq(-5, 5, by = .1)
v <- seq(-5, 5, by = .1)
M <- expand.grid(u,v) # this will be the value of the axis in the 3D plot

x <- M$Var1
y <- M$Var2
sigma <- matrix(c(0.5, 0.2, 0.2, 0.5), nrow = 2, byrow = TRUE)
z <- dmvnorm(x = M, sigma = sigma)

scatterplot3js(x, y, z, phi = 40, theta = 20,
               color=rainbow(length(z)),
               colkey = FALSE,
               cex = .3,
               main = "Bivariate Normal")
