# https://danny-sc.github.io/Tutorial_1/one-dimensional-example.html

# our test function
func <- function(x) {
  2*x + 3*x*sin(5*pi*(x-0.1)/0.4)
}

# generate data
# the example is interested in the range from [0, 0.6]
ranges1d <- list(x = c(0, 0.6))
# but we generate data excluding [0.5, 0.6]
x <- seq(0.05, 0.5, by = 0.05)
data1d <- data.frame(x = x, f = func(x))

# train the emulator
em1d_1 <- emulator_from_data(data1d, c('f'), ranges1d)

# inspect the covariance at each of the data point we were given:
print(em1d_1$f$get_cov(data1d))

# and the expectation matches the data
print(em1d_1$f$get_exp(data1d) - data1d$f)

# and looking at test points:
test_points <- data.frame(x = seq(0, 0.6, by = 0.001))
em_exp <- em1d_1$f$get_exp(test_points)
em_var <- em1d_1$f$get_cov(test_points)
print(em_exp)
print(em_var)

# for plotting
plotting1d <- data.frame(
  x = test_points$x,
  f = func(test_points$x),
  E = em_exp,
  max = em_exp + 3*sqrt(em_var),
  min = em_exp - 3*sqrt(em_var)
)

# make the figure
plot(data = plotting1d, f ~ x, ylim = c(min(plotting1d[,-1]), max(plotting1d[,-1])),
     type = 'l', main = "Emulation of a Simple 1-dimensional Function", xlab = "Parameter value", 
     ylab = "Function value")
lines(data = plotting1d, E ~ x, col = 'blue')
lines(data = plotting1d, min ~ x, col = 'red', lty = 2)
lines(data = plotting1d, max ~ x, col = 'red', lty = 2)
points(data = data1d, f ~ x, pch = 16, cex = 1)
legend('topleft', inset = c(0.05, 0.05), legend = c("Function value", "Emulated value", "Uncertainty Bounds"),
       col = c('black', 'blue', 'red'), lty = c(1,1,2))