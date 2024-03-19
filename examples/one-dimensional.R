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

# now for the history matching

# start with the targets for the history matching
target1d <- list(f = list(val = 0, sigma = 0.05))

# and our new points
new_points1d <- generate_new_design(em1d_1, 10, target1d,
                                  method = 'lhs', 
                                  measure.method = 'maximin')

# and plot
plot(data = plotting1d, f ~ x, ylim = c(min(plotting1d[,-1]), max(plotting1d[,-1])),
     type = 'l', main = "Emulation of a Simple 1-dimensional Function", xlab = "Parameter value", 
     ylab = "Function value")
lines(data = plotting1d, E ~ x, col = 'blue')
lines(data = plotting1d, min ~ x, col = 'red', lty = 2)
lines(data = plotting1d, max ~ x, col = 'red', lty = 2)
points(data = data1d, f ~ x, pch = 16, cex = 1)
legend('topleft', inset = c(0.05, 0.05), legend = c("Function value", "Emulated value", "Uncertainty Bounds"),
       col = c('black', 'blue', 'red'), lty = c(1,1,2))
abline(h = target1d$f$val, lty = 2)
abline(h = target1d$f$val + 3*target1d$f$sigma, lty = 2)
abline(h = target1d$f$val - 3*target1d$f$sigma, lty = 2)
points(x = unlist(new_points1d, use.names = F), y = func(unlist(new_points1d, use.names = F)), 
       pch = 16, col = 'blue')

# and the second wave
new_data1d <- data.frame(x = unlist(new_points1d, use.names = F), f = func(unlist(new_points1d, use.names = F)))

em1d_2 <- emulator_from_data(new_data1d, c('f'), ranges1d)
em1d_2_results <- data.frame(E = em1d_2[[1]]$get_exp(test_points), V = em1d_2[[1]]$get_cov(test_points))
plotting1d2 <- data.frame(x = plotting1d$x, f = plotting1d$f, E = em1d_2_results$E,
                          max = em1d_2_results$E + 3*sqrt(abs(em1d_2_results$V)),
                          min = em1d_2_results$E - 3*sqrt(abs(em1d_2_results$V)))
plot(data = plotting1d2, f ~ x,  ylim = c(min(plotting1d2[,-1]), max(plotting1d2[,-1])),
     type = 'l', main = "Emulator of a Simple 1-dimensional Function: Wave 2", xlab = "Parameter value", 
     ylab = "Function value")
lines(data = plotting1d2, E ~ x, col = 'blue')
lines(data = plotting1d2, max ~ x, col = 'red', lty = 2)
lines(data = plotting1d2, min ~ x, col = 'red', lty = 2)
points(data = new_data1d, f ~ x, pch = 16, cex = 1)
legend('topleft', inset = c(0.05, 0.05), legend = c("Function value", "Emulated value", "Uncertainty Bounds"), 
       col = c('black', 'blue', 'red'), lty = c(1,1,2))

# use both waves!
new_new_points1d <-  generate_new_design(c(em1d_2, em1d_1), 10, z = c(target1d, target1d),
                                      method = 'lhs', measure.method = 'maximin')
plot(data = plotting1d2, f ~ x, ylim = c(min(plotting1d2[,-1]), max(plotting1d2[,-1])),
     type = 'l', main = "Emulator of a Simple 1-dimensional Function: Wave 2", xlab = "Parameter value", 
     ylab = "Function value")
lines(data = plotting1d2, E ~ x, col = 'blue')
lines(data = plotting1d2, max ~ x, col = 'red', lty = 2)
lines(data = plotting1d2, min ~ x, col = 'red', lty = 2)
points(data = new_data1d, f ~ x, pch = 16, cex = 1)
legend('topleft', inset = c(0.05, 0.05), legend = c("Function value", "Emulated value (wave 2)",
                                                    "Uncertainty Bounds"), col = c('black', 'blue', 'red'), 
                                                    lty = c(1,1,2))
abline(h = target1d$f$val, lty = 2)
abline(h = target1d$f$val + 3*target1d$f$sigma, lty = 2)
abline(h = target1d$f$val - 3*target1d$f$sigma, lty = 2)
points(x = unlist(new_new_points1d, use.names = F), y = func(unlist(new_new_points1d, use.names = F)), 
       pch = 16, col = 'blue')