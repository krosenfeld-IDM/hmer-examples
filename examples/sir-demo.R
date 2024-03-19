# https://cran.r-project.org/web/packages/hmer/vignettes/demonstrating-the-hmer-package.html

# Summarize the SIRSample dataframe
summary(SIRSample)

# Get the column names of the training dataset
summary(SIRSample$training)
colnames(SIRSample$training)
# print "the number of data points" and the number of rows in SIRSample$training
print(paste("number of training data points:", nrow(SIRSample$training)))
print(paste("number of validation data points:", nrow(SIRSample$validation)))

###################################
# Training
# train the emulator
input_ranges <- list(aSI = c(0.1, 0.8), aIR = c(0, 0.5), aSR = c(0, 0.05))
output_names <- c('nS', 'nI', 'nR')
wave1_emulators <- emulator_from_data(SIRSample$training, output_names, input_ranges)

print(wave1_emulators$nS)

###################################
# Validation
targets <- list(
  nS = c(580, 651),
  nI = list(val = 169, sigma = 8.45),
  nR = c(199, 221)
)

invalid_points <- validation_diagnostics(wave1_emulators, targets, SIRSample$validation, plt = TRUE)

# what is the type of SIRSample?
print(class(SIRSample))

###################################
# Visualize
emulator_plot(wave1_emulators)
emulator_plot(wave1_emulators$nS, params = c('aSI', 'aIR'), fixed_vals = list(aSR = 0.03))
emulator_plot(wave1_emulators$nI, plot_type = 'var')
emulator_plot(wave1_emulators$nI, plot_type = 'sd')

emulator_plot(wave1_emulators$nS, params = c('aIR', 'aSI')) + ggplot2::geom_point(data = SIRSample$training, ggplot2::aes(x = aSI, y = aIR))

# plot implausibility across parameter space
emulator_plot(wave1_emulators, plot_type = 'imp', targets = targets)
plot_lattice(wave1_emulators, targets) # $$


space_removed(wave1_emulators, targets, ppd = 15)# + geom_vline(xintercept = 3, lty = 2)

# generate a new design
new_points <- generate_new_design(wave1_emulators, 90, targets)
#> Proposing from LHS...
#> LHS has high yield; no other methods required.
#> Proposing from LHS...
#> Selecting final points using maximin criterion...
plot(rbind(rbind(SIRSample$training, SIRSample$validation)[,names(input_ranges)], new_points), pch = 16, cex = 0.8, col = rep(c('black', 'blue'), each = 90))

# look at proposed points
wave.points <- list(SIRSample$training[,c('aSI', 'aIR', 'aSR')], new_points)
wave_points(wave.points, c('aSI', 'aIR', 'aSR'))

# plot multiple waves
wave_values(SIRMultiWaveData, targets)

wave_dependencies(SIRMultiWaveData, targets)

simulator_plot(SIRMultiWaveData, targets, barcol = 'white')

