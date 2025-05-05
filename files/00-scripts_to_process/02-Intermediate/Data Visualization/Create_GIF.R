
install.packages("animation")
library(animation)


#This creates a GIF that cycles through histogram plots of data generated 
#from the normal distribution using mean = 0 and different standard deviations. 

## Create vector of standard deviations to be used.
vars <- seq(0.1,2, by=0.2)

## GIF creating function
saveGIF(
  # Loop to create individual plots
  for(i in 1:10){
    # Generate data
    data <- rnorm(100, 0, sd=vars[i])
    # Plot histogram
    ## xlim is specified so that plots are all comparable
    hist(data, xlim=c(-5,5))
    legend("topright", paste("SD =", vars[i]))
  }
  # Name the output GIF file and specify time between plot switches (not sure if works as coded)
  , movie.name = "histograms_gif.gif", extra.opts = list(delay="1>")
)

