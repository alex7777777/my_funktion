# ######################################################################## #
# 2019-03-10 - HLP/Jaeger lab blog                                         #
# Correlation plot matrices using the ellipse library - Creating image     #
# Source: https://hlplab.wordpress.com/                                    #
# ######################################################################## #

my_ellipse_corr_img <- function(corr_df, main_header="Predictor correlations")
{
  # Here we play around with the colors, colors are selected from a list with colors recycled
  # Thus to map correlations to colors we need to make a list of suitable colors
  # To start, pick the end (and mid) points of a scale, here a red to white to blue for neg to none to pos correlation
  colsc=c(rgb(241, 54, 23, maxColorValue=255), 'white', rgb(0, 61, 104, maxColorValue=255))
  
  # Build a ramp function to interpolate along the scale, I've opted for the Lab interpolation rather than the default rgb, check the documentation about the differences
  colramp = colorRampPalette(colsc, space='Lab')
  
  # I'll show two types of color styles using this color ramp
  # the first
  # Use the same number of colors along the scale for the number of variables
  colors = colramp(length(corr_df[1,]))
  
  # then plot an example with only ellipses, without a diagonal and with a main title
  # the color selection stuff here multiplies the correlations such that they can index individual colors and create a sufficiently large list
  # incase you are confused, r allows vector indexing with non-integers by rounding down, i.e. colors[1.8] == colors[1]

  source("my_function/44_function.R")
  my_ellipse_corr(corr_df, col=colors[5*corr_df + 6], main=main_header)
  
  # the second form
  # we could, alternatively, make a scale with 100 points
  colors = colramp(100)
  # then pick colors along this 100 point scale given the correlation value * 100 rounded down to the nearest integer
  # to do that we need to move the correlation range from [-1, 1] to [0, 100]
  # now plot again with ellipses along the diagonal
  my_ellipse_corr(corr_df, col=colors[((corr_df + 1)/2) * 100], diag='ellipse', main=main_header)
  
  # or, add numbers to the bottom of the chart
  my_ellipse_corr(corr_df, col=colors[((corr_df + 1)/2) * 100], diag='ellipse', lower.panel="number", main=main_header)
  
  # or, switch the numbers and ellipses and reduce the margins
  return(my_ellipse_corr(corr_df, col=colors[((corr_df + 1)/2) * 100], diag='ellipse', upper.panel="number", mar=c(0,2,0,0), main=main_header))

  # # or, drop the diagonal and numbers
  # my_ellipse_corr(corr_df, col=colors[((corr_df + 1)/2) * 100], upper.panel="none", mar=c(0,2,0,0), main=main_header)
  

}
