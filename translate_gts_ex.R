####################################################################################################
##### Translate Phanerozoic sample ages between timescales
#####
##### example
####################################################################################################


####################################################################################################
##### source scripts
####################################################################################################

source("timescale_library.R")
source("translate_gts_fns.R")


####################################################################################################
##### Example with dummy data
####################################################################################################

# Synthetic section mostly spanning the Barremian-Aptian interval, where the GTS2012 -> GTS2020
# change is fairly noticeable. Two ages are intentionally missing to demonstrate depth fallback.
# Two ages are flagged as radiometric so they should remain numerically identical across timescales.

dummy_depth <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45)

dummy_old_age <- c(
  129.8,
  128.4,
  127.1,
  NA,
  124.4,
  123.2,
  NA,
  120.9,
  119.4,
  118.1
)

dummy_radiometric_flag <- c(
  FALSE,
  FALSE,
  TRUE,
  FALSE,
  FALSE,
  TRUE,
  FALSE,
  FALSE,
  FALSE,
  FALSE
)

result <- translate_gts_age(
  old_age = dummy_old_age,
  depth = dummy_depth,
  old_timescale = "GTS2012",
  new_timescale = "GTS2020",
  allow_depth_fallback = TRUE,
  radiometric_flag = dummy_radiometric_flag
)

print(result, row.names = FALSE)


####################################################################################################
##### plot
####################################################################################################

plot_translation_demo(result)
