# Figure for DVI example ------------------------------------------------------

library(dvir)
library(magick)

source("../utils.R")

# Load data
dvi = readRDS("dvi.rds")

# Plot
plotDVI(dvi, marker = 1)

# Create figure
quickpng("dviEx-uncropped.png", w = 4.8, h = 3.5, 
         plotDVI(dvi, labs = leaves, cex.main = 1.2, widths = c(.3,.7)))

# Crop whitespace
image_read("dviEx-uncropped.png") |> 
  image_trim() |> 
  image_write("dviEx-cropped.png")
