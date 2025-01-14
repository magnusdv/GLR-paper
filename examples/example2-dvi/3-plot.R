# Figure for DVI example ------------------------------------------------------

library(dvir)
library(magick)

source("../utils.R")

# Load data
dvi = readRDS("dvi.rds")

# Plot
plotDVI(dvi, marker = 1, style = 2)

# Create figure for paper
pdf("dvi-uncropped.pdf", width = 4.8, height = 3.5)
plotDVI(dvi, labs = leaves, widths = c(.3,.7), style = 2)
dev.off()

# Crop whitespace
# Requires pdfcrop: tinytex::tlmgr_install('pdfcrop')
system("pdfcrop dvi-uncropped.pdf dvi-cropped.pdf")
