#####################################
### Various R utils for GLR paper ###
#####################################

# Utility for saving png
quickpng = function(filename, expr, w = dev.size()[1], h = dev.size()[2], units = "in", res = 300) {
  if(!endsWith(filename, "png"))
    filename = paste0(filename, ".png")
  png(filename, width = w, height = h, units = units, res = res)
  on.exit(dev.off())
  expr
  browseURL(filename)
}



# Wrapper of `xtable()` adding the following features:
# 2. Removes row names
# 3. Single-string `display` is split: 'ssf' -> c('s','s','f'). 
# 4. Enforces booktabs = T and placement = "tb".
# 5. Automatically places the output on the clipboard

xtableMDV = function(x, display, rownames = F, digits = NULL, 
                     removeZero = FALSE, ...) {
  
  # Handle 'display' argument
  if(length(display) == 1) {
    if(nchar(display) == 1)
      display = rep(display, ncol(x))
    else
      display = strsplit(display, split = "")[[1]]
  }
  
  # xtable requires display of row name column (we remove it in print() below)
  if(length(display) == ncol(x))
    display = c("d", display)
  
  if(removeZero) {
    if(digits) y = round(x, digits = digits)
    x[y == 0] = NA
  }
  
  # Apply xtable()
  tab = xtable(x, display = display, digits = digits, ...)
  
  # Print with options
  s = print.xtable(tab, type = "latex", booktabs = TRUE, 
                   include.rownames = rownames, 
                   table.placement = "tb",
                   sanitize.text.function = identity)
  
  # Write to clipboard
  clipr::write_clip(s)
}
