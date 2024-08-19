# Parametric GLR example: Sample swap -------------------------------------

library(pedsuite)
library(ggplot2)
library(patchwork)
source("utils.R") # quickpng and xtable wrapper


# Load data
x = readRDS("swapped.rds")

# Pedigree plot
quickpng("ped_swapped.png", h = 4, w = 4, res = 600,
  plot(x, hatched = typedMembers, labs = typedMembers, margin = c(3,1,3,4))
)

# Genotype table
g = getGenotypes(x, ids = LETTERS[1:4]) |> t() |> as.data.frame()
g
xtableMDV(g, display = "s", label = "tab:ex3data", caption = "Genotypes for Example 3.")

# checkPairwise: Result table
tab = checkPairwise(x, nsim = 10000, pvalThreshold = 0.05, labels = T, seed = 123)
tab

# checkPairwise: ggplot2 (returns plot object)
triangle1 = checkPairwise(x, nsim = 1000, pvalThreshold = 0.05, labels = NULL, seed = 123, plotTy = "gg")

# Data frame for placing labels in plot
labdat = data.frame(ids = paste(tab$id1, tab$id2, sep = "-"),
               x = tab$k0 + c(-0.05, 0.06, 0.08, 0.05, -0.04, 0.08),
               y = tab$k2 + c(0.04, 0.03, 0.03, 0.05, 0.04, 0.03),
               col = c(2,2,4,3,4,4))

# Finalise triangle plot
triangle1 = triangle1 + 
  annotate("text", x = labdat$x, y = labdat$y, label = labdat$ids, color = labdat$col) + 
  scale_size_manual(values = c(big = 8), labels = "*p*-value < 0.05", name = NULL) + 
  theme(legend.text = ggtext::element_markdown(),
        plot.margin = margin(l = 0.2, r = 0.2, unit ="cm"))
triangle1
ggplot2::ggsave(triangle1, filename = "triangle1.pdf", h = 4, w = 4) 

# Patchwork: pedigree + triangle
ped_swapped_png = grid::rasterGrob(png::readPNG("ped_swapped.png"))
plot1 = wrap_elements(panel = ped_swapped_png) + triangle1 + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 18))

plot1

### Save combined plot. USED IN PUBLICATION
ggplot2::ggsave(plot1, filename = "combined1.pdf", h = 5.2, w = 10) 


# Corrected version -------------------------------------------------------

y = x |> 
  relabel(old = c("B", "D"), new = c("D", "B")) |> 
  transferMarkers(from = x, to = _) |> 
  relabel(old = c("B", "D"), new = c("B*", "D*"))

# Corrected pedigree plot
quickpng("ped_correct.png", h = 4, w = 4, res = 600,
  plot(y, hatched = typedMembers, starred = c("B", "D"), labs = typedMembers, margin = c(3,1,3,4))
)

# checkPairwise: Result table (after correction)
tab2 = checkPairwise(y, nsim = 10000, pvalThreshold = 0.05, labels = T, seed = 123)
tab2[4:6, 1:2] = tab2[4:6, 2:1]

# Triangle plot (after correction)
triangle2 = checkPairwise(y, nsim = 1000, pvalThreshold = 0.05, labels = NULL, seed = 123, plotTy = "gg")

labdat2 = data.frame(ids = paste(tab2$id1, tab2$id2, sep = "-"),
               x = tab2$k0 + c(-0.05, 0.06, 0.06, 0.05, -0.04, 0.06),
               y = tab2$k2 + c(0.04, 0.03, 0.03, 0.04, 0.04, 0.03),
               col = c(4,2,2,4,4,3))

triangle2 = triangle2 + 
  annotate("text", x = labdat2$x, y = labdat2$y, label = labdat2$ids, color = labdat2$col) +
  theme(plot.margin = margin(l = 0.2, r = 0.2, unit ="cm"))
triangle2

# Patchwork: pedigree + triangle
ped_correct_png = grid::rasterGrob(png::readPNG("ped_correct.png"))
plot2 = wrap_elements(panel = ped_correct_png) + triangle2 + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 18))

plot2

### Save combined plot. USED IN PUBLICATION
ggplot2::ggsave(plot2, filename = "combined2.pdf", h = 5.2, w = 10) 



# Latex table -------------------------------------------------------------

library(xtable)


# Auxiliary function used below
# Simplify output of checkPairwise (for latex)
simplifyTable = function(x) {
  with(x, data.frame(Pair = paste(id1, id2, sep = ","),
                     "$\\widehat\\kappa$" = sprintf("$(%.2f,%.2f,%.2f)$", k0,k1,k2),
                     "$\\kappa_\\PP$" = sprintf("$(%.2g,%.2g,%.2g)$", kappa0,kappa1,kappa2),
                     "$GLR$" = sprintf("%.2f", GLR), "$p$" = pval, check.names = F))
}


# Result table combining swapped and corrected results
totalTab = cbind(simplifyTable(tab), simplifyTable(tab2)[3:5])

xtableMDV(totalTab, display = "sssffsff", digits = 4, 
          label = "tab:checkpairwise", 
          caption = "Test results for each pairwise relationship in Example 3.")

