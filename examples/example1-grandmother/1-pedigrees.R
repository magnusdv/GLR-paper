library(pedsuite, quietly = T)
source("../utils.R")

# Create pedigrees ---------------------------------------------------

ped = linearPed(2) |> relabel(c("GF", "A", "FA", "MO1", "C"))

peds = list(
  Ped1 = ped |> addDaughter(c("FA", "MO1"), id = "B") |> reorderPed(c("B", "C")),
  Ped2 = ped |> addDaughter(c("FA", "MO2"), id = "B") |> reorderPed(c("B", "C")),
  Ped3 = list(ped, singleton("B", sex = 2))
)

# Save (for simulations)
saveRDS(peds, "peds.rds")

# Quick glance
plotPedList(peds, hatched = c("A", "B", "C"))


# Plot for publication ---------------------------------------------------

# Move singleton (looks better)
pp = peds
pp$Ped3 = rev(pp$Ped3)

# Titles used in publication plot
hyptit = c("H1: A grandmother of B", "H2: A and B unrelated")
pedtit = c("Ped1", "Ped2", "Ped3")

# Create pdf
pdf("grandmother-peds.pdf", width = 6, height = 2.5)
plotPedList(pp, hatched = c("A", "B", "C"), margins = c(1, 1.5, 1, 1.5),
            fmar = 0.02, groups = list(1:2, 3:4), titles = hyptit, 
            grouptitlesArgs = list(line = 1.1, cex = 0.9, font = 1))
# Add pedigree titles
mtext(pedtit, line = 1.7, at = c(-0.1, 0.59, 1.45), cex = 0.75, font = 2)
dev.off()
