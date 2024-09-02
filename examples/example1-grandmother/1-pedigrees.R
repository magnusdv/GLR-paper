library(pedsuite, quietly = T)
source("../utils.R")

# Create pedigrees ---------------------------------------------------

ped = linearPed(2) |> relabel(c("GF", "A", "FA", "MO1", "C"))

peds = list(
  Ped1 = ped |> addDaughter(c("FA", "MO1"), id = "B") |> reorderPed(c("B", "C")),
  Ped2 = ped |> addDaughter(c("FA", "MO2"), id = "B") |> reorderPed(c("B", "C")),
  Ped3 = list(ped, singleton("B", sex = 2))
)

# Save
saveRDS(peds, "peds.rds")
# peds = readRDS("peds.rds")


# Quick glance
plotPedList(peds, hatched = c("A", "B", "C"))


# Titles used in publication plot
hyptit = c("H1: A grandmother of B", "H2: A and B unrelated")
pedtit = c("Ped1", "Ped2", "Ped3")

# Main plot. USED IN PUBLICATION
quickpng("grandmother-peds.png", w = 6, h = 2.5, {
         plotPedList(peds, hatched = c("A", "B", "C"), margins = c(1, 1.5, 4.1, 1.5),
                     groups = list(1:2, 3:4), titles = character(2), fmar = 0.02)
         mtext(pedtit, line = 1.7, at = c(-1.29, -0.23, 1.05), cex = 0.75, font = 2)
         mtext(hyptit, line = 2.8, at = c(-0.71, 1.05), cex = 0.9)
})

