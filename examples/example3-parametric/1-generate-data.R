library(pedsuite)

# Pedigree
ped = nuclearPed(3, sex = c(2,2,1)) |> addSon(5)
  
# Generate profiles
db = NorwegianFrequencies[1:23]
x0 = ped |> profileSim(markers = db, ids = c(1,3,4,7), seed = 2)

# Introduce sample swap
x = swapGenotypes(x0, c(3,7)) |> 
  relabel(old = c(1,3,4,7), new = LETTERS[1:4])

# Save
saveRDS(x, file = "swapped.rds")
