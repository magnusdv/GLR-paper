library(pedsuite, quietly = T)
source("utils.R")

# Simulations -------------------------------------------------------------

peds = readRDS("peds.rds")

# Function used in sims below: Simulate from truePed and return LR13, LR23 and GLR
gmSim = function(peds, truePed, nsim = 2, seed = NULL){
  
  # Simulate from indicated truth (NB: simplify1 = F allows nsim = 1)
  sims = profileSim(peds[[truePed]], ids = c("A", "B", "C"), seed = seed, N = nsim, simplify1 = F)

  # LRs
  lrs = sapply(sims, function(s) {
    hyps = peds
    hyps[[truePed]] = s # insert `s` in correct spot
    kinshipLR(hyps, ref = 3, source = truePed)$LRtotal[-3]
  })

  data.frame(LR13 = lrs[1,], LR23 = lrs[2,], GLR = pmax(lrs[1,], lrs[2,]))
}


# Norwegian
db = NorwegianFrequencies[1:23]

# Mutation rate
r = 0.001 

# Attach markers
peds = lapply(peds, function(p)
  p |> setMarkers(locusAttr = db) |> setMutmod(model = "prop", rate = r))


# Do the simulations
nsim = 100
seed = 1729
sim1 = gmSim(peds, truePed = 1, nsim = nsim, seed = seed)
sim2 = gmSim(peds, truePed = 2, nsim = nsim, seed = seed)
sim3 = gmSim(peds, truePed = 3, nsim = nsim, seed = seed)
GLR = data.frame(Ped1 = sim1$GLR, Ped2 = sim2$GLR, Ped3 = sim3$GLR)

# Plot (not finished)
library(ggplot2)
ggplot(log10(sim3), aes(LR13, LR23)) + geom_point()
#################################################

# Table \label{tab:rates}
foo = function(x){
  p1 = length(x[x >= 4])
  p2 = length(x[x <=  -4] )
  pNO = length(x[x > -4 & x < 4])
  c(H1 = p1, H2 = p2, Inconclusive = pNO)/length(x)
}

tab = apply(log10(GLR), 2, foo)
xtable(tab, caption = "Table for ....", label = "tab:rates", digits = 3)
#################################################

