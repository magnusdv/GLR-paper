library(dvir)
library(xtable)

source("utils.R")

# Load data
dvi = readRDS("dvi.rds")

# Plot
plotDVI(dvi, marker = 1)

# Pairwise matrix
lr = pairwiseLR(dvi)
lr$LRmatrix
xtable(lr$LRmatrix, digits = 2) |> print(floating = F, booktabs = T)

# Joint table
j = dviJoint(dvi, verbose = F)
head(j$joint)

# Abbreviated for paper
jj = j$joint[c(1:11, 34), 1:4]
xtable(jj, digits = 4) |> print(floating = F, booktabs = T)

# GLR matrix
j$GLRmatrix
xtable(j$GLRmatrix, digits = -2)

# GLR score for V1 = M1
exp(j$joint$loglik[1] - j$joint$loglik[3])

# GLR score for {V2,V3} = {M2,M3}
exp(j$joint$loglik[1] - j$joint$loglik[5])



# Not used in paper -------------------------------------------------------

# Condition on V1 = M1
dvi2 = setPairing(dvi, victim = "V1", missing = "M1")$dviReduced
plotDVI(dvi2, marker = 1)

pairwiseLR(dvi2)$LRmatrix |> 
  round(2)
