library(dvir)
library(xtable)

source("../utils.R")

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
head(j)

# Abbreviated for paper
jj = j[c(1:11, 34), 1:4]

# Change loglik to log10-lik (requested by reviewer)
jj$loglik = log10(exp(jj$loglik))

# Format 
xtable(jj, digits = 2) |> print(floating = F, booktabs = T)

# Minor manual tweaks for paper:
# 1. Replace * by $\;*$
# 2. Insert after row 9: \vdots &&&& \\ 

# GLR matrix
glr = dvir:::pairwiseGLR(dvi, jointTable = j)
xtable(glr$GLRmatrix, digits = -2)

# GLR score for V1 = M1
exp(j$loglik[1] - j$loglik[3])
10^(jj$loglik[1] - jj$loglik[3])

# GLR score for {V2,V3} = {M2,M3}
exp(j$loglik[1] - j$loglik[5])
10^(jj$loglik[1] - jj$loglik[5])



# Not used in paper -------------------------------------------------------

# Condition on V1 = M1
dvi2 = setPairing(dvi, victim = "V1", missing = "M1")$dviReduced
plotDVI(dvi2, marker = 1)

pairwiseLR(dvi2)$LRmatrix |> 
  round(2)
