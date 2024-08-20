library(ggplot2)
library(pedsuite, quietly = T)
source("../utils.R")

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
nsim = 2000
seed = 1729
sim1 = gmSim(peds, truePed = 1, nsim = nsim, seed = seed)
sim2 = gmSim(peds, truePed = 2, nsim = nsim, seed = seed)
sim3 = gmSim(peds, truePed = 3, nsim = nsim, seed = seed)

simdat = do.call(rbind, list(sim1, sim2, sim3))
simdat$True = rep(c("Ped1", "Ped2", "Ped3"), each = nsim)
simdat$Concl = ifelse(simdat$GLR <= 1e-4, "H2", ifelse(simdat$GLR >= 1e4, "H1", "Inc"))

lim = function(x) range(c(-6, 6, max(x), max(-20, min(x))))
br4 = function(lim) c(seq(4,lim[1], by=-4), seq(4,lim[2], by=4))

# Plot (version 1)
ggplot(simdat, aes(log10(LR13), log10(LR23))) + 
  theme_bw(base_size = 12) +
  annotate("rect", xmin= c(-Inf,4), xmax= c(Inf,Inf), ymin= c(4,-Inf), ymax= c(Inf, 4), 
           fill = 3, alpha = 0.1) +
  annotate("rect", xmin= -Inf, xmax= -4, ymin= -Inf, ymax= -4, fill = 2, alpha = 0.1) +
  geom_hline(yintercept = 4, color = 3, linetype = 2, linewidth = 1) + 
  geom_vline(xintercept = 4, color = 3, linetype = 2, linewidth = 1) +
  annotate("segment", x = -Inf, y = -4, xend = -4, color = 2, linetype = 2, linewidth = 1) + 
  annotate("segment", y = -Inf, x = -4, yend = -4, color = 2, linetype = 2, linewidth = 1) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  geom_point(alpha = 0.2, size = 0.8) + 
  facet_wrap(~True, scales = "free") +
  labs(x = expression(log[10](LR[1:3])),
       y = expression(log[10]~(LR[2:3])))+
  scale_x_continuous(limits = lim, breaks = br4) +
  scale_y_continuous(limits = lim, breaks = br4) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 2, size = 12)) 

ggsave("example1_version1.pdf", height = 3, width = 8)
ggsave("example1_version1.png", height = 3, width = 8, dpi = 300)


# Version 2 ---------------------------------------------------------------

ggplot(simdat, aes(log10(LR13), log10(LR23))) + 
  theme_bw(base_size = 12) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  annotate("rect", xmin= c(-Inf,4), xmax= c(Inf,Inf), ymin= c(4,-Inf), ymax= c(Inf, 4), 
           fill = 3, alpha = .1) + 
  annotate("rect", xmin= -Inf, xmax= -4, ymin= -Inf, ymax= -4, fill = 2, alpha = 0.1) +
  annotate("segment", x = -Inf, y = -4, xend = -4, color = 2, linetype = 2, linewidth = 1) + 
  annotate("segment", y = -Inf, x = -4, yend = -4, color = 2, linetype = 2, linewidth = 1) +
  annotate("segment", x = -Inf, y = 4, xend = 4, color = 3, linetype = 2, linewidth = 1) + 
  annotate("segment", y = -Inf, x = 4, yend = 4, color = 3, linetype = 2, linewidth = 1) +
  geom_point(alpha = 0.3, size = 0.5) + 
  facet_wrap(~True, scales = "free") +
  labs(x = expression(log~LR[1:3]),
       y = expression(log~LR[2:3]))+
  scale_x_continuous(limits = lim, breaks = br4) +
  scale_y_continuous(limits = lim, breaks = br4) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 2, size = 12),
        panel.grid = element_blank()) 

ggsave("example1_version2.pdf", height = 3, width = 8, )
ggsave("example1_version2.png", height = 3, width = 8, dpi = 300)

#################################################

# Version 3 ---------------------------------------------------------------

ggplot(simdat, aes(log10(LR13), log10(LR23), col = Concl)) + 
  theme_bw(base_size = 12) +
  geom_hline(yintercept = 0, col = 8) + geom_vline(xintercept = 0, col = 8) + 
  geom_point(alpha = .5, size = 0.4) + 
  scale_color_manual(values = c(H1 = 3, H2 = 2, Inc = 1), guide = "none") +
  annotate("segment", x = -Inf, y = -4, xend = -4, color = 2, linetype = 2) + 
  annotate("segment", y = -Inf, x = -4, yend = -4, color = 2, linetype = 2) +
  annotate("segment", x = -Inf, y = 4, xend = 4, color = 3, linetype = 2) + 
  annotate("segment", y = -Inf, x = 4, yend = 4, color = 3, linetype = 2) +
  facet_wrap(~True, scales = "free") +
  labs(x = expression(log~LR[1:3]),
       y = expression(log~LR[2:3]))+
  scale_x_continuous(limits = lim, breaks = br4) +
  scale_y_continuous(limits = lim, breaks = br4) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 2, size = 12),
        panel.grid = element_blank()) 

ggsave("example1_version3.pdf", height = 3, width = 8, )
ggsave("example1_version3.png", height = 3, width = 8, dpi = 300)




# Table for latex ---------------------------------------------------------
# \label{tab:rates}

library(xtable)

foo = function(x){
  p1 = length(x[x >= 4])
  p2 = length(x[x <=  -4] )
  pNO = length(x[x > -4 & x < 4])
  c(H1 = p1, H2 = p2, Inconclusive = pNO)/length(x)
}

GLR = data.frame(Ped1 = sim1$GLR, Ped2 = sim2$GLR, Ped3 = sim3$GLR)

tab = apply(log10(GLR), 2, foo)
xtable(tab, caption = "Table for ....", label = "tab:rates", digits = 3)
#################################################

