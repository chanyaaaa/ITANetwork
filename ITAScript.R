#Load necessary packages:
library(ergm)	
library(sna)	
library(coda)
library(latticeExtra)
library(igraph)
library(intergraph)
library(dplyr)
library(parallel)

#useful function
expit <- function(x){
  coefs <- exp(x)/(1+exp(x))
  coefs
}

#data clean-ups:
#extract meta data only for relevant disputes:
#meta data
ita_meta <- ita_disputes_short[match(colnames(ita_mat),ita_disputes_short$short_title),]
write.csv(ita_meta, file = "ita_meta.csv")
write.csv(ita_mat, file = "ita_mat.csv")

# 1) year of awards
# 2) in-favor
ita_in_favor <- ita_meta$in_favor
tmp <- c("State", "Investor")

for (i in 1:nrow(ita_meta)) {
  if (!is.element(ita_in_favor[i], tmp)) {
    ita_in_favor[i] <- "Others"
  }
}
write.csv(ita_in_favor, file = "ita_in_favor.csv")

# 3) arbitration rules matrix
ita_arb_rules <- ita_meta$arbitration_rules
tmp <- c("ICSID", "UNCITRAL")

for (i in 1:nrow(ita_meta)) {
  if (!is.element(ita_arb_rules[i], tmp)){
    if (ita_arb_rules[i] == "ICSID Additional Facility") {
      ita_arb_rules[i] <- "ICSID"
    } else if (ita_arb_rules[i] == "UNCITRAL Arbitration Rules (2013)") {
      ita_arb_rules[i] <- "UNCITRAL"
    } else {
      ita_arb_rules[i] <- "Others"
    }
  }
}

write.csv(ita_arb_rules, file = "ita_arb_rules.csv")

ita_meta$arbitration_rules[which(ita_meta$arbitration_rules == 
                                   "ICSID Additional Facility - Arbitration Rules")] <- "ICSID Additional Facility"
shared_arb_rules_mat <- matrix(0, nrow = 418, ncol = 418)
rownames(shared_arb_rules_mat) <- ita_meta$short_title
colnames(shared_arb_rules_mat) <- ita_meta$short_title

for (i in 1:nrow(shared_arb_rules_mat)) {
  for (j in 1:ncol(shared_arb_rules_mat)){
    if ( i != j) {
      if (ita_meta$arbitration_rules[i] == ita_meta$arbitration_rules[j]) {
        shared_arb_rules_mat[i,j] = 1
      }
    }
  }
}

#4) BIT matrix
shared_BIT_mat <- matrix(0,nrow = 418, ncol = 418)
rownames(shared_BIT_mat) <- ita_meta$short_title
colnames(shared_BIT_mat) <- ita_meta$short_title

for (i in 1:nrow(shared_BIT_mat)) {
  for (j in 1:ncol(shared_BIT_mat)) {
    if ((i != j) & (ita_meta$case_treaties[i] != "N/A")) {
      if (ita_meta$case_treaties[i] == ita_meta$case_treaties[j]) {
        shared_BIT_mat[i,j] = 1
      }
    }
  }
}

write.csv(shared_arb_rules_mat, file = "shared_arbitration_rules.csv")
write.csv(shared_BIT_mat, file = "shared_BITs.csv")

#create networks:
ita_net <- network(ita_mat, matrix.type = "adjacency", directed = TRUE)
ita_arb_rules_net <- network(as.matrix(shared_arbitration_rules), matrix.type = "adjacency", directed = FALSE)
ita_BIT_net <- network(as.matrix(shared_BITs), matrix.type = "adjacency", directed = FALSE)
ita_net_igraph <- asIgraph(ita_net)

#add vertex attributes
ita_net_igraph <- set.vertex.attribute(ita_net_igraph, name = "dispute_name", value = ita_meta$short_title)
ita_net_igraph <- set.vertex.attribute(ita_net_igraph, name = "award_yr", value = ita_meta$year_of_award)
ita_net_igraph <- set.vertex.attribute(ita_net_igraph, name = "in_favor", value = ita_in_favor)
ita_net_igraph <- set.vertex.attribute(ita_net_igraph, name = "arbitration_rule", value = ita_arb_rules)
ita_net_igraph <- set.vertex.attribute(ita_net_igraph, name = "BIT", value = ita_meta$case_treaties)

#initial plots
ita_net_igraph_noIso <- delete.vertices(ita_net_igraph,which(igraph::degree(ita_net_igraph)==0))
plot.igraph(ita_net_igraph_noIso,
            edge.arrow.size = 0.1,
            vertex.color = rgb(0,51,102, maxColorValue = 255),
            #vertex.shape = ifelse(V(got_net)$Gender == 1, "square", "circle"),
            edge.color = rgb(51, 51, 51, 80, maxColorValue = 255),
            vertex.label.color = rgb(1, 1, 1),
            #vertex.label = V(got_net)$Label,
            vertex.label.cex = 0.01,
            vertex.size = 2,
            layout=layout.fruchterman.reingold
            #edge.width = E(got_net)$weight/20
)

#ergm model
ita_net <- asNetwork(ita_net_igraph)
network::set.vertex.attribute(ita_net,"in_favor", ita_in_favor)
network::set.vertex.attribute(ita_net, "dispute_name", ita_meta$short_title)
network::set.vertex.attribute(ita_net, "award_yr", ita_meta$year_of_award)
network::set.vertex.attribute(ita_net, "arbitration_rule", ita_arb_rules)
network::set.vertex.attribute(ita_net, "BIT", ita_meta$case_treaties)

ita_model1 <- ergm(ita_net ~ edges)

#add node main effect (no dyatic interdependence)
ita_model2 <- ergm(ita_net ~ edges + nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base = 2))
ita_model3 <- ergm(ita_net ~ edges 
                   + nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                   + nodematch("arbitration_rule", diff = T))
ita_model4 <- ergm(ita_net ~ edges
                   + nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                   + nodematch("arbitration_rule", diff = T)
                   + nodematch("BIT"))

#add relational attribute effects
ita_model5 <- ergm(ita_net ~ edges
                   + nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                   + nodematch("arbitration_rule", diff = T)
                   #+ nodematch("BIT")
                   + edgecov(as.matrix(shared_arbitration_rules)) 
                   + edgecov(as.matrix(shared_BITs))
                   )

#add structural effects:
ita_model6 <- ergm(ita_net ~ edges
                   + nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                   + nodematch("arbitration_rule", diff = T)
                   #+ nodematch("BIT")
                   + edgecov(as.matrix(shared_arbitration_rules)) 
                   + edgecov(as.matrix(shared_BITs))
                   + gwidegree(0.693,fixed=T) + gwodegree(0.693,fixed=T)
                   + gwesp(0.693,fixed=T),
                   control = control.ergm(parallel = 8, MCMLE.check.degeneracy = T, MCMC.samplesize = 20000, MCMC.interval = 100), verbose = T)


ita_model7 <- ergm(ita_net ~ edges
                   + nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                   + nodematch("arbitration_rule", diff = T)
                   #+ nodematch("BIT")
                   #+ edgecov(as.matrix(shared_arbitration_rules)) 
                   + edgecov(as.matrix(shared_BITs))
                   + gwidegree(0.7,fixed=T) + gwodegree(0.7,fixed=T)
                   + gwesp(0.7,fixed=T),
                   control = control.ergm(parallel = 8, MCMLE.check.degeneracy = T, MCMC.samplesize = 50000, MCMC.interval = 1000), verbose = T)

ita_model7a <- ergm(ita_net ~ edges
                   + nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                   + nodematch("arbitration_rule", diff = T)
                   #+ nodematch("BIT")
                   #+ edgecov(as.matrix(shared_arbitration_rules)) 
                   + edgecov(as.matrix(shared_BITs))
                   + gwidegree(0.693,fixed=T) + gwodegree(0.693,fixed=T)
                   + gwesp(0.693,fixed=T),
                   control = control.ergm(parallel = 8, MCMLE.check.degeneracy = T, MCMC.samplesize = 100000, MCMC.interval = 5000, init= coef(ita_model7), MCMLE.maxit = 5), verbose = T)

ita_model8 <- ergm(ita_net ~ edges
                   + nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                   + nodematch("arbitration_rule", diff = T)
                   + nodematch("BIT")
                   #+ edgecov(as.matrix(shared_arbitration_rules)) 
                   #+ edgecov(as.matrix(shared_BITs))
                   + gwidegree(0.6,fixed=T) + gwodegree(0.6,fixed=T)
                   + gwesp(0.6,fixed=T),
                   control = control.ergm(parallel = 8, MCMLE.check.degeneracy = T, MCMC.burnin = 50000, MCMC.samplesize = 100000, MCMC.interval = 2000), verbose = T)

ita_model8a <- ergm(ita_net ~ edges
                   + nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                   + nodematch("arbitration_rule", diff = T)
                   + nodematch("BIT")
                   #+ edgecov(as.matrix(shared_arbitration_rules)) 
                   #+ edgecov(as.matrix(shared_BITs))
                   + gwidegree(0.6,fixed=T) + gwodegree(0.6,fixed=T)
                   + gwesp(0.6,fixed=T),
                   control = control.ergm(parallel = 8, MCMLE.check.degeneracy = T, MCMC.burnin = 50000, MCMC.samplesize = 100000, MCMC.interval = 2000, init= coef(ita_model8)), 
                   verbose = T)


ita_model9 <- ergm(ita_net ~ edges
                   #+ nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                   + nodematch("arbitration_rule", diff = T)
                   #+ nodematch("BIT")
                   #+ edgecov(as.matrix(shared_arbitration_rules)) 
                   + edgecov(as.matrix(shared_BITs))
                   + gwidegree(0.693,fixed=T) + gwodegree(0.693,fixed=T)
                   + gwesp(0.693,fixed=T),
                   control = control.ergm(parallel = 8, MCMLE.check.degeneracy = T, MCMC.samplesize = 100000, MCMC.interval = 5000, MCMLE.maxit = 5), verbose = T)


ita_model9a <- ergm(ita_net ~ edges
                   #+ nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                   + nodematch("arbitration_rule", diff = T)
                   #+ nodematch("BIT")
                   #+ edgecov(as.matrix(shared_arbitration_rules)) 
                   + edgecov(as.matrix(shared_BITs))
                   + gwidegree(0.693,fixed=T) + gwodegree(0.693,fixed=T)
                   + gwesp(0.693,fixed=T),
                   control = control.ergm(parallel = 8, MCMLE.check.degeneracy = T, MCMC.samplesize = 100000, MCMC.interval = 5000, MCMLE.maxit = 10, init= coef(ita_model9)), verbose = T)

ita_model9b <- ergm(ita_net ~ edges
                    #+ nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                    + nodematch("arbitration_rule", diff = T)
                    #+ nodematch("BIT")
                    #+ edgecov(as.matrix(shared_arbitration_rules)) 
                    + edgecov(as.matrix(shared_BITs))
                    + gwidegree(0.693,fixed=T) + gwodegree(0.693,fixed=T)
                    + gwesp(0.693,fixed=T),
                    control = control.ergm(parallel = 8, MCMLE.check.degeneracy = T, MCMC.samplesize = 100000, MCMC.interval = 5000, MCMLE.maxit = 10, init= coef(ita_model9a)), verbose = T)

ita_model9c <- ergm(ita_net ~ edges
                    #+ nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                    + nodematch("arbitration_rule", diff = T)
                    #+ nodematch("BIT")
                    #+ edgecov(as.matrix(shared_arbitration_rules)) 
                    + edgecov(as.matrix(shared_BITs))
                    + gwidegree(0.693,fixed=T) + gwodegree(0.693,fixed=T)
                    + gwesp(0.693,fixed=T),
                    control = control.ergm(parallel = 8, MCMLE.check.degeneracy = T, MCMC.samplesize = 100000, MCMC.interval = 5000, MCMLE.maxit = 10, init= coef(ita_model9b)), verbose = T)

ita_model10 <- ergm(ita_net ~ edges
                    #+ nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                    + nodematch("arbitration_rule", diff = T)
                    + nodematch("BIT")
                    #+ edgecov(as.matrix(shared_arbitration_rules)) 
                    #+ edgecov(as.matrix(shared_BITs))
                    + gwidegree(0.5,fixed=T) + gwodegree(0.5,fixed=T)
                    + gwesp(0.5,fixed=T),
                    control = control.ergm(parallel = 8, MCMLE.check.degeneracy = T, MCMC.burnin = 50000, MCMC.samplesize = 200000, MCMC.interval = 5000, MCMLE.maxit = 5), verbose = T)

ita_model10a <- ergm(ita_net ~ edges
                    #+ nodeifactor("in_favor", base = 2) + nodeofactor("in_favor", base =2)
                    + nodematch("arbitration_rule", diff = T)
                    + nodematch("BIT")
                    #+ edgecov(as.matrix(shared_arbitration_rules)) 
                    #+ edgecov(as.matrix(shared_BITs))
                    + gwidegree(0.5,fixed=T) + gwodegree(0.5,fixed=T)
                    + gwesp(0.5,fixed=T),
                    control = control.ergm(parallel = 8, MCMLE.check.degeneracy = T, MCMC.burnin = 50000, MCMC.samplesize = 200000, MCMC.interval = 5000, MCMLE.maxit = 20, init = coef(ita_model10)), verbose = T)



#diagnostic analysis:
mcmc.diagnostics(ita_model6)

#simulation:
ita_sims_model6 <- simulate(ita_model6, nsim = 100)

#gof:
ita_model6.gof.model <- gof(wto_model8 ~ model)
ita_model6.gof.others <- gof(wto_model8 ~ degree + esp + distance)
plot(ita_model6.gof.model)
plot(ita_model6.gof.others)


