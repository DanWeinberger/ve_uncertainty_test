
VE_dist_func <- function(obs.VEs , nsim = 10000) {
  ve.all <- c(obs.VEs[1], obs.VEs[2], obs.VEs[3])
  
  ve.all[ve.all == 100] <- 99.99 # Prevents an infinite log_OR
  
  log_OR <- log(1 - ve.all / 100)
  
  names(log_OR) <- c('mean', 'ucl', 'lcl')
  
  log_or_sd <-  (log_OR['ucl'] - log_OR['mean'])/1.96
  
  sim.log.or <- rnorm(n = nsim, mean = log_OR['mean'], sd = log_or_sd)
  
  sim.or <- exp(sim.log.or)
  
  sim.VE <- 100 * (1 - exp(sim.log.or))
  
  return(sim.VE)
}



  
