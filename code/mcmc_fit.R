# This file is made available under a CC-BY-NC 4.0 International License.
# Details of the license can be found at
# <https://creativecommons.org/licenses/by-nc/4.0/legalcode>. 
# 
# Giving appropriate credit includes citation of the related publication and
# providing a link to the repository:
# 
# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C Cohen,
# MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2021) _SARS-CoV-2 reinfection
# trends in South Africa: analysis of routine surveillance data_. _medRxiv_
# <https://www.medrxiv.org/content/10.1101/2021.11.11.21266068>
# 
# Repository: <https://github.com/jrcpulliam/reinfections>
#
# The MCMC sampler is based on code originally written by Steve Bellan as part of the 
# International Clinics on Infectious Disease Dynamics and Data (ICI3D) program, 
# which is made available via a CC-BY International license. (Bellan 2015)
# https://github.com/ICI3D/RTutorials/blob/master/ICI3D_Lab8_MCMC-SI_HIV.R

suppressPackageStartupMessages({
  library(coda)
  library(parallel)
  library(data.table)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('utils', 'fit_fxn_null.RData'),
  file.path('test.json'), # NOTE: change this to do full run!
  file.path('output', 'posterior_90_null.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

if(grepl('test', .args[3]) & !grepl('test', tail(.args, 1))){
  target <- file.path('output', 'posterior_90_null_test.RData')
  warning(paste0('Changing output file to ', target, '.'))
}else{
  target <- tail(.args, 1)
}

# Script to implement MCMC estimation of re-infection parameters

# Prepare data
ts <- readRDS(.args[1])

load(.args[2]) # Fitting functions

configpth <- .args[3]
attach(jsonlite::read_json(configpth))

# From here on I adjusted Steve's code. ~CvS

## Function that makes a list of disease parameters with default values
disease_params <- function(lambda = .000000015 ## hazard coefficient
                           , kappa = 0.1 ## dispersion (inverse)
) return(as.list(environment()))

## Log-Prior (assume uninformative)
lprior <- function(parms=disease_params()) with(parms, {
  lp <- 0 ## whatever the parameters are, assume they have the same probability
  return(lp)
})

## Convenience function that sums log-likelihood & log-prior for
## evaluation inside MCMC sampler.
llikePrior <- function(fit.params=NULL, ## parameters to fit
                       ref.params = disease_params(), ## reference parameters
                       obsDat=ts[date <= fit_through]) { ## observed data
  parms <- within(ref.params, { ## subs fitting parameters into reference parameter vector
    for(nm in names(fit.params)) assign(nm, as.numeric(fit.params[nm]))
    rm(nm)
  })
  -nllikelihood(parms, obsDat=obsDat) + lprior(parms)
}

## Want to be able to easily log and unlog parameters
logParms <- function(fit.params) {
  fit.params <- log(fit.params)
  names(fit.params) <- paste0('log',names(fit.params))
  return(fit.params)
}
unlogParms <- function(fit.params) {
  fit.params <- exp(fit.params)
  names(fit.params) <- sub('log','', names(fit.params))
  return(fit.params)
}

## set bounds on initial parameter guesses
initBounds <- data.frame(rbind( ## for initial conditions
  c(log(1.2e-09),log(1.75e-07)) ## lambda
  ,c(log(1/1000), log(1/0.5)))) ## kappa

colnames(initBounds) <- c('lower','upper')
rownames(initBounds) <- c('loglambda','logkappa')
class(initBounds[,2]) <- class(initBounds[,1]) <- 'numeric'

## Randomly select a value that is uniformly distributed between these bounds
initRand <- function(fit.params) {
  fit.params <- logParms(fit.params)
  tempnm <- names(fit.params)
  for(nm in tempnm) fit.params[nm] <- runif(1, min = initBounds[rownames(initBounds)==nm, 'lower'], 
                                            max =  initBounds[row.names(initBounds)==nm, 'upper'])
  return(unlogParms(fit.params))
}

## Flexible Metropolis-Hastings Sampler
mcmcSampler <- function(init.params, ## initial parameter guess
                        randInit = mcm$rand_init, ## if T then randomly sample initial parameters instead of above value
                        seed = 1, ## RNG seed
                        ref.params=disease_params(), ## fixed parameters
                        obsDat = ts[date <= fit_through], ## data
                        proposer = default.proposer(sdProps=sdProps), ## proposal distribution
                        niter = mcmc$n_iter, ## MCMC iterations
                        nburn = mcmc$burnin, ## iterations to automatically burn
                        adaptiveMCMC = mcmc$adaptive, ## adapt proposal distribution?
                        startAdapt = 150, ## start adapting at what iteration?
                        adptBurn = mcmc$burnin, ## ignore first so many iterations for adapting posterior
                        verbose=0, ## if >2 browses, if >1 prints progress
                        tell = 100) { ## how often to print progress
  if(verbose>2) browser()
  set.seed(seed)
  if(randInit) init.params <- initRand(init.params)
  current.params <- init.params
  nfitted <- length(current.params) ## number fitted parameters
  vv <- 2 ## mcmc iteration (started at 1 so we're already on 2
  accept <- 0 ## initialize proportion of iterations accepted
  ## Calculate log(likelihood X prior) for first value
  curVal <- llikePrior(current.params, ref.params = ref.params, obsDat=obsDat)
  ## Initialize matrix to store MCMC chain
  out <- matrix(NA, nr = niter, nc=length(current.params)+1)
  out[1,] <- c(current.params, ll = curVal) ## add first value
  colnames(out) <- c(names(current.params), 'll') ## name columns
  ## Store original covariance matrix
  if(adaptiveMCMC & proposer$type=='block') originalCovar <- get('covar', envir = environment(proposer$fxn))
  while(vv <= niter) {
    if ((verbose > 1) || (verbose && (vv%%tell == 0))) print(paste("on iteration",vv,"of", niter + 1))
    ## Adaptive MCMC: adapt covariance every 50 iterations (don't
    ## do it more often because it adds to computational burden.
    if(adaptiveMCMC & proposer$type=='block' & vv > startAdapt & vv %% 50 == 0) {
      adptBurn <- min((startAdapt-50), adptBurn)
      ## Below equation gives ideal covariance-variance matrix based on posterior
      adaptedCovar <- 2.38^2 / nfitted * cov.wt(log(out[adptBurn:(vv-1),1:nfitted]))$cov
      ## Take a weighted average of the original & the empirical cov-var matrices to ensure
      ## that we never let the matrix collapse to zero (ie if the empirical one is zero
      ## because we haven't accepted anything yet)
      adaptedCovar <- adaptedCovar*.95 + originalCovar*.05 ## 95% adapted & 5% original
      rownames(adaptedCovar) <- colnames(adaptedCovar) <- names(current.params)
      assign('covar', adaptedCovar, envir = environment(proposer$fxn))
    }
    proposal <- proposer$fxn(logParms(current.params))
    proposal <- unlogParms(proposal)
    propVal <- llikePrior(proposal, ref.params = ref.params, obsDat=obsDat)
    lmh <- propVal - curVal ## likelihood ratio = log likelihood difference
    if (is.na(lmh)) { ## if NA, print informative info but don't accept it
      print(list(lmh=lmh, proposal=exp(proposal), vv=vv, seed=seed))
    } else { ## if it's not NA then do acception/rejection algorithm
      if (verbose > 1) print( c(lmh=lmh, propVal=propVal) )
      ## if MHR >= 1 or a uniform random # in [0,1] is <= MHR, accept otherwise reject
      if ( (lmh >= 0) | (runif(1,0,1) <= exp(lmh)) ) {
        current.params <- proposal
        if (vv>nburn) accept <- accept + 1 ## only track acceptance after burn-in
        curVal <- propVal
      }
    }
    out[vv, ] <- c(current.params, ll = curVal)
    vv <- vv+1
    aratio <- accept/((vv-nburn))
  }
  colnames(out) <- c(names(current.params), 'll')
  samp <- as.mcmc(out[1:nrow(out)>(nburn),], start = nburn + 1)
  return(list(ref.params=ref.params
              , seed = seed
              , init.params = init.params
              , aratio = aratio
              , samp = samp
  ))
}

## Sequential proposal function: Propose one parameter at a time
sequential.proposer <- function(sdProps) {
  nfitted <- length(sdProps)
  on <- 0
  return(list(sdProps = sdProps, type = 'sequential',
              fxn = function(current) {
                proposal <- current
                proposal[on + 1] <- proposal[on + 1] + rnorm(1, mean = 0, sd = sdProps[on + 1])
                on <<- (on+1) %% nfitted
                proposal
              }))
}

## default proposal function
default.proposer <- function(sdProps) {
  return(list(sdProps = sdProps, type = 'default',
              fxn = function(current) {
                proposal <- current
                proposal <- proposal + rnorm(2, mean = 0, sd = sdProps)
                proposal
              }))
}

mcmcParams <- list(init.params = c(lambda = NA, kappa = NA)
                   , seed = NA
                   , proposer = default.proposer(sdProps = c(.01, .3))
                   , randInit = mcmc$rand_init
                   , niter = mcmc$n_iter)

## Parallel MCMC: Use mclapply to call on
## mcmcSampler once for each seed on a different core.
doChains <- function(x, mcmcParams) {
  print(system.time(
    ## Below line uses mclapply to parallelize do.call over seeds
    chains <- mclapply(x, function(x) do.call(mcmcSampler, within(mcmcParams, {seed <- x})))
  ))
  aratio <- mean(unlist(lapply(chains, '[[', 'aratio'))) ## average across chains
  chains <- lapply(chains, '[[', 'samp') ## pull out posterior samples only
  chains <- as.mcmc.list(chains) ## make into mcmc.list
  return(list(chains=chains, aratio = aratio))
}

## Run n_chains chains with seeds at 1:n_chains.
mcmc.run <- doChains(1:mcmc$n_chains, mcmcParams) 

if(!grepl('test', .args[3])){
  gelman.diag(mcmc.run$chains)
  summary(mcmc.run$chains)
}

## Draw sample from posterior
niter <- mcmc$n_iter - mcmc$burnin  #iterations in MCMC
smpls <- mcmc$n_posterior / mcmc$n_chains 

lambda.post <- kappa.post <- numeric(0)
for(ii in 1:mcmc$n_chains){ 
  lambda.post <- c(lambda.post, mcmc.run$chains[[ii]][(niter+1-smpls):niter, 1])
  kappa.post <- c(kappa.post, mcmc.run$chains[[ii]][(niter+1-smpls):niter, 2])
}

save(mcmc.run, lambda.post, kappa.post, file = target)
