# Run from the project root (open ipps.Rproj in RStudio).
library(tidyverse)
library(simcf)
library(tile)
library(MASS)
library(survival)
library(RColorBrewer)

ipps <- read.csv(file.path("data", "ipps.csv"))


# 1. Setup -------------------------------------------------------------

# Clean Data
ipps1 <- ipps %>%
  mutate(
    # Convert Success to yes/no (numeric 1/0)
    successyn = ifelse(Success > 0, 1, 0),
    # Convert Success to yes/no (numeric 1/0)
    chn = ifelse(tolower(Chinese.sponsor) == "no", 0, 1),
    # Convert Date to date class, set to year/month
    Date = floor_date(as.Date(Date, tryFormats = c("%m/%d/%Y", "%Y-%m")),
                      unit = "month")
    ) %>%
  # Select relevant columns
  dplyr::select(Date, IPP, successyn, chn, Plant.capacity)

# Convert data to cross-sectional

date <- as.Date(character())
ipp <- c()
chn <- c()
successyn <- c()
capacity <- c()
prev <- c()
daterange <- seq.Date(min(ipps1$Date), max(ipps1$Date), by = "month")
for (d in 1:length(daterange)) {
  for (i in unique(ipps1$IPP)) {
    date <- c(date, daterange[d])
    ipp <- c(ipp, i)
    chn <- c(chn, unique(ipps1$chn[ipps1$IPP == i]))
    capacity <- c(capacity,
                  unique(ipps1$Plant.capacity[ipps1$IPP == i]))
    
    match_idx <- ipps1$Date == daterange[d] & ipps1$IPP == i
    if (any(match_idx)) {
      successyn <- c(successyn,
                     ipps1$successyn[ipps1$Date == daterange[d] &
                                       ipps1$IPP == i])
    }
    else {
      successyn <- c(successyn, 0)
    }
    previndex <- length(unique(ipps1$IPP)) * (d-1) + which(unique(ipps1$IPP) == i)
    prev <- c(prev, max(c(prev[previndex], successyn)))
  }
}
ippdf <- data.frame(date = date,
                    ipp = ipp,
                    chn = chn,
                    capacity = capacity,
                    successyn = successyn,
                    prev = prev) %>%
  mutate(prev = ifelse(successyn == 1, 0, prev))


# Model for hazard ratio or glm
model1 <- successyn ~ chn + Plant.capacity

# 2. Optimization ---------------------------------------------------------

# Function for partial Cox log likelihood
llk.coxpartial <- function(beta, X, time, event, prev) {
  # beta: numeric vector of length p
  # X:    n x p matrix of covariates
  # time: event/censoring time vector of length n
  # event: 1 = event occurs at 'time', 0 = no event, vector of length n
  # prev:  0 = still at risk at this row, 1 = event already happened before, vector of length n
  
  # coerce X to matrix
  X <- as.matrix(X)
  
  # sort by time (in case data not ordered)
  o <- order(time)
  time  <- time[o]
  event <- event[o]
  prev  <- prev[o]
  X     <- X[o, , drop = FALSE]
  
  # linear predictor
  eta <- as.vector(X %*% beta)
  
  loglik <- 0
  
  # indices where an event occurs for the first time
  event_idx <- which(event == 1)
  
  for (k in event_idx) {
    t_k <- time[k]
    
    # risk set: still at risk at t_k
    risk_set <- which(time >= t_k & prev == 0)
    
    loglik <- loglik + eta[k] - log(sum(exp(eta[risk_set])))
  }
  
  return(-loglik)
}

# With survival library
cfit <- coxph(Surv(as.numeric(date), successyn) ~ chn + capacity, data = ippdf)
beta_init <- coef(cfit)

# Set parameters and initial guess for beta
x <- cbind(ippdf$chn, ippdf$capacity)
time <- ippdf$date
event <- ippdf$successyn
prev <- ippdf$prev

fit <- optim(beta_init,
             llk.coxpartial,
             method = "BFGS",
             hessian = TRUE,
             X = x,
             time = time,
             event = event,
             prev = prev)

# Confidence Intervals

vcov <- solve(fit$hessian)
pe <- fit$par

ci <- c(pe - 1.96 * sqrt(diag(vcov)), pe + 1.96 * sqrt(diag(vcov)))


# 3. Compute baseline hazards ------------------------------------------------

## Get weights
baseline <- function(beta, X, time, event, prev) {
  X <- as.matrix(X)
  
  o <- order(time)
  time  <- time[o]
  event <- event[o]
  prev  <- prev[o]
  X     <- X[o, , drop = FALSE]
  
  eta <- as.vector(X %*% beta)
  w   <- exp(eta)
  
  event_rows  <- which(event == 1 & prev == 0)
  event_times <- sort(unique(time[event_rows]))
  
  h0 <- H0 <- S0 <- numeric(length(event_times))
  sumH0 <- 0
  
  for (k in seq_along(event_times)) {
    t_k <- event_times[k]
    
    idx_events_tk <- which(time == t_k & event == 1 & prev == 0)
    d_k <- length(idx_events_tk)                  # number of events
    
    risk_set <- which(time >= t_k & prev == 0)    # risk set at t_k
    denom    <- sum(w[risk_set])                  # sum exp(η_j)
    
    h0_k <- d_k / denom                           # hazard increment
    sumH0 <- sumH0 + h0_k                         # cumulative
    
    h0[k] <- h0_k 
    H0[k] <- sumH0
    S0[k] <- exp(-sumH0)
  }
  
  data.frame(
    time = event_times,
    h0   = h0, # hazard increments
    H0   = H0, # Cumulative hazard
    S0   = S0 # Survival
  )
}

## Baseline with actual estimate
pe.baseline <- baseline(pe,
                        X = cbind(ippdf$chn, ippdf$capacity),
                        time = ippdf$date,
                        event = ippdf$successyn,
                        prev = ippdf$prev)

# 4. Simulate Predicted Probabilities -------------------------------------------------

# Simulate for predicted probabilities
## Simulate betas
sims <- 1e4
set.seed(2025)
betasims <- mvrnorm(sims, pe, vcov)

# Set alpha
alpha <- 0.05

## Create counterfactuals
chnseq <- c(0,1)
capacityseq <- c(10, 100, 100, 1638)
timeseq <- unique(ippdf$date[ippdf$successyn == 1])

cf <- data.frame(cfnumber = 1:(length(chnseq)*length(capacityseq)),
                 chn = c(0,0,0,0,1,1,1,1),
                 capacity = rep(capacityseq, 2))

## dataframe to store pred prob at pe, lower and upper bounds
lower <- upper <- probs <- matrix(NA,
                                  nrow = nrow(cf),
                                  ncol = length(timeseq))

# Iterate across counterfactuals
for (j in 1:nrow(cf)) {
  
  this.x <- cbind(cf$chn[j], cf$capacity[j])
  
  pe.eta <- as.numeric(this.x %*% pe)
  probs[j, ] <- 1 - pe.baseline$S0 ^ exp(pe.eta)
  
  # matrix to store probabilities of event by each t for this x_new
  this.prob <- matrix(NA, nrow = sims, ncol = length(timeseq))
  ## For each set of betas,
  for (i in 1:nrow(betasims)) {
    # compute baseline and cumulative survival
    this.beta <- betasims[i,]
    this.baseline <- baseline(this.beta,
                              X = cbind(ippdf$chn, ippdf$capacity),
                              time = ippdf$date,
                              event = ippdf$successyn,
                              prev = ippdf$prev)
    this.eta <- as.numeric(this.x %*% this.beta)
    this.prob[i,] <- 1 - this.baseline$S0 ^ exp(this.eta)
  }
  
  ## Select lower and upper quantiles
  lower[j, ] <- apply(this.prob, 2, quantile, probs = alpha/2)
  upper[j, ] <- apply(this.prob, 2, quantile, probs = 1-alpha/2)
}


# 5. Visualize ---------------------------------------------------------------

traces <- list()
colvec <- colorRampPalette(brewer.pal(9, "YlGnBu"))(4)
for (i in 1:nrow(cf)) {
  traces[[i]] <- lineplot(x = timeseq,
                          y = probs[i, ],
                          lower = lower[i, ],
                          upper = upper[i, ],
                          plot = cf$chn[i] + 1,
                          col = colvec[((i-1) %% length(capacityseq)) + 1])
}


tile(traces,
     xaxistitle = list(labels = "Date"),
     xaxis = list(at = c(18628, 18993, 19358, 19723, 20088),
                  labels = c("2021", "2022", "2023", "2024", "2025")),
     yaxistitle = list(labels = "P(Faced renegotiation)"),
     plottitle = list(labels = c("Locally sponsored", "Chinese-Sponsored")))


# 2. Check logit -------------------------------------------------------------

fit1 <- glm(model1, family = binomial(link = "logit"), data = ipps1)

## Table output
stargazer::stargazer(fit1, type = "text")

# Check for Perfect Separation
pred <- predict(fit1, type = "response")

plot(ipps1$Plant.capacity, pred)
