library(HTSCluster)
library(MBCbook)
library(dplyr)
data(velibCount)

## The format of the Velib count data:
## data: number of available bikes of the 1189 stations at 181 time points 
## position: the longitude and latitude of the 1189 bike stations
## dates: the download dates
## bonus: indicates whether the station is on the hill
## names: names of the stations

X <- velibCount$data 

dates = strftime(as.POSIXct(velibCount$dates, origin = "1970-01-01"), format="%a-%I%P")
days = strftime(as.POSIXct(velibCount$dates, origin = "1970-01-01"), format="%u")
conds = strftime(as.POSIXct(velibCount$dates, origin = "1970-01-01"), format="%H")

X = X[, 14:181]
dates = dates[14:181]
days = days[14:181]
conds = conds[14:181]
## average over weekdays:
Mon <- unlist(array(round(rowMeans(X[days=="1"]), 0)))
Tue <- unlist(array(round(rowMeans(X[days=="2"]), 0)))
Wed <- unlist(array(round(rowMeans(X[days=="3"]), 0)))
Thu <- unlist(array(round(rowMeans(X[days=="4"]), 0)))
Fri <- unlist(array(round(rowMeans(X[days=="5"]), 0)))
Sat <- unlist(array(round(rowMeans(X[days=="6"]), 0)))
Sun <- unlist(array(round(rowMeans(X[days=="7"]), 0)))

## average over weekday-daytime (7 x 4 = 28 dimensions)
# Weekdays:
MonMor <- unlist(array(round(rowMeans(X[days=="1" & (conds=="06" | conds=="07" | conds=="08" | conds=="09" | conds=="10" | conds=="11")]), 0)))
MonDay <- unlist(array(round(rowMeans(X[days=="1" & (conds=="12" | conds=="13" | conds=="14" | conds=="15" | conds=="16" | conds=="17")]), 0)))
MonEve <- unlist(array(round(rowMeans(X[days=="1" & (conds=="18" | conds=="19" | conds=="20" | conds=="21" | conds=="22" | conds=="23")]), 0)))
MonNig <- unlist(array(round(rowMeans(X[days=="1" & (conds=="00" | conds=="01" | conds=="02" | conds=="03" | conds=="04" | conds=="05")]), 0)))
TueMor <- unlist(array(round(rowMeans(X[days=="2" & (conds=="06" | conds=="07" | conds=="08" | conds=="09" | conds=="10" | conds=="11")]), 0)))
TueDay <- unlist(array(round(rowMeans(X[days=="2" & (conds=="12" | conds=="13" | conds=="14" | conds=="15" | conds=="16" | conds=="17")]), 0)))
TueEve <- unlist(array(round(rowMeans(X[days=="2" & (conds=="18" | conds=="19" | conds=="20" | conds=="21" | conds=="22" | conds=="23")]), 0)))
TueNig <- unlist(array(round(rowMeans(X[days=="2" & (conds=="00" | conds=="01" | conds=="02" | conds=="03" | conds=="04" | conds=="05")]), 0)))
WedMor <- unlist(array(round(rowMeans(X[days=="3" & (conds=="06" | conds=="07" | conds=="08" | conds=="09" | conds=="10" | conds=="11")]), 0)))
WedDay <- unlist(array(round(rowMeans(X[days=="3" & (conds=="12" | conds=="13" | conds=="14" | conds=="15" | conds=="16" | conds=="17")]), 0)))
WedEve <- unlist(array(round(rowMeans(X[days=="3" & (conds=="18" | conds=="19" | conds=="20" | conds=="21" | conds=="22" | conds=="23")]), 0)))
WedNig <- unlist(array(round(rowMeans(X[days=="3" & (conds=="00" | conds=="01" | conds=="02" | conds=="03" | conds=="04" | conds=="05")]), 0)))
ThuMor <- unlist(array(round(rowMeans(X[days=="4" & (conds=="06" | conds=="07" | conds=="08" | conds=="09" | conds=="10" | conds=="11")]), 0)))
ThuDay <- unlist(array(round(rowMeans(X[days=="4" & (conds=="12" | conds=="13" | conds=="14" | conds=="15" | conds=="16" | conds=="17")]), 0)))
ThuEve <- unlist(array(round(rowMeans(X[days=="4" & (conds=="18" | conds=="19" | conds=="20" | conds=="21" | conds=="22" | conds=="23")]), 0)))
ThuNig <- unlist(array(round(rowMeans(X[days=="4" & (conds=="00" | conds=="01" | conds=="02" | conds=="03" | conds=="04" | conds=="05")]), 0)))
FriMor <- unlist(array(round(rowMeans(X[days=="5" & (conds=="06" | conds=="07" | conds=="08" | conds=="09" | conds=="10" | conds=="11")]), 0)))
FriDay <- unlist(array(round(rowMeans(X[days=="5" & (conds=="12" | conds=="13" | conds=="14" | conds=="15" | conds=="16" | conds=="17")]), 0)))
FriEve <- unlist(array(round(rowMeans(X[days=="5" & (conds=="18" | conds=="19" | conds=="20" | conds=="21" | conds=="22" | conds=="23")]), 0)))
FriNig <- unlist(array(round(rowMeans(X[days=="5" & (conds=="00" | conds=="01" | conds=="02" | conds=="03" | conds=="04" | conds=="05")]), 0)))
# Weekends:
SatMor <- unlist(array(round(rowMeans(X[days=="6" & (conds=="06" | conds=="07" | conds=="08" | conds=="09" | conds=="10" | conds=="11")]), 0)))
SatDay <- unlist(array(round(rowMeans(X[days=="6" & (conds=="12" | conds=="13" | conds=="14" | conds=="15" | conds=="16" | conds=="17")]), 0)))
SatEve <- unlist(array(round(rowMeans(X[days=="6" & (conds=="18" | conds=="19" | conds=="20" | conds=="21" | conds=="22" | conds=="23")]), 0)))
SatNig <- unlist(array(round(rowMeans(X[days=="6" & (conds=="00" | conds=="01" | conds=="02" | conds=="03" | conds=="04" | conds=="05")]), 0)))
SunMor <- unlist(array(round(rowMeans(X[days=="7" & (conds=="06" | conds=="07" | conds=="08" | conds=="09" | conds=="10" | conds=="11")]), 0)))
SunDay <- unlist(array(round(rowMeans(X[days=="7" & (conds=="12" | conds=="13" | conds=="14" | conds=="15" | conds=="16" | conds=="17")]), 0)))
SunEve <- unlist(array(round(rowMeans(X[days=="7" & (conds=="18" | conds=="19" | conds=="20" | conds=="21" | conds=="22" | conds=="23")]), 0)))
SunNig <- unlist(array(round(rowMeans(X[days=="7" & (conds=="00" | conds=="01" | conds=="02" | conds=="03" | conds=="04" | conds=="05")]), 0)))

## Multivariate Poisson Model
## function for computing the multivariate mixture density
dnsmix <- function(x, pi, lambda){
  ncomp <- nrow(lambda)
  dim <- ncol(lambda)
  dcomp <- matrix(NA, nrow = ncomp, ncol = dim)
  comp <- c()
  for (k in 1:ncomp){
    for (d in 1:dim){
      dcomp[k, d] <- dpois(x[d], lambda[k, d])  
    }
    comp[k] <- pi[k]*prod(dcomp[k, ])
  }
  comp[comp<1e-316] <- 1e-316  ## seems to be one of the smallest numbers with which R can still work
  list(     ifelse(sum(comp)<1e-316, 1e-316, sum(comp)),
            comp) 
}

## collect the prepared data:
poisRMix = cbind(MonMor, MonDay, MonEve, MonNig, TueMor, TueDay, TueEve, TueNig,
                 WedMor, WedDay, WedEve, WedNig, ThuMor, ThuDay, ThuEve, ThuNig,
                 FriMor, FriDay, FriEve, FriNig,
                 SatMor, SatDay, SatEve, SatNig, SunMor, SunDay, SunEve, SunNig)


d_ <- ncol(poisRMix)
n = nrow(poisRMix)
m.max = 15
B = 100 # size of bootstrap sample (for LRT with bootstrap)

loglik  <- c()
BIC <- c()
ICL <- c()
Z <- c()

## Loop:
for (m in 1: m.max){  
  cat("testing for ", iter <- m, "components \n")
  set.seed(m)
  ## initialize values
  pi.old <- runif(m)
  pi.old = pi.old/sum(pi.old)
  lambda.old <- matrix(runif(m*d_, 1, 50), nrow=m, ncol=d_)
  
  loglik.old = 0
  # compute loglik corresponding to the initialized parameters:
  loglik.up = sum(log( sapply(1:n, function(z) dnsmix(unlist(array(poisRMix[z, ])), pi.old, lambda.old)[[1]]) ))
  diff =  loglik.up  - loglik.old
  loglik.old = loglik.up
  
  k = 0 # <- iteration
  ## EM-loop:
  while((abs(diff) >=10 ^{-7}) && (k < 200)){ ##k < 300
    k = k+1
    print(k)
    ## E-step: 
    Z.tilde <- t(sapply(1:n, function(z) dnsmix(unlist(array(poisRMix[z, ])), pi.old, lambda.old)[[2]] ))/ sapply(1:n, function(z) dnsmix(unlist(array(poisRMix[z, ])), pi.old, lambda.old)[[1]])
    Z.tilde <- matrix(Z.tilde, nrow=n, ncol=m)
    
    ## M-step:
    pi.up = rep(0,m)
    lambda.up = matrix(rep(0,m*d_), nrow=m, ncol=d_)
    for(j in 1:m){
      # update parameter values:
      pi.up[j] <- mean(Z.tilde[, j])
      for (d in 1:d_) {
        lambda.up[j, d] <- sum(Z.tilde[, j]*unlist(array(poisRMix[, d])))/sum(Z.tilde[, j]) # pointwise multiplication
      }
    }
    # update log-likelihood and the difference:
    loglik. <- log( sapply(1:n, function(z) dnsmix(unlist(array(poisRMix[z, ])), pi.up, lambda.up)[[1]]) )
    loglik.up  = sum(loglik.[loglik.>-Inf])
    diff =  loglik.up - loglik.old 
    # reassign the parameters:
    pi.old = pi.up
    lambda.old =   lambda.up
    loglik.old =  loglik.up  
  } # end while (end EM-loop)
  
  loglik[m] <- loglik.old
  BIC[m] <- 2*loglik[m] - (m-1 + m*d_)*log(n)
  ## computational problems occur when values of Z.tilde are close to 0 (then log -> -Inf)
  ## we deal with it by setting the minimal of the product to 0
  Z[m] <- sum(ifelse(is.nan(Z.tilde*log(Z.tilde))==T, 0, Z.tilde*log(Z.tilde)))
  ICL[m] <- BIC[m] + Z[m] 
} ## end loop for m

which.max(BIC) 
which.max(ICL) 

library(ggplot2)
library(reshape2)

crit <- data.frame(m=c(1:15), bic=BIC, icl=ICL)
crit. <- melt(crit, id.vars="m")

gg <- ggplot(crit., aes(x=m, y=value, group=variable))+
  geom_line(aes(color=variable))+geom_point(aes(color=variable)) +
  ggtitle("BIC and ICL methods for Velib data (28 dimentions)")
gg

## Prediction:
Z. <- matrix(rep(0,nrow(Z.tilde)*ncol(Z.tilde)), nrow=nrow(Z.tilde), ncol=ncol(Z.tilde))
pred <- sapply(1:n, function(z) which.max(Z.tilde[z,]))

lat <-  unlist(array(velibCount$position[2]))
long <- unlist(array(velibCount$position[1]))
dfpl <- data.frame(lat=lat, long=long, group=pred)

# plot the predicted groups:
ggplot(dfpl, aes(x=lat, y=long)) +
  geom_point(aes(color=factor(group))) +
  ggtitle("Groups predicted by BIC and ICL methods for the Velib data (28 dimensions)")


## Compare with the book (by C.Bouveyron et al., 2019) results:
X_book <- velibCount$data 

Sys.setlocale("LC_TIME", "en_US.UTF-8")
dates_book = strftime(as.POSIXct(velibCount$dates, origin="1970-01-01"), format="%a-%I%P")

## remove Saturdays & Sundays:
days_book = strftime(as.POSIXct(velibCount$dates, origin = "1970-01-01"), format="%u")
X_book = X_book[days_book=="1" | days_book=="2" | days_book=="3" | days_book=="4" | days_book=="5"]

## Hour effect:
conds_book = strftime(as.POSIXct(velibCount$dates, origin = "1970-01-01"), format="%H")
conds_book = conds_book[days_book %in% 1:5]

## Clustering with HTSCluster:
run <- PoisMixClusWrapper(as.matrix(X_book), gmin = 2, gmax = 15, conds = conds_book)
# implement the EM and CEM algorithms for parameter estimation in a Poisson mixture model for clustering high throughput sequencing observations

summary(run)

## display the cluster proportions:
pred_book <- run$all.results[[14]]$labels

lat <-  unlist(array(velibCount$position[2]))
long <- unlist(array(velibCount$position[1]))
dfpl_book <- data.frame(lat=lat, long=long, group=pred_book)
#dfpl. <- melt(dpfl, id.vars="group")
ggplot(dfpl_book, aes(x=lat, y=long)) +
  geom_point(aes(color=factor(group))) +
    ggtitle("Groups predicted by the PoisMixClusWrapper function for the Velib data")



## LRT approach
em.optimize <- function(sample, numcomp){
  ## initialize values
  pi.old <- runif(numcomp)
  pi.old = pi.old/sum(pi.old)
  lambda.old <- matrix(runif(numcomp*d_, 1, 10), nrow=numcomp, ncol=d_)
  
  loglik.old = 0
  # compute loglik corresponding to the initialized parameters:
  loglik.up = sum(log( sapply(1:n, function(z) dnsmix(unlist(array(sample[z, ])), pi.old, lambda.old)[[1]]) ))
  diff =  loglik.up  - loglik.old
  loglik.old = loglik.up
  
  k = 0 # <- iteration
  ## EM-loop:
  while((abs(diff) >=10 ^{-7}) && (k < 200)){ ##k < 300
    k = k + 1
    print(k)
    ## E-step: 
    Z.tilde <- t(sapply(1:n, function(z) dnsmix(unlist(array(sample[z, ])), pi.old, lambda.old)[[2]] ))/ sapply(1:n, function(z) dnsmix(unlist(array(sample[z, ])), pi.old, lambda.old)[[1]])
    Z.tilde <- matrix(Z.tilde, nrow=n, ncol=numcomp)
    
    ## M-step:
    pi.up = rep(0,numcomp)
    lambda.up = matrix(rep(0,numcomp*d_), nrow=numcomp, ncol=d_)
    for(j in 1:numcomp){
      # update parameter values:
      pi.up[j] <- mean(Z.tilde[, j])
      for (d in 1:d_) {
        lambda.up[j, d] <- sum(Z.tilde[, j]*unlist(array(sample[, d])))/sum(Z.tilde[, j]) # pointwise multiplication
      }
    }
    # update log-likelihood and the difference:
    loglik.up  = sum(log( sapply(1:n, function(z) dnsmix(unlist(array(sample[z, ])), pi.up, lambda.up)[[1]]) ))
    diff =  loglik.up - loglik.old 
    # reassign the parameters:
    pi.old = pi.up
    lambda.old =   lambda.up
    loglik.old =  loglik.up  
  } # end while (end EM-loop) 
  return(list(pi.old, lambda.old, loglik.old))
}


m = 1
loglik.est  <- c()
pi.est <- list()
lambda.est <- list()
lrt <- c()
uq <- c()
lrt[1] = 0
uq[1] = 0

repeat{  ##  begin loop for m
  cat("testing m = ", m, "\n")
  ## maximize loglik for m
  pi.est[[m]] <- em.optimize(poisRMix, m)[[1]]
  lambda.est[[m]] <- em.optimize(poisRMix, m)[[2]]
  loglik.est[m] <- em.optimize(poisRMix, m)[[3]]
  
  ## bootstrap:       
  if (m > 1){  ## begin loop (m>1)
    loglik0 <- c()
    loglik1 <- c()
    
    for (b in 1:B){
      cat("bootstrap iter=", b, "\n")
      set.seed(123*b)
      ## generate sample from mixture with optimal par-s for (m-1) components:
      Z0 <- rmultinom(n, 1, pi.est[[(m-1)]])
      lambda.est. <- lambda.est[[(m-1)]]
      poisRMix0 <- matrix(NA, nrow=n, ncol=d_)
      for (d0 in 1:d_) {
        for (p0 in 1:n) {
          z0 <- which.max(Z0[, p0])
          poisRMix0[p0, d0] <- rpois(1, lambda.est.[z0, d0])
        } 
      }  #  end loop for sampled data
      
      ## optimize for (m-1) components:
      loglik0[b] <- em.optimize(poisRMix0, (m-1))[[3]]
      ## optimize for m components:
      loglik1[b] <- em.optimize(poisRMix0, (m))[[3]]    
    } # end loop for bootstrap
    
    ## compute bootstrapped LRTs:
    lik.ratio <- -2*(loglik0-loglik1) # vector of length B   
    uq[m] <- sort(lik.ratio)[B*0.95] ## 95%-quantile 
    ## compute LRT
    lrt[m] <- -2*(loglik.est[m-1]-loglik.est[m])
  } # end loop for  (m>1)
  
  if( (lrt[m] < uq[m]) || (m >= 15))
  {break}
  m <- m + 1
} ## end loop for m
m.est = m-1


## Plot the results:
library(ggplot2)
library(reshape2)

## Prediction:
pi. = pi.est[[m.est]]
lambda. = lambda.est[[m.est]]

Z.tilde <- t(sapply(1:n, function(z) dnsmix(unlist(array(poisRMix[z, ])), pi., lambda.)[[2]] ))/ sapply(1:n, function(z) dnsmix(unlist(array(poisRMix[z, ])), pi., lambda.)[[1]])
Z.tilde <- matrix(Z.tilde, nrow=n, ncol=m.est)
Z. <- matrix(rep(0,nrow(Z.tilde)*ncol(Z.tilde)), nrow=nrow(Z.tilde), ncol=ncol(Z.tilde))
pred <- sapply(1:n, function(z) which.max(Z.tilde[z,]))

lat <-  unlist(array(velibCount$position[2]))
long <- unlist(array(velibCount$position[1]))
dfpl <- data.frame(lat=lat, long=long, group=pred)
ggplot(dfpl, aes(x=lat, y=long)) +
  geom_point(aes(color=factor(group))) +
    ggtitle("Groups predicted by The LRT method for the Velib data (28 dimensions)")


# plot the mixing proportions:
mixprop <- data.frame(group=c("1", "2", "3"),
                      proportion=pi.est[[m.est]])

ggplot(data=mixprop, aes(x=group, y=proportion, fill=group)) +
  geom_bar(stat="identity") + 
    ggtitle("Mixing proportions computed via the LRT method for the Velib data (28 dimensions)")
