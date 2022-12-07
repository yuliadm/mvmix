library(MBCbook)
library(dplyr)
library(ggplot2)
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
station.id <- rownames(X)  


X = X[, 14:181]
dates = dates[14:181]
days = days[14:181]
conds = conds[14:181]
station.id = data.frame(id=station.id)

capacity <- read.csv("velib_capacity.csv", header=TRUE)
capacities <- left_join(station.id, capacity, by="id") 
cap <- capacities[complete.cases(capacities), ]



## average over weekdays:
Mon <- unlist(array(round(rowMeans(X[days=="1"]), 0)))
Tue <- unlist(array(round(rowMeans(X[days=="2"]), 0)))
Wed <- unlist(array(round(rowMeans(X[days=="3"]), 0)))
Thu <- unlist(array(round(rowMeans(X[days=="4"]), 0)))
Fri <- unlist(array(round(rowMeans(X[days=="5"]), 0)))
Sat <- unlist(array(round(rowMeans(X[days=="6"]), 0)))
Sun <- unlist(array(round(rowMeans(X[days=="7"]), 0)))

## average over weekday-daytime (20 dim-s)
## average over weekday-daytime (20 dim-s)
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
SatMor <- unlist(array(round(rowMeans(X[days=="6" & (conds=="06" | conds=="07" | conds=="08" | conds=="09" | conds=="10" | conds=="11")]), 0)))
SatDay <- unlist(array(round(rowMeans(X[days=="6" & (conds=="12" | conds=="13" | conds=="14" | conds=="15" | conds=="16" | conds=="17")]), 0)))
SatEve <- unlist(array(round(rowMeans(X[days=="6" & (conds=="18" | conds=="19" | conds=="20" | conds=="21" | conds=="22" | conds=="23")]), 0)))
SatNig <- unlist(array(round(rowMeans(X[days=="6" & (conds=="00" | conds=="01" | conds=="02" | conds=="03" | conds=="04" | conds=="05")]), 0)))
SunMor <- unlist(array(round(rowMeans(X[days=="7" & (conds=="06" | conds=="07" | conds=="08" | conds=="09" | conds=="10" | conds=="11")]), 0)))
SunDay <- unlist(array(round(rowMeans(X[days=="7" & (conds=="12" | conds=="13" | conds=="14" | conds=="15" | conds=="16" | conds=="17")]), 0)))
SunEve <- unlist(array(round(rowMeans(X[days=="7" & (conds=="18" | conds=="19" | conds=="20" | conds=="21" | conds=="22" | conds=="23")]), 0)))
SunNig <- unlist(array(round(rowMeans(X[days=="7" & (conds=="00" | conds=="01" | conds=="02" | conds=="03" | conds=="04" | conds=="05")]), 0)))



## Multivariate Poisson
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


poisRMix = cbind(MonMor, MonDay, MonEve, MonNig, TueMor, TueDay, TueEve, TueNig,
                 WedMor, WedDay, WedEve, WedNig, ThuMor, ThuDay, ThuEve, ThuNig,
                 FriMor, FriDay, FriEve, FriNig)#, SatMor, SatDay, SatEve, SatNig,
                 #SunMor, SunDay, SunEve, SunNig)


# poisRMix <- cbind(Mon, Tue, Wed, Thu, Fri, Sat, Sun)
d_ <- ncol(poisRMix)
n = nrow(poisRMix)
m.max = 15
B = 100 # size of bootstrap sample (for LRT with bootstrap)


## LRT
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

repeat{  ##  begin loop for m
  cat("testing m = ", m, "\n")
  ## maximize loglik for m:
  pi.est[[m]] <- em.optimize(poisRMix, m)[[1]]
  lambda.est[[m]] <- em.optimize(poisRMix, m)[[2]]
  loglik.est[m] <- em.optimize(poisRMix, m)[[3]]
  
  ## maximize loglik for (m+1):
  pi.est[[(m+1)]] <- em.optimize(poisRMix, (m+1))[[1]]
  lambda.est[[(m+1)]] <- em.optimize(poisRMix, (m+1))[[2]]
  loglik.est[(m+1)] <- em.optimize(poisRMix, (m+1))[[3]]
  
  ## compute LRT
  lrt[m] <- -2*(loglik.est[m]-loglik.est[(m+1)])
  
  ## bootstrap:       
  loglik0 <- c()
  loglik1 <- c()
  
  for (b in 1:B){
    cat("bootstrap iter=", b, "\n")
    set.seed(123*b)
    ## generate sample from mixture with optimal par-s for m components:
    Z0 <- rmultinom(n, 1, pi.est[[m]])
    lambda.est. <- lambda.est[[m]]
    poisRMix0 <- matrix(NA, nrow=n, ncol=d_)
    for (d0 in 1:d_) {
      for (p0 in 1:n) {
        z0 <- which.max(Z0[, p0])
        poisRMix0[p0, d0] <- rpois(1, lambda.est.[z0, d0])
      } 
    }  #  end loop for sampled data
    
    ## optimize for m components:
    loglik0[b] <- em.optimize(poisRMix0, m)[[3]]
    ## optimize for (m+1) components:
    loglik1[b] <- em.optimize(poisRMix0, (m+1))[[3]]    
  } # end loop for bootstrap
  
  ## compute bootstrapped LRTs:
  lik.ratio <- -2*(loglik0-loglik1) # vector of length B   
  uq[m] <- sort(lik.ratio)[B*0.95] ## 95%-quantile 
  
  if( (lrt[m] < uq[m]) || (m >= 15))
  {break}
  m <- m + 1
} ## end loop for m
m.est = m


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
  geom_point(aes(color=factor(group)))

# compare with capacities:
lat <- cap$latitude
long <- cap$longitude
capacity.gr <- ifelse(0<=cap$capacity & cap$capacity<=20,"small", ifelse(21<=cap$capacity & cap$capacity<=35,"medium", "large"))
dfpl <- data.frame(lat=lat, long=long, group=capacity.gr)
ggplot(dfpl, aes(x=lat, y=long)) +
  geom_point(aes(color=factor(group)))


grp <- function(z) {
if(z<=14)
  return(1)
if(14 < z & z <= 18 )
  return(2)
if(18 < z & z <= 22 )
  return(3)
if(22 < z & z <= 26 )
  return(4)
if(26 < z & z <= 30 )
  return(5)
if(30 < z & z <= 34 )
  return(6)
if(34 < z & z <= 38 )
  return(7)
if(38 < z & z <= 42 )
  return(8)
if(42 < z & z <= 46 )
  return(9)
if(46 < z & z <= 50 )
  return(10)
if(50 < z & z <= 54 )
  return(11)
if(54 < z & z <= 58 )
  return(12)
if(58 < z & z <= 62 )
  return(13)
if(62 < z & z <= 66 )
  return(14)
if(66 < z & z <= 70 )
  return(15)
}

capgr <- sapply(cap$capacity, FUN=function(z) grp(z))
  
dfpl <- data.frame(lat=lat, long=long, group=capgr)  ## floor(cap$capacity/10)
ggplot(dfpl[-c(236,397,815,898),], aes(x=lat, y=long)) +
  geom_point(aes(color=factor(group)))


## check:
m=2
## optimize for m=2:
pi.est2 <- em.optimize(poisRMix, m)[[1]]
lambda.est2 <- em.optimize(poisRMix, m)[[2]]
loglik.est2 <- em.optimize(poisRMix, m)[[3]]

## optimize for m=3:
pi.est3 <- em.optimize(poisRMix, (m+1))[[1]]
lambda.est3 <- em.optimize(poisRMix, (m+1))[[2]]
loglik.est3 <- em.optimize(poisRMix, (m+1))[[3]]

## compute LRT
lrt <- -2*(loglik.est2-loglik.est3)


## bootstrap:
B=50
loglik.boot0 <- c()
loglik.boot1 <- c()

for (b in 1:B){
  cat("bootstrap iter=", b, "\n")
  set.seed(123*b)
  ## generate sample from mixture with optimal par-s for m components:
  Z0 <- rmultinom(n, 1, pi.est2)
  lambda.est. <- lambda.est2
  poisRMix.boot <- matrix(NA, nrow=n, ncol=d_)
  for (d0 in 1:d_) {
    for (p0 in 1:n) {
      z0 <- which.max(Z0[, p0])
      poisRMix.boot[p0, d0] <- rpois(1, lambda.est.[z0, d0])
    } 
  }  #  end loop for sampled data
  
  ## optimize for m components:
  loglik.boot0[b] <- em.optimize(poisRMix.boot, m)[[3]]
  ## optimize for (m+1) components:
  loglik.boot1[b] <- em.optimize(poisRMix.boot, (m+1))[[3]]    
} # end loop for bootstrap

lrt.boot <- -2*(loglik.boot0-loglik.boot1)
uq <- sort(lrt.boot)[0.95*B]
  
lrt<=uq




## check:
m=3
## optimize for m=3:
pi.est3 <- em.optimize(poisRMix, m)[[1]]
lambda.est3 <- em.optimize(poisRMix, m)[[2]]
loglik.est3 <- em.optimize(poisRMix, m)[[3]]

## optimize for m=4:
pi.est4 <- em.optimize(poisRMix, (m+1))[[1]]
lambda.est4 <- em.optimize(poisRMix, (m+1))[[2]]
loglik.est4 <- em.optimize(poisRMix, (m+1))[[3]]

## compute LRT
lrt <- -2*(loglik.est3-loglik.est4)


## bootstrap:
B=50
loglik.boot0 <- c()
loglik.boot1 <- c()

for (b in 1:B){
  cat("bootstrap iter=", b, "\n")
  set.seed(123*b)
  ## generate sample from mixture with optimal par-s for m components:
  Z0 <- rmultinom(n, 1, pi.est3)
  lambda.est. <- lambda.est3
  poisRMix.boot <- matrix(NA, nrow=n, ncol=d_)
  for (d0 in 1:d_) {
    for (p0 in 1:n) {
      z0 <- which.max(Z0[, p0])
      poisRMix.boot[p0, d0] <- rpois(1, lambda.est.[z0, d0])
    } 
  }  #  end loop for sampled data
  
  ## optimize for m components:
  loglik.boot0[b] <- em.optimize(poisRMix.boot, m)[[3]]
  ## optimize for (m+1) components:
  loglik.boot1[b] <- em.optimize(poisRMix.boot, (m+1))[[3]]    
} # end loop for bootstrap

lrt.boot <- -2*(loglik.boot0-loglik.boot1)
uq <- sort(lrt.boot)[0.95*B]

lrt<=uq




