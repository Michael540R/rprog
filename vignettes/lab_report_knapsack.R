## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(knapsack)
library(bench)
library(tidyverse)

set.seed(42)
n <- 2000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
                               v = runif(n = n, 0, 10000))

## -----------------------------------------------------------------------------
brute_force_knapsack<-function(x,W){
  if(!(is.data.frame(x)&ncol(x)==2&all(x>0))) stop("Incorrect input.")
  S<-c()
  L<-list()
  for (i in 1:(2^nrow(x))){
    if (sum(x[as.logical(intToBits(i-1)),][,1])>W) S[i]<-0
    else S[i]<-sum(x[as.logical(intToBits(i-1)),][,2])
    L[[i]]<-as.numeric(rownames(x[as.logical(intToBits(i-1)),]))
    L[[i]]<-as.numeric(rownames(x[as.logical(intToBits(i-1)),]))
  }
  list(value=max(S), elements=L[[which.max(S)]])
}

## -----------------------------------------------------------------------------
brute_force_knapsack(x = knapsack_objects[1: 8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)

## ----eval=FALSE---------------------------------------------------------------
#  bruteforce_st1<-system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))
#  bruteforce_st2<-system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))

## -----------------------------------------------------------------------------
data("bruteforce_st1")
bruteforce_st1
data("bruteforce_st2")
bruteforce_st2

## -----------------------------------------------------------------------------
greedy_knapsack<-function(x,W){
  if(!(is.data.frame(x)&ncol(x)==2&all(x>0))) stop("Incorrect input.")
  x[,3]<-x[,2]/x[,1]
  x<-x[order(x[,3], decreasing=TRUE),]
  S<-0
  L<-c()
  s_w<-0
  for(i in 1:nrow(x)){
    if (s_w+x[i,1]<=W) {
      s_w<-s_w+x[i,1]
      S<-S+x[i,2]
      L<-c(L, as.numeric(rownames(x[i,])))
    }
  }
  list(value=S, elements=L)
}

## -----------------------------------------------------------------------------
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

## ----eval=FALSE---------------------------------------------------------------
#  set.seed(42)
#  n2 <- 1000000
#  knapsack_objects2 <- data.frame(w = sample(1:4000, size = n2, replace = TRUE),
#                                 v <- runif(n = n2, 0, 10000))
#  
#  greedy_st1<-system.time(greedy_knapsack(x = knapsack_objects2, W = 3500))
#  greedy_st2<-system.time(greedy_knapsack(x = knapsack_objects2, W = 2000))

## -----------------------------------------------------------------------------
data("greedy_st1")
greedy_st1
data("greedy_st2")
greedy_st2

## -----------------------------------------------------------------------------
newbruteforce<-function(x,W){
 if(!(is.data.frame(x)&ncol(x)==2&all(x>0))) stop("Incorrect input.")
  y<-sapply(0:(2^(nrow(x))-1), function(k) {
     colSums(x[as.logical(intToBits(k)),])})
  y<-cbind(t(y),1:nrow(t(y)))
  y<-subset(y, y[,1]<=W)
  list(value=max(y[,2]),
       elements= as.numeric(rownames(x[as.logical(intToBits(y[which.max(y[,2]),3]-1)),])))
}

## ----eval=FALSE---------------------------------------------------------------
#  bruteforcebench<-bench::mark(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500),
#              newbruteforce(x = knapsack_objects[1:8,], W = 3500), iterations=1000)

## -----------------------------------------------------------------------------
data("bruteforcebench")
bruteforcebench[,-1]

## -----------------------------------------------------------------------------
newgreedy<-function(x,W){
  x<-as.matrix(x)
  x<-cbind(x,x[,2]/x[,1],1:nrow(x))
  x<-x[order(x[,3], decreasing=TRUE),]
  S<-0
  L<-c()
  s_w<-0
  for(i in 1:nrow(x)){
    if (s_w+x[i,1]<=W) {
      s_w<-s_w+x[i,1]
      S<-S+x[i,2]
      L<-c(L,x[i,4])
    }
  }
  list(value=S, elements=L)
}

## ----eval=FALSE---------------------------------------------------------------
#  greedybench<-bench::mark(greedy_knapsack(x = knapsack_objects[1:800,], W = 3500),
#                           newgreedy(x = knapsack_objects[1:800,], W = 3500), iterations=1000)

## -----------------------------------------------------------------------------
data(greedybench)
greedybench[,-1]

