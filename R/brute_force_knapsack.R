#' Solve Knapsack Problems by Using Brute Force
#'
#' A function that uses the brute force approach for finding the maximum value for a knapsack problem.
#' @param x data.frame with two variables (item weight and item value) that is to be used for the brute force search.
#' @param W limit for the amount of weight that the knapsack can take on.
#'
#' @return list containing the maximum knapsack value and the positions of the associated items.
#' @export
#'
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
#'                              v <- runif(n = n, 0, 10000))
#'
#' brute_force_knapsack(x = knapsack_objects[1: 8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)

brute_force_knapsack<-function(x,W){
  if(!(is.data.frame(x)&ncol(x)==2&all(x>0))) stop("Incorrect input.")
  y<-sapply(0:(2^(nrow(x))-1), function(k) {
     colSums(x[as.logical(intToBits(k)),])})
  y<-cbind(t(y),1:nrow(t(y)))
  y<-subset(y, y[,1]<=W)
  list(value=max(y[,2]),
       elements= as.numeric(rownames(x[as.logical(intToBits(y[which.max(y[,2]),3]-1)),])))
}

