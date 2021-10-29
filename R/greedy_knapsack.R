#' Solve Knapsack Problems by Using a Greedy Heuristic
#'
#' A function that uses a greedy heuristic for estimating the maximum value for a knapsack problem.
#' @param x data.frame with two variables (item weight and item value).
#' @param W limit for the amount of weight that the knapsack can take on.
#'
#' @return list containing the estimated maximum knapsack value and the positions of the associated items.
#' @export
#'
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
#'                              v <- runif(n = n, 0, 10000))
#'
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

greedy_knapsack<-function(x,W){
  if(!(is.data.frame(x)&ncol(x)==2&all(x>0))) stop("Incorrect input.")
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
