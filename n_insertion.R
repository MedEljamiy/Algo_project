#Simulation des n villes :
villes <- function(n){
  max_x <- 20
  max_y <- 20
  set.seed(9999779)
  villes <- data.frame(x = runif(n, max = max_x),y = runif(n, max = max_y))
  return(villes)
}


##Distance' function: calculates the (symmetrical) matrix of distances between the n cities:
distance <- function(n) {
  return(as.matrix(stats::dist(dplyr::select(villes(n), x, y), diag = TRUE, upper = TRUE)))
}

##Insertion – Find the edge {i, j}, belonging to the partial tour, that
##minimizes cik + ckj − cij. Insert k between i and j.

insertion_cost <- function(x,k,n,order) {
  if (length(order) == 1) {
    cost[0] = x[append(order, k)]
    
  }
  else{
    for (i in 1:n) {
      
      link_add1 = dist_fun(i,k)
      link_add2 =  dist_fun(i+1,k)
      link_remove = dist_fun(i,i+1)
      
      #printf("link_add1,link_add2", link_add1, link_add2, link_remove);
      length<-length(order)
      ##check for infinity
      if(link_add1 == -Inf || link_add2 == -Inf|| link_remove == Inf) 
        cost[i] = -Inf
      else if(link_add1 == Inf || link_add2 == Inf || link_remove == -Inf) 
        cost[i] = Inf
      else    
        cost[i] = link_add1 + link_add2 - link_remove;
    }
    
    
    return(cost)
  }
  
  tsp_insertion <- function(x,N, control = NULL){
    ## since sample has an annoying convenience feature for
    ## lenght(x) == 1
    #choose1 <- function(x) if(length(x) > 1) sample(x, 1) else x
    
    ## this is slower than which.min and which.max but works also
    ## correctly for only values Inf in x and breaks ties randomly
    #choose1_min <- function(X) choose1(which(X == min(X[,1])))
    #choose1_max <- function(x) choose1(which(x == max(x)))
    
    ## n_of_cities
    n <- N
    
    x <- as.matrix(x)
    
    ## place first city
    start = sample(n, 1)
    start <- as.integer(start)
    
    placed <- logical(n)
    placed[start] <- TRUE
    order <- c(start)
    
    ## place other cities
    while(any(placed == FALSE)) {
      ## find city to be inserted
      ks <- which(!placed)
      js <- which(placed)
      
      ##find the city j for which cij (distance or cost from i to j) is minimum
      ## and build the partial tour (i, j)
      
      #m=matrix(rep(0.0,2*length(ks)),length(ks),20)
      #m_1<-x[ks,]
      
      m<-c(x[js,-js])
      #m<-min(c(x[js,-js]))
      
      ## nearest insertion
      winner_index <- which.min(m)[[1]]
      k <- ks[winner_index]
      
      ## do insertion
      placed[k] <- TRUE
      dist_fun <- function(i, j) {vapply(seq_along(i), function(k) distance(N)[i[k], j[k]], numeric(1L))}
      dist_fun(js,k)
      
      if(length(order) == 1) order <- append(order, k)
      else {
        pos <- which.min(insertion_cost(x,k,n,order))
        order <- append(order, k, after = pos)
      }
      return(order)
    }
  }
  