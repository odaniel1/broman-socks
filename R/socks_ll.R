socks_ll <- function(p,s,k){
  
  # it is not possible to choose more than p+s distinct socks
  if(k > p + s) return(-Inf)
  
  # log likelihood terms for the log-sum-exp trick.
  f <- purrr::map(0:k, function(j){
    (k-j)*log(2) + lchoose(s,j) +lchoose(p,k-j) - lchoose(2*p + s,k)
  })
  
  # the log likelihood
  ll <- matrixStats::logSumExp(f)
  
  return(ll)
}

# socks_ll(p = 3, s = 4, k = 4)

socks_likelihood_grid <- function(p_max,s_max,k, prior = NULL){
  
  grid <- crossing(p = 0:p_max, s = 0:s_max, k = k) %>%
    rowwise() %>%
    mutate(
      ll = socks_ll(p,s,k)
    )
  
  return(grid)
}

